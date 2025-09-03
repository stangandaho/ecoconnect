#' Calculate the Protected Connected Indicator
#'
#' @description
#' The Protected Connected (ProtConn) indicator quantifies the percentage of
#' a given ecoregion that is both protected and functionally connected,
#' considering species' dispersal abilities and potential transboundary linkages
#' between protected areas (PAs). It extends the concept of protected area (PA)
#' coverage by incorporating connectivity into conservation effectiveness
#' assessments.
#'
#' @param pa An `sf` object of protected areas. Must include a `type` column
#'   identifying `"transboundary"` PAs as retuned by [ec_add_transboundary_pa()].
#' @inheritParams ec_pc
#' @param include_fraction Logical. If `TRUE`, ProtConn fractions are also
#'   calculated following Saura et al. (2017). Default: `FALSE`.
#' @param prepared_data Optional. A prepared dataset (see [ec_prepare_data()])
#' containing ecoregions and PA information. Required if `include_fraction = TRUE`.
#'
#' @return
#' A tibble with the following indicators (all expressed as percentages):
#' \describe{
#'   \item{ProtConn}{Protected Connected indicator: percentage of the ecoregion
#'   that is protected and connected.}
#'   \item{Prot}{Protected area coverage: percentage of the ecoregion that is
#'   protected, regardless of connectivity.}
#'   \item{ProtUnconn}{Protected unconnected: the difference between coverage
#'   (Prot) and connected coverage (ProtConn).}
#'   \item{RelConn}{Relative connectivity: proportion of protected area that is
#'   connected, i.e. `ProtConn / Prot × 100`.}
#' }
#'
#' If `include_fraction = TRUE`, additional columns are included that decompose
#' ProtConn into fractions (intra-PA connectivity, connectivity through direct
#' links, connectivity through transboundary PAs, etc.).
#'
#' @details
#' ProtConn is based on the Equivalent Connected Area (ECA) concept (Saura et al.
#' 2011; Saura et al. 2017). The ECA integrates PA area and dispersal
#' probabilities into a measure of the amount of habitat that is both protected
#' and connected. ProtConn expresses ECA as a percentage of the ecoregion area,
#' enabling comparisons across regions of different size.
#'
#' Formally:
#' \deqn{ProtConn = 100 \times \frac{ECA}{A_{L}}}
#' where:
#' \itemize{
#'   \item \eqn{ECA} is the Equivalent Connected Area (\eqn{\text{km}^2}),
#'   \item \eqn{A_{L}} is the total area of the ecoregion (\eqn{\text{km}^2}).
#' }
#'
#' @inherit ec_dispersal_probability references
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(sf)
#' # Benin - treated as single ecoregion
#' benin <- fiwac %>%
#'   dplyr::filter(ISO3_CODE == "BEN")
#'
#' # Protected area
#' prot_area <- pas
#'
#' # repare data first
#' prepared_data <- ec_prepare_data(
#'   pa = prot_area,
#'   ecoregion = benin,
#'   min_area = 1
#' )
#' # Add transboundary PAs with default 70km buffer
#' complete_pa <- ec_add_transboundary_pa(prepared_data, buffer_km = 70)
#'
#' # Calculate edge-to-edge distances
#' pair_dist <- ec_pa_distances(complete_pa)
#' prob_matrix <- ec_dispersal_probability(pair_dist, median_dispersal = 10)
#' pstar <- ec_max_product_probability(prob_matrix)
#'
#' ec_protconn(pa = complete_pa,
#'             max_prob_matrix = pstar,
#'             ecoregion_area = benin)
#' }
#'
#' @export

ec_protconn <- function(pa,
                        max_prob_matrix,
                        include_fraction = FALSE,
                        prepared_data = NULL,
                        ecoregion_area) {

  if (!"sf" %in% class(pa)) {
    cli::cli_abort("pa must be a simple feature object")
  }

  ecoregion_area <- ecoregion_area(ecoregion_area)

  # Create area vector for all PAs (internal + transboundary)
  pa_areas <- pa %>%
    dplyr::mutate("area" = dplyr::case_when(type == "transboundary" ~ 0,
                                          TRUE ~ as.numeric(sf::st_area(.)) / 1e6)) %>%
    dplyr::pull("area")

  if (length(pa_areas) != nrow(max_prob_matrix)) {
    cli::cli_abort("Areas must have the same lenght as vertice size in max_prob_matrix")
  }

  # Calculate ECA including transboundary effects
  eca_total <- ec_eca(pa = pa, max_prob_matrix)

  # Calculate ProtConn as percentage of ecoregion area
  protconn <- 100 * eca_total / ecoregion_area

  # Calculate PA Coverage (Prot)
  prot <- 100 * sum(pa_areas) / ecoregion_area

  # Calculate Protected Unconnected
  protunconn <- prot - protconn

  # Calculate Relative Connectivity
  relconn <- ifelse(prot > 0, 100 * protconn / prot, 0)

  metrics <- list(
    ProtConn = protconn,
    Prot = prot,
    ProtUnconn = protunconn,
    RelConn = relconn
  )

  # Calculate ProtConn fractions
  if (include_fraction) {
    if (is.null(prepared_data)) {
      cli::cli_abort("prepared_data cannot be missed if include_fraction is TRUE")
    }
    fractions <- ec_protconn_fractions(pa = pa,
                                       prepared_data = prepared_data,
                                       max_prob_matrix = max_prob_matrix,
                                       ecoregion_area = ecoregion_area,
                                       protconn = protconn)
    all_metrics <- dplyr::bind_cols(metrics) %>% dplyr::bind_cols(fractions)
  }else{
    all_metrics <- dplyr::bind_cols(metrics)
  }

  return(all_metrics)
}
