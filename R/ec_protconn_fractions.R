#' Calculate ProtConn Fraction Components
#'
#' @description
#' Decomposes the overall Protected Connected Indicator (ProtConn; Saura et al. 2017)
#' into fractions that quantify the relative contribution of different connectivity
#' pathways: movement within individual protected areas, through contiguous protected
#' areas, through unprotected lands, and across international boundaries.
#'
#' @param pa An `sf` object of protected areas as returned by [ec_add_transboundary_pa()].
#' @param prepared_data A list as returned by preprocessing functions [ec_prepare_data()].
#' @inheritParams ec_pc
#' @param protconn Numeric. Overall ProtConn value as calculated by [ec_protconn()].
#'
#' @return
#' A tibble with the following ProtConn fraction components (percentages of ProtConn):
#' \itemize{
#'   \item \code{ProtConn_Prot} – contribution of movement restricted to protected lands.
#'   \item \code{ProtConn_Within} – contribution of movements confined within individual PAs.
#'   \item \code{ProtConn_Contig} – contribution of movements across contiguous (adjacent) PAs.
#'   \item \code{ProtConn_Unprot} – contribution of movements requiring unprotected lands.
#'   \item \code{ProtConn_Trans} – contribution of transboundary linkages between ecoregions
#'   \item \code{Correction_Factor} – adjustment factor applied to avoid overestimation
#'     due to transboundary PA aggregation - double counting.
#' }
#'
#'
#' @references
#' Saura, S., Bastin, L., Battistella, L. et al. (2017).
#' Protected areas in the world’s ecoregions: How well connected are they?
#' Ecological Indicators, 76, 144–158. \doi{10.1016/j.ecolind.2016.12.047}
#'
#' Saura, S. & Rubio, L. (2010).
#' A common currency for the different ways in which patches and links can
#' contribute to habitat availability and connectivity in the landscape.
#' Ecography, 33, 523–537. \doi{10.1111/j.1600-0587.2009.05760.x}
#'
#' @seealso [ec_protconn()], [ec_eca()], [ec_max_product_probability()]
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
#' # Calculate protconn fractions
#' ec_protconn_fractions(pa = complete_pa,
#'                       prepared_data = prepared_data,
#'                       max_prob_matrix = prob_matrix,
#'                       ecoregion_area = benin,
#'                       protconn = 14)
#' }
#'
#' @export
ec_protconn_fractions <- function(pa,
                                  prepared_data,
                                  max_prob_matrix,
                                  ecoregion_area,
                                  protconn) {

  ecoregion_area <- ecoregion_area(ecoregion_area)
  pa_areas <- pa %>%
    dplyr::mutate("area" = dplyr::case_when(type == "transboundary" ~ 0,
                                          TRUE ~ as.numeric(sf::st_area(.)) / 1e6)) %>%
    dplyr::pull("area")

  #1. ProtConn[Prot] - movement only through protected lands within ecoregion
  protconn_prot <- 100 * (100 * sqrt(sum(pa_areas^2)) / ecoregion_area) / protconn

  #2. ProtConn[Within] - movement within individual PAs
  raw_pa_area <- prepared_data$raw_pas %>%
    dplyr::mutate("area" = as.numeric(sf::st_area(.)) / 1e6) %>%
    dplyr::pull("area")

  pa_areas <- pa %>%
    dplyr::filter(type == "transboundary") %>%
    dplyr::mutate("area" = as.numeric(sf::st_area(.)) / 1e6) %>%
    dplyr::pull("area")

  correction_factor <- sqrt(sum(pa_areas)/sum(raw_pa_area))
  protconn_within <- 100*((100*sqrt(sum(raw_pa_area^2))/ecoregion_area)*correction_factor)/protconn

  #3. ProtConn[Contig] - movement through contiguous PAs
  protconn_contig <- protconn_prot - protconn_within

  #4. ProtConn[Trans] - transboundary contribution
  # Calculate ECA with transboundary
  eca_with_trans <- ec_eca(pa = pa, max_prob_matrix)
  # Calculate ECA without transboundary PAs
  eca_no_trans <- ec_eca(pa = pa, max_prob_matrix, with_transboundary = FALSE)
  protconn_trans <- 100 * (100 * (eca_with_trans - eca_no_trans) / ecoregion_area) / protconn

  # ProtConn[Unprot] - movement through unprotected lands
  # ProtConn [Within] + ProtConn [Contig] + ProtConn [Unprot] + ProtConn [Trans] = 100
  protconn_unprot <- 100 - protconn_within - protconn_contig - protconn_trans

  # Build list
  prot_faction <- list(
    ProtConn_Prot = protconn_prot,
    ProtConn_Within = protconn_within,
    ProtConn_Contig = protconn_contig,
    ProtConn_Unprot = protconn_unprot,
    ProtConn_Trans = protconn_trans,
    Correction_Factor = round(correction_factor, 3)
  )

  return(dplyr::bind_cols(prot_faction))
}
