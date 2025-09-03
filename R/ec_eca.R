#' Calculate Equivalent Connected Area (ECA)
#'
#' This function calculates the Equivalent Connected Area (ECA) metric, which represents
#' the size (area) that a single protected area would need to have to provide the same
#' amount of reachable protected land as the current network of protected areas.
#'
#' @param pa sf object containing protected areas with a 'type' column distinguishing
#'   between "ecoregion" and "transboundary" areas
#' @param max_prob_matrix Numeric matrix of maximum product probabilities between all
#'   protected areas
#' @param with_transboundary Logical. If TRUE, includes transboundary areas in the
#'   calculation (with area = 0 to avoid double-counting coverage). If FALSE, considers
#'   only areas within the ecoregion. Default is TRUE
#'
#' @return Numeric value representing the ECA in square kilometers
#'
#' @details
#' The ECA metric is calculated as:
#' \deqn{ECA = \sqrt{\sum_{i=1}^{n} \sum_{j=1}^{n} a_i \times a_j \times p_{ij}^*}}{%
#' ECA = sqrt(sum_i sum_j (a_i * a_j * p_ij*))}
#'
#' where:
#' \itemize{
#'   \item \eqn{a_i} and \eqn{a_j} are the areas of protected areas i and j
#'   \item \eqn{p_{ij}^*} is the maximum probability of connection between areas i and j
#'   \item For transboundary areas, \eqn{a_i} = 0 to avoid counting area outside the ecoregion
#' }
#'
#' ECA provides an intuitive measure of connectivity that can be directly compared to
#' the total protected area coverage. Higher ECA values indicate better connectivity
#' within the protected area network.
#'
#' @references
#' Saura, S., Bastin, L., Battistella, L., Mandrici, A., & Dubois, G. (2017).
#' Protected areas in the world's ecoregions: How well connected are they?
#' Ecological Indicators, 76, 144-158.
#'
#' Saura, S., Estreguil, C., Mouton, C., & Rodríguez-Freire, M. (2011). Network analysis
#' to assess landscape connectivity trends: application to European forests (1990-2000).
#' Ecological Indicators, 11(2), 407-416. \doi{10.1016/j.ecolind.2010.06.011}
#'
#' @examples
#' \donttest{
#'
#' library(dplyr)
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
#'
#' # Add transboundary PAs with default 70km buffer
#' complete_pa <- ec_add_transboundary_pa(prepared_data, buffer_km = 70)
#'
#' # Calculate edge-to-edge distances
#' dist_matrix <- ec_pa_distances(complete_pa, method = "edge")
#'
#' # For a more mobile species (30km median dispersal)
#' prob_matrix <- ec_dispersal_probability(dist_matrix, median_dispersal = 30)
#'
#' # Limit to maximum 3 stepping stones
#' max_prob <- ec_max_product_probability(prob_matrix, max_steps = 3)
#'
#' # Calculate ECA including transboundary effects
#' eca_value <- ec_eca(
#'   pa = complete_pa,
#'   max_prob_matrix = max_prob,
#'   with_transboundary = TRUE
#' )
#'
#' }
#' @export
ec_eca <- function(pa,
                   max_prob_matrix,
                   with_transboundary = TRUE) {

  if (!"sf" %in% class(pa)) {
    cli::cli_abort("pa must be a simple feature object")
  }

  if (with_transboundary) {
    max_matrix <- max_prob_matrix
    pa_areas <- pa %>%
      dplyr::mutate("pa_area" = dplyr::case_when(type == "transboundary" ~ 0,
                                               TRUE ~ as.numeric(sf::st_area(.)) / 1e6))%>%
      dplyr::pull("pa_area")
  }else{
    not_trans <- which(pa$type != "transboundary")
    max_matrix <- max_prob_matrix[not_trans, not_trans]
    pa_areas <- pa[not_trans, ] %>%
      dplyr::mutate("pa_area" = as.numeric(sf::st_area(.)) / 1e6)%>%
      dplyr::pull("pa_area")
  }

  if (length(pa_areas) != nrow(max_matrix)) {
    cli::cli_abort("Areas must have the same lenght as vertice size in max_prob_matrix")
  }

  n <- length(pa_areas)
  eca_sum <- 0; proba <- 0

  for (i in 1:n) {
    for (j in 1:n) {
      eca_sum <- eca_sum + (pa_areas[i] * pa_areas[j] * max_matrix[i, j])
      proba <- max_matrix[i, j]
    }
  }

  eca <- sqrt(eca_sum)
  return(eca)
}
