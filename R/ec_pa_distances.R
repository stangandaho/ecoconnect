#' Calculate Inter-Protected Area Distances
#'
#' This function calculates distances between all pairs of protected areas in a dataset.
#'
#' @param pa sf object containing protected areas, typically the output from
#'   [ec_add_transboundary_pa()]
#' @param method Character string specifying the distance calculation method.
#'   Options are:
#'   \itemize{
#'     \item "edge": calculates minimum edge-to-edge distances between PA boundaries
#'     \item "centroid": calculates distances between PA centroids
#'   }
#'   Default is "edge"
#'
#' @return A numeric matrix where element represents the distance in kilometers
#'   between protected area i and protected area j. The matrix is symmetric with
#'   zeros on the diagonal
#'
#' @details
#' Edge-to-edge distances are more appropriate for connectivity analysis as they represent
#' the minimum distance an organism would need to travel between protected areas.
#' Centroid-to-centroid distances may overestimate actual travel distances, especially
#' for large or elongated protected areas.
#'
#' @examples
#' library(dplyr)
#' library(sf)
#'
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
#' dist_matrix[1:5, 1:5]
#' @export
ec_pa_distances <- function(pa, method = "edge") {

  if (!method %in% c("edge", "centroid")) {
    cli::cli_abort("Method must be one of {c('edge', 'centroid')}")
  }

  if (method == "edge") {
    pair_dist <- sf::st_distance(pa)
  }else if(method == "centroid"){
    pair_dist <- suppressWarnings(sf::st_distance(sf::st_centroid(pa) %>% sf::st_as_sf()))
  }

  # Convert from meters to kilometers
  dist_matrix <- as.matrix(pair_dist %>% units::drop_units()) / 1000

  return(dist_matrix)
}
