#' Prepare Protected Areas Data for ProtConn Analysis
#'
#' This function prepares protected areas (PA) and ecoregion data for connectivity analysis
#' by processing spatial layers, filtering by minimum area, and performing spatial intersections.
#' The function handles both file paths and sf objects as input.
#'
#' @param pa Either a file path to a protected areas shapefile or an sf object containing
#'   protected areas polygons
#' @param ecoregion Either a file path to an ecoregions shapefile or an sf object containing
#'   ecoregion boundaries
#' @param min_area Numeric. Minimum area threshold in square kilometers for protected areas
#'   to be included in the analysis. Default is 1 km^2
#' @param projection_crs Coordinate reference system for area calculations. Can be a crs object
#'   or input string for [sf::st_crs()]. If NULL, uses the CRS of the ecoregions layer.
#'   Default is NULL
#'
#' @return A list containing four elements:
#'   `pas`: sf object with dissolved protected areas polygons
#'   `raw_pas`: sf object with original (non-dissolved) protected areas intersected with ecoregions
#'   `ecoregions`: sf object with ecoregions data
#'   `ecoregion_pas`: sf object with protected areas clipped to ecoregion boundaries
#'
#' @references
#' Saura, S., Bastin, L., Battistella, L., Mandrici, A., & Dubois, G. (2017).
#' Protected areas in the world's ecoregions: How well connected are they?
#' Ecological Indicators, 76, 144-158. \doi{10.1016/j.ecolind.2016.12.047}
#'
#' @export
#' @examples
#' library(dplyr)
#' # Benin - treated as single ecoregion
#' benin <- fiwac %>%
#'   dplyr::filter(ISO3_CODE == "BEN")
#'
#' # Protected area
#' prot_area <- pas
#'
#' # Prepare data
#' prepared_data <- ec_prepare_data(
#'   pa = prot_area,
#'   ecoregion = benin,
#'   min_area = 1
#' )
#' @export
ec_prepare_data <- function(pa,
                          ecoregion,
                          min_area = 1,
                          projection_crs = NULL){

  # Load protected areas and ecoregions
  if ("sf" %in% class(pa)) {
    pa_data <- pa %>% sf::st_make_valid()
  }else{
    pa_data <- sf::st_read(pa, quiet = TRUE) %>% sf::st_make_valid()
  }

  if ("sf" %in% class(ecoregion)) {
    ecoregions <- ecoregion %>% sf::st_make_valid()
  }else{
    ecoregions <- sf::st_read(ecoregion, quiet = TRUE) %>%
      sf::st_make_valid()
  }

  if (!is.null(projection_crs)) {
    pa_data <- pa_data %>%
      sf::st_transform(crs = sf::st_crs(ecoregions))
  }

  raw_pas <- pa_data %>%
    sf::st_transform(crs = sf::st_crs(ecoregions)) %>%
    sf::st_make_valid() %>%
    dplyr::mutate("area_km2" = as.numeric(sf::st_area(.)) / 1e6) %>%
    dplyr::filter("area_km2" >= min_area) %>%
    suppressWarnings(sf::st_intersection(y = ecoregions))%>%
    dplyr::mutate(pa_id = 1:nrow(.))

  # Dissolve overlapping PAs to avoid double counting
  pa_dissolved <- sf::st_union(pa_data) %>%
    sf::st_as_sf() %>%
    sf::st_cast(to = "POLYGON") %>%
    sf::st_make_valid()

  # Calculate areas and filter by minimum size
  pa_dissolved <- pa_dissolved %>%
    dplyr::mutate("area_km2" = as.numeric(sf::st_area(.)) / 1e6) %>%
    dplyr::filter("area_km2" >= min_area) %>%
    dplyr::mutate("pa_id" = 1:nrow(.))

  # Perform intersection

  pa_eco_intersect <- suppressWarnings(sf::st_intersection(pa_dissolved, ecoregions)) %>%
    dplyr::mutate("area_km2" = as.numeric(sf::st_area(.)) / 1e6)

  return(list(pas = pa_dissolved %>% dplyr::as_tibble() %>% sf::st_as_sf(),
              raw_pas = raw_pas,
              ecoregions = ecoregions %>% dplyr::as_tibble() %>% sf::st_as_sf(),
              ecoregion_pas = pa_eco_intersect%>% dplyr::as_tibble() %>% sf::st_as_sf()))
}
