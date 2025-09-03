#' Add Transboundary Protected Areas to Analysis
#'
#' This function identifies and includes protected areas located outside an ecoregion boundary
#' that may contribute to connectivity between protected areas within the ecoregion by acting
#' as stepping stones or corridors.
#'
#' @param prepared_data List object returned by [ec_prepare_data()] containing processed
#'   protected areas and ecoregion data
#' @param buffer_km Numeric. Buffer distance in kilometers around the protected area
#'   to search for transboundary protected areas. Default is 100 km
#'
#' @return An sf object containing both ecoregion and transboundary protected areas with
#'   a 'type' column indicating whether each PA is within the ecoregion ("ecoregion") or
#'   outside it ("transboundary")
#'
#' @inherit ec_prepare_data references
#'
#' @examples
#'\donttest{
#' library(dplyr)
#' library(sf)
#'
#' # Benin - treated as single ecoregion
#' benin <- fiwac %>%
#'   dplyr::filter(ISO3_CODE == "BEN")
#'
#' # repare data first
#' prepared_data <- ec_prepare_data(
#'   pa = pas,
#'   ecoregion = benin,
#'   min_area = 1
#' )
#'
#' # Add transboundary PAs with default 70km buffer
#' complete_pa <- ec_add_transboundary_pa(prepared_data, buffer_km = 70)
#' head(complete_pa, 5)
#'}
#'
#' @export
ec_add_transboundary_pa <- function(prepared_data,
                                    buffer_km = 100) {

  pas <- prepared_data$pas
  ecoregion_pas <- prepared_data$ecoregion_pas

  ecoregion_pas_union <- ecoregion_pas %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_make_valid()

  # Apply buffer to PAs in the ecoregion
  pas_buffer <- ecoregion_pas_union %>%
    sf::st_buffer(dist = buffer_km * 1000) %>%
    sf::st_difference(y = ecoregion_pas_union)

  # Find PA out of ecoregion
  pas_out_ecoregion <- pas %>%
    suppressWarnings(sf::st_difference(ecoregion_pas_union)) %>%
    sf::st_as_sf()

  # Find those PAs appearing in the buffer
  in_buffer_idx <- suppressWarnings({
    sf::st_intersects(x = pas_buffer %>% sf::st_make_valid(),
                      y = pas_out_ecoregion%>% sf::st_make_valid())
  })%>% unlist()
  transboundary_pas <- pas_out_ecoregion[in_buffer_idx, ]

  # ecoregion  pa
  ecoregion_pas <- ecoregion_pas %>%
    dplyr::mutate("type" = "ecoregion") %>%
    dplyr::select(dplyr::all_of(c("pa_id", "type")))

  # Return complete pa
  if (nrow(transboundary_pas) > 0) {
    complete_area <- transboundary_pas %>%
      dplyr::mutate("type" = "transboundary") %>%
      dplyr::select(dplyr::all_of(c("pa_id", "type"))) %>%
      dplyr::bind_rows(ecoregion_pas)
  }else{
    complete_area <- ecoregion_pas
  }
  return(complete_area)
}
