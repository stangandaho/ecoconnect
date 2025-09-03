#' Caluclate ecoregion area
#' @noRd
#' @keywords internal
ecoregion_area <- function(ecoregion_area) {
  if ("sf" %in% class(ecoregion_area)) {
    ecoregion_area <- ecoregion_area %>%
      dplyr::mutate("area" = as.numeric(sf::st_area(.)) / 1e6) %>%
      dplyr::pull("area")
  }else{
    ecoregion_area
  }
  return(ecoregion_area)
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
magrittr::`%>%`

#' Pipe operator
#'
#' @name %<>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
#' @export
magrittr::`%<>%`
