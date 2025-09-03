#' Protected Areas in West Africa (Benin, Niger, Burkina Faso, Togo, Nigeria)
#'
#' A spatial dataset of protected areas (PAs) downloaded from the
#' \href{https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA}{World Database on Protected Areas (WDPA)}
#' and clipped to the political boundaries of Benin, Niger, Burkina Faso, Togo, and Nigeria.
#' Only selected designations with biodiversity conservation relevance were retained.
#'
#' @format An `sf` object with polygon geometries and the following attributes:
#' \describe{
#'   \item{NAME}{Character. Name of the protected area.}
#'   \item{DESIG_ENG}{Character. Designation in English (e.g. "National Park").}
#'   \item{DESIG_TYPE}{Character. Type of designation (e.g. National, International).}
#'   \item{ISO3}{Character. Country ISO3 code (e.g. "BEN", "NER").}
#' }
#'
#' @details
#' The dataset was prepared as follows:
#' \enumerate{
#'   \item Downloaded from WDPA (2025).
#'   \item Clipped to the boundaries of Benin, Niger, Burkina Faso, Togo, and Nigeria.
#'   \item Filtered to retain only PAs with designations relevant for connectivity analysis:
#'     \itemize{
#'       \item National Park
#'       \item Classified Forest
#'       \item Partial Fauna Reserve
#'       \item Faunal Reserve
#'       \item Strict Nature Reserve
#'       \item World Heritage Site (natural or mixed)
#'       \item Partial Wildlife Reserve
#'       \item Game Reserve
#'       \item Classified Forest and Total Wildlife Reserve
#'       \item Nature Reserve
#'       \item Wildlife Sanctuary
#'     }
#' }
#'
#' This dataset is suitable for testing connectivity metrics such as Equivalent Connected Area (ECA)
#' and ProtConn.
#'
#' @source \href{https://www.protectedplanet.net/en}{UNEP-WCMC and IUCN (2025) World Database on Protected Areas}.
#'
"pas"

#' Five West African Country Boundaries (Benin, Niger, Burkina Faso, Togo, Nigeria)
#'
#' A spatial dataset of national boundaries for five West African countries,
#' sourced from the \pkg{giscoR} package (Eurostat GISCO database).
#' These boundaries are used as the ecoregion extent for connectivity analyses.
#'
#' @format An `sf` object with polygon geometries and the following attributes:
#' \describe{
#'   \item{CNTR_ID}{Character. Country identifier used by GISCO.}
#'   \item{CNTR_NAME}{Character. Country name in original language (UTF-8 encoded).}
#'   \item{ISO3_CODE}{Character. ISO3 country code (e.g. "BEN").}
#'   \item{NAME_ENGL}{Character. Country name in English.}
#'   \item{FID}{Character. Unique feature identifier.}
#'   \item{geometry}{`sfc_MULTIPOLYGON`. Country boundary geometry in WGS84 CRS.}
#' }
#'
#' @details
#' The dataset was prepared as follows:
#' \enumerate{
#'   \item Downloaded using \pkg{giscoR::gisco_countries()}.
#'   \item Filtered to retain only the following ISO3 country codes:
#'     \itemize{
#'       \item BEN — Benin
#'       \item NER — Niger
#'       \item BFA — Burkina Faso
#'       \item TGO — Togo
#'       \item NGA — Nigeria
#'     }
#'   \item Boundaries are provided in geodetic CRS WGS84 (EPSG:4326).
#' }
#'
#' This dataset provides the regional extent for connectivity metrics
#' (e.g. ECA, ProtConn) and for clipping protected areas.
#'
#' @source \href{https://ec.europa.eu/eurostat/web/gisco}{Eurostat GISCO} via the \pkg{giscoR} package.
#'
"fiwac"
