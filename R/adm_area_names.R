#' List of names of administrative areas form the GADM data base
#'
#' @description A dataset containing names of all the avilable administrative areas from the
#' GADM data base. Names describe distict administrative areas in five levels.
#'
#' @format A data frame with 339127 rows and 17 columns.
#' \describe{
#'   \item{ISO3}{character, country code.}
#'   \item{NAME_level}{character, name of administrative areas at levels from 0 to 5.}
#'   \item{TYPE_level}{character, type of administrative areas in native lenguage at
#'   levels from 1 to 5.}
#'   \item{ENGTYPE_level}{character, type of administrative areas in English at
#'   levels from 1 to 5.}
#' }
#' @source \url{https://gadm.org/}
#'
#' @examples
#' data(adm_area_names)
#' View(adm_area_names)
#'
#' # Country names: Level 0
#' unique(adm_area_names$NAME_0)
#'
#' # Provinces of a country: Level 1
#' as.vector(unique(adm_area_names[adm_area_names$NAME_0 == "Ecuador", "NAME_1"]))
#'
#' # Some administrative areas at level 2 of India
#' lev2_India <- as.vector(unique(adm_area_names[adm_area_names$NAME_0 == "India", "NAME_2"]))
#' head(lev2_India, 30)
"adm_area_names"

