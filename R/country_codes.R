#' List of country names and ISO codes
#'
#' @description A dataset containing codes for identifying countries according
#' to ISO norms.
#'
#' @format A data frame with 247 rows and 4 columns.
#' \describe{
#'   \item{Country_or_Area_Name}{character, names.}
#'   \item{ISO_ALPHA.2_Code}{character, country code.}
#'   \item{ISO_ALPHA.3_Code}{character, country code.}
#'   \item{ISO_Numeric_Code_UN_M49_Numerical_Code}{numeric, country numeric codes.}
#' }
#' @source \url{https://www.nationsonline.org/oneworld/country_code_list.htm}
#'
#' @examples
#' data("country_codes", package = "rangemap")
#' head(country_codes)
"country_codes"


#' List of names of administrative areas form the GADM data base
#'
#' @description A dataset containing names of all the available administrative
#' areas from the GADM data base. Names describe distinct administrative areas
#' in five levels.
#'
#' @format A data frame with 41535 rows and 4 columns.
#' \describe{
#'   \item{ISO3}{character, country code.}
#'   \item{NAME_0}{character, name of administrative areas at level 0.}
#'   \item{NAME_1}{character, name of administrative areas at level 1.}
#'   \item{NAME_2}{character, name of administrative areas at level 2.}
#' }
#' @source \url{https://gadm.org/}
#'
#' @examples
#' data("adm_area_names", package = "rangemap")
#' adm_area_names[1:10, 1:4]
#'
#' # Country names: Level 0
#' unique(adm_area_names$NAME_0)
#'
#' # Provinces of a country: Level 1
#' as.character(unique(adm_area_names[adm_area_names$NAME_0 == "Ecuador", "NAME_1"]))
#'
#' # Some administrative areas at level 2 of India
#' l2_India <- as.character(unique(adm_area_names[adm_area_names$NAME_0 == "India",
#'                                                "NAME_2"]))
"adm_area_names"
