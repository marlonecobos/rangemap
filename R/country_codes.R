#' List of country names and ISO codes
#'
#' @description A dataset containing codes for identifying countries according to ISO
#' norms.
#'
#' @format A data frame with 247 rows and 4 columns.
#' \describe{
#'   \item{Country_or_Area_Name}{character, names.}
#'   \item{ISO_ALPHA.2_Code}{character, country code.}
#'   \item{ISO_ALPHA.3_Code}{character, country code.}
#'   \item{ISO_Numeric_Code_UN_M49_Numerical_Code}{numeric, country numeric codes.}
#' }
#' @source \url{http://www.nationsonline.org/oneworld/country_code_list.htm}
#'
#' @examples
#' data(country_codes)
#' View(country_codes)
"country_codes"
