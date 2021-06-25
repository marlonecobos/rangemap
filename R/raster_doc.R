#' An ecological niche model created with Maxent
#'
#' A RasterLayer containing an ecological niche model for the a tick
#' (*Amblyomma americanum*).
#'
#' @format A RasterLayer with 150 rows, 249 columns, and 37350 cells:
#' \describe{
#'   \item{Suitability}{suitability, in probability values.}
#' }
#'
#' @source \url{https://kuscholarworks.ku.edu/handle/1808/26376}
#'
#' @name sp_model
#'
#' @return No return value, used with function \code{\link[raster]{raster}} to
#' bring an example of ecological niche modeling output.
#'
#' @examples
#' model <- raster::raster(system.file("extdata", "sp_model.tif",
#'                                     package = "rangemap"))
#'
#' raster::plot(model)
NULL


#' A set of environmental variables for examples
#'
#' A RasterStack containing four bioclimatic variables downloaded from the
#' WorldClim database 1.4.
#'
#' @format A RasterStack with 180 rows, 218 columns, 39240 cells, and 4 layers:
#' \describe{
#'   \item{variables.1}{bio5.}
#'   \item{variables.2}{bio6.}
#'   \item{variables.3}{bio13.}
#'   \item{variables.4}{bio14.}
#' }
#'
#' @source \url{https://www.worldclim.org/data/v1.4/worldclim14.html}
#'
#' @name variables
#'
#' @return No return value, used with function \code{\link[raster]{stack}} to
#' bring an example of a set of environmental variables.
#'
#' @examples
#' vars <- raster::stack(system.file("extdata", "variables.tif",
#'                                   package = "rangemap"))
#' names(vars) <- c("bio5", "bio6", "bio13", "bio14")
#'
#' raster::plot(vars)
NULL
