#' Example SpatialPolygonsDataFrame of a species range
#'
#' @description A SpatialPolygonsDataFrame representing the distribution of a
#' species from North America.
#'
#' @format SpatialPolygonsDataFrame with 1 feature.
#' \describe{
#'   \item{features}{SpatialPolygons, 1.}
#'   \item{data.frame}{ID = 1.}
#' }
#'
#' @examples
#' data("spdf_range", package = "rangemap")
#' spdf_range
"spdf_range"


#' Example of sp_range* object based on buffers
#'
#' @description A sp_range_iucn object containing the results of the function
#' \code{\link{rangemap_buffer}}.
#'
#' @format sp_range_iucn with 6 slots.
#' \describe{
#'   \item{name}{character, name identifying the origing of the object.}
#'   \item{summary}{data.frame, summary of results.}
#'   \item{species_range}{SpatialPolygonsDataFrame of species range.}
#'   \item{species_unique_records}{SpatialPointsDataFrame of species occurrences.}
#'   \item{extent_of_occurrence}{SpatialPolygonsDataFrame of species extent of occurrence.}
#'   \item{area_of_occupancy}{SpatialPolygonsDataFrame of species area of occupancy.}
#' }
#'
#' @examples
#' data("buffer_range", package = "rangemap")
#' summary(buffer_range)
"buffer_range"


#' Example of sp_range* object based on convex hulls
#'
#' @description A sp_range_iucn object containing the results of the function
#' \code{\link{rangemap_hull}}.
#'
#' @format sp_range_iucn with 6 slots.
#' \describe{
#'   \item{name}{character, name identifying the origing of the object.}
#'   \item{summary}{data.frame, summary of results.}
#'   \item{species_range}{SpatialPolygonsDataFrame of species range.}
#'   \item{species_unique_records}{SpatialPointsDataFrame of species occurrences.}
#'   \item{extent_of_occurrence}{SpatialPolygonsDataFrame of species extent of occurrence.}
#'   \item{area_of_occupancy}{SpatialPolygonsDataFrame of species area of occupancy.}
#' }
#'
#' @examples
#' data("cxhull_range", package = "rangemap")
#' summary(cxhull_range)
"cxhull_range"



#' Example of sp_range* object based on concave hulls
#'
#' @description A sp_range_iucn object containing the results of the function
#' \code{\link{rangemap_hull}}.
#'
#' @format sp_range_iucn with 6 slots.
#' \describe{
#'   \item{name}{character, name identifying the origing of the object.}
#'   \item{summary}{data.frame, summary of results.}
#'   \item{species_range}{SpatialPolygonsDataFrame of species range.}
#'   \item{species_unique_records}{SpatialPointsDataFrame of species occurrences.}
#'   \item{extent_of_occurrence}{SpatialPolygonsDataFrame of species extent of occurrence.}
#'   \item{area_of_occupancy}{SpatialPolygonsDataFrame of species area of occupancy.}
#' }
#'
#' @examples
#' data("cvehull_range", package = "rangemap")
#' summary(cvehull_range)
"cvehull_range"
