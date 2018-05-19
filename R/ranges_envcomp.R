#' Comparison of species ranges in environmental space
#'
#' @description ranges_envcomp generates a two dimensional comparison of a species'
#' ranges created using distinct algortihms, to visualize implications of selecting
#' one of them if environmental conditions are considered.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param variables a RasterStack object of environmental variables that will be used for
#' performing the trend surface analysis.
#' @param ranges a list of objects produced with any of the following functions:
#' \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}.
#' @param export (logical) if TRUE a figure in format = format will be written in the working
#' directory, appart of the returned object.
#' @param format (character) format of the figure that will be written in the working directory
#' if export = TRUE.
#' @param ... other arguments from function \code{\link[Base]{plot}}.
#'
#' @return A figure showing, in the environmental space, the species ranges generated with any
#' of the functions: \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}},
#' \code{\link{rangemap_hull}}, \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}.
#'
#' @details Trend surface analysis Is a method based on low-order polynomials of spatial coordinates
#' for estimating a regular grid of points from scattered observations.
#'
#' @examples

# Dependencies: maps (map), maptools (map2SpatialPolygons), viridis?, ggplot2?, vegan?

ranges_envcomp <- function(occurrences, variables, ranges, export = FALSE, format = "png", ...) {
  # plot a background environment (e.g., the world or a region) in two dimensions (PCs)
  # plot environments of ranges created with distinct algorithms with transparencies
  # plot an ellipsoid
  # insert a legend
  # present a figure with multiple panels?
  # return results
}
