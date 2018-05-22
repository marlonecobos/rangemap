#' Figures of species range maps
#'
#' @description rangemap_fig generates customizable figures of species range maps
#' using the objects produced by other function of this package.
#'
#' @param range an object produced with any of the following functions:
#' \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}.
#' @param polygons a SpatialPolygon object to be used as base map for plotting the species range.
#' If not provided, a simplified world map will be used.
#' @param xlim two element numeric vector giving a range of longitudes, expressed in degrees,
#' to which drawing should be restricted. Longitude is measured in degrees east of Greenwich,
#' so that, in particular, locations in the USA have negative longitude. If fill = TRUE, polygons
#' selected by region must be entirely inside the xlim range. The default value of this argument
#' spans the entire longitude range of the database.
#' @param ylim two element numeric vector giving a range of latitudes, expressed in degrees,
#' to which drawing should be restricted. Latitude is measured in degrees north of the equator,
#' so that, in particular, locations in the USA have positive latitude. If fill = TRUE, polygons
#' selected by region must be entirely inside the ylim range. The default value of this argument
#' spans the entire latitude range of the database.
#' @param grid (character) units to be used in the grid. It can be "null", "measured", or "graticules".
#' @param appearance (character) type of grid if grid is different than "null". It can be "labels",
#' "grids", or "ticks".
#' @param sides (character) sides in which the labels will be placed in the figure.
#' @param northarrow (logical) if TRUE, a simple north arrow will be placed in northarrow_position.
#' @param northarrow_position (character) site in the figure where the north arrow will be placed.
#' @param scalebar (logical) if TRUE a simple scale bar will be inserted in scalebar_position.
#' @param scalebar_position (character) place for the scale bar insertion.
#' @param save_figure (logical) if TRUE a figure in figure_fromat format will be written in the working
#' directory, appart of the returned object.
#' @param figure_fromat (character) format of the figure that will be written in the working directory
#' if save_figure = TRUE, default = "png". Options include: "bmp", "jpeg", "png", "tif", and "pdf".
#' @param figure_size (numeric) vector of two values in mm, c(width, height), of the size in which the
#' figure will be saved. Recommended widths for scientific publications are 79, 107.5, and 165 mm.
#' Default = c(165, 165)
#' @param figure_resolution (numeric) value of the resolution (ppi) in which the figure will be saved,
#' default = 300.
#' @param ... other arguments from function \code{\link[Base]{plot}}.
#'
#' @return A figure of the species distributional range in a geographical context, with the
#' map components defined by the user.
#'
#' @details Position of distinct elements depend on the spatial configuration of the species range.
#' Therefore, their positiuon may need to be changed if the elements are needed. Position options are
#' the same than in keywords for representing x and y in the function \code{\link[Base]{plot}}
#'
#' @examples

# Dependencies: ggplot2?,
#               maps (map),
#               maptools (map2SpatialPolygons),
#               scales (alpha),
#               sp (SpatialPointsDataFrame, spTransform),
#               viridis?,


rangemap_fig <- function(range, polygons, xlim, ylim, grid = "measured", appearance = "labels",
                         sides = "bottomleft", northarrow = FALSE, northarrow_position = "topleft",
                         scalebar = FALSE, scalebar_position = "bottomleft",
                         save_figure = FALSE, figure_fromat = "png", figure_size = c(165, 165),
                         figure_resolution = 300, ...) {

  # bringing maps if polygons false

  # project for mantaining shapes

  # plot a background map (e.g., the world or a close up to a region)
  # plot the object created git previous functions
  # add north arrow if asked
  # add grid arrow if asked
  # add leggend if asked
  # add text if asked
  # save figure if asked
}


