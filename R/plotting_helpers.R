#' Helper to plot multiple ranges on top of environmental layers
#' @param sp_ranges list of SpatalPolygonsDataFrames representing species ranges.
#' @param sp_records a SpatialPointsDataFrame of species occurrences.
#' @param variable a RasterLayer representing an environmental variable.
#' @param range_colors vector of colors for borders of species ranges. If
#' \code{NULL}, the default, distinct levels of gray will be used.
#' @param color_variable a color palette (a vector of continuous colors generated
#' by functions like heat.colors). If \code{NULL}, the default, rev(terrain.colors(255))
#' will be used.
#' @param xlim the x limits (x1, x2) of the plot. \code{NULL} indicates that
#' the range of values to be plotted will define limits.
#' @param ylim the y limits (x1, x2) of the plot.
#' @return
#' A plot showing species ranges on top of a environmental variable.
#' @usage
#' plot_ranges(sp_ranges, sp_records = NULL, variable, range_colors = NULL,
#'             color_variable = NULL, xlim = NULL, ylim = NULL)
#' @export
#' @importFrom raster plot minValue maxValue
#' @importFrom sp plot
#' @importFrom graphics points
#' @examples
#' # example data
#' data("buffer_range", package = "rangemap")
#' data("cxhull_range", package = "rangemap")
#'
#' ranges <- list(buffer_range@species_range, cxhull_range@species_range)
#'
#' var <- raster::stack(system.file("extdata", "variables.tif",
#'                                  package = "rangemap"))[[1]]
#'
#' # plotting
#' plot_ranges(ranges, variable = var)

plot_ranges <- function(sp_ranges, sp_records = NULL, variable, range_colors = NULL,
                        color_variable = NULL, xlim = NULL, ylim = NULL) {
  if (missing(sp_ranges)) {
    stop("Argument 'sp_ranges' must be defined. See function's help for details.")
  }
  if (missing(variable)) {
    stop("Argument 'variable' must be defined See function's help for details.")
  }

  # par settings
  #opar <- par(no.readonly = TRUE)
  #on.exit(par(opar))

  # range and variable colors
  if (is.null(range_colors)) {
    range_colors <- gray.colors(length(sp_ranges))
  }

  if (is.null(color_variable)) {
    color_variable <- rev(terrain.colors(255))
  }

  # raster
  if (!is.null(xlim) & !is.null(ylim)) {
    raster::plot(variable, col = color_variable, xlim = xlim,
                 ylim = ylim, legend = FALSE, axes = FALSE)
  } else {
    if (!is.null(xlim)) {
      raster::plot(variable, col = color_variable, xlim = xlim,
                   legend = FALSE, axes = FALSE)
    }
    if (!is.null(ylim)) {
      raster::plot(variable, col = color_variable, ylim = ylim,
                   legend = FALSE, axes = FALSE)
    }
    if (is.null(xlim) & is.null(ylim)) {
      raster::plot(variable, col = color_variable, legend = FALSE, axes = FALSE)
    }
  }

  # ranges
  for (j in 1:length(sp_ranges)) {
    sp::plot(sp_ranges[[j]], col = NA, border = range_colors[j], add = TRUE)
  }

  if (!is.null(sp_records)) {
    points(sp_records, pch = 20, cex = 1)
  }

  # legend
  var_range <- c(ceiling(raster::minValue(variable)),
                 floor(raster::maxValue(variable)))

  raster::plot(variable, legend.only = TRUE, col = color_variable,
               legend.width = 1, legend.shrink = 0.8,
               axis.args = list(at = c(var_range[1], var_range[2]),
                                labels = c(var_range[1], var_range[2]),
                                line = NULL, cex.axis = 0.8),
               legend.args = list(text = names(variable), side = 4, font = 2,
                                  line = 0.5, cex = 0.9))
}
