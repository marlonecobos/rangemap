#' Helper to plot multiple ranges on top of environmental layers
#' @param sp_ranges list of SpatialPolygonsDataFrame objects representing
#' species ranges.
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



#' Helper to add north arrow to map plots
#' @description north_arrow plots a North arrow in user defined places in a map.
#'
#' @param position (character or numeric) position of the North arrow. If
#' character, options are: "topright", "topleft", "bottomleft", or "bottomright".
#' Default = "topright".
#' @param xlim (numeric) vector of two numbers indicating the x limits of the
#' plotting area. Default = \code{NULL}.
#' @param ylim (numeric) vector of two numbers indicating the y limits of the
#' plotting area. Default = \code{NULL}.
#'
#' @return
#' Plot of a simple North arrow located in the \code{position} of the plot
#' specified.
#'
#' @export
#' @importFrom graphics polygon
#' @examples
#' # simple plot
#' plot(1:10, 1:10, col = "transparent")
#'
#' # north arrows
#' north_arrow(position = "topright")
#' north_arrow(position = "bottomright")

north_arrow <- function(position = "topright", xlim = NULL, ylim = NULL) {
  if (is.null(xlim) & is.null(ylim)) {
    xlim <- par("usr")[1:2]
    ylim <- par("usr")[3:4]
  } else {
    if(is.null(xlim)) {
      xlim <- par("usr")[1:2]
    }
    if(is.null(ylim)) {
      ylim <- par("usr")[3:4]
    }
  }

  # defining key points
  xdif <- (xlim[2] - xlim[1])
  ydif <- (ylim[2] - ylim[1])

  if (class(position) == "character") {
    if (!position %in% c("topright", "bottomright", "bottomleft", "topleft")) {
      stop("Argument 'position' is not valid, see function's help.")
    }

    if (position == "topright") {
      xmulta <- c(0.91, 0.93, 0.93)
      ymulta <- c(0.90, 0.955, 0.92)
      xmultb <- c(0.93, 0.95, 0.93)
      ymultb <- c(0.955, 0.90, 0.92)

    }
    if (position == "topleft") {
      xmulta <- c(0.05, 0.07, 0.07)
      ymulta <- c(0.90, 0.955, 0.92)
      xmultb <- c(0.07, 0.09, 0.07)
      ymultb <- c(0.955, 0.90, 0.92)
    }
    if (position == "bottomleft") {
      xmulta <- c(0.05, 0.07, 0.07)
      ymulta <- c(0.045, 0.1, 0.065)
      xmultb <- c(0.07, 0.09, 0.07)
      ymultb <- c(0.1, 0.045, 0.065)
    }
    if (position == "bottomright") {
      xmulta <- c(0.91, 0.93, 0.93)
      ymulta <- c(0.045, 0.1, 0.065)
      xmultb <- c(0.93, 0.95, 0.93)
      ymultb <- c(0.1, 0.045, 0.065)
    }

    # coordinates for polygons
    xarrowa <- c((xlim[1] + (xdif * xmulta[1])),
                 (xlim[1] + (xdif * xmulta[2])),
                 (xlim[1] + (xdif * xmulta[3])))
    yarrowa <- c((ylim[1] + (ydif * ymulta[1])),
                 (ylim[1] + (ydif * ymulta[2])),
                 (ylim[1] + (ydif * ymulta[3])))

    xarrowb <- c((xlim[1] + (xdif * xmultb[1])),
                 (xlim[1] + (xdif * xmultb[2])),
                 (xlim[1] + (xdif * xmultb[3])))
    yarrowb <- c((ylim[1] + (ydif * ymultb[1])),
                 (ylim[1] + (ydif * ymultb[2])),
                 (ylim[1] + (ydif * ymultb[3])))
  } else {
    xmulta <- c(0, 0.02, 0.02)
    ymulta <- c(0.075, 0.02, 0.055)
    xmultb <- c(0.02, 0.04, 0.02)
    ymultb <- c(0.02, 0.075, 0.055)

    # coordinates for polygons
    xarrowa <- c((position[1] + (xdif * xmulta[1])),
                 (position[1] + (xdif * xmulta[2])),
                 (position[1] + (xdif * xmulta[3])))
    yarrowa <- c((position[2] - (ydif * ymulta[1])),
                 (position[2] - (ydif * ymulta[2])),
                 (position[2] - (ydif * ymulta[3])))

    xarrowb <- c((position[1] + (xdif * xmultb[1])),
                 (position[1] + (xdif * xmultb[2])),
                 (position[1] + (xdif * xmultb[3])))
    yarrowb <- c((position[2] - (ydif * ymultb[1])),
                 (position[2] - (ydif * ymultb[2])),
                 (position[2] - (ydif * ymultb[3])))
  }

  polygon(xarrowb, yarrowb, border = "black", col = "white")
  polygon(xarrowa, yarrowa, border = "black", col = "black")
}
