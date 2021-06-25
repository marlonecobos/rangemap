#' Plot of sp_range* objects
#'
#' @description rangemap_plot generates customizable figures of species range maps
#' using objects produced by other functions of this package.
#'
#' @param sp_range a sp_range object produced with any of the following functions:
#' \code{\link{rangemap_buffer}}, \code{\link{rangemap_boundaries}},
#' \code{\link{rangemap_hull}}, \code{\link{rangemap_enm}}, and
#' \code{\link{rangemap_tsa}}.
#' @param polygons (optional) a SpatialPolygons* object to be used as base for
#' the map. If \code{NULL}, a simplified world map will be used.
#' @param add_EOO (logical) if \code{TRUE}, the extent of occurrence of the
#' species will be added to the figure. Ignored if the \code{sp_range} is product
#' of the \code{\link{rangemap_boundaries}} function and administrative areas were
#' selected only based on names. Default = \code{FALSE}.
#' @param add_occurrences (logical) if \code{TRUE}, the species occurrence records
#' will be added to the figure. Ignored if the \code{sp_range} is product of the
#' \code{\link{rangemap_boundaries}} function and administrative areas were selected
#' only based on names. Default = \code{FALSE}.
#' @param basemap_color color for the basemap (\code{polygons}) to be plotted.
#' Default = "gray93".
#' @param range_color color for the species \code{sp_range} to be plotted.
#' Default = "darkgreen".
#' @param extent_color color for the species extent of occurrence to be plotted.
#' Default = "blue".
#' @param occurrences_color color for the species \code{occurrences} to be plotted.
#' Default = "yellow".
#' @param grid (logical) if \code{TRUE}, labels and grid division ticks will be
#' inserted in \code{grid_sides}. Default = \code{FALSE}.
#' @param grid_sides (character) sides in which the labels will be placed in the
#' figure. Options are the same than for other position character indicators
#' (see details). Default = "bottomleft".
#' @param ylabels_position (numeric) if \code{grid} = \code{TRUE}, separation
#' (in lines) of y axis labels from the axis. Bigger numbers will increase
#' separation. Default = 1.3.
#' @param legend (logical) if \code{TRUE}, a legend of the plotted features will
#' be added to the figure at \code{legend_position}. Default = \code{FALSE}.
#' @param legend_position (numeric or character) site in the figure where the
#' legend will be placed. If numeric, vector of length two indicating x and y
#' coordinates to be used to position the legend. See details for options of
#' character indicators of position. Default = "bottomright".
#' @param northarrow (logical) if \code{TRUE}, a simple north arrow will be placed
#' in \code{northarrow_position}. Default = \code{FALSE}.
#' @param northarrow_position (numeric or character) site in the figure where the
#' north legend will be placed. If numeric, vector of length two indicating x and
#' y coordinates to be used to position the north arrow. See details for options
#' of character indicators of position. Default = "topright".
#' @param scalebar (logical) if \code{TRUE}, a simple scale bar will be inserted
#' in the figure at \code{scalebar_position} with a length of \code{scalebar_length}.
#' Default = \code{FALSE}.
#' @param scalebar_position (numeric or character) site in the figure where the
#' scale bar will be placed. If numeric, vector of length two indicating x and y
#' coordinates to be used to position the scale bar. See details for options of
#' character indicators of position. Default = "bottomleft".
#' @param scalebar_length (numeric) length of the scale bar in km. Using entire
#' numbers divisible for two is recommended. Default = 100.
#' @param zoom (numeric) zoom factor when plotting the species range in a map.
#' Default = 1. Larger values will zoom in into the species range and smaller
#' values will zoom out. A value of 0.5 will duplicate the area that the biggest
#' range is covering.
#'
#' @return
#' A plot of the species range in a geographic context, with some map components
#' defined by the user.
#'
#' @details
#' Position of distinct elements depend on the spatial configuration of the
#' species range. Therefore, their position may need to be changed if the elements
#' are needed. Position options are: "bottomright", "bottomleft", "topleft", and
#' "topright". Numerical descriptions of positions are also allowed.
#'
#' @usage
#' rangemap_plot(sp_range, polygons, add_EOO = FALSE, add_occurrences = FALSE,
#'               basemap_color = "gray93", range_color = "darkgreen",
#'               extent_color = "blue", occurrences_color = "yellow",
#'               grid = FALSE, grid_sides = "bottomleft", ylabels_position = 1.3,
#'               legend = FALSE, legend_position = "bottomright",
#'               northarrow = FALSE, northarrow_position = "topright",
#'               scalebar = FALSE, scalebar_position = "bottomleft",
#'               scalebar_length = 100, zoom = 1)
#'
#' @export
#'
#' @importFrom sp CRS spTransform plot
#' @importFrom scales alpha
#' @importFrom maps map.scale
#' @importFrom graphics points box
#' @import rnaturalearthdata
#'
#' @examples
#' # example data
#' data("cvehull_range", package = "rangemap")
#'
#' # arguments for the species range figure
#' extent <- TRUE
#' occ <- TRUE
#' legend <- TRUE
#'
#' # creating the species range figure
#' rangemap_plot(cvehull_range, add_EOO = extent, add_occurrences = occ,
#'               legend = legend)

rangemap_plot <- function(sp_range, polygons = NULL, add_EOO = FALSE,
                          add_occurrences = FALSE, basemap_color = "gray93",
                          range_color = "darkgreen", extent_color = "blue",
                          occurrences_color = "yellow", grid = FALSE,
                          grid_sides = "bottomleft", ylabels_position = 1.3,
                          legend = FALSE, legend_position = "bottomright",
                          northarrow = FALSE, northarrow_position = "topright",
                          scalebar = FALSE, scalebar_position = "bottomleft",
                          scalebar_length = 100, zoom = 1) {

  # testing for potential errors
  if (missing(sp_range)) {
    stop("'sp_range' must be defined. Check the function's help for more details.")
  }
  if (add_EOO == TRUE) {
    if (class(sp_range)[1] == "sp_range") {
      message("extent of occurrence not found in 'sp_range', setting 'add_EOO' = FALSE")
      add_EOO <- FALSE
    } else {
      if (length(sp_range@extent_of_occurrence) == 0) {
        message("extent of occurrence not found in 'sp_range', setting 'add_EOO' = FALSE")
        add_EOO <- FALSE
      }
    }
  }

  # projection
  f_proj <- sp_range@species_range@proj4string

  # bringing maps if polygons false
  if (is.null(polygons)) {
    requireNamespace(package = "rnaturalearthdata", quietly = TRUE)
    data("countries50", package = "rnaturalearthdata", envir = environment())
    polygons <- countries50
  }
  polygons <- sp::spTransform(polygons, f_proj)


  # getting species range
  range_sp <- sp_range@species_range

  if (add_EOO == TRUE) {
    extent_sp <- sp_range@extent_of_occurrence # species extent of occ
  }

  if (add_occurrences == TRUE) {
    occ_sp <- sp_range@species_unique_records # species records
  }

  # plot a background map and the range
  ## limits of map
  xbox <- as.numeric(c(range_sp@bbox[1, 1:2]))
  ybox <- as.numeric(c(range_sp@bbox[2, 1:2]))

  xlim <- c(xbox[1] - ((((xbox[2] - xbox[1]) * 1/zoom) - (xbox[2] - xbox[1])) / 2),
            xbox[2] + ((((xbox[2] - xbox[1]) * 1/zoom) - (xbox[2] - xbox[1])) / 2))
  ylim <- c(ybox[1] - ((((ybox[2] - ybox[1]) * 1/zoom) - (ybox[2] - ybox[1])) / 2),
            ybox[2] + ((((ybox[2] - ybox[1]) * 1/zoom) - (ybox[2] - ybox[1])) / 2))

  ## range plot
  sp::plot(polygons, xlim = xlim, ylim = ylim, col = basemap_color, xaxt = "n",
           yaxt = "n")
  sp::plot(range_sp, col = scales::alpha(range_color, 0.75), border = FALSE,
           add = TRUE)
  box()

  if (add_EOO == TRUE) {
    sp::plot(extent_sp, col = scales::alpha(extent_color, 0.4), border = FALSE,
             add = TRUE)
  }

  if (add_occurrences == TRUE) {
    points(occ_sp, pch = 21, bg = scales::alpha(occurrences_color, 0.8),
           cex = 0.95)
  }

  ## grid
  if (grid == TRUE) {
    if (grid_sides == "bottomleft") {
      axis(side = 1, tcl = 0.3, lwd.ticks = 1,
           mgp = c(0, -1.3, 0), cex.axis = 0.7)
      axis(side = 2, tcl = 0.3, lwd.ticks = 1,
           mgp = c(0, -ylabels_position, 0), cex.axis = 0.7, las = 1)
    }
    if (grid_sides == "bottomright") {
      axis(side = 1, tcl = 0.3, lwd.ticks = 1,
           mgp = c(0, -1.3, 0), cex.axis = 0.7)
      axis(side = 4, tcl = 0.3, lwd.ticks = 1,
           mgp = c(0, -ylabels_position, 0), cex.axis = 0.7, las = 1)
    }
    if (grid_sides == "topleft") {
      axis(side = 3, tcl = 0.3, lwd.ticks = 1,
           mgp = c(0, -1.3, 0), cex.axis = 0.7)
      axis(side = 2, tcl = 0.3, lwd.ticks = 1,
           mgp = c(0, -ylabels_position, 0), cex.axis = 0.7, las = 1)
    }
    if (grid_sides == "topright") {
      axis(side = 3, tcl = 0.3, lwd.ticks = 1,
           mgp = c(0, -1.3, 0), cex.axis = 0.7)
      axis(side = 4, tcl = 0.3, lwd.ticks = 1,
           mgp = c(0, -ylabels_position, 0), cex.axis = 0.7, las = 1)
    }
  }

  ## north arrow
  if (northarrow == TRUE) {
    north_arrow(position = northarrow_position)
  }

  ## scale bar
  if (scalebar == TRUE) {
    if (class(scalebar_position) == "character") {
      if (scalebar_position == "topright") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.80)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.93)
      }
      if (scalebar_position == "topleft") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.02)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.93)
      }
      if (scalebar_position == "bottomleft") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.02)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.06)
      }
      if (scalebar_position == "bottomright") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.80)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.06)
      }
    } else {
      xscale <- scalebar_position[1]
      yscale <- scalebar_position[2]
    }

    maps::map.scale(x = xscale, y = yscale, relwidth = 0.1, metric = TRUE,
                    ratio = F, cex = 0.8)
  }

  ## legend
  if (legend == TRUE) {
    if (class(legend_position) == "character") {
      if (add_EOO == FALSE & add_occurrences == FALSE) {
        legend(legend_position, legend = c("Species range"),
               bty = "n", inset = 0.07, pt.bg = scales::alpha(range_color, 0.75),
               pch = 22, col = scales::alpha(range_color, 0.75), pt.cex = 2,
               cex = 0.8)
      }
      if (add_EOO == TRUE & add_occurrences == TRUE) {
        legend(legend_position, legend = c("Occurrences", "Species range",
                                           "Extent of occurrence"),
               bty = "n", inset = 0.07, pch = c(21, 22, 22),
               col = c("black", scales::alpha(range_color, 0.75),
                       scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8),
                         scales::alpha(range_color, 0.75),
                         scales::alpha(extent_color, 0.4)),
               pt.cex = c(1, 2, 2), cex = 0.8)
      }
      if (add_EOO == TRUE & add_occurrences == FALSE) {
        legend(legend_position, legend=c("Species range", "Extent of occurrence"),
               bty="n", inset = 0.07, pch = c(22, 22),
               col = c(scales::alpha(range_color, 0.75),
                       scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(range_color, 0.75),
                         scales::alpha(extent_color, 0.4)),
               pt.cex = c(2, 2), cex = 0.8)
      }
      if (add_EOO == FALSE & add_occurrences == TRUE) {
        legend(legend_position, legend=c("Species range", "Ocurrences"),
               bty="n", inset = 0.07, pch = c(21, 22),
               col = c("black", scales::alpha(range_color, 0.75)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8),
                         scales::alpha(range_color, 0.75)),
               pt.cex = c(1, 2), cex = 0.8)
      }
    } else {
      xleg <- legend_position[1]
      yleg <- legend_position[2]
      if (add_EOO == FALSE & add_occurrences == FALSE) {
        legend(x = xleg, y = yleg, legend = c("Species range"),
               bty = "n", inset = 0.07, pt.bg = scales::alpha(range_color, 0.75),
               pch = 22, col = scales::alpha(range_color, 0.75), pt.cex = 2,
               cex = 0.8)
      }
      if (add_EOO == TRUE & add_occurrences == TRUE) {
        legend(x = xleg, y = yleg, legend = c("Occurrences", "Species range",
                                              "Extent of occurrence"),
               bty = "n", inset = 0.07, pch = c(21, 22, 22),
               col = c("black", scales::alpha(range_color, 0.75),
                       scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8),
                         scales::alpha(range_color, 0.75),
                         scales::alpha(extent_color, 0.4)),
               pt.cex = c(1, 2, 2), cex = 0.8)
      }
      if (add_EOO == TRUE & add_occurrences == FALSE) {
        legend(x = xleg, y = yleg, legend=c("Species range", "Extent of occurrence"),
               bty="n", inset = 0.07, pch = c(22, 22),
               col = c(scales::alpha(range_color, 0.75),
                       scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(range_color, 0.75),
                         scales::alpha(extent_color, 0.4)),
               pt.cex = c(2, 2), cex = 0.8)
      }
      if (add_EOO == FALSE & add_occurrences == TRUE) {
        legend(x = xleg, y = yleg, legend=c("Species range", "Ocurrences"),
               bty="n", inset = 0.07, pch = c(21, 22),
               col = c("black", scales::alpha(range_color, 0.75)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8),
                         scales::alpha(range_color, 0.75)),
               pt.cex = c(1, 2), cex = 0.8)
      }
    }
  }
}
