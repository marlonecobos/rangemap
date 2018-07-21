#' Figures of species range maps
#'
#' @description rangemap_fig generates customizable figures of species range maps
#' using objects produced by other functions of this package.
#'
#' @param range an object produced with any of the following functions:
#' \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}.
#' @param polygons a SpatialPolygon object to be used as base map for plotting the species range.
#' If not provided, a simplified world map will be used.
#' @param add_extent (logical) if TRUE the extent of occurrence of the species will be added to
#' the figure. Ignored if the \code{range} is product of the \code{\link{rangemap_bound}} function
#' and administrative areas were selected only based on names.
#' @param add_occurrences (logical) if TRUE the species occurrence records will be added to
#' the figure. Ignored if the \code{range} is product of the \code{\link{rangemap_bound}} function
#' and administrative areas were selected only based on names.
#' @param basemap_color color for the basemap (\code{polygons}) to be ploted in the figure.
#' Default = "grey93".
#' @param range_color color for the species \code{range} to be ploted in the figure.
#' Default = "darkgreen".
#' @param extent_color color for the species extent of occurrence to be ploted in the figure.
#' Default = "blue".
#' @param occurrences_color color for the species \code{occurrences} to be ploted in the figure.
#' Default = "yellow".
#' @param grid (logical) if TRUE labels and grid division ticks will be inserted in \code{grid_sides}.
#' @param grid_sides (character) sides in which the labels will be placed in the figure. Options
#' are the same than for other position character options indicators (see details).
#' @param ylabels_position (numeric) if \code{grid} = TRUE, separation (in lines) of y axis labels from
#' the axis. Bigger numbers will increase separation. Default = 1.8.
#' @param legend (logical) if TRUE a legend of the plotted features will be added to the figure in
#' \code{legend_position}.
#' @param legend_position (character) site in the figure where the north legend will be placed. See
#' options in details.
#' @param northarrow (logical) if TRUE, a simple north arrow will be placed in \code{northarrow_position}.
#' @param northarrow_position (character) site in the figure where the north arrow will be placed.
#' @param scalebar (logical) if TRUE a simple scale bar will be inserted at the bottom left part
#' of the figure.
#' @param save_fig (logical) if TRUE the figure will be written in the working directory. Default = FALSE.
#' @param name (character) if \code{save_fig} = TRUE, name of the figure to be exported. Default = "range_fig".
#' @param format (character) if \code{save_fig} = TRUE, format in which the figure will be written. Options
#' include "bmp", "png", "jpeg", "tiff", and "pdf". Default = "png".
#' @param resolution (numeric) if \code{save_fig} = TRUE, resolution in ppi in wich the figure will be exported.
#' Default = 300.
#' @param width (numeric) if \code{save_fig} = TRUE, width of the figure in mm. Default = 166.
#' @param height (numeric) if \code{save_fig} = TRUE, height of the figure in mm. Default = 166.
#'
#' @return A figure of the species distributional range in a geographical context, with map components
#' defined by the user.
#'
#' @details Position of distinct elements depend on the spatial configuration of the species range.
#' Therefore, their position may need to be changed if the elements are needed. Position options are:
#' "bottomright", "bottomleft", "topleft", and "topright". This version of the function only allows
#' scale bars in the "bottomleft" part of the figure, and cannot be changed; thus, avoid using this
#' position for other components is recommended.
#'
#' @examples
#' if(!require(rgbif)){
#'   install.packages("rgbif")
#'   library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' species <- name_lookup(query = "Dasypus kappleri",
#'                        rank="species", return = "data") # information about the species
#'
#' occ_count(taxonKey = species$key[14], georeferenced = TRUE) # testing if keys return records
#'
#' key <- species$key[14] # using species key that return information
#'
#' occ <- occ_search(taxonKey = key, return = "data") # using the taxon key
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
#'              c("name", "decimalLongitude", "decimalLatitude")]
#'
#' level <- 0
#' adm <- "Ecuador"
#' dissolve <- FALSE
#' save <- FALSE
#' countries <- c("PER", "BRA", "COL", "VEN", "ECU", "GUF", "GUY", "SUR", "BOL")
#'
#' # creating the species range map
#' range <- rangemap_bound(occurrences = occ_g, country_code = countries, adm_areas = adm,
#'                         boundary_level = level, dissolve = dissolve, save_shp = save)
#'
#' # arguments for the species range figure
#' extent <- TRUE
#' occ <- TRUE
#' grid <- TRUE
#' sides <- "bottomleft"
#' legend <- TRUE
#' north <- TRUE
#'
#' # creating the species range figure
#' range_map <- rangemap_fig(range, add_extent = extent, add_occurrences = occ,
#'                           grid = grid, grid_sides = sides, legend = legend,
#'                           northarrow = north)
#'
#' #dev.off() # for returning to default par settings

rangemap_fig <- function(range, polygons, add_extent = FALSE, add_occurrences = FALSE, basemap_color = "grey93",
                         range_color = "darkgreen", extent_color = "blue", occurrences_color = "yellow",
                         grid = FALSE, grid_sides = "bottomleft", ylabels_position = 1.8, legend = FALSE,
                         legend_position = "bottomright", northarrow = FALSE, northarrow_position = "topright",
                         scalebar = FALSE, save_fig = FALSE, name = "range_fig", format = "png", resolution = 300,
                         width = 166, height = 166) {

  suppressMessages(library(maptools))

  # projections
  if (class(range) == "list") {
    AEQD <- range$Species_range@proj4string # initial
  }
  if (class(range) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
    AEQD <- range@proj4string # initial
  }
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") # generic
  ROBIN <- sp::CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") # for pretty maps

  # bringing maps if polygons false
  if (missing(polygons)) {
    data(wrld_simpl)
    polygons <- wrld_simpl
  }
  rm("wrld_simpl", pos = ".GlobalEnv")

  # project for mantaining shapes
  polygons <- sp::spTransform(polygons, ROBIN) # base map
  if (class(range) == "list") {
    range_sp <- sp::spTransform(range$Species_range, ROBIN) # species range
  }
  if (class(range) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
    range_sp <- sp::spTransform(range, ROBIN) # species range
  }

  if (add_extent == TRUE) {
    extent_sp <- sp::spTransform(range$Extent_of_occurrence, ROBIN) # species extent of occ
  }

  if (add_occurrences == TRUE) {
    occ_sp <- sp::spTransform(range$Species_unique_records, ROBIN) # species records
  }

  # plot a background map and the range
  ## limits of map
  xlim <- as.numeric(c(range_sp@bbox[1, 1:2]))
  ylim <- as.numeric(c(range_sp@bbox[2, 1:2]))

  ## generic plot
  par(mar = c(0, 0, 0, 0))
  sp::plot(polygons, xlim = xlim, ylim = ylim, col = basemap_color, xaxt = "n", yaxt = "n")
  sp::plot(range_sp, col = scales::alpha(range_color, 0.75), border = FALSE, add = TRUE)  #plot the species range
  box()

  # adding other attributes to the map
  ## entent of occurrence
  if (add_extent == TRUE) {
    sp::plot(extent_sp, col = scales::alpha(extent_color, 0.4), border = FALSE, add = TRUE)
  }

  ## occurrences
  if (add_occurrences == TRUE) {
    points(occ_sp, pch = 21, bg = scales::alpha(occurrences_color, 0.8), cex = 0.95)  #plot my sample sites
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
    if (northarrow_position == "topright"){
      xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.97)
      ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.97)
    }
    if (northarrow_position == "topleft") {
      xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.03)
      ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.97)
    }
    if (northarrow_position == "bottomleft") {
      xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.03)
      ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.03)
    }
    if (northarrow_position == "bottomright") {
      xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.97)
      ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.03)
    }
    text(x = xpos , y = ypos, cex = 1.6, labels = "N")
  }

  ## scale
  if (scalebar == TRUE) {
    suppressWarnings(maps::map.scale(ratio = FALSE, relwidth = 0.1,
                                     cex = 0.55))
  }

  ## legend
  if (legend == TRUE) {
    if (legend_position == "bottomleft" & scalebar == TRUE) {
      warning("Legend and scale bar overlap.")
    }
    if (add_extent == FALSE & add_occurrences == FALSE) {
      legend(legend_position, legend = c("Species range"),
             bty = "n", inset = 0.05, pt.bg = scales::alpha(range_color, 0.75),
             pch = 22, col = scales::alpha(range_color, 0.75), pt.cex = 2, cex = 0.8)
    }
    if (add_extent == TRUE & add_occurrences == TRUE) {
      legend(legend_position, legend = c("Occurrences", "Species range", "Extent of occurrence"),
             bty = "n", inset = 0.05, pch = c(21, 22, 22),
             col = c("black", scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
             pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
             pt.cex = c(1, 2, 2), cex = 0.8)
    }
    if (add_extent == TRUE & add_occurrences == FALSE) {
      legend(legend_position, legend=c("Species range", "Extent of occurrence"),
             bty="n", inset = 0.05, pch = c(22, 22),
             col = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
             pt.bg = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
             pt.cex = c(2, 2), cex = 0.8)
    }
    if (add_extent == FALSE & add_occurrences == TRUE) {
      legend(legend_position, legend=c("Species range", "Ocurrences"),
             bty="n", inset = 0.05, pch = c(21, 22),
             col = c("black", scales::alpha(range_color, 0.75)),
             pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75)),
             pt.cex = c(1, 2), cex = 0.8)
    }
  }

  # saving the figure
  if (save_fig == TRUE) {
    cat("\nWriting figure in working directory.\n")
    if (format == "bmp") {
      bmp(filename = paste(name, "bmp", sep = "."), width = width, height = height,
          units = "mm", res = resolution)
    }
    if (format == "png") {
      png(filename = paste(name, "png", sep = "."), width = width, height = height,
          units = "mm", res = resolution)
    }
    if (format == "jpeg") {
      jpeg(filename = paste(name, "jpg", sep = "."), width = width, height = height,
          units = "mm", res = resolution)
    }
    if (format == "tiff") {
      tiff(filename = paste(name, "tif", sep = "."), width = width, height = height,
           units = "mm", res = resolution)
    }
    if (format == "pdf") {
      pdf(file = paste(name, "pdf", sep = "."), width = width)
    }

    par(mar = c(0, 0, 0, 0), cex = 0.85)
    sp::plot(polygons, xlim = xlim, ylim = ylim, col = basemap_color, xaxt = "n", yaxt = "n")
    sp::plot(range_sp, col = scales::alpha(range_color, 0.75), border = FALSE, add = TRUE)  #plot the species range
    box()

    # adding other attributes to the map
    ## entent of occurrence
    if (add_extent == TRUE) {
      sp::plot(extent_sp, col = scales::alpha(extent_color, 0.4), border = FALSE, add = TRUE)
    }

    ## occurrences
    if (add_occurrences == TRUE) {
      points(occ_sp, pch = 21, bg = scales::alpha(occurrences_color, 0.8), cex = 0.95)  #plot my sample sites
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
      if (northarrow_position == "topright"){
        xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.97)
        ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.97)
      }
      if (northarrow_position == "topleft") {
        xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.03)
        ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.97)
      }
      if (northarrow_position == "bottomleft") {
        xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.03)
        ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.03)
      }
      if (northarrow_position == "bottomright") {
        xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.97)
        ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.03)
      }
      text(x = xpos , y = ypos, cex = 1.3, labels = "N")
    }

    ## scale
    if (scalebar == TRUE) {
      suppressWarnings(maps::map.scale(ratio = FALSE, relwidth = 0.1,
                                       cex = 0.55))
    }

    ## legend
    if (legend == TRUE) {
      if (legend_position == "bottomleft" & scalebar == TRUE) {
        warning("Legend and scale bar overlap.")
      }
      if (add_extent == FALSE & add_occurrences == FALSE) {
        legend(legend_position, legend = c("Species range"),
               bty = "n", inset = 0.05, pt.bg = scales::alpha(range_color, 0.75),
               pch = 22, col = scales::alpha(range_color, 0.75), pt.cex = 2, cex = 0.8)
      }
      if (add_extent == TRUE & add_occurrences == TRUE) {
        legend(legend_position, legend = c("Occurrences", "Species range", "Extent of occurrence"),
               bty = "n", inset = 0.05, pch = c(21, 22, 22),
               col = c("black", scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.cex = c(1, 2, 2), cex = 0.8)
      }
      if (add_extent == TRUE & add_occurrences == FALSE) {
        legend(legend_position, legend=c("Species range", "Extent of occurrence"),
               bty="n", inset = 0.05, pch = c(22, 22),
               col = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.cex = c(2, 2), cex = 0.8)
      }
      if (add_extent == FALSE & add_occurrences == TRUE) {
        legend(legend_position, legend=c("Species range", "Ocurrences"),
               bty="n", inset = 0.05, pch = c(21, 22),
               col = c("black", scales::alpha(range_color, 0.75)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75)),
               pt.cex = c(1, 2), cex = 0.8)
      }
    }

    invisible(dev.off())
  }
}

