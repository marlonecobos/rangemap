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
#' are the same than for other position character indicators (see details). Default = "bottomleft".
#' @param ylabels_position (numeric) if \code{grid} = TRUE, separation (in lines) of y axis labels from
#' the axis. Bigger numbers will increase separation. Default = 2.3.
#' @param legend (logical) if TRUE a legend of the plotted features will be added to the figure in
#' \code{legend_position}.
#' @param legend_position (numeric or character) site in the figure where the north legend will be placed. If
#' numeric, vector of leght two indicating x and y coordinates to be used to position the legend. See
#' details for options of character indicators of position. Default = "bottomright".
#' @param northarrow (logical) if TRUE, a simple north arrow will be placed in \code{northarrow_position}.
#' @param northarrow_position (numeric or character) site in the figure where the north legend will be placed. If
#' numeric, vector of leght two indicating x and y coordinates to be used to position the north arrow. See
#' details for options of character indicators of position. Default = "topright".
#' @param scalebar (logical) if TRUE a simple scale bar will be inserted in the figure at
#' \code{scalebar_position} with a length of \code{scalebar_length}.
#' @param scalebar_position (numeric or character) site in the figure where the north legend will be placed. If
#' numeric, vector of leght two indicating x and y coordinates to be used to position the scale bar. See
#' details for options of character indicators of position. Default = bottomleft".
#' @param scalebar_length (numeric) length of the scale bar in km. Using entire numbers divisble for
#' two is recommended. Default = 100.
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
#' "bottomright", "bottomleft", "topleft", and "topright". Future releases will include numerical options
#' for positioning these elememts.
#'
#' Scale bar is ploted using a modification of the "scalebar" function developed by Tanimura et al. (2007)
#' \url{http://hdl.handle.net/10.18637/jss.v019.c01}.
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
#' rangemap_fig(range, add_extent = extent, add_occurrences = occ,
#'              grid = grid, grid_sides = sides, legend = legend,
#'              northarrow = north)
#'
#' #dev.off() # for returning to default par settings

rangemap_fig <- function(range, polygons, add_extent = FALSE, add_occurrences = FALSE, basemap_color = "grey93",
                         range_color = "darkgreen", extent_color = "blue", occurrences_color = "yellow",
                         grid = FALSE, grid_sides = "bottomleft", ylabels_position = 2.3, legend = FALSE,
                         legend_position = "bottomright", northarrow = FALSE, northarrow_position = "topright",
                         scalebar = FALSE, scalebar_position = "bottomleft", scalebar_length = 100, save_fig = FALSE,
                         name = "range_fig", format = "png", resolution = 300, width = 166, height = 166) {

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
    if (class(northarrow_position) == "character") {
      if (northarrow_position == "topright") {
        xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.9315)
        ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.975)
        xarrow <- c((xlim[1] + ((xlim[2] - xlim[1]) * 0.91)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.93)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.95)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.93)))
        yarrow <- c((ylim[1] + ((ylim[2] - ylim[1]) * 0.91)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.955)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.91)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.92)))
      }
      if (northarrow_position == "topleft") {
        xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.0615)
        ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.975)
        xarrow <- c((xlim[1] + ((xlim[2] - xlim[1]) * 0.04)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.06)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.08)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.06)))
        yarrow <- c((ylim[1] + ((ylim[2] - ylim[1]) * 0.91)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.955)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.91)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.92)))
      }
      if (northarrow_position == "bottomleft") {
        xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.0615)
        ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.085)
        xarrow <- c((xlim[1] + ((xlim[2] - xlim[1]) * 0.04)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.06)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.08)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.06)))
        yarrow <- c((ylim[1] + ((ylim[2] - ylim[1]) * 0.02)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.065)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.02)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.03)))
      }
      if (northarrow_position == "bottomright") {
        xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.9315)
        ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.085)
        xarrow <- c((xlim[1] + ((xlim[2] - xlim[1]) * 0.91)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.93)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.95)),
                    (xlim[1] + ((xlim[2] - xlim[1]) * 0.93)))
        yarrow <- c((ylim[1] + ((ylim[2] - ylim[1]) * 0.02)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.065)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.02)),
                    (ylim[1] + ((ylim[2] - ylim[1]) * 0.03)))
      }
    }else {
      xpos <- northarrow_position[1] + ((xlim[2] - xlim[1]) * 0.0215)
      ypos <- northarrow_position[2]
      xarrow <- c(northarrow_position[1],
                  (northarrow_position[1] + ((xlim[2] - xlim[1]) * 0.02)),
                  (northarrow_position[1] + ((xlim[2] - xlim[1]) * 0.04)),
                  (northarrow_position[1] + ((xlim[2] - xlim[1]) * 0.02)))
      yarrow <- c((northarrow_position[2] - ((ylim[2] - ylim[1]) * 0.065)),
                  (northarrow_position[2] - ((ylim[2] - ylim[1]) * 0.02)),
                  (northarrow_position[2] - ((ylim[2] - ylim[1]) * 0.065)),
                  (northarrow_position[2] - ((ylim[2] - ylim[1]) * 0.055)))
    }

    polygon(xarrow, yarrow, border = "black", col = "grey25")
    text(x = xpos , y = ypos, cex = 0.6, labels = "N")
  }

  ## scale
  if (scalebar == TRUE) {
    scalebarf <- function(loc, length, unit = "km", division.cex = .8,...) {
      if(missing(loc)) stop("loc is missing")
      if(missing(length)) stop("length is missing")
      x <- c(0, length / c(4, 2, 4 / 3, 1), length * 1.1) + loc[1]
      y <- c(0, length / (10 * 3:1)) + loc[2]
      cols <- rep(c("black", "white"),2)
      for (i in 1:4) rect(x[i], y[1], x[i + 1], y[2], col = cols[i])
      for (i in 1:5) segments(x[i], y[2], x[i], y[3])
      n_op <- options()
      options(scipen = 999)
      labels <- (x[c(1, 3)] - loc[1]) /1000
      labels <- append(labels, paste((x[5] - loc[1]) / 1000, unit))
      text(x[c(1, 3, 5)], y[4], labels = labels, adj = .5, cex = division.cex)
      options(n_op)
    }

    if (class(scalebar_position) == "character") {
      if (scalebar_position == "topright"){
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.80)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.93)
      }
      if (scalebar_position == "topleft") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.02)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.93)
      }
      if (scalebar_position == "bottomleft") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.02)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.04)
      }
      if (scalebar_position == "bottomright") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.80)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.04)
      }
    }else {
      xscale <- scalebar_position[1]
      yscale <- scalebar_position[2]
    }

    scalebarf(loc = c(xscale, yscale), length = (scalebar_length * 1000),
             division.cex = 0.7)
  }

  ## legend
  if (legend == TRUE) {
    if (class(legend_position) == "character") {
      if (add_extent == FALSE & add_occurrences == FALSE) {
        legend(legend_position, legend = c("Species range"),
               bty = "n", inset = 0.07, pt.bg = scales::alpha(range_color, 0.75),
               pch = 22, col = scales::alpha(range_color, 0.75), pt.cex = 2, cex = 0.8)
      }
      if (add_extent == TRUE & add_occurrences == TRUE) {
        legend(legend_position, legend = c("Occurrences", "Species range", "Extent of occurrence"),
               bty = "n", inset = 0.07, pch = c(21, 22, 22),
               col = c("black", scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.cex = c(1, 2, 2), cex = 0.8)
      }
      if (add_extent == TRUE & add_occurrences == FALSE) {
        legend(legend_position, legend=c("Species range", "Extent of occurrence"),
               bty="n", inset = 0.07, pch = c(22, 22),
               col = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.cex = c(2, 2), cex = 0.8)
      }
      if (add_extent == FALSE & add_occurrences == TRUE) {
        legend(legend_position, legend=c("Species range", "Ocurrences"),
               bty="n", inset = 0.07, pch = c(21, 22),
               col = c("black", scales::alpha(range_color, 0.75)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75)),
               pt.cex = c(1, 2), cex = 0.8)
      }
    }else {
      xleg <- legend_position[1]
      yleg <- legend_position[2]
      if (add_extent == FALSE & add_occurrences == FALSE) {
        legend(x = xleg, y = yleg, legend = c("Species range"),
               bty = "n", inset = 0.07, pt.bg = scales::alpha(range_color, 0.75),
               pch = 22, col = scales::alpha(range_color, 0.75), pt.cex = 2, cex = 0.8)
      }
      if (add_extent == TRUE & add_occurrences == TRUE) {
        legend(x = xleg, y = yleg, legend = c("Occurrences", "Species range", "Extent of occurrence"),
               bty = "n", inset = 0.07, pch = c(21, 22, 22),
               col = c("black", scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.cex = c(1, 2, 2), cex = 0.8)
      }
      if (add_extent == TRUE & add_occurrences == FALSE) {
        legend(x = xleg, y = yleg, legend=c("Species range", "Extent of occurrence"),
               bty="n", inset = 0.07, pch = c(22, 22),
               col = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.bg = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
               pt.cex = c(2, 2), cex = 0.8)
      }
      if (add_extent == FALSE & add_occurrences == TRUE) {
        legend(x = xleg, y = yleg, legend=c("Species range", "Ocurrences"),
               bty="n", inset = 0.07, pch = c(21, 22),
               col = c("black", scales::alpha(range_color, 0.75)),
               pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75)),
               pt.cex = c(1, 2), cex = 0.8)
      }
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
      if (class(northarrow_position) == "character") {
        if (northarrow_position == "topright") {
          xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.9315)
          ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 1.035)
          xarrow <- c((xlim[1] + ((xlim[2] - xlim[1]) * 0.91)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.93)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.95)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.93)))
          yarrow <- c((ylim[1] + ((ylim[2] - ylim[1]) * 0.97)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 1.015)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 0.97)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 0.98)))
        }
        if (northarrow_position == "topleft") {
          xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.0715)
          ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 1.035)
          xarrow <- c((xlim[1] + ((xlim[2] - xlim[1]) * 0.01)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.03)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.05)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.03)))
          yarrow <- c((ylim[1] + ((ylim[2] - ylim[1]) * 0.97)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 1.015)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 0.97)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 0.98)))
        }
        if (northarrow_position == "bottomleft") {
          xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.0715)
          ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.055)
          xarrow <- c((xlim[1] + ((xlim[2] - xlim[1]) * 0.05)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.07)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.09)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.07)))
          yarrow <- c((ylim[1] + ((ylim[2] - ylim[1]) * -0.01)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 0.035)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * -0.01)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 0.00)))
        }
        if (northarrow_position == "bottomright") {
          xpos <- xlim[1] + ((xlim[2] - xlim[1]) * 0.9315)
          ypos <- ylim[1] + ((ylim[2] - ylim[1]) * 0.055)
          xarrow <- c((xlim[1] + ((xlim[2] - xlim[1]) * 0.91)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.93)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.95)),
                      (xlim[1] + ((xlim[2] - xlim[1]) * 0.93)))
          yarrow <- c((ylim[1] + ((ylim[2] - ylim[1]) * -0.01)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 0.035)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * -0.01)),
                      (ylim[1] + ((ylim[2] - ylim[1]) * 0.00)))
        }
      }else{
        xpos <- northarrow_position[1] + ((xlim[2] - xlim[1]) * 0.0215)
        ypos <- northarrow_position[2]
        xarrow <- c(northarrow_position[1],
                    (northarrow_position[1] + ((xlim[2] - xlim[1]) * 0.02)),
                    (northarrow_position[1] + ((xlim[2] - xlim[1]) * 0.04)),
                    (northarrow_position[1] + ((xlim[2] - xlim[1]) * 0.02)))
        yarrow <- c((northarrow_position[2] - ((ylim[2] - ylim[1]) * 0.065)),
                    (northarrow_position[2] - ((ylim[2] - ylim[1]) * 0.02)),
                    (northarrow_position[2] - ((ylim[2] - ylim[1]) * 0.065)),
                    (northarrow_position[2] - ((ylim[2] - ylim[1]) * 0.055)))
      }

      polygon(xarrow, yarrow, border = "black", col = "grey25")
      text(x = xpos , y = ypos, cex = 0.6, labels = "N")
    }

    ## scale
    if (scalebar == TRUE) {
      if (class(scalebar_position) == "character") {
        if (scalebar_position == "topright"){
          xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.75)
          yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 1.01)
        }
        if (scalebar_position == "topleft") {
          xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.07)
          yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 1.01)
        }
        if (scalebar_position == "bottomleft") {
          xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.07)
          yscale <- ylim[1] + ((ylim[2] - ylim[1]) * -0.01)
        }
        if (scalebar_position == "bottomright") {
          xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.75)
          yscale <- ylim[1] + ((ylim[2] - ylim[1]) * -0.01)
        }
      }else {
        xscale <- scalebar_position[1]
        yscale <- scalebar_position[2]
      }

      scalebarf(loc = c(xscale, yscale), length = (scalebar_length * 1000),
                division.cex = 0.7)
    }

    ## legend
    if (legend == TRUE) {
      if (class(legend_position) == "character") {
        if (add_extent == FALSE & add_occurrences == FALSE) {
          legend(legend_position, legend = c("Species range"),
                 bty = "n", inset = 0.07, pt.bg = scales::alpha(range_color, 0.75),
                 pch = 22, col = scales::alpha(range_color, 0.75), pt.cex = 2, cex = 0.8)
        }
        if (add_extent == TRUE & add_occurrences == TRUE) {
          legend(legend_position, legend = c("Occurrences", "Species range", "Extent of occurrence"),
                 bty = "n", inset = 0.07, pch = c(21, 22, 22),
                 col = c("black", scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
                 pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
                 pt.cex = c(1, 2, 2), cex = 0.8)
        }
        if (add_extent == TRUE & add_occurrences == FALSE) {
          legend(legend_position, legend=c("Species range", "Extent of occurrence"),
                 bty="n", inset = 0.07, pch = c(22, 22),
                 col = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
                 pt.bg = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
                 pt.cex = c(2, 2), cex = 0.8)
        }
        if (add_extent == FALSE & add_occurrences == TRUE) {
          legend(legend_position, legend=c("Species range", "Ocurrences"),
                 bty="n", inset = 0.07, pch = c(21, 22),
                 col = c("black", scales::alpha(range_color, 0.75)),
                 pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75)),
                 pt.cex = c(1, 2), cex = 0.8)
        }
      }else {
        xleg <- legend_position[1]
        yleg <- legend_position[2]
        if (add_extent == FALSE & add_occurrences == FALSE) {
          legend(x = xleg, y = yleg, legend = c("Species range"),
                 bty = "n", inset = 0.07, pt.bg = scales::alpha(range_color, 0.75),
                 pch = 22, col = scales::alpha(range_color, 0.75), pt.cex = 2, cex = 0.8)
        }
        if (add_extent == TRUE & add_occurrences == TRUE) {
          legend(x = xleg, y = yleg, legend = c("Occurrences", "Species range", "Extent of occurrence"),
                 bty = "n", inset = 0.07, pch = c(21, 22, 22),
                 col = c("black", scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
                 pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
                 pt.cex = c(1, 2, 2), cex = 0.8)
        }
        if (add_extent == TRUE & add_occurrences == FALSE) {
          legend(x = xleg, y = yleg, legend=c("Species range", "Extent of occurrence"),
                 bty="n", inset = 0.07, pch = c(22, 22),
                 col = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
                 pt.bg = c(scales::alpha(range_color, 0.75), scales::alpha(extent_color, 0.4)),
                 pt.cex = c(2, 2), cex = 0.8)
        }
        if (add_extent == FALSE & add_occurrences == TRUE) {
          legend(x = xleg, y = yleg, legend=c("Species range", "Ocurrences"),
                 bty="n", inset = 0.07, pch = c(21, 22),
                 col = c("black", scales::alpha(range_color, 0.75)),
                 pt.bg = c(scales::alpha(occurrences_color, 0.8), scales::alpha(range_color, 0.75)),
                 pt.cex = c(1, 2), cex = 0.8)
        }
      }
    }

    invisible(dev.off())
  }
}

