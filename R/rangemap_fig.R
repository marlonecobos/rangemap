#' Figures of species range maps
#'
#' @description rangemap_fig generates customizable figures of species range maps
#' using objects produced by other functions of this package.
#'
#' @param range an object produced with any of the following functions:
#' \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}.
#' @param polygons a SpatialPolygon object to be used as base map for plotting the species
#' range. If not provided, a simplified world map will be used.
#' @param add_extent (logical) if TRUE, the extent of occurrence of the species will be
#' added to the figure. Ignored if the \code{range} is product of the \code{\link{rangemap_bound}}
#' function and administrative areas were selected only based on names. Default = FALSE.
#' @param add_occurrences (logical) if TRUE, the species occurrence records will be added
#' to the figure. Ignored if the \code{range} is product of the \code{\link{rangemap_bound}}
#' function and administrative areas were selected only based on names. Default = FALSE.
#' @param basemap_color color for the basemap (\code{polygons}) to be ploted in the figure.
#' Default = "grey93".
#' @param range_color color for the species \code{range} to be ploted in the figure.
#' Default = "darkgreen".
#' @param extent_color color for the species extent of occurrence to be ploted in the figure.
#' Default = "blue".
#' @param occurrences_color color for the species \code{occurrences} to be ploted in the
#' figure. Default = "yellow".
#' @param grid (logical) if TRUE, labels and grid division ticks will be inserted in
#' \code{grid_sides}. Default = FALSE.
#' @param grid_sides (character) sides in which the labels will be placed in the figure.
#' Options are the same than for other position character indicators (see details). Default =
#' "bottomleft".
#' @param ylabels_position (numeric) if \code{grid} = TRUE, separation (in lines) of y axis
#' labels from the axis. Bigger numbers will increase separation. Default = 1.3.
#' @param legend (logical) if TRUE, a legend of the plotted features will be added to the
#' figure at \code{legend_position}. Default = FALSE.
#' @param legend_position (numeric or character) site in the figure where the legend will
#' be placed. If numeric, vector of leght two indicating x and y coordinates to be used to
#' position the legend. See details for options of character indicators of position. Default =
#' "bottomright".
#' @param northarrow (logical) if TRUE, a simple north arrow will be placed in
#' \code{northarrow_position}. Default = FALSE.
#' @param northarrow_position (numeric or character) site in the figure where the north
#' legend will be placed. If numeric, vector of leght two indicating x and y coordinates
#' to be used to position the north arrow. See details for options of character indicators
#' of position. Default = "topright".
#' @param scalebar (logical) if TRUE, a simple scale bar will be inserted in the figure at
#' \code{scalebar_position} with a length of \code{scalebar_length}. Default = FALSE.
#' @param scalebar_position (numeric or character) site in the figure where the scale bar
#' will be placed. If numeric, vector of leght two indicating x and y coordinates to be used
#' to position the scale bar. See details for options of character indicators of position.
#' Default = "bottomleft".
#' @param scalebar_length (numeric) length of the scale bar in km. Using entire numbers
#' divisble for two is recommended. Default = 100.
#' @param zoom (numeric) zoom factor when ploting the species range in a map. Default = 1.
#' Values lower than 1 will zoom in into the species range and values bigger than 1 will
#' zoom out. A value of 2 will duplicate the area that the figure is covering.
#' @param save_fig (logical) if TRUE, the figure will be written in the working directory.
#' Default = FALSE.
#' @param name (character) if \code{save_fig} = TRUE, name of the figure to be exported.
#' Default = "range_fig".
#' @param format (character) if \code{save_fig} = TRUE, format in which the figure will be
#' written. Options include "bmp", "png", "jpeg", "tiff", and "pdf". Default = "png".
#' @param resolution (numeric) if \code{save_fig} = TRUE, resolution (ppi) in wich the figure
#' will be exported. Default = 300.
#' @param width (numeric) if \code{save_fig} = TRUE, width of the figure in mm. Default = 166.
#' @param height (numeric) if \code{save_fig} = TRUE, height of the figure in mm. Default = 166.
#'
#' @return A figure of the species distributional range in a geographical context, with map
#' components defined by the user.
#'
#' @details Ranges should be generated with any of the functions: \code{\link{rangemap_buff}},
#' \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}}, \code{\link{rangemap_enm}}, and
#' \code{\link{rangemap_tsa}}.
#'
#' Position of distinct elements depend on the spatial configuration of the species range.
#' Therefore, their position may need to be changed if the elements are needed. Position
#' options are: "bottomright", "bottomleft", "topleft", and "topright". Numerical descriptions
#' of positions are also allowed.
#'
#' @usage
#' rangemap_fig(range, polygons, add_extent = FALSE, add_occurrences = FALSE,
#'     basemap_color = "grey93", range_color = "darkgreen", extent_color = "blue",
#'     occurrences_color = "yellow", grid = FALSE, grid_sides = "bottomleft",
#'     ylabels_position = 1.3, legend = FALSE, legend_position = "bottomright",
#'     northarrow = FALSE, northarrow_position = "topright", scalebar = FALSE,
#'     scalebar_position = "bottomleft", scalebar_length = 100, zoom = 1,
#'     save_fig = FALSE, name = "range_fig", format = "png", resolution = 300,
#'     width = 166, height = 166)
#'
#' @export
#'
#' @importFrom sp CRS spTransform plot
#' @importFrom rnaturalearth ne_countries
#' @importFrom scales alpha
#' @importFrom maps map.scale
#' @importFrom graphics points
#'
#' @examples
#' suppressWarnings({if(!require(spocc)){
#' install.packages("spocc")
#' library(spocc)
#' }})
#'
#' # getting the data from GBIF
#' occs <- occ(query = "Dasypus kappleri", from = "gbif",
#'             limit = 1000)$gbif$data[[1]]
#'
#' # keeping only georeferenced records
#' occ_g <- occs[!is.na(occs$latitude) & !is.na(occs$longitude),
#'               c("name", "longitude", "latitude")]
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
#' legend <- TRUE
#' north <- TRUE
#'
#' # creating the species range figure
#' rangemap_fig(range, add_extent = extent, add_occurrences = occ,
#'              legend = legend, northarrow = north)
#'
#' #dev.off() # for returning to default par settings

rangemap_fig <- function(range, polygons, add_extent = FALSE, add_occurrences = FALSE,
                         basemap_color = "grey93", range_color = "darkgreen", extent_color = "blue",
                         occurrences_color = "yellow", grid = FALSE, grid_sides = "bottomleft",
                         ylabels_position = 1.3, legend = FALSE, legend_position = "bottomright",
                         northarrow = FALSE, northarrow_position = "topright", scalebar = FALSE,
                         scalebar_position = "bottomleft", scalebar_length = 100, zoom = 1,
                         save_fig = FALSE, name = "range_fig", format = "png", resolution = 300,
                         width = 166, height = 166) {

  # testing for potential errors
  if (missing(range)) {
    stop("range must exist. Check the function's help for more details.")
  }

  # projections
  #WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") # generic
  f_proj <- range$Species_range@proj4string

  # bringing maps if polygons false
  if (missing(polygons)) {
    polygons <- rnaturalearth::ne_countries(scale = 50)
  }
  polygons <- sp::spTransform(polygons, f_proj)


  # getting species range
  if (class(range) == "list") {
    range_sp <- range$Species_range # species range
  }
  if (class(range) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {
    range_sp <- range # species range
  }

  if (add_extent == TRUE) {
    extent_sp <- range$Extent_of_occurrence # species extent of occ
  }

  if (add_occurrences == TRUE) {
    occ_sp <- range$Species_unique_records # species records
  }

  # plot a background map and the range
  ## limits of map
  xbox <- as.numeric(c(range_sp@bbox[1, 1:2]))
  ybox <- as.numeric(c(range_sp@bbox[2, 1:2]))

  xlim <- c(xbox[1] - ((((xbox[2] - xbox[1]) * zoom) -
                          (xbox[2] - xbox[1])) / 2),
            xbox[2] + ((((xbox[2] - xbox[1]) * zoom) -
                          (xbox[2] - xbox[1])) / 2))
  ylim <- c(ybox[1] - ((((ybox[2] - ybox[1]) * zoom) -
                          (ybox[2] - ybox[1])) / 2),
            ybox[2] + ((((ybox[2] - ybox[1]) * zoom) -
                          (ybox[2] - ybox[1])) / 2))

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
    north_arrow(position = northarrow_position, xlim, ylim, Ncex = 0.6)
  }

  ## scale bar
  if (scalebar == TRUE) {
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

    maps::map.scale(x = xscale, y = yscale, relwidth = 0.1, metric = TRUE,
                    ratio = F, cex = 0.8)
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
      north_arrow(position = northarrow_position, xlim, ylim, Ncex = 0.6)
    }

    ## scale bar
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

      maps::map.scale(x = xscale, y = yscale, relwidth = 0.1, metric = TRUE,
                      ratio = F, cex = 0.8)
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


#' North arrow for map plots

#' @description north_arrow plots a North arrow in user defined places in a map.
#'
#' @param position (character or numeric) position of the North arrow. If character, options
#' are: "topright", "topleft", "bottomleft", or "bottomright". Default = "topright".
#' @param xlim (numeric) vector of two numbers indicating the x limits of the plotting area.
#' @param ylim (numeric) vector of two numbers indicating the y limits of the plotting area.
#' @param Ncex (numeric) cex for the North label (N).
#' @param exproting (logical) whether or not the map will be exported as a figure.
#'
#' @export
#' @importFrom graphics polygon

north_arrow <- function(position = "topright", xlim, ylim, Ncex = 0.6) {

  if (class(position) == "character") {
    if (position == "topright") {
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
    if (position == "topleft") {
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
    if (position == "bottomleft") {
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
    if (position == "bottomright") {
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
    xpos <- position[1] + ((xlim[2] - xlim[1]) * 0.0215)
    ypos <- position[2]
    xarrow <- c(position[1],
                (position[1] + ((xlim[2] - xlim[1]) * 0.02)),
                (position[1] + ((xlim[2] - xlim[1]) * 0.04)),
                (position[1] + ((xlim[2] - xlim[1]) * 0.02)))
    yarrow <- c((position[2] - ((ylim[2] - ylim[1]) * 0.065)),
                (position[2] - ((ylim[2] - ylim[1]) * 0.02)),
                (position[2] - ((ylim[2] - ylim[1]) * 0.065)),
                (position[2] - ((ylim[2] - ylim[1]) * 0.055)))
  }

  polygon(xarrow, yarrow, border = "black", col = "grey25")
  #text(x = xpos , y = ypos, cex = 0.6, labels = "N")
}
