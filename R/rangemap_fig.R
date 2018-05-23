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
#' @param add_extent (logical) if TRUE the extent of occurrence of the species will be added to
#' the figure.
#' @param add_occurrences (logical) if TRUE the species occurrence records will be added to
#' the figure.
#' @param grid (logical) if TRUE labels and ticks of coordinates will be inserted in sides.
#' @param sides (character) sides in which the labels will be placed in the figure. Options
#' for this are the same than for other position character options indicators.
#' @param northarrow (logical) if TRUE, a simple north arrow will be placed in northarrow_position.
#' @param northarrow_position (character) site in the figure where the north arrow will be placed.
#' @param scalebar (logical) if TRUE a simple scale bar will be inserted in scalebar_position.
#' @param scalebar_position (character) place for the scale bar insertion.
#'
#' @return A figure of the species distributional range in a geographical context, with the
#' map components defined by the user.
#'
#' @details Position of distinct elements depend on the spatial configuration of the species range.
#' Therefore, their positiuon may need to be changed if the elements are needed. Position options are:
#' "bottomright", "bottomleft", "topleft", and "topright".
#'
#' @examples
#'
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
#' dissolve <- FALSE
#' save <- FALSE
#' countries <- c("PER", "BRA", "COL", "VEN", "ECU", "GUF", "GUY", "SUR", "BOL")
#'
#' range <- rangemap::rangemap_bound(occurrences = occ_g, country_code = countries, boundary_level = level,
#'                                 dissolve = FALSE, save_shp = FALSE)
#'
#' extent <- TRUE
#' occ <- TRUE
#' grid <- TRUE
#' sides <- "bottomleft"
#'
#' range_map <- rangemap_fig(range, add_extent = extent, add_occurrences = occ,
#'                           grid = grid, sides = sides)

# Dependencies: maptools (data(wrld_simpl)),
#               scales (alpha),
#               sp (plot, spTransform, CRS),
#               GISTools ()

rangemap_fig <- function(range, polygons, add_extent = FALSE, add_occurrences = FALSE,
                         grid = FALSE, sides = "bottomleft", northarrow = FALSE,
                         northarrow_position = "topright", scalebar = FALSE,
                         scalebar_position = "bottomleft") {

  suppressMessages(library(maptools))

  # projections
  AEQD <- range$`Species unique records`@proj4string # initial
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
  range_sp <- sp::spTransform(range$`Species range`, ROBIN) # species range
  extent_sp <- sp::spTransform(range$`Extent of occurrence`, ROBIN) # species extent of occ
  occ_sp <- sp::spTransform(range$`Species unique records`, ROBIN) # species records

  # plot a background map and the range
  ## limits of map
  xlim <- as.numeric(c(range_sp@bbox[1, 1:2]))
  ylim <- as.numeric(c(range_sp@bbox[2, 1:2]))

  ## generic plot
  par(mar = c(0, 0, 0, 0), tcl = 0.25)
  sp::plot(polygons, xlim = xlim, ylim = ylim, col = "grey93")
  sp::plot(range_sp, col = scales::alpha("darkgreen", 0.75), border = FALSE, add = TRUE)  #plot the species range
  box()

  # adding other attributes to the map
  ## entent of occurrence
  if (add_extent == TRUE) {
    sp::plot(extent_sp, col = scales::alpha("blue", 0.4), border = FALSE, add = TRUE)
  }

  ## occurrences
  if (add_occurrences == TRUE) {
    points(occ_sp, pch = 21, bg = scales::alpha("yellow", 0.8), cex = 0.95)  #plot my sample sites
  }

  ## grid
  if (grid == TRUE) {
    if (sides == "bottomleft") {
      axis(side = 1)
      axis(side = 2)
    }
    if (sides == "bottomright") {
      axis(side = 1)
      axis(side = 4)
    }
    if (sides == "topleft") {
      axis(side = 3)
      axis(side = 2)
    }
    if (sides == "topright") {
      axis(side = 3)
      axis(side = 4)
    }
  }

  ## north arrow
  #if (northarrow == TRUE) {
  #  GISTools::north.arrow(xb=15.75, yb = 43.25, len = 0.05, lab = "N")
  #}

  ## scale
  #if (scalebar == TRUE) {
  #  maps::map.scale(ratio = FALSE, relwidth = 0.1, cex = 0.6)
  #}

  ## legend
  #if (legend == TRUE) {
  #  if (add_extent == FALSE & add_occurrences == FALSE) {
  #    legend(legend_position, legend = c("Species range"),
  #           bty = "n", inset = 0.05, fill = scales::alpha("darkgreen", 0.75))
  #  }
  #  if (add_extent == TRUE & add_occurrences == TRUE) {
  #    legend(legend_position, legend = c("Species range", "Extent of occurrence", "Ocurrences"),
  #           bty="n", inset = 0.05, pch = c(NA, NA, 21),
  #           bg = c(NA, NA, scales::alpha("yellow", 0.8)),
  #           fill = c(scales::alpha("darkgreen", 0.75), scales::alpha("blue", 0.4),
  #                    NA))
  #  }
  #  if (add_extent == TRUE & add_occurrences == FALSE) {
  #    legend(legend_position, legend=c("Species range", "Extent of occurrence"),
  #           bty="n", inset = 0.05, fill = c(scales::alpha("darkgreen", 0.75),
  #                                           scales::alpha("blue", 0.4)))
  #  }
  #  if (add_extent == FALSE & add_occurrences == TRUE) {
  #    legend(legend_position, legend=c("Species range", "Ocurrences"),
  #           bty="n", inset = 0.05, pch = c(-1, 21),
  #           col = c(NA, scales::alpha("yellow", 0.8)),
  #           fill = c(scales::alpha("darkgreen", 0.75), NA))
  #  }
  #}
}


