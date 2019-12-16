#' Exploring occurrences before creating range maps
#'
#' @description rangemap_explore generates simple figures to visualize species occurrence
#' data in the geographic space.
#'
#' @param occurrences a data.frame containing geographic coordinates of species occurrences,
#' columns must be: Species, Longitude, and Latitude. Geographic coordinates must be in
#' decimal degrees.
#' @param show_countries (logical) if TRUE, ISO 3 country codes will label country polygons.
#' @param graphic_device (logical) if TRUE, a new graphic device is opened to plot the figure.
#' Default = FALSE.
#'
#' @return A simple figure of the species occurrences in a geographical context.
#'
#' @details Base map of countries of the world is a SpatialPolygonDataFrame downloaded from
#' the Natural Earth database using the \code{\link[rnaturalearth]{ne_countries}} function (scale = 50).
#'
#' @export
#'
#' @importFrom sp CRS spTransform plot SpatialPointsDataFrame
#' @importFrom rnaturalearth ne_countries
#' @importFrom rgeos gCentroid
#' @importFrom graphics points text axis
#'
#' @examples
#' suppressWarnings({if(!require(spocc)){
#'   install.packages("spocc")
#'   library(spocc)
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
#' # simple figure of the species occurrence data
#' explore_map <- rangemap_explore(occurrences = occ_g, show_countries = TRUE)
#'
#' #dev.off() # for returning to default par settings

rangemap_explore <- function(occurrences, show_countries = FALSE, graphic_device = FALSE) {

  suppressMessages(library(maptools))

  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument occurrences is necessary to perform the analysis")
  }

  if (dim(occurrences)[2] != 3) {
    stop("occurrences data.frame must have the following columns: \nSpecies, Longitude, and Latitude")
  }

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  # make a spatial object from coordinates
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") # generic
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # bringing maps if polygons false
  data(wrld_simpl)
  polygons1 <- wrld_simpl
  rm("wrld_simpl", pos = ".GlobalEnv")
  polygons1 <- sp::spTransform(polygons1, WGS84)

  # keeping only records in land with better resolution polygon
  polygons <- rnaturalearth::ne_countries(scale = 50)
  polygons <- sp::spTransform(polygons, WGS84)

  occ_sp <- occ_sp[polygons, ]
  occ_poly <- polygons1[occ_sp, ] # polygons with occurrences

  # plot a background map and the range
  ## limits of map
  xlim <- as.numeric(c(occ_poly@bbox[1, 1:2]))
  ylim <- as.numeric(c(occ_poly@bbox[2, 1:2]))

  ## labels
  if (show_countries == TRUE) {
    lab <- polygons@data$adm0_a3_is
    cent <- rgeos::gCentroid(polygons, byid = TRUE)
    cords <- cent@coords
  }

  ## generic plot
  if (graphic_device == TRUE) {
    if (.Platform$OS.type == "unix") {
      quartz()
    } else {
      x11()
    }
  }

  par(mar = c(0, 0, 0, 0), tcl = 0.25)
  sp::plot(polygons, xlim = xlim, ylim = ylim, col = "grey92", xaxt = "n", yaxt = "n") # base map
  points(occ_sp, pch = 21, bg = scales::alpha("yellow", 0.3), cex = 1.2)  #plot my sample sites

  if (show_countries == TRUE) {
    text(x = cords, labels = lab, cex = 0.65)
  }

  axis(side = 1, tcl = 0.3, lwd.ticks = 1,
       mgp = c(0, -1.3, 0), cex.axis = 0.7)
  axis(side = 2, tcl = 0.3, lwd.ticks = 1,
       mgp = c(0, -1.3, 0), cex.axis = 0.7, las = 1)

  box()
}
