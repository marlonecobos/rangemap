#' Get a simplified SpatialPolygons of the world
#' @param which (charatcer) name of type of SpatialPolygons to be obtained. Options
#' are: "simple" and "simplest"; default = "simplest".
#' @export
#' @importFrom sp CRS SpatialPolygonsDataFrame spTransform
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @examples
#' map <- simple_wmap()

simple_wmap <- function(which = "simplest") {
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  if (which == "simplest") {
    data("wrld_simpl", package = "maptools")
    polygons <- sp::spTransform(wrld_simpl, WGS84)
  } else {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE)

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1])
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84)
    polygons <- sp::SpatialPolygonsDataFrame(polygons, data = data.frame(ID = 1:length(polygons)),
                                             match.ID = FALSE)
  }
  return(polygons)
}


#' Get SpatialPolygons of countries at distinct administrative levels
#' @param country_code (character) code of country or countries of interest.
#' @param boundary_level (numeric) level of admisnistrative division to be considered.
#' @param keep_data (logical) whether or not to keep downloaded files. Default = FALSE.
#' @export
#' @importFrom raster getData
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @examples
#' map <- simple_wmap()

GADM_spoly <- function(country_code, boundary_level, keep_data = FALSE) {
  bounds <- list()
  for (i in 1:length(country_code)){
    bounds[[i]] <- raster::getData('GADM', country = country_code[i], level = boundary_level)
  }
  polygon <- do.call("rbind", bounds)
  polygon@data$GID_0 <- 1:length(polygon@data$GID_0)

  a_names <- ifelse(boundary_level == 0, "NAME_0",
                    paste("NAME", boundary_level, sep = "_"))

  polygon@data[, !names(polygon@data) %in% c("GID_0", a_names)] <- NULL # erase columns
  names(polygon@data) <- c("GID_0", "adm_names")

  if (keep_data == FALSE) {
    erase_rds <- list.files(path = ".", pattern = "^GADM_", full.names = TRUE)
    unlink(erase_rds)
  }
  return(polygon)
}


#' Extent of occurrence of a species based on convex hull polygons
#' @param occurrences a data.frame containing geographic coordinates of species occurrences,
#' columns must be: Species, Longitude, and Latitude. Geographic coordinates must be in decimal
#' degrees.
#' @param polygons SpatialPolygon object to clip convex hulls to these limits.
#' Projection of this object defines projection of the extent of occurrence. This
#' projection must be one that allows safe calculation of areas (e.g., Eckert IV; EPSG:54012)
#' @export
#' @importFrom sp CRS SpatialPolygons Polygons Polygon proj4string over spTransform
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom grDevices chull
#' @importFrom raster area
#' @importFrom rgeos gIntersection
#' @examples
#' # occurrences
#' data("occ_p", package = "rangemap")
#' occ <- unique(occ_p)
#'
#' # polygons
#' data("wrld_simpl", package = "maptools")
#' ECK4 <- sp::CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#' poly_pr <- sp::spTransform(wrld_simpl, ECK4)
#'
#' # EOO
#' eoo_pe <- eoo(occurrences = occ, polygons = poly_pr)

eoo <- function(occurrences, polygons) {
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  species <- as.character(occurrences[1, 1])
  coord <- as.data.frame(occurrences[, 2:3]) # spatial point dataframe to data frame keeping only coordinates
  covexhull <- chull(coord) # convex hull from points
  coord_pol <- coord[c(covexhull, covexhull[1]), ] # defining coordinates
  covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
  sp::proj4string(covexhull_polygon) <- WGS84 # project
  covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, polygons@proj4string) # reproject
  c_hull_extent <- rgeos::gIntersection(polygons, covexhull_polygon_pr,
                                        byid = TRUE, drop_lower_td = TRUE) # area of interest

  eockm2 <- raster::area(c_hull_extent) / 1000000
  eocckm2 <- sum(eockm2) # total area of the species range

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species, eockm2),
                                                    match.ID = FALSE)

  return(list(spolydf = extent_occurrence, area = eocckm2))
}


#' Area of occupancy of a species as defined by the IUCN
#' @param occ_pr SpatialPointsDataFrame of ocurrence records. Projection must be
#' one that allows safe calculation of areas (e.g., Eckert IV; EPSG:54012)
#' @param species (character) scientific name of the species.
#' @export
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom raster area extent rasterize
#' @examples
#' # data
#' data("occ_p", package = "rangemap")
#' occ <- unique(occ_p)
#' WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#' occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
#'                                      proj4string = WGS84)
#'
#' ECK4 <- sp::CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#' occ_pr <- sp::spTransform(occ_sp, ECK4)
#'
#' sp <- as.character(occ[1, 1])
#'
#' # AOO
#' aoo_pe <- aoo(occ_pr = occ_pr, species = sp)

aoo <- function(occ_pr, species) {
  grid <- raster::raster(ext = raster::extent(occ_pr) + 10000,
                         res = c(2000, 2000), crs = occ_pr@proj4string)
  raster_sp <- raster::rasterize(occ_pr[, 2:3], grid)[[1]] # raster from points
  grid_sp <- as(raster_sp, "SpatialPolygonsDataFrame") # raster to polygon

  aockm2 <- raster::area(grid_sp) / 1000000
  aocckm2 <- sum(aockm2) # area calculation

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp,  # area of occupancy
                                                 data = data.frame(species, aockm2),
                                                 match.ID = FALSE)
  return(list(spolydf = area_occupancy, area = aocckm2))
}
