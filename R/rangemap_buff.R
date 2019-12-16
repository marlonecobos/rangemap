#' Species distributional ranges based on buffered occurrences
#'
#' @description rangemap_buff generates a distributional range for a given species by
#' buffering provided occurrences using a defined distance. An approach to the species extent
#' of occurrence (using convex hulls) and the area of occupancy according to the IUCN criteria
#' are also generated. Shapefiles can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing geographic coordinates of species occurrences,
#' columns must be: Species, Longitude, and Latitude. Geographic coordinates must be in decimal
#' degrees.
#' @param buffer_distance (numeric) distance, in meters, to be used for creating the buffer areas
#' around occurrences, default = 100000.
#' @param polygons (optional) a SpatialPolygon object to clip buffer areas and adjust the species
#' range and other polygons to these limits. Projection must be Geographic (longitude, latitude).
#' If not defined, a default, simple world map will be used.
#' @param final_projection (character) string of projection arguments for resulting Spatial objects.
#' Arguments must be as in the PROJ.4 documentation. See \code{\link[sp]{CRS-class}} for details.
#' Default = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" = WGS84.
#' @param save_shp (logical) if TRUE, shapefiles of the species range, occurrences, extent of
#' occurrence and area of occupancy will be written in the working directory. Default = FALSE.
#' @param name (character) valid if \code{save_shp} = TRUE. The name of the shapefile to be
#' exported. A suffix will be added to \code{name} depending on the object as follows: species
#' extent of occurrence = "_extent_occ", area of occupancy = "_area_occ", and occurrences =
#' "_unique_records". Default = "range_buffer".
#'
#' @return
#' A named list containing: (1) a data.frame with information about the species range,
#' and SpatialPolygon objects of (2) unique occurrences, (3) species range, (4) extent of
#' occurrence, and (5) area of occurpancy.
#'
#' @details
#' All resultant Spatial objects in the list of results will be projected to the \code{final_projection}.
#' Areas are calculated in square kilometers using the Azimuthal equal area projection.
#'
#' @usage
#' rangemap_buff(occurrences, buffer_distance = 1e+05, polygons, final_projection,
#'     save_shp = FALSE, name = "range_buffer", overwrite = FALSE)
#'
#' @export
#'
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sp SpatialPolygons Polygons Polygon proj4string over spTransform
#' @importFrom raster disaggregate area extent rasterize
#' @importFrom rgeos gCentroid gUnaryUnion gIntersection gBuffer
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @importFrom rgdal writeOGR
#'
#' @examples
#' suppressWarnings({if(!require(spocc)){
#'   install.packages("spocc")
#'   library(spocc)
#' }})
#'
#' # getting the data from GBIF
#' occs <- occ(query = "Peltophryne empusa", from = "gbif",
#'             limit = 1000)$gbif$data[[1]]
#'
#' # keeping only georeferenced records
#' occ_g <- occs[!is.na(occs$latitude) & !is.na(occs$longitude),
#'               c("name", "longitude", "latitude")]
#'
#' # buffer distance
#' dist <- 100000
#' save <- TRUE
#' name <- "test"
#'
#' buff_range <- rangemap_buff(occurrences = occ_g, buffer_distance = dist,
#'                             save_shp = save, name = name, overwrite = TRUE)
#'
#' # see the species range in a figure
#' extent <- TRUE
#' occ <- TRUE
#' legend <- TRUE
#' north <- TRUE
#'
#' # creating the species range figure
#' rangemap_fig(buff_range, add_extent = extent, add_occurrences = occ,
#'              legend = legend, northarrow = north, legend_position = "bottomleft")

rangemap_buff <- function(occurrences, buffer_distance = 100000, polygons, final_projection,
                          save_shp = FALSE, name = "range_buffer", overwrite = FALSE) {
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
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # world map or user map fro creating species range
  if (missing(polygons)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
  }

  # keeping only records in land
  occ_sp <- occ_sp[!is.na(sp::over(occ_sp, polygons)), ]

  # project the points using their centriods as reference
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  occ_pr <- sp::spTransform(occ_sp, AEQD)

  # project polygons
  polygons <- sp::spTransform(polygons, AEQD)

  # create a buffer based on a user-defined distance
  buff_area <- rgeos::gBuffer(occ_pr, width = buffer_distance)

  buff_area <- raster::disaggregate(buff_area)

  # clip a world map based on the created buffer
  polygons <- suppressWarnings(rgeos::gBuffer(polygons, byid = TRUE, width = 0)) # to avoid topology problems
  polygons <- rgeos::gUnaryUnion(polygons)

  clip_area <- rgeos::gIntersection(polygons, buff_area, byid = TRUE, drop_lower_td = TRUE)

  # calculate areas in km2
  areakm2 <- raster::area(clip_area) / 1000000
  areackm2 <- sum(areakm2) # total area of the species range

  ## extent of occurrence
  coord <- as.data.frame(occ[, 2:3]) # spatial point dataframe to data frame keeping only coordinates
  covexhull <- chull(coord) # convex hull from points
  coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
  covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
  sp::proj4string(covexhull_polygon) <- WGS84 # project
  covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, AEQD) # reproject
  c_hull_extent <- rgeos::gIntersection(polygons, covexhull_polygon_pr, byid = TRUE, drop_lower_td = TRUE) # area of interest

  eockm2 <- raster::area(c_hull_extent) / 1000000
  eocckm2 <- sum(eockm2) # total area of the species range

  ## area of occupancy
  grid <- raster::raster(ext = raster::extent(occ_pr) + 10000, res = c(2000, 2000), crs = AEQD)
  raster_sp <- raster::rasterize(occ_pr[, 2:3], grid)[[1]] # raster from points
  grid_sp <- as(raster_sp, "SpatialPolygonsDataFrame") # raster to polygon

  aockm2 <- raster::area(grid_sp) / 1000000
  aocckm2 <- sum(aockm2) # area calculation

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(clip_area, # species range
                                            data = data.frame(species, areakm2),
                                            match.ID = FALSE)

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species, eockm2),
                                                    match.ID = FALSE)

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp,  # area of occupancy
                                                 data = data.frame(species, aockm2),
                                                 match.ID = FALSE)

  # reprojection
  if (missing(final_projection)) {
    final_projection <- WGS84
  } else {
    final_projection <- sp::CRS(final_projection) # character to projection
  }

  clip_area <- sp::spTransform(clip_area, final_projection)
  extent_occurrence <- sp::spTransform(extent_occurrence, final_projection)
  area_occupancy <- sp::spTransform(area_occupancy, final_projection)
  occ_pr <- sp::spTransform(occ_pr, final_projection)

  # exporting
  if (save_shp == TRUE) {
    cat("Writing shapefiles in the working directory.")
    rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile", overwrite_layer = overwrite)
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile", overwrite_layer = overwrite)
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile", overwrite_layer = overwrite)
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"), driver = "ESRI Shapefile", overwrite_layer = overwrite)
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], areackm2, eocckm2, aocckm2) # extent of occ = total area?
  colnames(sp_dat) <- c("Species", "Unique_records", "Range_area", "Extent_of_occurrence", "Area_of_occupancy")

  results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy)
  names(results) <- c("Summary", "Species_unique_records", "Species_range", "Extent_of_occurrence",
                      "Area_of_occupancy")
  return(results)
}
