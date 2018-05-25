#' Species distributional ranges based on distinct hull polygons
#'
#' @description rangemap_hull generates a species range polygon for a given species
#' by considering all the polygons of political entities in which the species has
#' been detected. Shape files can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param hull_type (character) type of hull polygons to be created. Available options are: "convex",
#' "concave", and "alpha" hulls.
#' @param buffer_distance (numeric) distance, in meters, to be used for creating the buffer areas
#' around occurrences, default = 50000.
#' @param split_distance (numeric) distance in meters that will limit connectivity among
#' hull polygons created with chunks of points separated by long distances, , default = 250000.
#' @param polygons (optional) a SpatialPolygon object that will be clipped with the buffer areas
#' to create species ranges based on actual limits. Projection must be Geographic (longitude, latitude).
#' If not defined, a default, simple world map will be used.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' species <- name_lookup(query = "Panthera onca",
#'                        rank = "species", return = "data") # information about the species
#'
#' occ_count(taxonKey = species$key[17], georeferenced = TRUE) # testing if keys return records
#'
#' key <- species$key[17] # using species key that return information
#'
#' occ <- occ_search(taxonKey = key, return = "data", limit = 2000)
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
#'              c("name", "decimalLongitude", "decimalLatitude")]
#'
#' dist <- 50000
#' hull <- "convex"
#' split <- 2500000
#'
#' hull_range <- rangemap_hull(occurrences = occ_g, hull_type = hull, buffer_distance = dist,
#'                             split_distance = split)

# Dependencies: sp (SpatialPointsDataFrame, spTransform),
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gIntersection, gCentroid),

rangemap_hull <- function(occurrences, hull_type = "concave", buffer_distance = 50000,
                          split_distance = 250000, polygons, save_shp = FALSE, name) {
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

  #split groups of points based on the split distance
  ## defining a hierarchical cluster method for the occurrences
  cluster_method <- hclust(dist(data.frame(rownames = 1:length(occ_pr@data[,1]), x = sp::coordinates(occ_pr)[,1],
                                y = sp::coordinates(occ_pr)[,2])), method = "complete")

  ## defining wich points are clustered based on the user-defined distance
  cluster_vector <- cutree(cluster_method, h = split_distance)

  ## Join results to meuse sp points
  occ_pr@data <- data.frame(occ_pr@data, clusters = cluster_vector)

  ## splitting points
  if (sum(unique(occ_pr@data$clusters)) > 1) {
    occ_prs <- split(occ_pr, occ_pr@data$clusters)
  }else {
    occ_prs <- occ_pr
  }

  # create hulls depending in the user-defined argument (convex, concave, and alpha)
  # and per each group of points if they were clustered
  if (hull_type == "convex" | hull_type == "concave" | hull_type == "alpha") {
    if (hull_type == "convex") {

      hulls <- list()

      for (i in 1:length(occ_prs)) {
        coord <- as.data.frame(sp::coordinates(occ_prs[[i]])) # spatial point dataframe to data frame keeping only coordinates

        if (dim(coord)[1] > 1) {
          covexhull <- chull(coord) # convex hull from points
          coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
          hulls[[i]] <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
          sp::proj4string(hulls[[i]]) <- AEQD # project
        }else {
          hulls[[i]] <- sp::SpatialPointsDataFrame(coords = coord, data = coord,
                                                   proj4string = AEQD)
        }
      }
    }
    if (hull_type == "concave") {
      hulls
    }
    if (hull_type == "alpha") {
      hulls
    }
  }else {
    stop(paste("hull_type is not a valid option, potential options are:",
               "\n\"convex\", \"concave\", or \"alpha\""))
  }

  # create a buffer based on a user-defined distance
  hulls_buffer <- list()

  for (i in 1:length(hulls)) {
    hulls_buffer[[i]] <- rgeos::gBuffer(hulls[[i]], width = buffer_distance)
  }

  # union buffered polygons
  hulls_buff_un <- do.call(sp::rbind.SpatialPolygons, c(hulls_buffer, list(makeUniqueIDs = TRUE)))

  # Clipping with the world
  hulls_buf_world <- rgeos::gIntersection(hulls_buff_un, polygons, byid = FALSE, drop_lower_td = TRUE) # area of interest

  # calculate areas in km2
  areakm2 <- sum(raster::area(hulls_buf_world) / 1000000) # total area of the species range

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
  clip_area <- sp::SpatialPolygonsDataFrame(clip_area, data = data.frame(species, areakm2, # species range
                                                                         eocckm2, aocckm2),
                                            match.ID = FALSE)

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species,
                                                                      eockm2),
                                                    match.ID = FALSE)

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp, data = data.frame(species, # area of occupancy
                                                                            aockm2),
                                                 match.ID = FALSE)

  # exporting
  if (save_shp == TRUE) {
    cat("Writing shapefiles in the working directory.")
    rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile")
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"), driver = "ESRI Shapefile")
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], areakm2, eocckm2, aocckm2) # extent of occ = total area?
  colnames(sp_dat) <- c("Species", "Unique records", "Range area", "Extent of occurrence", "Area of occupancy")

  results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy)
  names(results) <- c("Summary", "Species unique records", "Species range", "Extent of occurrence",
                      "Area of occupancy")
  return(results)
}


