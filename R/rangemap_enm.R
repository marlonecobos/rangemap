#' Species distributional ranges based on ecological niche models
#'
#' @description rangemap_enm generates species range polygons for a given species
#' using a continuous raster layer produced with an ecological niche modeling tool.
#' This function split the model in suitable and unsuitable habitats using a user
#' specified level of error or a given threshold value. Shape files can be saved in
#' the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param model a RasterLayer object that will binarized using the threshold value defined
#' by the user or a value calculated based on a threshold (from 0 - 100) defined in threshold.
#' @param threshold_value (numeric) decimal value used for reclasifying the model. This value will
#' be the lowest considered as suitable for the species.
#' @param threshold (numeric) percentage of occurrence records to be excluded from suitable areas
#' considering their values of suitability in the continuous model (e.g., 0, 5, or 10).
#' @param polygons (optional) a SpatialPolygon object that will be clipped with the buffer areas
#' to create species ranges based on actual limits. Projection must be Geographic (longitude, latitude).
#' If not defined, a default, simple world map will be used.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory, default = FALSE.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details If threshold_value is provided, argument threshold is ignored.
#'
#' @examples
#' if(!require(devtools)){
#' install.packages("devtools")
#' library(devtools)
#' }
#'
#' if(!require(kuenm)){
#' install_github("marlonecobos/kuenm")
#' library(kuenm)
#' }
#'
#' # parameters
#' data(sp_mod)
#' data(sp_train)
#' occ_sp <- data.frame("A_americanum", sp_train)
#' thres <- 5
#' save <- TRUE
#' name <- "test
#'
#' enm_range <- rangemap_enm(occurrences = occ_sp, model = sp_mod,  threshold = thres,
#'                           save_shp = save, name = name)

# Dependencies: sp (SpatialPointsDataFrame, spTransform), rgdal?,
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gUnaryUnion, gIntersection, gCentroid)

rangemap_enm <- function(occurrences, model, threshold_value, threshold,
                         polygons, save_shp = FALSE, name) {

  # check for errors
  if (missing("model")) {
    stop("model must exist to perform the calculations.")
  }

  if (!missing("threshold_value") | !missing("threshold") | !missing("occurrences")) {
    if (!missing("threshold_value")) {
      binary <- model
      raster::values(binary)[raster::values(binary) < threshold_value] <- 0
      raster::values(binary)[raster::values(binary) >= threshold_value] <- 1
    }else {
      if (!missing("threshold") & !missing("occurrences")) {
        # keeping only coordinates
        occ <- occurrences[, 2:3]

        # threshold value calculation
        o_suit <- raster::extract(model, occ)
        o_suit_sort <- sort(o_suit)
        thres <- o_suit_sort[ceiling(length(occ[, 1]) * threshold / 100) + 1]

        # binarization
        binary <- model
        raster::values(binary)[raster::values(binary) < thres] <- 0
        raster::values(binary)[raster::values(binary) >= thres] <- 1
      }else {
        stop("Parameters threshold and occ.tra, or threshold_value must be defined to perform the calculations.")
      }
    }
  }
  # keeping only areas of presence
  presence <- binary
  raster::values(presence)[raster::values(presence) == 0] <- NA

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  # convert raster to polygons
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  raster::crs(presence) <- WGS84@projargs
  enm_range <- raster::rasterToPolygons(presence)

  # world map or user map for creating extent of occurrence
  if (missing(polygons)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
  }

  # project area and occurrences using the area centriod as reference
  centroid <- rgeos::gCentroid(enm_range, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))


  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)
  occ_pr <- sp::spTransform(occ_sp, AEQD)

  enm_range_pr <- sp::spTransform(enm_range, AEQD)

  polygons <- sp::spTransform(polygons, AEQD)

  # calculate areas in km2
  area <- raster::area(enm_range_pr) / 1000000
  areakm2 <- sum(area) # total area of the species range

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


