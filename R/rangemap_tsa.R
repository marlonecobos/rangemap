#' Species distributional ranges based on a trend surface analysis
#'
#' @description rangemap_tsa generates species range polygons for a given species
#' using a trend surface analysis. An approach to the species extent of occurrence
#' (using convex hulls) and the area of occupancy according to the IUCN criteria
#' are also generated. Shape files can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param region_of_interest a SpatialPolygon object on which the trend surface analysis
#' will be performed. For instance, a country, an ecoregion, or a biogeogeographical region.
#' @param resolution (numeric) resolution in minutes in which the resultant surface will be created,
#' default = 2.5. It can be 0.5, 2.5, 5, and 10, for correspondence with the bioclimatic variables from
#' the WorldClim database \url{http://www.worldclim.org/}.
#' @param threshold (numeric) percentage of occurrence records to be excluded when deciding
#' the minimum surface value to be considered part of the species range, default = 0.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details Trend surface analysis Is a method based on low-order polynomials of spatial coordinates
#' for estimating a regular grid of points from scattered observations. This method assumes that all
#' cells not occupied by occurrences are absences; hence its use depends on the quality of data and
#' the certainty of having or not a complete sampling of the regiong_of_interest.
#'
#' @examples

# Dependencies: sp (SpatialPointsDataFrame, spTransform), rgdal?,
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gUnaryUnion, gIntersection, gCentroid), saptial?

rangemap_tsa <- function(occurrences, region_of_interest, resolution = 2.5,
                         threshold = 0, save_shp = FALSE, name) {
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

  # getting one variable from WC
  ## getting data
  if (resolution == 0.5) {
    # tiles for the region of interest
    ## create a fishnet equal to the one of  worldclim
    #
    var <- raster::getData("worldclim", var = "bio", download = FALSE,
                           res = resolution, long = , lat = )[[1]]

  }else {
    var <- raster::getData("worldclim", var = "bio", download = FALSE, res = resolution)[[1]]
  }

  # give a value to get the area

  # calculate areas in km2
  area <- raster::area(hulls_buff_un) / 1000000
  areakm2 <- sum(area) # total area of the species range

  ## extent of occurrence
  coord <- as.data.frame(occ[, 2:3]) # spatial point dataframe to data frame keeping only coordinates
  covexhull <- chull(coord) # convex hull from points
  coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
  covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
  sp::proj4string(covexhull_polygon) <- WGS84 # project
  covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, AEQD) # reproject
  #c_hull_extent <- rgeos::gIntersection(polygons, covexhull_polygon_pr, byid = TRUE, drop_lower_td = TRUE) # area of interest

  eockm2 <- raster::area(covexhull_polygon_pr) / 1000000
  eocckm2 <- sum(eockm2) # total area of the species range

  ## area of occupancy
  grid <- raster::raster(ext = raster::extent(occ_pr) + 10000, res = c(2000, 2000), crs = AEQD)
  raster_sp <- raster::rasterize(occ_pr[, 2:3], grid)[[1]] # raster from points
  grid_sp <- as(raster_sp, "SpatialPolygonsDataFrame") # raster to polygon

  aockm2 <- raster::area(grid_sp) / 1000000
  aocckm2 <- sum(aockm2) # area calculation

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(hulls_buff_un, data = data.frame(species, area, # species range
                                                                             eocckm2, aocckm2),
                                            match.ID = FALSE)

  extent_occurrence <- sp::SpatialPolygonsDataFrame(covexhull_polygon_pr, # extent of occurrence
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


