#' Species distributional ranges based on political boundaries
#'
#' @description rangemap_bound generates a species range polygon for a given species
#' by considering all the polygons of political entities in which the species has
#' been detected. Shape files can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param country_code (character) vector of country codes that will be considered when
#' creating the species range. Codes follow the ISO-3166-1 norm as in function \code{\link[raster]{getData}}.
#' @param boundary_level (numeric) the level of the administrative boundaries from 0 to 5 (0 is
#' the country and higher values equal finer divisions), default = 0.
#' @param polygons (optional) a SpatialPolygon object that will be used instead of boundaries
#' to create species ranges based on overlapping of species records with these layer. If defined,
#' argument boundaries will not be considered.
#' @param disolve (logical) if TRUE distint polygons selected as part of the species range will
#' be disolved for creating simpler polygons, default = TRUE.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details Boundaries used are loaded using the \code{\link[raster]{getData}} funcion.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' occ <- occ_search(taxonKey = 2440788, return = "data")
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
#'             c("name", "decimalLongitude", "decimalLatitude")]
#'
#' level <- 0
#' disolve <- TRUE
#' save <- TRUE
#' name <- "test"
#' countries <- c("PER", "BRA", "COL", "VEN", "ECU", "GUF", "GUY", "SUR")
#'
#' bound_range <- rangemap_bound(occurrences = occ_g, country_code = countries, boundary_level = level,
#'                               disolve = disolve, save_shp = save, name = name)

# Dependencies: maps (map),
#               maptools (map2SpatialPolygons),
#               raster (area, rasterize, extent, getData),
#               rgdal (writeOGR),
#               rgeos (gIntersection, gCentroid, gBuffer, gUnaryUnion),
#               sp (SpatialPointsDataFrame, spTransform, SpatialPolygonsDataFrame,
#                   CRS, over, Polygons, Polygon, SpatialPolygons, proj4string)

rangemap_bound <- function(occurrences, country_code, boundary_level = 0,
                           polygons, disolve = TRUE, save_shp = FALSE, name) {
  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument occurrences is necessary to perform the analysis")
  }

  if (dim(occurrences)[2] != 3) {
    stop("occurrences data.frame must have the following columns: \nSpecies, Longitude, and Latitude")
  }

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))

  # make a spatial object from coordinates
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # world map or user map fro creating species range
  if (missing(polygons)) {
    bounds <- list()
    for (i in 1:length(country_code)){
      bounds[[i]] <- raster::getData('GADM', country = country_code[i], level = boundary_level)
    }
    polygon <- do.call("rbind", bounds)
    polygon@data$OBJECTID <- 1:length(bounds)
    polygons <- sp::spTransform(polygon, WGS84)
  }

  # keeping only records in land
  occ_sp <- occ_sp[!is.na(sp::over(occ_sp, as(polygons, "SpatialPolygons"))), ]

  # project the points using their centriods as reference
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  occ_pr <- sp::spTransform(occ_sp, AEQD)

  # project polygons
  polygons <- sp::spTransform(polygons, AEQD)

  # select polygons that overlap with points
  boundaries <- polygons[!is.na(sp::over(polygons, as(occ_pr, "SpatialPoints"))), ]

  # disolve?


  # calculate areas in km2
  areas_km2 <- raster::area(boundaries) / 1000000
  area_km2 <- sum(areas_km2) # total area of the species range

  ## extent of occurrence
  coord <- as.data.frame(occ[, 2:3]) # spatial point dataframe to data frame keeping only coordinates
  covexhull <- chull(coord) # convex hull from points
  coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
  covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
  sp::proj4string(covexhull_polygon) <- WGS84 # project
  covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, AEQD) # reproject
  c_hull_extent <- rgeos::gIntersection(polygons, covexhull_polygon_pr, byid = TRUE, drop_lower_td = TRUE) # area of interest

  ext_occ_area_km2 <- raster::area(c_hull_extent) / 1000000
  extent_occ_km2 <- sum(ext_occ_area_km2) # total area of the species range

  ## area of occupancy
  grid <- raster::raster(ext = raster::extent(occ_pr) + 10000, res = c(2000, 2000), crs = AEQD)
  raster_sp <- raster::rasterize(occ_pr[, 2:3], grid)[[1]] # raster from points
  grid_sp <- as(raster_sp, "SpatialPolygonsDataFrame") # raster to polygon

  area_occ_area_km2 <- raster::area(grid_sp) / 1000000
  area_occ_km2 <- sum(area_occ_area_km2) # area calculation

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  boundaries <- sp::SpatialPolygonsDataFrame(boundaries, data = data.frame(species, areas_km2, # species range
                                                                           extent_occ_km2,
                                                                           area_occ_km2),
                                            match.ID = FALSE)

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species,
                                                                      ext_occ_area_km2),
                                                    match.ID = FALSE)

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp, data = data.frame(species, # area of occupancy
                                                                            area_occ_area_km2),
                                                 match.ID = FALSE)

  # exporting
  if (save_shp == TRUE) {
    cat("\nWriting shapefiles in the working directory.\n")
    rgdal::writeOGR(boundaries, ".", name, driver = "ESRI Shapefile")
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile")
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], area_km2, extent_occ_km2, area_occ_km2) # extent of occ = total area?
  names(sp_dat) <- c("Species", "Non-duplicate records", "Range area", "Extent of occurrence", "Area of occupancy")

  results <- list(sp_dat, boundaries, extent_occurrence, area_occupancy)
  names(results) <- c("Summary", "Species range", "Extent of occurrence",
                      "Area of occupancy")
  return(results)
}


