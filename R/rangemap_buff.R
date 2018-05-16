# rangemap_buff

# documentation

# dependencies

rangemap_buff <- function(occ, dist, poly) {
  # erase duplicate records
  occ <- occ[row.names(unique(occ[, 2:3])), ]

  # make a spatial object from coordinates
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  # create a buffer based on a user-defined distance
  buff_area <- raster::buffer(occ_sp, width = dist)

  # disolve polygons


  # world map or user map
  if (missing(poly)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    poly <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = crs) # map to polygon
  }

  # clip a world map based on the created buffer (resolution?)
  clip_area <- rgeos::gIntersection(poly, buff_area, byid = TRUE, drop_lower_td = TRUE)

  # reproject (test in areas are being calculated well even if that is not the center of the polygon,
  # if no change the lat long based on the centroid of the polygon)
  clip_area_pro <- sp::spTransform(clip_area, CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

  # calculate areas
  area_a <- raster::area(clip_area_pro) # check this

  area_t <- sum(area_a[, 4]) # check this

  area_o <- length(occ[, 1]) * 4 # area of occupancy separated more than 4 km (make a grid for this?)

  area_e <- ### extent of occurrence

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], length(occ[, 1]), area_t, area_o, area_e) # extent of occ = total area?
  names(sp_dat) <- c("Species", "Individual records", "Total area", "Area of occupancy", "Extent of occurrence")

  results <- list(sp_dat, clip_area, area_a)
  return(results)
}
