# rangemap_buff

rangemap_buff <- function(occ, dist, poly) {
  # erase duplicate records


  # make a spatial object from coordinates
  occ_sp <- SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  # create a buffer based on a user-defined distance
  buff_area <- raster::buffer(ch_Pp, width = dist)

  # world map or user map
  if (missing(poly)) {
    w_map <- map(database = "world", fill = TRUE, plot = FALSE) #map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) #preparing data to create polygon
    poly <- map2SpatialPolygons(w_map, IDs = w_po, proj4string = crs) #map to polygon
  }

  # clip a world map based on the created buffer (resolution?)
  clip_area <- rgeos::gIntersection(poly, buff_area, byid = TRUE, drop_lower_td = TRUE)

  # reproject
  clip_area_pro <- spTransform(clip_area, CRS("+proj=aeqd"))

  # calculate areas
  area <- raster::area(clip_area_pro)

  # return results
  results <- list(occ[1, 1], clip_area, clip_area_pro, area)

}
