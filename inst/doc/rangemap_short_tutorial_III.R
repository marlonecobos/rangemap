## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE---------------------------------------
# devtools would help us to install rangemap from GitHub
if(!require(devtools)){
  install.packages("devtools")
}

# installing rangemap
if(!require(rangemap)){
  devtools::install_github("marlonecobos/rangemap")
  library(rangemap)
}

# rgibif would help us to get some species occurrence data in our example
if(!require(rgbif)){
  install.packages("rgbif")
  library(rgbif)
}

# maps and maptools will help us to get geographic data for using one of the functions
if(!require(maps)){
install.packages("maps")
library(maps)
}
if(!require(maptools)){
 install.packages("maptools")
 library(maptools)
}

# data from kuenm will be used for using one of the functions
if(!require(kuenm)){
  devtools::install_github("marlonecobos/kuenm")
  library(kuenm)
}

## ---- cache=TRUE---------------------------------------------------------
# getting the data from GBIF
species <- name_lookup(query = "Muscigralla brevicauda",
                       rank="species", return = "data") # information about the species

species$key # to see all the keys we can try

# key 4 was the one with georeferenced occurrences
occ_count(taxonKey = species$key[13], georeferenced = TRUE) 
key <- species$key[13] # using species key that return information

occ <- occ_search(taxonKey = key, return = "data", limit = 1200) # using the taxon key

# keeping only species name, and coordinates
occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
            c("name", "decimalLongitude", "decimalLatitude")]


## ---- warning=FALSE, cache=TRUE------------------------------------------
# checking which countries may be involved in the analysis
rangemap_explore(occurrences = occ_g)

## ---- cache=TRUE, warning=FALSE------------------------------------------
# Defining parameters
## region of interest
WGS84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") # initial projection
w_map <- map(database = "world", regions = c("ECU", "PER"), fill = TRUE, plot = FALSE) # map
w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
reg <- map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon

# All this part is for excluding Galapagos Islands
areas <- lapply(reg@polygons, function(x) sapply(x@Polygons, function(y) y@area))
bigpolys <- unlist(lapply(areas, function(x) which(x > 1)))
reg@polygons[[1]]@Polygons <- reg@polygons[[1]]@Polygons[bigpolys]
reg@polygons[[1]]@plotOrder <- reg@polygons[[1]]@plotOrder[reg@polygons[[1]]@plotOrder %in% bigpolys]
slot(reg, "polygons") <- lapply(slot(reg, "polygons"), "comment<-", NULL)

# Now we can create the species range based a tsa
# Remember, this is a statistical approach, it may take some time depending on the area
thres <- 1 # this argument implies that 1% of the occurrences will be excluded

tsa_range <- rangemap_tsa(occurrences = occ_g, region_of_interest = reg, threshold = thres)

## ---- fig.show='hold', warning=FALSE-------------------------------------
rangemap_fig(tsa_range, add_occurrences = TRUE, polygons = reg, 
             range_color = "transparent")
rangemap_fig(tsa_range, northarrow = TRUE,  polygons = reg,
             legend = TRUE, legend_position = "bottomleft")

## ---- cache=TRUE---------------------------------------------------------
# Getting the data from kuenm
sp_mod <- raster::raster(list.files(system.file("extdata", package = "kuenm"), # model
                                    pattern = "sp_model.tif", full.names = TRUE))
sp_train <- read.csv(list.files(system.file("extdata", package = "kuenm"), # occurrences
                                pattern = "sp_train.csv", full.names = TRUE))

# A rapid look at this data
par(mar = c(0, 0, 0, 0))
raster::image(sp_mod, axes = FALSE, col = rev(terrain.colors(255)))
points(sp_train, pch = 16, cex = 0.8)

## ---- cache=TRUE---------------------------------------------------------
occ_sp <- data.frame("A_americanum", sp_train) # adding the species name as a first column
thresh <- 1 # threshold 1 to be used 
thresh1 <- 5 # threshold 2 to be used 
thresh2 <- 10 # threshold 3 to be used 

# Now we can create the species range from the models
enm_range <- rangemap_enm(occurrences = occ_sp, model = sp_mod, threshold_omission = thresh)
enm_range1 <- rangemap_enm(occurrences = occ_sp, model = sp_mod, threshold_omission = thresh1)
enm_range2 <- rangemap_enm(occurrences = occ_sp, model = sp_mod, threshold_omission = thresh2)

## ---- fig.show='hold', warning=FALSE-------------------------------------
rangemap_fig(enm_range, add_occurrences = TRUE, zoom = 2,
             range_color = "transparent")
rangemap_fig(enm_range, zoom = 2)
rangemap_fig(enm_range1, zoom = 2)
rangemap_fig(enm_range2, zoom = 2, legend = TRUE,
             legend_position = c(-6800000, 3500000))

