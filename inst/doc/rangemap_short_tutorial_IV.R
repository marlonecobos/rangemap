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
if(!require(raster)){
  install.packages("raster")
  library(raster)
}

## ---- cache=TRUE---------------------------------------------------------
# getting the data from GBIF
species <- name_lookup(query = "Eumyias thalassinus",
                       rank="species", return = "data") # information about the species

species$key # to see all the keys we can try

# key 4 was the one with georeferenced occurrences
occ_count(taxonKey = species$key[6], georeferenced = TRUE) 
key <- species$key[6] # using species key that return information

occ <- occ_search(taxonKey = key, return = "data", limit = 5000) # using the taxon key

# keeping only species name, and coordinates
occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
            c("name", "decimalLongitude", "decimalLatitude")]

## ---- warning=FALSE, cache=TRUE------------------------------------------
# checking which countries may be involved in the analysis
rangemap_explore(occurrences = occ_g)

## ---- cache=TRUE---------------------------------------------------------
# Defining parameters
dist <- 300000 # buffer distance in meters

# Now we can create the species range based on buffers
buff_range <- rangemap_buff(occurrences = occ_g, buffer_distance = dist)

## ---- cache=TRUE---------------------------------------------------------
# Defining parameters
dist <- 200000
hull <- "convex"

# Now we can create the species range based on administrative areas
hull_range <- rangemap_hull(occurrences = occ_g, hull_type = hull, buffer_distance = dist)

## ---- cache=TRUE---------------------------------------------------------
# Defining parameters
dist <- 200000
hull <- "concave"

# Now we can create the species range based on administrative areas
hull_range1 <- rangemap_hull(occurrences = occ_g, hull_type = hull, buffer_distance = dist)

## ---- fig.show='hold', warning=FALSE-------------------------------------
rangemap_fig(buff_range, add_occurrences = TRUE, 
             northarrow = TRUE, range_color = "transparent")
rangemap_fig(buff_range)
rangemap_fig(hull_range)
rangemap_fig(hull_range1, legend = TRUE, legend_position = "bottomleft")

## ---- cache=TRUE---------------------------------------------------------
# Getting bioclimatic variables (some of them)
vars <- getData("worldclim", var = "bio", res = 10)[[c("bio1", "bio7", "bio12", "bio15")]]
vars_c <- crop(vars, extent(60, 140, -15, 45)) # crop vairable sto a smaller extent

# List of ranges
ranges <- list(buffer = buff_range, convex = hull_range, concave = hull_range1)

## ---- warning=FALSE, fig.width=6.2, fig.height=4.7-----------------------
ranges_emaps(ranges = ranges, variables = vars_c, 
             legend_position = "bottomleft")

## ---- eval=FALSE---------------------------------------------------------
#  # For avoiding too much complexity we will use only two of the previous ranges
#  ranges1 <- ranges[1:2]
#  
#  # now the figure (the figure does not appear in this tutorial, but you will see it in your viewer)
#  espace_claud <- ranges_espace(ranges = ranges1, variables = vars_c,
#                                ranges_representation = "clouds",
#                                range_colors = c("red", "yellow"),
#                                add_occurrences = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  # same figure but distinct representation
#  # the figure does not appear in this tutorial, but you will see it in your viewer
#  espace_ellip <- ranges_espace(ranges = ranges1, variables = vars_c,
#                                ranges_representation = "ellipsoids",
#                                range_colors = c("blue", "yellow"),
#                                add_occurrences = FALSE, alpha = 0.08)

