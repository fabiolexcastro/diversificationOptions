
# Load libraries
require(pacman)
pacman::p_load(tidyverse, raster, rgdal, rgeos, gtools, stringr)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())

# Load data
wrl <- shapefile('../_shp/all_countries.shp')
extent(wrl)
ext <- as(raster::extent(-180, 180, -35, 35), "SpatialPolygons")

wrlcut <- crop(wrl, ext)
plot(wrl)
plot(ext, add = TRUE, border = 'red')
crs(ext) <- crs(wrl)
ext <- SpatialPolygonsDataFrame(ext, data.frame(A = 1))

writeOGR(obj = ext, dsn = '../_shp', layer = 'mask', driver = 'ESRI Shapefile')
