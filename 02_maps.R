
# Load libraries
library(pacman)
pacman::p_load(tidyverse, rgdal, raster, ggplot2, stringr, rgbif)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use
makeMap <- function(pos){
  # pos <- 1
  print(paste0('To start ', pos))
  fl <- paste0(fls[pos])
  tbl <- readRDS(fl)[[3]]
  tbl <- sac1[[3]]
  sp <- tbl %>% distinct(name) %>% pull()
  print(sp)
  tbl <- tbl %>% dplyr::select(name, decimalLatitude, decimalLongitude)
  tbl <- na.omit(tbl)
  pnt <- tbl
  coordinates(pnt) <- ~ decimalLongitude + decimalLatitude
  print(paste0('To make the map ', sp))
  gg <- ggplot() +
    geom_polygon(data = wrl, aes(x = long, y = lat, group = group), color = 'grey', fill = NA) + 
    geom_point(data = tbl, aes(x = decimalLongitude, y = decimalLatitude), color = 'brown', size = 0.8) +
    coord_equal() +
    xlab('Longitude') +
    ylab('Latitude') +
    theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'top',
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste0(sp))
  ggsave(plot = gg, filename  = paste0('../_png/_maps/_rgbif/', sp, '.png'), units = 'in', width = 12, height = 6, dpi = 300)
}

# Load data
wrl <- shapefile('../_shp/all_countries.shp')

# Sacharum; URL: http://www.platicar.go.cr/images/buscador/documents/pdf/00/00300-canaazucaraspectostecnicos.pdf 
sac1 <- occ_search(scientificName = 'Saccharum officinarum L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
sac2 <- occ_search(scientificName = 'Saccharum officinarum L.', limit = 200000)

saveRDS(object = sac1, file = '../_rds/sacoff1.rds')
saveRDS(object = sac2, file = '../_rds/sacoff2.rds')
