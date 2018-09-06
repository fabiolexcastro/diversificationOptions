
# Load data
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, stringr, rgeos, gtools, stringr)
require(ggplot2)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use
extractAltitude <- function(pos){
  # pos <- 5
  tbl <- read_csv(fls[pos])  
  sps <- unique(tbl$name)
  vls <- raster::extract(alt, tbl[,c('decimalLongitude', 'decimalLatitude')])
  vls <- vls %>% na.omit() %>% data.frame(altitude = .)
  
  gg <- ggplot(data = vls, aes(vls$altitude)) +
    geom_histogram(bins = 10) +
    labs(x = 'Altitude', y = 'Frequency') +
    theme(axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9)) 
  ggsave(plot = gg, filename = paste0('../_png/_figures/hist_', sps, '.png'), units = 'cm', width = 8, height = 6, dpi = 300)
  
  tbl <- tbl %>% mutate(altitude = vls)
  print('Done..!')
}


# Load data
fls <- list.files('../_tbl/_vls', full.names = TRUE, pattern = '.csv$')
nms <- basename(fls)
shp <- shapefile('../_shp/all_countries.shp')
alt <- raster('../_grid/_srtm/srtm_v41_30s')
tbl <- read_csv(fls[[1]])

# Apply for all the presences
lapply(1:length(fls), function(k) extractAltitude(pos = k))
