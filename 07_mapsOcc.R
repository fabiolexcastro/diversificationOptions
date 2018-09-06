
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr, flextable)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use
createMap <- function(fle){
  # fle <- fls[6]
  tbl <- read_csv(fle)
  ttl <- paste0(basename(fle) %>% str_replace('.csv', ''))
  gg <- ggplot() +
    geom_polygon(data = shp, aes(x = long, y = lat, group = group), color = 'grey', fill = NA) +
    geom_point(data = tbl, aes(x = decimalLongitude, y = decimalLatitude), color = 'brown', size = 0.8) +
    coord_equal() +
    theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'top',
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste0(basename(fle) %>% str_replace('.csv', ''))) +
    xlab('Longitude') + 
    ylab('Latitude')
  ggsave(plot = gg, filename = paste0('../_png/_maps/_presencesRmvOtlrs/', ttl, '.png'), units = 'in', width = 9)
  print('To make the summary by country')
  smm <- raster::extract(shp, tbl[,c('decimalLongitude', 'decimalLatitude')]) %>% 
    dplyr::select(ENGLISH) %>% 
    group_by(ENGLISH) %>%
    dplyr::summarize(count = n()) %>% 
    arrange(desc(count)) %>%
    mutate(porc = (count / sum(count)) * 100,
           sp = ttl)
  write.csv(smm, paste0('../_tbl/_countyByCountry/countyByCountry_', ttl, '.csv'), row.names = FALSE)
  print('Done...!')
  return(smm)
}

# Load data
fls <- list.files('../_tbl/_vls', full.names = TRUE, pattern = '.csv$')
nms <- basename(fls)
shp <- shapefile('../_shp/all_countries.shp')

read.csv(fls[9]) %>% nrow()
# Apply the function for all the species
createMap(fle = fls[9])

# For all the species
rsl <- lapply(1:length(fls), function(k) createMap(fls[k]))





