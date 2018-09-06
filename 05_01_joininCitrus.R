
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, velox, sf, outliers)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fls <- list.files('../_tbl/_vls', pattern = 'Citrus', full.names = TRUE)
vrs <- c(paste0('prec_', 1:12), paste0('tmin_', 1:12), paste0('tmean_', 1:12), paste0('tmax_', 1:12)) %>% paste0(., '$')
vrs <- c(paste0('bio_', 1:19, '$'))
lyr <- '../_grid/_climate/_current' %>%
  list.files(full.names = TRUE) %>%
  grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>%
  stack()

tbl <- lapply(fls, read_csv)
tbl <- lapply(tbl, dplyr::select, name, decimalLatitude, decimalLongitude)
tbl <- bind_rows(tbl)
vls <- raster::extract(lyr, tbl[,3:2]) %>% cbind(tbl, .) %>% na.omit()

# To identify the outliers values
norm <- scores(vls[,4:ncol(vls)], 'z') ; norm_na <- norm
norm_na[abs(norm_na)>3.5]  <- NA 
normpoints <- cbind(vls[,1:3], norm_na) %>%
  na.omit(.) %>%
  tbl_df()
occ.swd <- raster::extract(lyr, normpoints[,3:2]) %>% cbind(normpoints[,1:3], .)
occ.swd <- as.tibble(occ.swd)

write.csv(occ.swd, '../_tbl/_vls/Citrus.csv', row.names = FALSE)


