
# Load libraries
require(pacman)
pacman::p_load(tidyverse, raster, rgdal, rgeos, gtools)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to clean the data frame
cleanTbl <- function(pos){
  print(pos)
  tbl <- fls[pos]
  tbl <- readRDS(tbl)[[3]]
  tbl <- tbl %>% dplyr::select(name, scientificName, decimalLongitude, decimalLatitude) #  georeferencedDate # coordinatePrecision
  return(tbl)

}

# Load data
fls <- list.files('../_rds', pattern = '1', full.names = TRUE) 

# Apply the function
tbls1 <- lapply(1:10, function(i) cleanTbl(pos =  i))
tbls2 <- lapply(14:length(fls), function(i) cleanTbl(pos =  i))
tbls3 <- fls[11] %>% readRDS() %>% dplyr::select(name, scientificName, decimalLongitude, decimalLatitude)
tbls4 <- fls[12] %>% readRDS() %>% dplyr::select(name, scientificName, decimalLongitude, decimalLatitude)
tbls5 <- fls[13] %>% readRDS() %>% dplyr::select(name, scientificName, decimalLongitude, decimalLatitude)

tbls <- c(tbls1, tbls2, list(tbls3, tbls4, tbls5))
tblall <- bind_rows(tbls)

write.csv(tblall, '../_tbl/all.csv', row.names = FALSE)
