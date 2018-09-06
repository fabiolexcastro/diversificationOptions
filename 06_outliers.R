
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, outliers, cclust, rgeos, gtools, reshape2)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to clean the dataframes
cleanTbl <- function(sp){
  # sp <- sps[1]
  tbl <- vls6 %>% filter(name == sp)
  vls <- raster::extract(crn, tbl[,3:4]) %>% cbind(tbl, .)
  df <- data.frame(sp = unique(tbl$name), type = 'Initial', count = nrow(vls)) %>%
    mutate(sp = as.character(sp),
           type = as.character(type),
           count = as.numeric(count))
  vls <- vls[complete.cases(vls),] %>% as.tibble()
  df <- rbind(df, c(sp, 'Without Nas', nrow(vls)))
  
  # To identify the outliers values
  norm <- scores(vls[,5:ncol(vls)], 'z') ; norm_na <- norm
  norm_na[abs(norm_na)>3.5]  <- NA 
  normpoints <- cbind(vls[,1:4], norm_na) %>%
    na.omit(.) %>%
    tbl_df()
  occ.swd <- raster::extract(crn, normpoints[,3:4]) %>% cbind(normpoints[,1:4], .)
  print(' To write the final table...')
  df <- rbind(df, c(sp, 'Removed the outliers', nrow(occ.swd)))
  write.csv(occ.swd, paste0('../_tbl/_vls/', sp, '.csv'), row.names = FALSE)
  print(paste0('Done ', sp, '!'))
  return(df)
}

# Load data
tbl <- read_csv('../_tbl/all.csv')
vrs <- c(paste0('prec_', 1:12), paste0('tmin_', 1:12), paste0('tmean_', 1:12), paste0('tmax_', 1:12)) %>% paste0(., '$')
fls <- list.files('../_grid/_climate/_current', full.names = TRUE) %>% 
  grep(paste0(vrs, collapse = '|'), ., value = TRUE) %>% 
  mixedsort()

# Extracting values
crn <- stack(fls)
vls <- raster::extract(crn, as.data.frame(tbl[,3:4]))
vls2 <- cbind(tbl, vls); vls2 <- tbl_df(vls2) 
write_csv(vls2, '../_tbl/all_swd.csv'); rm(crn, tbl, fls, vls)
vls2 <- read_csv('../_tbl/all.csv')

# Reviewing the species
sps <- vls2 %>% distinct(name) %>% pull()

# Subsetting Citrus
cit <- vls2[grep('Citrus', vls2$name, value = FALSE),]
cit <- cit %>% filter(name %in% c('Citrus medica', 'Citrus aurantiifolia', 'Citrus aurantium', 'Citrus reticulata'))
vls3 <- vls2[-grep('Citrus', vls2$name, value = FALSE),] %>% rbind(., cit)

# Subsetting Dioscorea
dio <- vls3[grep('Dioscorea', vls3$name, value = FALSE),]
dio <- dio %>% filter(name %in% 'Dioscorea alata')
vls4 <- vls3[-grep('Dioscorea', vls3$name, value = FALSE),] %>% rbind(., dio)

# Subsetting coffeea
cof <- vls4[grep('Coffea', vls4$name, value = FALSE),]
cof <- cof %>% filter(name == 'Coffea arabica')
vls5 <- vls4[-grep('Coffea', vls4$name, value = FALSE),] %>% rbind(., cof)

# Subsetting Musa Paradisiaca
mus <- vls5[grep('Musa', vls5$name, value = FALSE),]
mus <- mus %>% filter(name %in% 'Musa paradisiaca')
vls6 <- vls5[-grep('Musa', vls5$name, value = FALSE),] %>% rbind(., mus)

# Reviewing the species (again)
sps <- vls6 %>% distinct(name) %>% pull() %>% sort()
length(sps)

# Applying the function
vls.fnl <- lapply(1:length(sps), function(k) cleanTbl(sp = sps[k]))
saveRDS(object = vls.fnl, file = '../_rds/nrows_fnl.rds')
vls.fnl <- bind_rows(vls.fnl) 
vls.fnl <- dcast(vls.fnl, sp ~ type)
vls.fnl <- vls.fnl %>% setNames(c('sp', 'Initial', 'RemovedOtlrs', 'WithoutNas'))
vls.fnl <- vls.fnl %>% dplyr::select(sp, Initial, WithoutNas, RemovedOtlrs)

write.csv(vls.fnl, '../_tbl/_nrows/vls_rmvNas.csv', row.names = FALSE)







