
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use
myList <- function(pos.yr, pos.gcm){
  pos.yr <- 1
  pos.gcm <- 1
  gcm <- paste(base, gcms, 'r1i1p1', yrs[pos.yr], sep = '/') 
  fls <- list.files(fls[pos.gcm], full.names = TRUE)
  return(fls)
}
createCode <- function(code, inp, msk, out){
  
  sink(code)
  cat('import arcpy,os', fill = T)
  cat('from arcpy import env', fill = T)
  cat('from arcpy.sa import *', fill = T)
  cat('arcpy.CheckOutExtension("Spatial")', fill = T)
  cat('os.system("cls")', fill = T)
  cat(paste0('input = ', '"', inp, '"'), fill = T)
  cat(paste0('mask = ', '"', msk, '"'), fill = T)
  cat(paste0('out = ', '"', out, '"'), fill = T)
  cat('arcpy.env.snapRaster = input', fill = T)
  cat('outExtractByMask = ExtractByMask(input, mask)', fill = T)
  cat('outExtractByMask.save(out)', fill = T)
  cat('print "Done"')
  sink()
  
  shell(code)# system2(paste0('python ', code));# shell.exec(code)
  
  print('Done...')
  
}

# Load data
base <- '//dapadfs/data_cluster_2/gcm/cmip5/downscaled/rcp60/global_2_5min'
gcms <- list.files(base)[5:20]
yrs <- paste(base, gcms[1], 'r1i1p1', sep = '/') %>% list.dirs(recursive = F, full.names = F)
vrs <- paste(base, gcms[1], 'r1i1p1', yrs[1], sep = '/') %>% list.files(full.names = FALSE) 

# Applying the code
createCode(code = 'Z:/_gbif/_codes/extMskpy.py',
           inp = fls.gcm1[1],
           msk = 'Z:/_gbif/_shp/mask.shp',
           out = 'Z:/_gbif/_grid/_climate/_future/_rcp60/_2030/bcc_csm1_1/bio_100')

lapply(5:length(gcms), function(g){
  print(gcms[g])
  lapply(1:length(vrs), function(v){
    print(vrs[v])
    path_gcm <- paste0('Z:/_gbif/_grid/_climate/_future/_rcp60/_2050/', gcms[g])
    dir.create(path_gcm)
    createCode(
      code = 'Z:/_gbif/_codes/extMskpy.py',
      inp = paste(base, gcms[g], 'r1i1p1', yrs[2], vrs[v], sep = '/'),
      msk = 'Z:/_gbif/_shp/mask.shp',
      out = paste0(path_gcm, '/', vrs[v])
    )
    print(paste0('Done ', vrs[v], gcms[g]))
  })
})


list.files(paste0(base, gcms[1]))



createCode(code = '../_codes/extMskpy.py',
           inp = '',
           lyr = '', 
           out = '')









