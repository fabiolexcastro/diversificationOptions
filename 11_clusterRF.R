
# Load data
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr, velox, sf, dismo, randomForest, pROC, foreach, doSNOW)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('FunctionsRFclustering.R')

# Generation of background
myModel(fle = fls[3])
lapply(fls, myModel)

# Load data
fls <- list.files('../_rData/_run1_1', full.names = TRUE) %>% grep('clustereddata', ., value = TRUE)
myModel(fle = fls[[9]])

# Parralelization
fls.sub <- fls[3:length(fls)]
cl <- makeCluster(length(fls.sub))
registerDoSNOW(cl)
foreach(i = 1:length(fls.sub), .packages = c('raster', 'rgdal', 'tidyverse', 'rgeos', 'gtools', 'randomForest', 'pROC', 'dismo', 'stringr'), .verbose = TRUE) %dopar% {
  myModel(fle = fls.sub[[i]])
  print('Done!')
}

# Using lapply
lapply(10:length(fls), function(k) myModel(fls[[k]]))
lapply(fls.sub, myModel)













