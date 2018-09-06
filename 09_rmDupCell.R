
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, stringr, tidyverse, randomForest, foreach, doSNOW, parallel)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_gbif/_codes')
source('FunctionsRFclustering.R')

# Functions to use --------------------------------------------------------
rmDupCell <- function(msk, sp){
  # sp <- sps[1]
  tb <- tbl %>% dplyr::filter(name == sp)
  cn <- raster::extract(msk, tb[,c('decimalLongitude', 'decimalLatitude')], cellnumbers = TRUE)
  cl <- xyFromCell(msk, cn[,'cells'])
  dv <- duplicated(cl[,c('x', 'y')])
  fn <- tbl_df(tb[!dv,])
  print('Done')
  return(fn)
}
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  
  datRF_presences <- occ[,4:ncol(occ)]
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  datRF_presences <- as.data.frame(datRF_presences)
  # datRF_presences <- na.omit(datRF_presences)
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  detach(datRF_presences)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  return(list(labelRF, clusterdata))
  
}
myCluster <- function(tbl){
  # tbl <- rmv[[18]]
  sp <- unique(tbl[,1]) %>% pull()
  print(paste0('To make the extraction ', sp))
  tbl <- raster::extract(lyr, tbl[,c('decimalLongitude', 'decimalLatitude')]) %>%  
    cbind(tbl[,c('decimalLongitude', 'decimalLatitude')], .) %>% 
    as_data_frame() %>%
    mutate(specie = sp) %>% 
    dplyr::select(specie, decimalLongitude, decimalLatitude, bio_1:bio_19)
  tbl <- tbl %>% na.omit()
  env <- tbl[,4:ncol(tbl)] %>% as.matrix()
  dRF <- tbl[,4:ncol(tbl)] %>% as.data.frame()
  print(paste0('To make the random forest cluster ', sp))
  # rfC <- rf.clust(occ = tbl, nforest = 25, ntrees = 100, nVars = 8, nclasses = 3)
  datRF_presences <- tbl[,4:ncol(tbl)]
  attach(datRF_presences)
  no.forests <- 25
  no.trees <- 100
  nVars <- 8
  datRF_presences <- as.data.frame(datRF_presences)
  datRF_presences <- na.omit(datRF_presences)
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  detach(datRF_presences)
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  hcd <- as.dendrogram(clusterdata)
  png(filename = paste0('../_png/_figures/_dendogram/dend_', sp, '.png'), units = 'in', width = 12, height = 9, res = 300)
  plot(hcd,  ylab = "Height",  edgePar = list(col = 2:3, lwd = 2:1))
  dev.off()
  
  clusterdata$height %>% range()
  ths <- clusterdata$height %>% max() / 2
  nclasses <- which(clusterdata$height > ths) %>% length()
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  
  clusteredpresdata <- cbind(tbl %>% na.omit(), cluster = labelRF) %>% na.omit() %>% tbl_df()
  no.clusters <- nclasses
  save(dRF, file = paste0('../_rData/_run1_1/', sp, '_datRF.rData'))
  save(clusterdata, file = paste0('../_rData/_run1_1/', sp, '_clusterdata.rData'))
  save(tbl, clusteredpresdata, no.clusters, labelRF, file = paste0('../_rData/_run1_1/', sp, '_clustereddata.rData'))
  print('Done...!')
  
}

# Load data ---------------------------------------------------------------
lyr <- list.files('../_asc/_climate/_current', full.names = TRUE, pattern = '.asc$') %>%
  mixedsort() %>%
  grep('bio', ., value = TRUE) %>%
  stack()
msk <- lyr[[1]] * 0 + 1
fls <- list.files('../_tbl/_vls', full.names = TRUE, pattern = '.csv$')
tbl <- lapply(1:length(fls), function(k) read_csv(fls[[k]]))
tbl <- bind_rows(tbl)
sps <- unique(tbl$name)

# Removing duplicated by cell ---------------------------------------------
rmv <- lapply(1:length(sps), function(k) rmDupCell(msk = msk, sp = sps[k]))
smm <- rmv %>%
  dplyr::bind_rows() %>% 
  dplyr::group_by(name) %>%
  dplyr::summarize(count = n())
tormv <- smm %>% dplyr::filter(count < 100) %>% pull(1)
posts <- which(smm$name %in% tormv)
rmv <- rmv[-posts]
smm <- rmv %>%
  dplyr::bind_rows() %>% 
  dplyr::group_by(name) %>%
  dplyr::summarize(count = n())
write.csv(smm, '../_tbl/_nrows/vls_rmvDup.csv', row.names = FALSE)
Map('write.csv', rmv, paste0('../_tbl/_rmDupCell/', basename(fls)), row.names = FALSE)

# Clustering random forest ------------------------------------------------
tst <- myCluster(tbl = rmv[[1]], no.clusters = 3)
cl <- makeCluster(21)
registerDoSNOW(cl)

foreach(i = 1:nrow(smm), .packages = c('raster', 'rgdal', 'tidyverse', 'rgeos', 'gtools', 'randomForest'), .verbose = TRUE) %dopar% {
  myCluster(tbl = rmv[[i]], no.clusters = smm[i, 3])
  print('Done!')
}

lapply(1:nrow(smm), function(k){
  print(k)
  myCluster(tbl = rmv[[k]])
})

# Persea americana
smm %>% nrow()
which(smm$name == 'Persea americana')
myCluster(tbl = rmv[[16]])









