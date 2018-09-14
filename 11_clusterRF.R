
# Load data
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr, velox, sf, dismo, randomForest, pROC, foreach, doSNOW)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('FunctionsRFclustering.R')

# Functions to use
myModel <- function(fle){
  # fle <- fls[18]
  print('To load the data')
  load(fle)
  clusteredpresdata %>% as.tibble()
  sp <- unique(clusteredpresdata$specie)
  modelfolder <- paste0('../_RF/_run1/_models/', gsub(' ', '_', sp))
  
  print('To create the folder')
  dir.create(modelfolder, recursive = TRUE)
  vrs <- paste0('bio_', 1:19, '$')
  
  print('To list the layers')
  lyr.list <- list.files('../_grid/_climate/_current', full.names = TRUE) %>%
    mixedsort() %>%
    grep(paste0(vrs, collapse = '|'), ., value = TRUE) 
  stk <- stack(lyr.list)
  msk <- stk[[1]] * 0 + 1
  SPspecies <- SpatialPoints(clusteredpresdata[,c('decimalLongitude', 'decimalLatitude')])
  
  print('Generation of backgound - pseudoabsences')
  bck_lyr <- msk
  spcs <- raster::extract(bck_lyr, SPspecies, cellnumber = TRUE)
  bck_lyr[spcs[,1]] <- NA
  samplesize <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0) 
  NumberOfClusters <- max(clusteredpresdata$cluster) 
  ratio <- NumberOfClusters/1
  numberofpresences <- nrow(clusteredpresdata)
  back <- randomPoints(bck_lyr, 1*numberofpresences) %>% as_data_frame()
  coordinates(back) <- ~ x + y
  bck_swd <- raster::extract(stk, back) %>% cbind(coordinates(back), .) %>% as_data_frame()
  coordinates(bck_swd) <- ~ x + y
  
  if(!file.exists(paste0('../_tbl/_points/_run1/bck_', gsub(' ', '_', sp), '.csv'))){
    write.csv(bck_swd, paste0('../_tbl/_points/_run1/bck_', gsub(' ', '_', sp), '.csv'), row.names = F) 
  }
  
  print('Cluster analysis to the pseudo-absences')
  datRF <- as.data.frame(bck_swd[,3:ncol(bck_swd)])
  attach(datRF)
  no.forests <- 50
  no.trees <- 500
  distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)
  no.absenceclasses <- 2
  labelRF <- pamNew(distRF$cl1, no.absenceclasses)
  detach(datRF)
  classdata <- cbind(pb = as.factor(labelRF), bck_swd@data[,1:ncol(bck_swd)])
  
  print('Join presences and background')
  presvalue_swd  <- clusteredpresdata[,3:ncol(clusteredpresdata)] %>%
    cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
    na.omit() %>%
    as.data.frame() %>%
    mutate(cluster = cluster + no.absenceclasses)
  presvalue_swd <- dplyr::select(presvalue_swd, pb, bio_1:bio_19)
  presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))
  
  classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)]) # Background
  dim(classdata_2); dim(presvalue_swd)
  allclasses_swd <- rbind(classdata_2, presvalue_swd)
  
  print('To write all the classes into our workspace')
  write.csv(allclasses_swd, paste0('../_tbl/_points/_run1/all_classes_', sp, '.csv'), row.names = FALSE)
  
  print('To adjust the Random Forest Model')
  model1 <- as.formula(paste('factor(pb) ~', paste(paste('bio', c(1:19), sep = '_'), collapse = '+', sep = ' ')))
  rflist <- vector('list', 50)
  auc <- vector('list', 50)
  
  for(repe in 1:50){ # 50 bosques
    
    print(repe)
    pressample <- list()
    
    for (i in 1:(NumberOfClusters+no.absenceclasses)){
      
      if(any(i==c(1:no.absenceclasses))) { 
        
        rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                       size = samplesize*NumberOfClusters/2/no.absenceclasses)
      } else {
        rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size=samplesize)
      }
      pressample[[i]] <- allclasses_swd[rows,] 
    }
    
    species <- na.omit(do.call(rbind, pressample)) 
    head(species)
    Samplesplit <- sample(rownames(species)) 
    
    envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
    envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
    
    rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
    
    save(rfmodel, file = paste(modelfolder, '/', 'RF_' ,NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
    rflist[[repe]] <- rfmodel
    
    print('To fix the AUC')
    predicted <- as.numeric(predict(rfmodel, envtest))
    observed <- as.vector(envtest[,'pb'])
    auc[[repe]] <- auc(observed, predicted) 
    rm(rfmodel)
    
    cat(auc[[repe]] ,'\n')
    
  }
  
  auc <- unlist(auc)
  rff <- do.call(randomForest::combine, rflist)
  importance <- as.data.frame(rff$importance)
  importance <- importance %>% mutate(prcn = MeanDecreaseGini/sum(MeanDecreaseGini) * 100)
  
  paste0(modelfolder, '/_rData') %>% dir.create()
  
  save(rflist, file = paste(modelfolder, '/rflist_', NumberOfClusters, '.rData', sep = ''))
  save(importance, file = paste0(modelfolder, '/_rData/', 'importanceRF.rData'))
  save(auc, file = paste0(modelfolder, '/_rData/', 'aucRF_dist.rData'))
  save(rff, file = paste0(modelfolder, '/_rData/', 'rff_dist.rData'))
  
}




# Load data
fls <- list.files('../_rData/_run1_1', full.names = TRUE) %>% grep('clustereddata', ., value = TRUE)
myModel(fle = fls[[1]]) # Examples

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

# Generation of background
myModel(fle = fls[3])
lapply(fls, myModel)












