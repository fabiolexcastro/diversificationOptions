
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, tidyverse, rgeos, gtools, stringr, cclust, outliers, multcomp, magrittr, broom)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use
contrastPub <- function(descriptors, grouped, biomains, biolabels, pathGraph, nameGraph, Nrow, Ncol, width, height){   
  
  vls <- list()
  
  # biomains <- biomains_temp # biolabels <- biolabels_temp
  # pathGraph <- '../_png/_figures/_conntrasPlots/_run1'
  # nameGraph <- paste0('contrast_plot_ppt_', sp, '.png')
  # Nrow <- 3 # Ncol <- 3 # width <- 1200 # height <- 1000
  
  png((filename = paste(pathGraph, nameGraph, sep = '/')), width = width, height = height, res = 120)
  par(mfrow = c(Nrow, Ncol))
  
  for(i in 1:length(biolabels)){
    
    print('Formula')
    formula <- as.formula(paste(descriptors[i], "~ cluster"))
    Anov <- aov(formula = formula, data = grouped)
    cont <- glht(Anov, mcp(cluster = 'GrandMean'))
    vls[[i]] <- tidy(cont) %>% mutate(specie = sp, variable = descriptors[i])
    
    print('To make the graph')
    plot(cont, xlab = NA, sub = NULL, main = NA)
    title(main = biomains[i], line = 1.5)
    title(ylab = 'Group', line = 2.5)
    title(xlab = paste(biolabels[i], '\n', 'ø', round(mean(extract2((grouped[,descriptors[i]]), 1)), 1))) 
  
  }  
  
  dev.off()
  vls <- bind_rows(vls)
  vls <- vls %>% spread(variable, estimate)
  print('Done!!!')
  return(vls)
}
myGlhtModel <- function(fle){
  
  print('Load data')
  load(fle)
  print(no.clusters)
  clusteredpresdata
  labelRF %>% length()
  
  print('To tidy the grouped data')
  grouped <- clusteredpresdata %>% mutate(cluster = labelRF)
  occ_cluster <- cbind(grouped[,2:3], grouped[,'cluster'])
  sp <- grouped$specie %>% unique()
  save(occ_cluster, file = paste0('../_rData/_run1_1/occ_cluster_', sp, '.rData'))
  grouped <- grouped %>% mutate(cluster = as.factor(cluster))
  
  n.clusters <- unique(grouped$cluster) %>% length()
  
  print('Biomains')
  biomains_temp <- c('Annual mean temp', 'Mean diurnal range', 'Max temp of warmest Month', 'Min temp of coldest month', 
                     'Temp annual range', 'Mean temp of wettest quarter', 'Mean temp of driest quarter', 'Mean temp of warmest quarter', 
                     'Mean temp of coldest quarter')
  biomains_ppt <- c('Annual prec', 'Prec of wettest month', 'Prec of driest month', 'Prec seasonality', 
                    'Prec of wettest quarter', 'Prec of driest quarter', 'Prec of warmest quarter', 'Prec of coldest quarter')
  biomains_tmp2 <- c('Isothermality (bio 2/bio 7) * 100', 'Temperature seasonality (sd * 100)')
  
  biolabels_temp <- rep("°C", length(biomains_temp))
  biolabels_tmp_nogrados <- rep("-", length(biomains_tmp2)) 
  biolabels_ppt <- c(rep('mm', 3), '-', rep('mm',4))
  
  desc_temp <- c(paste0('bio_', 1:2), paste0('bio_', 5:11))
  desc_ppt <- paste0('bio_', 12:19)
  desc_tm2 <- paste0('bio_', 3:4)
  
  grouped_temp <- grouped %>% dplyr::select(desc_temp, cluster)
  grouped_ppt <- grouped %>% dplyr::select(desc_ppt, cluster)
  grouped_tm2 <- grouped %>% dplyr::select(desc_tm2, cluster)
  
  print('To make the contrasts plots')
  
  cp_temp <- contrastPub(descriptors = desc_temp, 
                         grouped = grouped_temp, 
                         biomains = biomains_temp, 
                         biolabels = biolabels_temp, 
                         pathGraph = paste0('../_png/_figures/_conntrasPlots/_run1'), 
                         nameGraph = paste0('contrast_plot_tmp1_', sp, '.png'), 
                         Nrow = 3, Ncol = 3, 
                         width = 1200, height = 1000)
  
  cp_ppt <- contrastPub(descriptors = desc_ppt, 
                        grouped = grouped_ppt, 
                        biomains = biomains_ppt, 
                        biolabels = biolabels_ppt, 
                        pathGraph = paste0('../_png/_figures/_conntrasPlots/_run1'), 
                        nameGraph = paste0('contrast_plot_ppt_', sp, '.png'), 
                        Nrow = 3, Ncol = 3, 
                        width = 1200, height = 1000)
  
  cp_tm2 <- contrastPub(descriptors = desc_tm2, 
                        grouped = grouped_tm2, 
                        biomains = biomains_tmp2, 
                        biolabels = biolabels_tmp_nogrados, 
                        pathGraph = paste0('../_png/_figures/_conntrasPlots/_run1'), 
                        nameGraph = paste0('contrast_plot_tmp2_', sp, '.png'), 
                        Nrow = 1, Ncol = 2, 
                        width = 900, height = 450)
  
  dfs <- list(cp_temp, cp_tm2, cp_ppt)
  df <- Reduce(function(x, y) merge(x, y, all=TRUE), dfs)
  print('To write the final dataframe')
  write.csv(x = df, file = paste0('../_tbl/_model/_run1/_glht/glht_', sp, '.csv'),row.names = FALSE)
  print('Done...!')
}

# Load data
fls <- list.files('../_rData/_run1_1', pattern = 'clustereddata', full.names = TRUE)

# Making the contrastplots
myGlhtModel(fle = fls[1])
lapply(fls, myGlhtModel)



