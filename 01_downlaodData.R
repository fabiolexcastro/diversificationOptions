
# Load libraries
require(pacman)
pacman::p_load(tidyverse, raster, rgdal, rgeos, gtools, readxl, rgbif)

# Initial setup
rm(list = ls())
g <- gc(reset = TRUE)
options(scipen = 999)

# Load data
sps <- read_excel('../_tbl/listadoSp.xlsx') %>% setNames(c('crop', 'scientific', 'table', 'found'))
sps <- sps %>% pull(scientific)
sps <- sps %>% na.omit() %>% as.character()

# Files rds
fls.rds <- list.files('../_rds') %>% str_sub(., start = 1, end = 3) %>% unique()

# Dacriodes Edulis
dac <- occ_search(scientificName = 'Dacryodes edulis (G.Don) H.J.Lam)', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
da2 <- occ_search(scientificName = 'Dacryodes edulis', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
da3 <- occ_search(scientificName = 'Dacryodes edulis', limit = 200000)
da4 <- occ_search(scientificName = 'Dacryodes edulis (G.Don) H.J.Lam)', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)

# Persea americana
per1 <- occ_search(scientificName = 'Persea americana Mill.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
per2 <- occ_search(scientificName = 'Persea americana Mill.', limit = 200000)

saveRDS(object = per1, file = '../_rds/per1.rds')
saveRDS(object = per2, file = '../_rds/per2.rds')

# Sacharum robustum
sac1 <- occ_search(scientificName = 'Saccharum robustum Brandes & Jesw. ex Grassl', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
sac2 <- occ_search(scientificName = 'Saccharum robustum Brandes & Jesw. ex Grassl', limit = 200000)

saveRDS(object = sac1, file = '../_rds/sac1.rds')
saveRDS(object = sac2, file = '../_rds/sac2.rds')

# Musa
mus1 <- occ_search(scientificName = 'Musa L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
mus2 <- occ_search(scientificName = 'Musa L.', limit = 200000)

saveRDS(object = mus1, file = '../_rds/mus1.rds')
saveRDS(object = mus2, file = '../_rds/mus2.rds')

mus1 <- readRDS('../_rds/mus1.rds')
mus2 <- readRDS('../_rds/mus2.rds')

# Arachis hypogaea L.
ara1 <- occ_search(scientificName = 'Arachis hypogaea L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
ara2 <- occ_search(scientificName = 'Arachis hypogaea L.', limit = 200000)

saveRDS(object = ara1, file = '../_rds/ara1.rds')
saveRDS(object = ara2, file = '../_rds/ara2.rds')

# Citrus spp.
cit1 <- occ_search(scientificName = 'Citrus L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
cit2 <- occ_search(scientificName = 'Citrus L.', limit = 200000)

saveRDS(object = cit1, file = '../_rds/cit1.rds')
saveRDS(object = cit2, file = '../_rds/cit2.rds')

# Psidium guajava L.
psi1 <- occ_search(scientificName = 'Psidium guajava L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
psi2 <- occ_search(scientificName = 'Psidium guajava L.', limit = 200000)

saveRDS(object = psi1, file = '../_rds/psi1.rds')
saveRDS(object = psi2, file = '../_rds/psi2.rds')

# Zea mays # CUIDADO
zea1 <- occ_search(scientificName = 'Zea mays L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
zea2 <- occ_search(scientificName = 'Zea mays L.', limit = 200000)

zea1 <- readRDS(file = '../_rds/zea_all.rds')
zea1 <- zea1[[3]] %>% dplyr::select(name, decimalLatitude, decimalLongitude)
zea1[complete.cases(zea2),]

saveRDS(object = zea1, file = '../_rds/zea_all.rds')
saveRDS(object = zea2, file = '../_rds/zea_all_1.rds')

# Xanthosoma sagittifolium
xan1 <- occ_search(scientificName = 'Xanthosoma sagittifolium (L.) Schott', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
xan2 <- occ_search(scientificName = 'Xanthosoma sagittifolium (L.) Schott', limit = 200000)

saveRDS(object = xan1, file = '../_rds/xan1.rds')
saveRDS(object = xan2, file = '../_rds/xan2.rds')

# Elaeis guineensis
ele1 <- occ_search(scientificName = 'Elaeis guineensis Jacq.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
ele2 <- occ_search(scientificName = 'Elaeis guineensis Jacq.', limit = 200000)

saveRDS(object = ele1, file = '../_rds/ele1.rds')
saveRDS(object = ele2, file = '../_rds/ele2.rds')

ele1 <- readRDS('../_rds/ele1.rds')
ele2 <- readRDS('../_rds/ele2.rds')

# Manihot esculenta Crantz 
man1 <- occ_search(scientificName = 'Manihot esculenta Crantz', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
man2 <- occ_search(scientificName = 'Manihot esculenta Crantz', limit = 200000)

# Coffee spp.
cff1 <- occ_search(scientificName = 'Coffea L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
cff2 <- occ_search(scientificName = 'Coffea L.', limit = 200000)

saveRDS(object = cff1, file = '../_rds/cff1.rds')
saveRDS(object = cff2, file = '../_rds/cff2.rds')

# Mangifera indica L. 
mgf1 <- occ_search(scientificName = 'Mangifera indica L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
mgf2 <- occ_search(scientificName = 'Mangifera indica L.', limit = 200000)

saveRDS(object = mgf1, file = '../_rds/mgf1.rds')
saveRDS(object = mgf2, file = '../_rds/mgf2.rds')

# Cola acuminata Schott & Endl.
col1 <- occ_search(scientificName = 'Cola acuminata Schott & Endl.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
col2 <- occ_search(scientificName = 'Cola acuminata Schott & Endl.', limit = 200000)

saveRDS(object = col1, file = '../_rds/col1.rds')
saveRDS(object = col2, file = '../_rds/col2.rds')

# Capsicum annuum L
cap1 <- occ_search(scientificName = 'Capsicum annuum L', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
cap2 <- occ_search(scientificName = 'Capsicum annuum L', limit = 200000)

saveRDS(object = cap1, file = '../_rds/cap1.rds')
saveRDS(object = cap2, file = '../_rds/cap2.rds')

# Dioscorea spp.
dio1 <- occ_search(scientificName = 'Dioscorea Plum. ex L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
dio2 <- occ_search(scientificName = 'Dioscorea Plum. ex L.', limit = 200000)

saveRDS(object = dio1, file = '../_rds/dio1.rds')
saveRDS(object = dio2, file = '../_rds/dio2.rds')

# Phaseolus vulgaris L.
pha1 <- occ_search(scientificName = 'Phaseolus vulgaris L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
pha2 <- occ_search(scientificName = 'Phaseolus vulgaris L.', limit = 200000)
  
saveRDS(object = pha1, file = '../_rds/pha1.rds')
saveRDS(object = pha2, file = '../_rds/pha2.rds')

# Capsicum annuum L.
cap1 <- occ_search(scientificName = 'Capsicum annuum L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
cap2 <- occ_search(scientificName = 'Capsicum annuum L.', limit = 200000)

saveRDS(object = cap1, file = '../_rds/cap1.rds')
saveRDS(object = cap2, file = '../_rds/cap2.rds')

# Ananas comosus (L.) Merr.
ana1 <- occ_search(scientificName = 'Ananas comosus (L.) Merr.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
ana2 <- occ_search(scientificName = 'Ananas comosus (L.) Merr.', limit = 200000)

saveRDS(object = cap1, file = '../_rds/cap1.rds')
saveRDS(object = cap2, file = '../_rds/cap2.rds')

# Coula edulis Baill.
col1 <- occ_search(scientificName = 'Coula edulis Baill.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
col2 <- occ_search(scientificName = 'Coula edulis Baill.', limit = 200000)

saveRDS(object = col1, file = '../_rds/col1.rds')
saveRDS(object = col2, file = '../_rds/col2.rds')

# Carica papaya
car1 <- occ_search(scientificName = 'Carica papaya L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
car2 <- occ_search(scientificName = 'Carica papaya L.', limit = 200000)

saveRDS(object = car1, file = '../_rds/car1.rds')
saveRDS(object = car2, file = '../_rds/car2.rds')

# Ricinodendron heudelotii (Baill.) Pierre ex Heckel
ric1 <- occ_search(scientificName = 'Ricinodendron heudelotii (Baill.) Pierre ex Heckel', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
ric2 <- occ_search(scientificName = 'Ricinodendron heudelotii (Baill.) Pierre ex Heckel', limit = 200000)

saveRDS(object = ric1, file = '../_rds/ric1.rds')
saveRDS(object = ric2, file = '../_rds/ric2.rds')

# Anacardium occidentale L.
aoc1 <- occ_search(scientificName = 'Anacardium occidentale L.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
aoc2 <- occ_search(scientificName = 'Anacardium occidentale L.', limit = 200000)

saveRDS(object = aoc1, file = '../_rds/aoc1.rds')
saveRDS(object = aoc2, file = '../_rds/aoc2.rds')

# Tectona grandis L.f.
tec1 <- occ_search(scientificName = 'Tectona grandis L.f.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
tec2 <- occ_search(scientificName = 'Tectona grandis L.f.', limit = 200000)

saveRDS(object = tec1, file = '../_rds/tec1.rds')
saveRDS(object = tec2, file = '../_rds/tec2.rds')

# Acacia mangium Willd.
aca1 <- occ_search(scientificName = 'Acacia mangium Willd.', limit = 200000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
aca2 <- occ_search(scientificName = 'Acacia mangium Willd.', limit = 200000)

saveRDS(object = tec1, file = '../_rds/tec1.rds')
saveRDS(object = tec2, file = '../_rds/tec2.rds')

# Distinct Musa
sps.musa <- mus1[[3]] %>% distinct(name)

# Musa paradisiaca
mus.par1 <- mus1[[3]] %>% dplyr::filter(name == 'Musa paradisiaca')
mus.par2 <- mus2[[3]] %>% dplyr::filter(name == 'Musa paradisiaca')

saveRDS(object = mus.par1, file = '../_rds/mus_par1.rds')
saveRDS(object = mus.par2, file = '../_rds/mus_par2.rds')

# Musa acuminata
mus.acu1 <- mus1[[3]] %>% dplyr::filter(name == 'Musa acuminata')
mus.acu2 <- mus2[[3]] %>% dplyr::filter(name == 'Musa acuminata')

saveRDS(object = mus.acu1, file = '../_rds/mus_acu1.rds')
saveRDS(object = mus.acu2, file = '../_rds/mus_acu2.rds')

# Musa balbisiana
mus.bal1 <- mus1[[3]] %>% dplyr::filter(name == 'Musa balbisiana')
mus.bal2 <- mus2[[3]] %>% dplyr::filter(name == 'Musa balbisiana')

saveRDS(object = mus.bal1, file = '../_rds/mus_bal1.rds')
saveRDS(object = mus.bal2, file = '../_rds/mus_bal2.rds')

# Musa all
mus.all2 <- read.delim('../_zip/musa/occurrence.txt', header = TRUE)
mus.all2 <- as_data_frame(mus.all2)
saveRDS(mus.all2, '../_rds/mus_all_all.rds')

mus.all2 <- mus.all2 %>% dplyr::select(scientificName, decimalLongitude, decimalLatitude, hasCoordinate, hasGeospatialIssues)
mus.all1 <- mus.all2 %>% dplyr::filter(hasCoordinate == 'true')

