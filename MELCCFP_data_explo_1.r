# Objectif - Exploration des jeux de donnees, identification de la structure
library(sf)
library(terra)
y <- 2012
path <- paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/MELCCFP_Utilisation_territoire/utilisation_territoire_", y, "/utilisation_territoire_", y, ".tif")

map <- rast(path)
map

disag <- disagg(map, fact = 3) # to obtain 10x10 res ?

map_proj <- project(map, "EPSG:6623")
levels(map)
cats <- cats(map)
dim(cats[[1]])
names(cats[[1]])
unique(cats[[1]]$Classe_dé)
unique(cats[[1]]$CODE_CAT)
unique(cats[[1]]$DESC_CAT)
unique(cats[[1]]$DESC_RCL27)
unique(cats[[1]]$DESC_RCL18)
#-----#
y <- 2020
path <- paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/MELCCFP_Utilisation_territoire/utilisation_territoire_", y, "/utilisation_territoire_", y, ".tif")
map2 <- rast(path)
map2
x11()
plot(map2)
levels(map2)
cats2 <- cats(map2)
dim(cats2[[1]])
cats2[[1]][1:10, ]
# names(map2)
unique(cats2[[1]]$classe_det)
unique(cats2[[1]]$CODE_CAT)
unique(cats2[[1]]$DESC_CAT)
unique(cats2[[1]]$DESC_RCL_A)
unique(cats2[[1]]$DESC_RCL_B)
unique(cats2[[1]]$DESC_PESTI)
unique(cats2[[1]]$DESC_Benth)

# change the 'active' category layer
activeCat(map2) # check the 'activate' column for categories - Be carefull the first column is 0
head(cats(map2)[[1]])
activeCat(map2) <- 5
map2
x11()
plot(map2)
freq(map2)
#-----#
y <- 2015
path <- paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/MELCCFP_Utilisation_territoire/utilisation_territoire_", y, "/utilisation_territoire_", y, ".tif")
map3 <- rast(path)
map3
# x11()
# plot(map3)
levels(map3)
cats3 <- cats(map3)
dim(cats3[[1]])
cats3[[1]][1:10, ]
# names(map2)
unique(cats[[1]]$classe_det)
