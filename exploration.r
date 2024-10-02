##### Calcul de l'indicateur Utilisation des terres pour 15+ ####
library(terra)
library(sf)
library(geodata)

### data management
##### Qc poly
# qc <- st_read("/home/claire/BDQC-GEOBON/data/QUEBEC_Unique_poly.gpkg")
can <- readRDS("/home/claire/BDQC-GEOBON/data/g15_indicators/gadm/gadm41_CAN_1_pk.rds")
qc <- st_as_sf(can[can$NAME_1 == "Québec", ])

#### 2000-2005-2010-2015-2020
coll_clip <- list()

for (y in c(
    2000,
    2005, 2010, 2015, 2020
)) {
    print(y)
    # Quebec cover - From UTM 17 to UTM 21
    # u16 <- rast("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use/2000/LU2000_u16/LU2000_u16_v4_2022_02.tif")
    u17 <- rast(paste0("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u17/LU", y, "_u17_v4_2022_02.tif"))
    u18 <- rast(paste0("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u18/LU", y, "_u18_v4_2022_02.tif"))
    u19 <- rast(paste0("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u19/LU", y, "_u19_v4_2022_02.tif"))
    u20 <- rast(paste0("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u20/LU", y, "_u20_v4_2022_02.tif"))
    u21 <- rast(paste0("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u21/LU", y, "_u21_v4_2022_02.tif"))
    # u22 <- rast("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use/2000/LU2000_u22/LU2000_u22_v4_2022_02.tif")



    # spatraster collection
    coll <- sprc(u17, u18, u19, u20, u21)

    # clip each layer begore to mmerge them

    for (i in 1:length(coll)) {
        print(varnames(coll[i]))
        qc_tr <- st_transform(qc, st_crs(coll[i]))
        map_c <- crop(coll[i], qc_tr)
        map_m <- mask(map_c, qc_tr)

        writeRaster(map_m, paste0("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use_treat/", y, "/", varnames(map_m), "_treat.tif"), overwrite = T)

        coll_clip[i] <- map_m
        names(coll_clip)[i] <- varnames(map_m)
    }
}

coll_clip
