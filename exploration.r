##### Calcul de l'indicateur Utilisation des terres pour 15+ ####
# Utilisation des donnees d'agriculture et agroalimentation Canada
library(terra)
library(sf)
library(geodata)

### data management
##### Qc poly
# Canada dl
# gadm(country = "CAN", level = 1, path = "/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/gadm/")
# qc <- st_read("/home/claire/BDQC-GEOBON/data/QUEBEC_Unique_poly.gpkg")
can <- readRDS("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/gadm/gadm41_CAN_1_pk.rds")
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
    u17 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u17/LU", y, "_u17_v4_2022_02.tif"))
    u18 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u18/LU", y, "_u18_v4_2022_02.tif"))
    u19 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u19/LU", y, "_u19_v4_2022_02.tif"))
    u20 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u20/LU", y, "_u20_v4_2022_02.tif"))
    u21 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u21/LU", y, "_u21_v4_2022_02.tif"))
    # u22 <- rast("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use/2000/LU2000_u22/LU2000_u22_v4_2022_02.tif")



    # spatraster collection
    coll <- sprc(u17, u18, u19, u20, u21)

    # clip each layer to fit with the Qc polygone

    for (i in 1:length(coll)) {
        print(varnames(coll[i]))
        qc_tr <- st_transform(qc, st_crs(coll[i]))
        map_c <- crop(coll[i], qc_tr)
        map_m <- mask(map_c, qc_tr)

        writeRaster(map_m, paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_treat/", y, "/", varnames(map_m), "_treat.tif"), overwrite = T)

        coll_clip[i] <- map_m
        names(coll_clip)[i] <- varnames(map_m)
    }
}

coll_clip

# test buckets s5cmd
x11()
par(mfrow = c(1, 5))
for (i in 18:21) {
    print(i)
    map <- rast(paste0("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/io/AAC_land_use_treat/2015/LU2015_u", i, "_v4_2022_02_treat.tif"))
    plot(map)
}

#### Doing the same with ecological region ####
#### crop & mask rasters for all ecological region level 1 ####

reg_ecol <- st_read("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/QUEBEC_regions/sf_CERQ_SHP/QUEBEC_CR_NIV_01.gpkg")
plot(st_geometry(reg_ecol))

# ---- #
# test <- reg_ecol[18, ]

# plot(st_geometry(reg_ecol))
# plot(st_geometry(test), add = T, col = "green")

coll_clip <- list()
# for (y in seq(2015, 2020, 5)) {
for (y in seq(2000, 2020, 5)) {
    u17 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u17/LU", y, "_u17_v4_2022_02.tif"))
    u18 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u18/LU", y, "_u18_v4_2022_02.tif"))
    u19 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u19/LU", y, "_u19_v4_2022_02.tif"))
    u20 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u20/LU", y, "_u20_v4_2022_02.tif"))
    u21 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u21/LU", y, "_u21_v4_2022_02.tif"))

    reg_coll <- sprc(u17, u18, u19, u20, u21)
    print(paste0("--------------------> année ", y))

    for (reg in 1:nrow(reg_ecol)) {
        test <- reg_ecol[reg, ]
        eco_name <- test$NOM_PROV_N
        print(paste0("----------> ", eco_name))
        for (i in 1:5) {
            map_m <- NULL

            tryCatch(
                {
                    print("-----> conversion")
                    test_proj <- st_transform(test, st_crs(reg_coll[i]))
                    print("-----> crop & mask")
                    map_c <- crop(reg_coll[i], test_proj)
                    map_m <- mask(map_c, test_proj)
                    varnames(map_m) <- paste0(varnames(map_m), "_", eco_name)
                },
                error = function(e) {
                    print("No overlap")
                }
            )

            coll_clip <- c(coll_clip, map_m)
        }
    }
}
names(coll_clip) <- unlist(lapply(coll_clip, varnames))

r <- coll_clip[[1]]
r
freq(r)

lapply(coll_clip, names)
names(coll_clip)[[1]]

final <- data.frame()
for (i in 1:length(coll_clip)) {
    fq <- freq(coll_clip[[i]])
    fq$info <- names(coll_clip)[[i]]

    final <- rbind(final, fq)
}
