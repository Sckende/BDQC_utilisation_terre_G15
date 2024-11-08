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
for (y in c(2020)) {
    # for (y in seq(2000, 2020, 5)) {
    u17 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u17/LU", y, "_u17_v4_2022_02.tif"))
    u18 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u18/LU", y, "_u18_v4_2022_02.tif"))
    u19 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u19/LU", y, "_u19_v4_2022_02.tif"))
    u20 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u20/LU", y, "_u20_v4_2022_02.tif"))
    u21 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/land_use_raw/", y, "/LU", y, "_u21/LU", y, "_u21_v4_2022_02.tif"))

    # reg_coll <- sprc(u17, u18, u19, u20, u21)
    reg_coll <- list(u17, u18, u19, u20, u21)
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
                    test_proj <- st_transform(test, st_crs(reg_coll[[i]]))
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
    fq <- terra::freq(coll_clip[[i]])
    fq$info <- names(coll_clip)[i]

    final <- rbind(final, fq)
}


# Centre Canadien de télédetection
library(terra)
library(sf)
library(dplyr)
# can <- readRDS("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/gadm/gadm41_CAN_1_pk.rds")
can <- readRDS("/home/claire/BDQC-GEOBON/data/g15_indicators/gadm/gadm41_CAN_1_pk.rds")
qc <- st_as_sf(can[can$NAME_1 == "Québec", ])

# t1 <- terra::rast("/home/local/USHERBROOKE/juhc3201/Downloads/landcover-2010-classification.tif")
# t2 <- terra::rast("/home/local/USHERBROOKE/juhc3201/Downloads/landcover-2015-classification.tif")
# t3 <- terra::rast("/home/local/USHERBROOKE/juhc3201/Downloads/landcover-2020-classification.tif")
t1 <- terra::rast("/home/claire/Downloads/landcover-2010-classification.tif")
t2 <- terra::rast("/home/claire/Downloads/landcover-2015-classification.tif")
t3 <- terra::rast("/home/claire/Downloads/landcover-2020-classification.tif")
t_ls <- list(t1, t2, t3)
qc <- st_transform(qc, st_crs(t1))

t_treat <- list()
for (i in 1:3) {
    print(paste0("crop map # ", i, " in progress"))
    tb <- crop(t_ls[[i]], qc)
    print(paste0("mask map # ", i, " in progress"))
    tc <- mask(tb, qc)

    t_treat[[i]] <- tc
}

x11()
par(mfrow = c(1, 3))
lapply(t_treat, plot)

# writeRaster(t_treat[[3]], "/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2020.tif")
# writeRaster(t_treat[[1]], "/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2010.tif")
# writeRaster(t_treat[[2]], "/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2015.tif")
# writeRaster(t_treat[[3]], "/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2020.tif")

# t1 <- terra::rast("/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2010_6623.tif")
# t2 <- terra::rast("/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2015_6623.tif")
# t3 <- terra::rast("/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2020_6623.tif")
t1 <- terra::rast("/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2010_6623.tif")
t2 <- terra::rast("/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2015_6623.tif")
t3 <- terra::rast("/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/couverture_terre_2020_6623.tif")

t_treat <- list(t1, t2, t3)

f <- freq(t_treat[[1]])
f
f2015 <- freq(t_treat[[2]])
f2020 <- freq(t_treat[[3]])
f
f2015
f2020
f$year <- 2010
f2015$year <- 2015
f2020$year <- 2020

final <- rbind(f, f2015)
final <- rbind(final, f2020)

final$cat[final$value %in% c(1, 2, 5, 6)] <- "forest"
final$cat[final$value %in% c(1, 2, 5, 6)] <- "forest"
final$cat[final$value %in% c(1, 2, 5, 6)] <- "forest"
final$cat[final$value == 13] <- "moorland"
final$cat[final$value == 15] <- "cropland"
final$cat[final$value == 14] <- "wetland"
final$cat[final$value == 16] <- "barren_land"
final$cat[final$value == 17] <- "urban"
final$cat[final$value == 18] <- "water"
final$cat[final$value == 19] <- "snow_ice"

final2 <- final[!is.na(final$cat), ]

# write.table(final2, "/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/freq_couv_territoire.txt")
final2 <- read.table("/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/freq_couv_territoire_6623.txt")

final3 <- final2 |>
    group_by(year, cat) |>
    summarise(tot = sum(count))
final3$area <- final3$tot * 30 * 30
# write.table(final3, "/home/claire/BDQC-GEOBON/data/g15_indicators/Centre_canadien_teledetection_treat/freq_couv_territoire_6623.txt")


library(ggplot2)
x11()
ggplot(final3, aes(x = year, y = area, color = cat)) +
    geom_line()
x11()
par(mfrow = c(2, 2))
plot(final3$year[final3$cat == "wetland"], final3$area[final3$cat == "wetland"] / 1000000, type = "b", main = "wetland", xlab = "year", ylab = "area (km2)")
plot(final3$year[final3$cat == "cropland"], final3$area[final3$cat == "cropland"] / 1000000, type = "b", main = "cropland", xlab = "year", ylab = "area (km2)")
plot(final3$year[final3$cat == "forest"], final3$area[final3$cat == "forest"] / 1000000, type = "b", main = "forest", xlab = "year", ylab = "area (km2)")
plot(final3$year[final3$cat == "urban"], final3$area[final3$cat == "urban"] / 1000000, type = "b", main = "urban", xlab = "year", ylab = "area (km2)")

#### MELCCFP data
# =====> see script MELCCFP_data_explo_2.r

#### land_use data

files <- list.files("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use_treat", pattern = "6623", full.names = T)

land_use <- data.frame()

for (i in files) {
    df <- read.table(i, h = T)
    land_use <- rbind(land_use, df)
}
names(land_use) <- c("layer", names(land_use)[1:3])
unique(land_use$land_type)

truc <- land_use |>
    group_by(year, land_type) |>
    summarise(sum_pix = sum(pix_num))
plot(x = truc$year[truc$land_type == "Settlement"], y = truc$sum_pix[truc$land_type == "Settlement"], type = "b", main = "Settlement", xlab = "year", ylab = "pixel number")
