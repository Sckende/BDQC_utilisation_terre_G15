library(stringr)
library(ggplot2)
library(dplyr)
library(sf)
library(terra)

##########
#### ----- ####
#### Analyse des donnees brutes ####
#### ----- ####
reg_ecol <- st_read("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/QUEBEC_regions/sf_CERQ_SHP/QUEBEC_CR_NIV_01.gpkg")

coll_clip <- list()

for (y in seq(2000, 2020, 5)) {
    # for (y in c(2020)) {
    u17 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/AAC_land_use_raw/", y, "/LU", y, "_u17/LU", y, "_u17_v4_2022_02.tif"))
    u18 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/AAC_land_use_raw/", y, "/LU", y, "_u18/LU", y, "_u18_v4_2022_02.tif"))
    u19 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/AAC_land_use_raw/", y, "/LU", y, "_u19/LU", y, "_u19_v4_2022_02.tif"))
    u20 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/AAC_land_use_raw/", y, "/LU", y, "_u20/LU", y, "_u20_v4_2022_02.tif"))
    u21 <- rast(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/AAC_land_use_raw/", y, "/LU", y, "_u21/LU", y, "_u21_v4_2022_02.tif"))

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
            # print(map_m)
            coll_clip <- c(coll_clip, map_m)
        }
    }
}
names(coll_clip) <- unlist(lapply(coll_clip, varnames))

# print(length(coll_clip))
# print(str(coll_clip))
# print(coll_clip)

final <- data.frame()

for (i in 1:length(coll_clip)) {
    fq <- terra::freq(coll_clip[[i]])
    fq$info <- names(coll_clip)[i]

    final <- rbind(final, fq)
}

write.table(final, "/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/results/AAC/AAC_final_per_region_land_use_local_compute.txt")

##########
#### ----- ####
#### Visualisation a partir de l'analyse des donnees brutes ####
#### ----- ####

# sup_qc <- 1542056 # km2 from wikipedia
qc <- st_read("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/QUEBEC_regions/sf_CERQ_SHP/QUEBEC_CR_NIV_01.gpkg")
# dt <- read.table("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/results/AAC/AAC_raw_land_use_per_region.txt")
# dt <- read.table("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/results/AAC/AAC_final_per_region_land_use.txt")
dt <- read.table("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/results/AAC/AAC_final_per_region_land_use_local_compute.txt")
# dt <- dt[!is.na(dt$value), ]
head(dt)
info_ls <- strsplit(dt$info, "_")

inf <- lapply(info_ls, function(x) {
    year <- substring(x[1], 3, 6)
    # reg <- x[7]
    reg <- x[6]

    obj <- data.frame(year, reg)
})
info <- do.call("rbind", inf)

dt2 <- cbind(dt, info)

sup_qc_km2 <- sum(dt2$count[dt2$year == 2000], na.rm = TRUE) * 30 * 30 / 1000000

# ---- #
# class treatment
dt3 <- dt2[!dt2$value %in% c(
    "Forest Regenerating after Harvest 20-29 years",
    "Forest Wetland Regenerating after Harvest 20-29 years",
    "Newly-Detected Settlement <10 years",
    "Newly-Detected Settlement Forest <10 years",
    "Newly-Detected Vegetated Settlement <10 years",
    "Newly-Detected High Reflectance Settlement <10 years",
    "Newly-Detected Very High Reflectance Settlement <10 years",
    "Annual Cropland",
    "Land Converted to Cropland",
    "Land Converted to Annual Cropland",
    "Snow and Ice",
    "Newly-Detected Road <10 years",
    "Grassland Unmanaged",
    "Other Land"
), ]

# groupement en classe premier niveau
dt3$class <- NA

dt3$class[dt3$value %in% c("Forest", "Forest Wetland", "Forest Regenerating after Harvest <20 years", "Forest Regenerating after Fire <20 years", "Forest Wetland Regenerating after Harvest <20 years")] <- "Forestier"

dt3$class[dt3$value %in% c("Settlement", "Settlement Forest", "Vegetated Settlement", "High Reflectance Settlement", "Very High Reflectance Settlement", "Roads")] <- "Anthropique"
dt3$class[dt3$value %in% c("Cropland")] <- "Agricole"
# dt3$class[dt3$value %in% c("Grassland Unmanaged")] <- "Grassland"
dt3$class[dt3$value %in% c("Wetland")] <- "Humide"
dt3$class[dt3$value %in% c("Water")] <- "Aquatique"
# dt3$class[dt3$value %in% c("Other Land")] <- "Other Land"

dt3 <- dt3[!is.na(dt3$class), ]

# groupement en classe deuxieme niveau
dt3$class2[dt3$class == "Anthropique"] <- "Anthropique"
dt3$class2[is.na(dt3$class2)] <- "Naturel"

df_colors <- data.frame(class = unique(dt3$class), color = c("brown2", "deepskyblue3", "darkolivegreen", "chocolate4", "aquamarine4"))
df_colors2 <- data.frame(class2 = unique(dt3$class2), color = c("brown2", "aquamarine4"))

qc_small <- as.data.frame(qc[, c("FID01", "NOM_PROV_N")])
qc_small <- qc_small[, c(1, 2)]
dt3 <- left_join(dt3, qc_small, by = join_by("reg" == "NOM_PROV_N"))
dt3 <- dt3[!is.na(dt3$count), ]

#### ----- ####
#### Calcul de proportion pour le QC ####
#### ----- ####

# ici prendre la proportion en fonction du nombre de pixels total par region ?
per_class <- dt3 |>
    group_by(class, year) |>
    summarize(sum_pix = sum(count))

per_class$prop <- (per_class$sum_pix * 30 * 30 / 1000000) * 100 / sup_qc_km2

per_class |> print(n = 25)

#### Visualisation ####
myColors <- df_colors$color
names(myColors) <- df_colors$class
ggplot(per_class, aes(x = as.numeric(year), y = prop, color = class)) +
    geom_line(linewidth = 1.5) +
    scale_color_manual(values = myColors) +
    xlab("Année") +
    ylab("Proportion (%)")

#### ----- ####
#### Calcul de proportion par region per class ####
#### ----- ####
dt3 |>
    group_by(reg, year) |>
    summarize(count_pix = sum(count)) |>
    print(n = 100) ### WARNING ! Reprendre ici ! Pas le même nombre de pixels par region !!! ###

dt_reg <- split(dt3, dt3$reg)
length(dt_reg) # 20 regions

reg_cl1 <- lapply(dt_reg, function(x) {
    reg <- unique(x$reg)

    j <- x |>
        group_by(class, year) |>
        summarise(sum = sum(count))
    j$reg <- reg
    j$area_km2 <- (j$sum * 30 * 30) / 1000000
    j$prop <- (j$area_km2 / sup_qc) * 100
    j <- left_join(j, df_colors, c("class"))
    j
})

reg_cl2 <- lapply(dt_reg, function(x) {
    reg <- unique(x$reg)
    j <- x |>
        group_by(class2, year) |>
        summarise(sum = sum(count))
    j$reg <- reg
    j$area_km2 <- (j$sum * 30 * 30) / 1000000
    j$prop <- (j$area_km2 / sup_qc) * 100
    j <- left_join(j, df_colors2, c("class2"))
    j
})

x11()
par(mfrow = c(4, 5))
# lapply(reg_cl1, function(x){
# ggplot(x, aes(x = as.numeric(year), y = sum, color = class)) +
#     geom_line()
# })



lapply(reg_cl1, function(x) {
    max_pix <- max(x$sum)
    x_ls <- split(x, x$class)
    l <- length(x_ls)
    plot(x_ls[[1]]$year, x_ls[[1]]$sum,
        ylim = c(0, max_pix + 10),
        type = "l", col = unique(x_ls[[1]]$color),
        bty = "n",
        xlab = "année",
        ylab = "nombre de pixels",
        main = unique(x_ls[[1]]$reg)
    )

    for (i in 2:l) {
        lines(x_ls[[i]]$year, x_ls[[i]]$sum,
            col = unique(x_ls[[i]])$color
        )
    }
})


# ----- #
x11()
par(mfrow = c(4, 5))

lapply(reg_cl2, function(x) {
    max_pix <- max(x$sum)
    x_ls <- split(x, x$class2)
    l <- length(x_ls)
    plot(x_ls[[1]]$year, x_ls[[1]]$sum,
        ylim = c(0, max_pix + 10),
        type = "l", col = unique(x_ls[[1]]$color),
        bty = "n",
        xlab = "année",
        ylab = "nombre de pixels",
        main = unique(x_ls[[1]]$reg)
    )

    for (i in 2:l) {
        lines(x_ls[[i]]$year, x_ls[[i]]$sum,
            col = unique(x_ls[[i]])$color
        )
    }
})

#### Graphic par region ####
# ----------------------- #

d <- reg_cl1[[5]]
ggplot(d, aes(x = as.numeric(year), y = prop, color = class)) +
    geom_line()


#### Viz interactive ####
# -------------------- #

library(lubridate)
library(ggplot2)
library(plotly)
library(sf)
library(terra)
st_read("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/QUEBEC_regions/sf_CERQ_SHP/QUEBEC_CR_NIV_01.gpkg")
plot(st_geometry(qc))
qc_ll <- st_transform(qc, st_crs("EPSG:4326"))

# p <- ggplot(qc) +
#   geom_sf()
# p
# ggplotly(p) %>%
#   highlight(
#     "plotly_hover",
#     selected = attrs_selected(line = list(color = "black"))
#   ) %>%
#   widgetframe::frameWidget()

library(leaflet)
library(leafpop)
img <- "/home/claire/Pictures/Screenshots/test.png"
img2 <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Mount_Eden.jpg/640px-Mount_Eden.jpg"
img3 <- "/home/claire/Desktop/fox.jpg"
img4 <- "https://miro.medium.com/v2/resize:fit:1400/1*jj818i6pGhnuWjwGOi8v8g.jpeg"
img5 <- "https://object-arbutus.cloud.computecanada.ca/bq-io/acer/ebv/rs_ebird.tif"

qc_ll$graph <- rep(c("https://upload.wikimedia.org/wikipedia/commons/thumb/3/30/Vulpes_vulpes_ssp_fulvus.jpg/800px-Vulpes_vulpes_ssp_fulvus.jpg", "https://shop.wwf.ca/cdn/shop/files/Dmitry-Deshevykh-WWF-Russia_red_fox.jpg?v=1694548960&width=1024", "https://naturecanada.ca/wp-content/uploads/2023/04/Red_Foxes_1980x1080.jpg", "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSyyCOQt9SxqPW_DEGrx2jfxolcRv_xSmeOy6GXrOhlTvJAr9f6R3sawaX-bwNLlaiNfpk&usqp=CAU"), 5)
p <- ggplot(data = meuse, aes(x = copper, y = cadmium, fill = as.factor(round(elev)))) +
    geom_point()
p

leaflet(qc_ll) %>%
    addTiles() %>%
    fitBounds(
        lng1 = -79.76332, # st_bbox(qc)[1],
        lat1 = 44.99136, # st_bbox(qc)[2],
        lng2 = -56.93521, # st_bbox(qc)[3],
        lat2 = 62.58192 # st_bbox(qc)[4]
    ) %>%
    addPolygons(
        fillColor = "grey",
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        layerId = qc_ll$NOM_PROV_N,
        popup = popupImage(qc_ll$graph),
        # popup = popupImage("/home/local/USHERBROOKE/juhc3201/Pictures/arctic_fox.jpg"),
        # popup = popupGraph(p),
        highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
        )
    )

# Remplacer foxes par lien local des graphes qui seront enregistres dans une colonne du dataframe final
