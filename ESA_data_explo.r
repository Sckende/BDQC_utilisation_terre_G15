library(stringr)
library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(rstac)

# extraction ESA from IO #
# ---------------------- #

io <- stac("https://io.biodiversite-quebec.ca/stac/")

ids <- io |>
    stac_search(collections = "esacci-lc") |>
    post_request() |>
    items_fetch() |>
    _$features |>
    sapply(X = _, function(i) {
        i$id
    })

url <- io |>
    stac_search(collections = "esacci-lc") |>
    post_request() |>
    items_fetch() |>
    _$features[[which(ids == ids[10])]]$assets[[1]]$href

esalc92 <- rast(paste0("/vsicurl/", url))
esalc20 <- rast(paste0("/vsicurl/", url))
x11()
par(mfrow = c(2, 1))
plot(esalc92)
plot(esalc20)
summary(values(esalc92))
crs(esalc92)

qc <- st_read("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/QUEBEC_regions/sf_CERQ_SHP/QUEBEC_CR_NIV_01.gpkg")
plot(st_geometry(qc))
crs(qc)
qc_ll <- st_transform(qc, crs = st_crs(esalc20))

## travail a echelle qc
# recup de toutes les categories possibles
cat_tot <- data.frame()
for (i in 1:length(ids)) {
    print(paste0("----------> ", ids[i]))
    url <- io |>
        stac_search(collections = "esacci-lc") |>
        post_request() |>
        items_fetch() |>
        _$features[[which(ids == ids[i])]]$assets[[1]]$href
    print("url retrieved")

    rast <- rast(paste0("/vsicurl/", url))
    lc_rast <- crop(rast, qc_ll)
    lc_rast <- mask(lc_rast, qc_ll)
    print("crop & mask done")

    v <- as.data.frame(unique(values(lc_rast)))
    names(v) <- "cat"
    v$layer <- ids[i]

    cat_tot <- rbind(cat_tot, v)
}
(u1 <- unique(cat_tot$layer))
length(u1)
(u2 <- unique(cat_tot$cat))
length(u2)

# => toutes contenues dans legende des categories esa

# evolution naturel vs artificiel
esalc_cat <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/CCI-LC_Maps_Legend.csv")

cat_freq <- data.frame()

for (i in 1:length(ids)) {
    print(paste0("----------> ", ids[i]))
    url <- io |>
        stac_search(collections = "esacci-lc") |>
        post_request() |>
        items_fetch() |>
        _$features[[which(ids == ids[i])]]$assets[[1]]$href
    print("url retrieved")

    rast <- rast(paste0("/vsicurl/", url))
    lc_rast <- crop(rast, qc_ll)
    lc_rast <- mask(lc_rast, qc_ll)
    print("crop & mask done")

    # modifications des baleurs du raster
    f1 <- freq(lc_rast)
    df1 <- as.data.frame(values(lc_rast))
    names(df1) <- "cat"

    df2 <- df1
    df2$cat[df2$cat %in% esalc_cat$cat1[esalc_cat$desc3 == "artificial"]] <- 1
    df2$cat[df2$cat %in% esalc_cat$cat1[esalc_cat$desc3 == "natural"]] <- 2
    lc_rast1 <- lc_rast
    values(lc_rast1) <- df2$cat
    f2 <- freq(lc_rast1)
    tot <- sum(f2$count)

    df <- data.frame(
        id = ids[i],
        pix_tot = tot,
        nat_prop = f2$count[f2$value == 2] / tot,
        artif_prop = f2$count[f2$value == 1] / tot,
        reg = "Qc"
    )

    cat_freq <- rbind(cat_freq, df)
}

cat_freq$reg <- "Qc"

## travail par region
cat_freq2 <- cat_freq
for (i in 1:length(ids)) {
    print(paste0("----------> ", ids[i]))
    url <- io |>
        stac_search(collections = "esacci-lc") |>
        post_request() |>
        items_fetch() |>
        _$features[[which(ids == ids[i])]]$assets[[1]]$href
    print("url retrieved")

    rast <- rast(paste0("/vsicurl/", url))

    for (j in 1:length(qc_ll$NAM_PROV_N)) {
        poly <- qc_ll[j, ]
        print(poly$NAM_PROV_N)
        lc_rast <- crop(rast, poly)
        lc_rast <- mask(lc_rast, poly)
        print("crop & mask done")

        # modifications des baleurs du raster
        f1 <- freq(lc_rast)
        df1 <- as.data.frame(values(lc_rast))
        names(df1) <- "cat"

        df2 <- df1
        df2$cat[df2$cat %in% esalc_cat$cat1[esalc_cat$desc3 == "artificial"]] <- 1
        df2$cat[df2$cat %in% esalc_cat$cat1[esalc_cat$desc3 == "natural"]] <- 2
        lc_rast1 <- lc_rast
        values(lc_rast1) <- df2$cat
        f2 <- freq(lc_rast1)
        # completion du df en cas de valeurs absentes
        if (is.element(1, f2$value) == FALSE) {
            f2 <- rbind(f2, data.frame(layer = 1, value = 1, count = 0))
        }
        if (is.element(2, f2$value) == FALSE) {
            f2 <- rbind(f2, data.frame(layer = 1, value = 2, count = 0))
        }
        tot <- sum(f2$count)

        df <- data.frame(
            id = ids[i],
            pix_tot = tot,
            nat_prop = f2$count[f2$value == 2] / tot,
            artif_prop = f2$count[f2$value == 1] / tot,
            reg = poly$NAM_PROV_N
        )

        cat_freq2 <- rbind(cat_freq2, df)
    }
}

# write.csv(cat_freq2, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/1992-2020_artif_vs_nat_per_reg.csv")

cat_freq2 <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/1992-2020_artif_vs_nat_per_reg.csv", h = T, sep = ",")

cat_reg_ls <- split(cat_freq2, cat_freq2$reg)
cat_reg_ls
length(cat_reg_ls)


### Visuaisation artif vs nat per region ###
x11()
par(mfrow = c(3, 7))

lapply(cat_reg_ls, function(x) {
    x$year <- 2020:1992
    #     x2 <- data.frame(year = rep(x$year, 2),
    #                 pix_tot = rep(x$pix_tot, 2),
    #                 cat = c(rep("natural", length(x$id)), rep("artificial", length(x$id))),
    #                 prop = c(x$nat_prop, x$artif_prop))

    # ggplot(data=x2, aes(x=year, y=prop, fill=cat)) +
    #   geom_bar(position = "fill", stat = "identity")

    barplot(cbind(nat_prop, artif_prop) ~ year, data = x, main = unique(x$reg), col = c("#859320FF", "#EF8A0CFF"), border = T, space = 0)
})


x2 <- data.frame(
    year = rep(x$year, 2),
    pix_tot = rep(x$pix_tot, 2),
    cat = c(rep("natural", length(x$id)), rep("artificial", length(x$id))),
    prop = c(x$nat_prop, x$artif_prop)
)


ggplot(data = x2, aes(x = year, y = prop, fill = cat)) +
    geom_bar(position = "fill", stat = "identity")












plot(lc_rast)
test <- lc_rast
values(test)[values(test) != c(160, 170, 180, 210)] <- NA
plot(test, col = "red", maxcell = 1e7)
plot(st_geometry(qc_ll), add = T)

lc_qc <- crop(esalc92, qc_ll)
lc_qc <- mask(lc_qc, qc_ll)

plot(lc_qc)

v <- as.data.frame(unique(values(lc_qc)))

## travail par region
lau_mer <- qc_ll[qc_ll$NOM_PROV_N == "Les Laurentides méridionales", ]
plot(st_geometry(lau_mer))

lc_laumer <- crop(esalc20, lau_mer)
lc_laumer <- mask(lc_laumer, lau_mer)
plot(lc_laumer)


bass_stla <- qc_ll[qc_ll$NOM_PROV_N == "Basses-terres du Saint-Laurent", ]
lc_bassstla <- crop(esalc92, bass_stla)
lc_bassstla <- mask(lc_bassstla, bass_stla)
plot(lc_bassstla)
table(values(lc_bassstla))

utm <- project(lc_bassstla, "EPSG:6623")


## recuperation de toutes les categories possibles pour une region

t <- st_read("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/QUEBEC_regions/QUEBEC_Vectors_details.gpkg")
