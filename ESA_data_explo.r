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

# Conserver unique a partir de 2010 pour eviter le pb de changement methodologique
ids <- ids[1:11]

url <- io |>
    stac_search(collections = "esacci-lc") |>
    post_request() |>
    items_fetch() |>
    _$features[[which(ids == ids[1])]]$assets[[1]]$href

# esalc92 <- rast(paste0("/vsicurl/", url))
esalc20 <- rast(paste0("/vsicurl/", url))
x11()
par(mfrow = c(2, 1))
# plot(esalc92)
plot(esalc20)
# summary(values(esalc92))
crs(esalc20)

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

# Echelle du QC
# evolution des categories cat0 - Cropland, Tree_cover, Shrubland, Lichens_mosses, Sparse_toundra, artificial, Bare, water, pmnt_snow_ice
esalc_cat <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/CCI-LC_Maps_Legend.csv")

cat_freq0 <- data.frame()

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
    names(df1) <- "cat1"

    df11 <- left_join(df1, esalc_cat[, c("cat1", "cat0")], by = "cat1")

    lc_rast1 <- lc_rast
    values(lc_rast1) <- df11$cat0

    f2 <- freq(lc_rast1)
    tot <- sum(f2$count)

    f2$cat0_prop <- f2$count / tot
    f2$id <- ids[i]
    cat_freq0 <- rbind(cat_freq0, f2)
}
names(cat_freq0)[2] <- "cat0"
info_cat0 <- esalc_cat[, c("cat0", "desc0")]
info_cat0 <- info_cat0[!duplicated(info_cat0), ]
cat_freq00 <- left_join(cat_freq0, info_cat0, by = "cat0")
cat_freq00$year <- substring(cat_freq00$id, 11, 14)
# write.csv(cat_freq00, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_frq_cat0_Qc.csv")

cat_freq00 <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_frq_cat0_Qc.csv", h = T)

#### visualisation
df <- cat_freq00 |>
    group_by(year, desc0)

ggplot(
    data = df,
    aes(x = year, y = cat0_prop, color = desc0)
) +
    geom_line(linewidth = 1)

#### Calcul du taux de variation par rapport à l'année 1 (2010) ####
# test pour "Tree_cover"
# tc <- cat_freq00 |> filter(desc0 == "Tree_cover")
# tc <- tc[order(tc$year), ]
# tc$comp_2010 <- tc$cat0_prop - tc$cat0_prop[1]
# plot(x = tc$year, y = tc$comp_2010, type = "b")
# --- #
# Application
comp_2010 <- cat_freq00[order(cat_freq00$year), ]
comp_2010_ll <- split(comp_2010, comp_2010$cat0)
comp_2010_ll2 <- lapply(comp_2010_ll, function(x) {
    x$comp_2010 <- x$cat0_prop - x$cat0_prop[1]
    x
})

c2010 <- do.call("rbind", comp_2010_ll2)

df2 <- c2010 |>
    group_by(year, desc0)
p <- ggplot(
    data = df2,
    aes(x = round(year), y = comp_2010 * 100, color = desc0)
) +
    geom_line(linewidth = 1) +
    labs(color = "Catégories") +
    scale_color_manual(
        labels = c("artificiel", "terrain nu", "terre agricole", "prairie", "lichens & mousses", "arbustaie", "terre type toundra", "forêt", "eau", "milieu humide"),
        values = c(
            "#CC0000",
            "#FFCCCC",
            "#993300",
            "#FFCC00",
            "#CC9900",
            "#33CC66",
            "#999900",
            "#006600",
            "#0000CC",
            "#006666"
        )
    ) +
    scale_x_continuous(name = "Année", limits = c(2010, 2020), breaks = 2010:2020) +
    scale_y_continuous(name = "Variation (%)", limits = c(-1.25, 1.25))

ggsave(file = "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/2010-2020_utilisation_terres_esa.svg", plot = p, width = 10, height = 8)

# ---- #
# Version avec la categorie "Autres terres" qui regroupe "Bare", "Lichens_mosses", "Shrubland", "Sparse_tundra"
esalc_cat <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/CCI-LC_Maps_Legend.csv")
esalc_cat$desc4 <- esalc_cat$desc0
esalc_cat$desc4[esalc_cat$desc0 %in% c("Bare", "Lichens_mosses", "Shrubland", "Sparse_toundra")] <- "Other_land"
esalc_cat$cat4 <- esalc_cat$cat0
esalc_cat$cat4[esalc_cat$cat0 %in% c(2, 5, 7, 8)] <- 12

# calcul des frequences
cat4_freq <- data.frame()

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
    names(df1) <- "cat1"

    df11 <- left_join(df1, esalc_cat[, c("cat1", "cat4")], by = "cat1")

    lc_rast1 <- lc_rast
    values(lc_rast1) <- df11$cat4

    f2 <- freq(lc_rast1)
    tot <- sum(f2$count)

    f2$cat4_prop <- f2$count / tot
    f2$id <- ids[i]
    cat4_freq <- rbind(cat4_freq, f2)
}
names(cat4_freq)[2] <- "cat4"
info_cat4 <- esalc_cat[, c("cat4", "desc4")]
info_cat4 <- info_cat4[!duplicated(info_cat4), ]
cat4_freq2 <- left_join(cat4_freq, info_cat4, by = "cat4")
cat4_freq2$year <- substring(cat4_freq2$id, 11, 14)
# write.csv(cat4_freq2, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_frq_cat4_Qc.csv")

cat4_fq <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_frq_cat4_Qc.csv", h = T)
# Calcul du taux de variation par rapport à l'année 1 (2010) #

comp_2010_cat4 <- cat4_fq[order(cat4_fq$year), ]
comp_2010_cat4_ll <- split(comp_2010_cat4, comp_2010_cat4$cat4)
comp_2010_cat4_ll2 <- lapply(comp_2010_cat4_ll, function(x) {
    x$comp_cat4_2010 <- x$cat4_prop - x$cat4_prop[1]
    x
})

cat4_2010 <- do.call("rbind", comp_2010_cat4_ll2)
cat4_2010 <- cat4_2010[cat4_2010$desc4 != "water", ]

df2_4 <- cat4_2010 |>
    group_by(year, desc4)
p4 <- ggplot(
    data = df2_4,
    aes(x = round(year), y = comp_cat4_2010 * 100, color = desc4)
) +
    geom_line(linewidth = 1) +
    labs(color = "Catégories") +
    scale_color_manual(
        labels = c("artificiel", "terre agricole", "prairie", "autres terres", "forêt", "milieu humide"),
        values = c(
            "#CC0000",
            "#993300",
            "#CC9900",
            "#33CC66",
            "#006600",
            "#006666"
        )
    ) +
    scale_x_continuous(name = "Année", limits = c(2010, 2020), breaks = 2010:2020) +
    scale_y_continuous(name = "Variation (%)", limits = c(-1.25, 1.25))

ggsave(file = "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/2010-2020_utilisation_terres_esa_version2.svg", plot = p4, width = 10, height = 8)


# ------------------------------------- #
#### evolution naturel vs artificiel ####
# ------------------------------------- #
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


### Visualisation artif vs nat per region ###
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

# visualisation des taux de variation par rapport à l'année 1
qc_tx <- cat_freq2[cat_freq2$reg == "Qc", ]
qc_tx <- qc_tx[1:11, ]
qc_tx2 <- data.frame(
    year = rep(2020:2010, 2),
    cat2 = c(rep("natural", length(2020:2010)), rep("artificial", length(2020:2010))),
    prop2 = c(qc_tx$nat_prop, qc_tx$artif_prop)
)
qc_tx2_ll <- split(qc_tx2, qc_tx2$cat2)
qc_tx2_lll <- lapply(qc_tx2_ll, function(x) {
    x$comp_2010 <- x$prop2 - x$prop2[x$year == 2010]
    x
})

qc_c2010 <- do.call("rbind", qc_tx2_lll)

ggplot(
    data = qc_c2010,
    aes(x = year, y = comp_2010, color = cat2)
) +
    geom_line(linewidth = 1)
