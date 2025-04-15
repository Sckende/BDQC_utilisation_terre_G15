#### ISQ vs ESA ####
#### Cas d'étude - Estrie ####
#### pour la periode 2000 - 2010

# ---- Polygone Estrie
reg <- st_read("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/QUEBEC_regions/BDGA_1M(adm)_SHP/regio_s.shp")
reg_conv <- st_transform(reg, crs = st_crs(4326))

estrie <- reg_conv[reg_conv$RES_NM_REG == "Estrie", ]

# ---- ESA raster 2000 & 2010
io <- stac("https://io.biodiversite-quebec.ca/stac/")

ids <- io |>
    stac_search(collections = "esacci-lc") |>
    post_request() |>
    items_fetch() |>
    _$features |>
    sapply(X = _, function(i) {
        i$id
    })
# Conserver deux couches - 2000 & 2010
id2000 <- ids[21]
id2010 <- ids[11]

url_esa2000 <- io |>
    stac_search(collections = "esacci-lc") |>
    post_request() |>
    items_fetch() |>
    _$features[[which(ids == id2000)]]$assets[[1]]$href
esa2000 <- rast(paste0("/vsicurl/", url_esa2000))

url_esa2010 <- io |>
    stac_search(collections = "esacci-lc") |>
    post_request() |>
    items_fetch() |>
    _$features[[which(ids == id2010)]]$assets[[1]]$href
esa2010 <- rast(paste0("/vsicurl/", url_esa2010))


es1 <- crop(esa2000, estrie)
es2000 <- mask(es1, estrie)

es2 <- crop(esa2010, estrie)
es2010 <- mask(es2, estrie)

x11(0, height = 24, width = 24)
par(mfrow = c(1, 2))
plot(es2000)
plot(es2010)

f2000 <- as.data.frame(freq(es2000))
f2000$layer <- 2000
f2010 <- as.data.frame(freq(es2010))
f2010$layer <- 2010

# ---- aggregation de categories
cat <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/CCI-LC_Maps_Legend.csv")
cat$IPCC_modif <- cat$IPCC_class
cat$IPCC_modif[cat$desc0 == "water"] <- "water"

f2000c <- left_join(f2000, cat[, c("IPCC_modif", "IPCC_class", "IPCC_class_code", "cat1")], by = join_by(value == cat1))
f2010c <- left_join(f2010, cat[, c("IPCC_modif", "IPCC_class", "IPCC_class_code", "cat1")], by = join_by(value == cat1))

sum_2000 <- f2000c |>
    group_by(IPCC_modif) |>
    summarise(n_pix = sum(count))
sum_2010 <- f2010c |>
    group_by(IPCC_modif) |>
    summarise(n_pix = sum(count))
sum(sum_2010$n_pix)

sum_2000$pourc_recouv <- sum_2000$n_pix / sum(sum_2000$n_pix) * 100
sum_2010$pourc_recouv <- sum_2010$n_pix / sum(sum_2010$n_pix) * 100

sum_2010$pourc_recouv - sum_2000$pourc_recouv

((sum_2010$n_pix - sum_2000$n_pix) / sum(sum_2000$n_pix)) * 100


verif <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_frq_cat1_per_reg.rds")
ver_es <- verif[verif$region == "Estrie" & verif$year == 2010, ]
ver_es2 <- left_join(ver_es, cat[, c("cat1", "IPCC_modif")])
es_sum <- ver_es2 |>
    group_by(IPCC_modif) |>
    summarise(n_pix = sum(cat1_count)) |>
    mutate(prop = n_pix / sum(n_pix) * 100)


isq_nat <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/ISQ/QC_meridional_natural_land_prop_2010.csv", h = T, encoding = "latin1")

isq_nat_estrie <- isq_nat[isq_nat$region == "Estrie", ]
ESA_nat <- sum_2010[sum_2010$IPCC_modif %in% c("forest", "water", "wetland"), ]

#### ISQ vs ESA ####
#### Comparaison prop mil. naturels####
#### pour 2010

# ---- Donnees ESA
hab_nat_reg <- data.frame()

for (i in 1:nrow(reg_conv)) {
    reg <- reg_conv$RES_NM_REG[i]
    ver_es <- verif[verif$region == reg & verif$year == 2010, ]
    ver_es2 <- left_join(ver_es, cat[, c("cat1", "IPCC_modif")])
    es_sum <- ver_es2 |>
        group_by(IPCC_modif) |>
        summarise(n_pix = sum(cat1_count)) |>
        mutate(prop = n_pix / sum(n_pix) * 100)
    es_sum$region <- reg
    es_sum$year <- 2010
    es_sum <- es_sum[es_sum$IPCC_modif %in% c("forest", "water", "wetland"), ]

    hab_nat_reg <- rbind(hab_nat_reg, es_sum)
}
hab_nat_reg
# saveRDS(hab_nat_reg, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/ESA_prop_hab_nat_2010.rds")

# ---- Donnees ISQ
isq_nat
names(isq_nat)
isq2010 <- isq_nat[isq_nat$year == 2010, ]

reg <- "Laval"
esa <- hab_nat_reg[hab_nat_reg$region == reg, ]
esa$data <- "esa"
names(esa)[1] <- "nat_hab"

isq <- isq2010[isq2010$region == reg, ]
isq$prop <- isq$prop * 100
isq$data <- "isq"
isq$region <- reg



data3 <- rbind(isq, esa[, c("region", "nat_hab", "prop", "year", "data")])
data3 <- data3 |> mutate(text = sprintf("Données: %s<br> Proportion: %s <br>", data, round(prop, digits = 2)))
data3 <- data3[order(data3$data, data3$nat_hab), ]

# data <- data.frame(x, y, y2, text)


fig <- data3 %>% plot_ly()

fig <- fig %>% add_trace(
    x = data3$nat_hab[data3$data == "esa"],
    y = data3$prop[data3$data == "esa"],
    type = "bar",
    text = data3$text[data3$data == "esa"],
    textposition = "auto",
    marker = list(
        color = c("rgba(46,72,62,0.8)", "rgba(123,181,177, 0.8)", "rgba(166,97,45, 0.8)"),
        line = list(
            color = c("rgba(46,72,62,1)", "rgba(123,181,177, 1)", "rgba(166,97,45, 1)"),
            width = 1.5
        )
    )
)

fig <- fig %>% add_trace(
    x = data3$nat_hab[data3$data == "isq"],
    y = data3$prop[data3$data == "isq"],
    type = "bar",
    text = data3$text[data3$data == "isq"],
    textposition = "auto",
    marker = list(
        color = c("rgba(46,72,62,0.5)", "rgba(123,181,177, 0.5)", "rgba(166,97,45, 0.5)"),
        line = list(
            color = c("rgba(46,72,62,1)", "rgba(123,181,177, 1)", "rgba(166,97,45, 1)"),
            width = 1.5
        )
    )
)

fig <- fig %>%
    layout(
        title = "Comparaison proportion milieux naturels ESA - ISQ",
        barmode = "group",
        xaxis = list(title = ""),
        yaxis = list(title = "")
    ) |>
    style(hoverinfo = "none") |>
    layout(showlegend = FALSE)


fig
