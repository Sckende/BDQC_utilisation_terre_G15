# Obj: comparaison des resultats produits par ISQ et ce qu'on obtient avec les donnees ESA

# Les limites géographiques du territoire d’étude par l'ISQ sont définies au nord par la couverture géographique commune au 3e et au 4e inventaire écoforestier, et à l’ouest, au sud et à l’est par le Système sur les découpages administratifs (SDA) du MERN.

# objectif est d'utiliser plusieurs regions admin couvertes totalement par ISQ pour comparer avec les donnees de ESA
# ==> cf tableau 2.2.1 dans https://statistique.quebec.ca/fr/fichier/comptes-des-terres-du-quebec-meridional-edition-revisee.pdf

# ---- packages
library(stringr)
library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(rstac)
library(plotly)

# ---- data pour decoupage admin
dec <- st_read("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/QUEBEC_regions/BDGA_1M(adm)_SHP/regio_s.shp")
plot(st_geometry(dec))

# selection des regions d'interet (12 régions pour lesquelles ISQ a une synthese)
# dec2 <- dec[dec$RES_NM_REG %in% c(
#     "Bas-Saint-Laurent",
#     "Capitale-Nationale",
#     "Estrie",
#     "Montréal",
#     "Outaouais",
#     "Abitibi-Témiscamingue",
#     "Gaspésie–Îles-de-la-Madeleine",
#     "Chaudière-Appalaches",
#     "Laval",
#     "Lanaudière",
#     "Laurentides",
#     "Montérégie"
# ), ]
# plot(st_geometry(dec2))
dec2 <- dec
# conversion to the same CRS than ESA
dec2 <- st_transform(dec2, crs = st_crs(4326))

# ---- ESA map
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

# ---- travail a echelle qc
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


    for (j in 1:nrow(dec2)) {
        poly <- dec2[j, ]
        lc_rast <- crop(rast, poly)
        lc_rast <- mask(lc_rast, poly)
        print("crop & mask done")

        freq <- as.data.frame(freq(lc_rast))
        v <- freq[, 2:3]
        names(v) <- c("cat1", "cat1_count")
        v$layer <- ids[i]
        v$year <- substr(v$layer, 11, 14)
        v$region <- poly$RES_NM_REG

        cat_tot <- rbind(cat_tot, v)
    }
}
# saveRDS(cat_tot, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_frq_cat1_per_reg.rds")
# ---- association avec IPCC categories
esalc_cat <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/CCI-LC_Maps_Legend.csv")

# ---- Synthese par region, par annee et par categories
cat_tot2 <- left_join(cat_tot, esalc_cat[, c("cat1", "IPCC_class", "IPCC_class_code")], by = join_by(cat1))
freq_synth <- cat_tot2 |>
    group_by(region, year, IPCC_class) |>
    summarise(count_tot = sum(cat1_count))

# ---- calcul du taux d'evolution dans le temps

fq_ls <- split(freq_synth, list(freq_synth$region, freq_synth$IPCC_class))
comp <- lapply(fq_ls, function(x) {
    x$comp2010 <- (x$count_tot - x$count_tot[x$year == 2010])
    x$comp_rate <- (x$comp2010 * 100) / x$count_tot[x$year == 2010]
    x
})

comp2010_tot <- do.call("rbind", comp)
d <- comp2010_tot |>
    group_by(region, year) |>
    summarise(pix_tot = sum(count_tot))
d2 <- d[d$year == 2010, ]
comp2010_tot <- left_join(comp2010_tot, d2[, c("region", "pix_tot")])
comp2010_tot$year <- as.numeric(comp2010_tot$year)

# ---- verifier si toutes les IPCC_class sont presentes dans chaque region
ipcc_class <- unique(comp2010_tot$IPCC_class)
class <- split(comp2010_tot, comp2010_tot$region)
class2 <- lapply(class, function(x) {
    miss_cl <- ipcc_class[!ipcc_class %in% unique(x$IPCC_class)]
    if (length(miss_cl) != 0) {
        df <- data.frame()
        for (i in 1:length(miss_cl)) {
            d <- data.frame(
                region = unique(x$region),
                year = 2010:2020,
                IPCC_class = miss_cl[i],
                count_tot = NA,
                comp2010 = NA,
                comp_rate = NA,
                pix_tot = 0
            )
            df <- rbind(df, d)
        }
    }
    x <- rbind(x, df)

    x
})

cl_df <- do.call("rbind", class2)
# saveRDS(cl_df, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_comp2010_ISQ_reg.rds")

# ---- Visualisation
# variation des taux
visu_ls <- split(comp2010_tot, comp2010_tot$region)

for (i in 1:length(visu_ls)) {
    x <- visu_ls[[i]]
    # x11()

    p4 <- ggplot(
        data = x,
        aes(x = year, y = comp_rate, group = IPCC_class, colour = IPCC_class)
    ) +
        geom_line(linewidth = 1) +
        labs(title = unique(x$region), color = "Catégories") +
        scale_color_manual(
            labels = c("agriculture", "forest", "grassland", "other", "settlement", "wetland"),
            values = c(
                "#993300",
                "#006600",
                "#CC9900",
                "#96ac9d",
                "#CC0000",
                "#019191"
            )
        ) +
        # scale_x_continuous(name = "Année", limits = c(2010, 2020), breaks = 2010:2020) +
        scale_y_continuous(name = "Variation (%)", limits = c(-35, 35))
    # print(p4)

    ggsave(file = paste0("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Indicators/G15_utilisation_terres/region_used_by_ISQ/2010-2020_utilisation_terres_esa_", unique(x$region), ".svg"), plot = p4, width = 10, height = 8)
}

# ---- proportion de type d'habitat à l'année 0 (soit 2010)





d <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_comp2010_ISQ_reg.rds")
dd <- d[d$year == 2010, c("region", "count_tot", "IPCC_class")]
dd_ls <- split(dd, dd$region)
data <- dd_ls[[1]]
colors <- c(
    "#993300", # agriculture
    "#006600", # forest
    "#CC9900", # grassland
    "#96ac9d", # other
    "#CC0000", # settlement
    "#019191" # wetland
)

fig <- plot_ly(data,
    labels = ~IPCC_class, values = ~count_tot, type = "pie", textposition = "inside",
    textinfo = "label+percent",
    insidetextfont = list(color = "#FFFFFF"),
    hoverinfo = "text",
    text = ~ paste("n = ", count_tot),
    marker = list(
        colors = colors,
        line = list(color = "#FFFFFF", width = 1)
    ),

    # The 'pull' attribute can also be used to create space between the sectors

    showlegend = FALSE
)

fig <- fig %>% layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
)


fig
