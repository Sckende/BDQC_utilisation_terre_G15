library(lubridate)
library(ggplot2)
library(plotly)
library(sf)
library(terra)

# data <- read.table("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/Perte_hab_areas_MAJ_2024-01-22.txt")
data <- read.table("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/results/summary_pix_per_land.txt")
data
summary(data)

# data$year <- year(data$year)

# data2 <- data[data$year %in% c(2000, 2005, 2010, 2015, 2020), ]
# data3 <- data2[data2$hab_type %in% c("urb", "crop", "grassland", "forest", "wetland", "water", "bare"), ]
# data3$hab_type[data3$hab_type == "bare"] <- "other_land"

data3 <- data
names(data3)[1] <- "hab_type"
unique(data3$hab_type)

# plot
# Formatage des data pour la figure
data3$hab_type[data3$hab_type == "Settlement"] <- "Lieux habités"
data3$hab_type[data3$hab_type == "Cropland"] <- "Terres cultivées"
data3$hab_type[data3$hab_type == "Forest"] <- "Forêts"
data3$hab_type[data3$hab_type == "Wetland"] <- "Terres humides"
data3$hab_type[data3$hab_type == "Grassland"] <- "Prairies"
data3$hab_type[data3$hab_type == "Water"] <- "Eaux"
data3$hab_type[data3$hab_type == "Other Land"] <- "Autres terres"

data4 <- data3[!data3$hab_type %in% c("Eaux", "Autres terres"), ]

data4$hab_surf_m2 <- data4$sum * 30 * 30


# habitat_freq <- data3[, -3]
# names(habitat_freq) <- c("Annee", "Habitat", "freq")
data3$hab_type <- as.factor(data3$hab_type)

# Mise en page graphique
# bdqc_colours <- c(
#     "dark_green" = "#24463B",
#     "yellow" = "#E5B54A",
#     "teal" = "#6AB3AF",
#     "ocre" = "#A75822",
#     "beige" = "#FBF8ED"
# )

theme_rlpi <- theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "lightgrey"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 1),
    axis.ticks = element_line(linewidth = 0),
    legend.key = element_blank(),
    legend.title = element_blank()
)

myColors <- c("#24463B", "#E5B54A", "#6AB3AF", "#A75822", "#e77f1e")
names(myColors) <- c("Forêts", "Terres cultivées", "Terres humides", "Lieux habités", "Prairies")

# Contour colour
habitat_plot <- ggplot(data4, aes(x = year, y = hab_surf_m2, colour = hab_type)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = myColors) +
    # labs(x = "Année", y = "Proportion d'habitat (%)") +
    # scale_y_continuous(trans = "log2", breaks = c(0.5, 4, 50), labels = c(0.5, 4, 50)) +
    theme_rlpi

habitat_plot


# Save plot as svg with 575 px x 360 px dimensions
# ggsave(glue::glue("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/fake_g15_util_terres.tif"), plot = habitat_plot, width = 575, height = 360, units = "px", dpi = 100)

### Creation of interactive map with lang use per region ###
qc <- st_read("/home/claire/BDQC-GEOBON/data/QUEBEC_regions/sf_CERQ_SHP/QUEBEC_CR_NIV_01.gpkg")
plot(st_geometry(qc))
qc_ll <- st_transform(qc, st_crs("EPSG:4326"))

p <- ggplot(qc) +
    geom_sf()
p
ggplotly(p) %>%
    highlight(
        "plotly_hover",
        selected = attrs_selected(line = list(color = "black"))
    ) %>%
    widgetframe::frameWidget()

plot_ly(qc, color = I("gray90"), stroke = I("black"), span = I(1))

library(leaflet)
library(leafpop)
img <- "/home/claire/Pictures/Screenshots/test.png"
img2 <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Mount_Eden.jpg/640px-Mount_Eden.jpg"
img3 <- "/home/claire/Desktop/fox.jpg"
img4 <- "https://miro.medium.com/v2/resize:fit:1400/1*jj818i6pGhnuWjwGOi8v8g.jpeg"
img5 <- "https://object-arbutus.cloud.computecanada.ca/bq-io/acer/ebv/rs_ebird.tif"
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
        # popup = popupImage(img5),
        popup = popupGraph(p),
        highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
        )
    )
?xyplot


#### Cas des donnees land use AAC ####
data <- read.table("/home/claire/BDQC-GEOBON/data/g15_indicators/land_use_treat/final_per_region_land_use.txt")
head(data)
t_reg <- strsplit(data$info, "_")
data$region <- unlist(lapply(t_reg, function(x) {
    x[7]
}))

data$year <- unlist(lapply(t_reg, function(x) {
    a <- x[1]
    b <- substring(a, 3, 6)
}))

head(data)
unique(data$region)
qc$NOM_PROV_N

data2 <- data[data$region == "Plateau central du Nord-du-Québec", ]

df2 <- data2[!data2$value %in% c(
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
    "Newly-Detected Road <10 years"
), ]

# groupement en classe
df2$class <- NA

df2$class[df2$value %in% c("Forest", "Forest Wetland", "Forest Regenerating after Harvest <20 years", "Forest Regenerating after Fire <20 years", "Forest Wetland Regenerating after Harvest <20 years")] <- "Forest"

df2$class[df2$value %in% c("Settlement", "Settlement Forest", "Vegetated Settlement", "High Reflectance Settlement", "Very High Reflectance Settlement", "Roads")] <- "Settlement"
df2$class[df2$value %in% c("Cropland")] <- "Cropland"
df2$class[df2$value %in% c("Grassland Unmanaged")] <- "Grassland"
df2$class[df2$value %in% c("Wetland")] <- "Wetland"
df2$class[df2$value %in% c("Water")] <- "Water"
df2$class[df2$value %in% c("Other Land")] <- "Other Land"

df2 <- df2[!is.na(df2$class), ]
unique(df2$class)

df3 <- df2 |>
    group_by(class, year) |>
    summarise(tot_pix = sum(count))
df3

ggplot(df3, aes(x = year, y = tot_pix, group = class)) +
    geom_line(aes(color = class))
