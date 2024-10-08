library(lubridate)
library(ggplot2)

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
