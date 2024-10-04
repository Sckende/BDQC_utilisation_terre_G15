# fake figure with old data
library(lubridate)

data <- read.table("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/Perte_hab_areas_MAJ_2024-01-22.txt")
data
summary(data)

data$year <- year(data$year)

data2 <- data[data$year %in% c(2000, 2005, 2010, 2015, 2020), ]
data3 <- data2[data2$hab_type %in% c("urb", "crop", "grassland", "forest", "wetland", "water", "bare"), ]
data3$hab_type[data3$hab_type == "bare"] <- "other_land"

unique(data3$hab_type)

# plot
# Formatage des data pour la figure
data3$hab_type[data3$hab_type == "urb"] <- "Lieux habités"
data3$hab_type[data3$hab_type == "crop"] <- "Terres cultivées"
data3$hab_type[data3$hab_type == "forest"] <- "Forêts"
data3$hab_type[data3$hab_type == "wetland"] <- "Terres humides"
data3$hab_type[data3$hab_type == "grassland"] <- "Prairies"
data3$hab_type[data3$hab_type == "water"] <- "Eaux"
data3$hab_type[data3$hab_type == "other_land"] <- "Autres terres"

habitat_freq <- data3[, -3]
names(habitat_freq) <- c("Annee", "Habitat", "freq")
habitat_freq$Habitat <- as.factor(habitat_freq$Habitat)
habitat_freq$Annee <- as.numeric(habitat_freq$Annee)

# Mise en page graphique
bdqc_colours <- c(
    "dark_green" = "#24463B",
    "yellow" = "#E5B54A",
    "teal" = "#6AB3AF",
    "ocre" = "#A75822",
    "beige" = "#FBF8ED"
)

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

myColors <- c("#24463B", "#E5B54A", "#6AB3AF", "#A75822")
names(myColors) <- c("Forêts", "Terres cultivées", "Terres humides", "Lieux habités")

# Contour colour
habitat_plot <- ggplot(habitat_freq, aes(x = Annee, y = freq, colour = Habitat)) +
    geom_line(linewidth = 1) +
    # scale_color_manual(values = myColors) +
    labs(x = "Année", y = "Proportion d'habitat (%)") +
    scale_y_continuous(trans = "log2", breaks = c(0.5, 4, 50), labels = c(0.5, 4, 50)) +
    theme_rlpi

habitat_plot

# Save plot as svg with 575 px x 360 px dimensions
ggsave(glue::glue("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/fake_g15_util_terres.tif"), plot = habitat_plot, width = 575, height = 360, units = "px", dpi = 100)
