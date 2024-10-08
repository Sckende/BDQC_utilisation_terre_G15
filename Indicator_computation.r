# obtention de la synthese du nombre de pixels par categories avec Narval #

df <- data.frame()
for (y in seq(2000, 2020, 5)) {
    t <- read.table(paste0("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/results/final_pix_per_land_type_", y, ".txt"))
    df <- rbind(df, t)
}

names(df) <- c("layer", names(df)[1:3])
names(df)
unique(df$land_type)

# sous-classes a retirer
# "Forest Regenerating after Harvest 20-29 years",
# "Forest Wetland Regenerating after Harvest 20-29 years",
# "Newly-Detected Settlement <10 years",
# "Newly-Detected Settlement Forest <10 years",
# "Newly-Detected Vegetated Settlement <10 years",
# "Newly-Detected High Reflectance Settlement <10 years",
# "Newly-Detected Very High Reflectance Settlement <10 years",
# "Annual Cropland",
# "Land Converted to Cropland",
# "Land Converted to Annual Cropland",
# "Snow and Ice",
# "Newly-detected Road <10 years"
df2 <- df[!df$land_type %in% c(
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

unique(df2$land_type)

# groupement en classe
df2$class <- NA

df2$class[df2$land_type %in% c("Forest", "Forest Wetland", "Forest Regenerating after Harvest <20 years", "Forest Regenerating after Fire <20 years", "Forest Wetland Regenerating after Harvest <20 years")] <- "Forest"

df2$class[df2$land_type %in% c("Settlement", "Settlement Forest", "Vegetated Settlement", "High Reflectance Settlement", "Very High Reflectance Settlement", "Roads")] <- "Settlement"
df2$class[df2$land_type %in% c("Cropland")] <- "Cropland"
df2$class[df2$land_type %in% c("Grassland Unmanaged")] <- "Grassland"
df2$class[df2$land_type %in% c("Wetland")] <- "Wetland"
df2$class[df2$land_type %in% c("Water")] <- "Water"
df2$class[df2$land_type %in% c("Other Land")] <- "Other Land"

unique(df2$class)

library(dplyr)
df_sum <- df2 %>%
    group_by(class, year) %>%
    summarise(sum = sum(pix_num))
df_sum %>% print(n = 50)
# write.table(df_sum, "/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/results/summary_pix_per_land.txt")
