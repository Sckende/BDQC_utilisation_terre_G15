# Generation synthese nb pixels par categorie d'utilisation du territoire
files <- list.files("/home/claire/BDQC-GEOBON/data/g15_indicators/MELCCFP_utilisation_territoire", full.names = T)
land_use <- data.frame()

for (i in files) {
    df <- read.table(i, h = T)
    land_use <- rbind(land_use, df)
}

head(land_use)
names(land_use) <- c("id", names(land_use)[-length(names(land_use))])
unique(land_use$land_type)
land_use$land_type[land_use$land_type %in% c("Coupe et regénération", "Coupe et régénération")] <- "Coupes et régénérations"
land_use$land_type[land_use$land_type %in% c("Sol nu et lande", "Sol nus et landes")] <- "Sols nus et landes"




library(dplyr)

df1 <- land_use |>
    group_by(year, land_type) |>
    summarise(pix = sum(pix_num))


library(ggplot2)
ggplot(df1, aes(x = year, y = pix, color = land_type)) +
    geom_line()
