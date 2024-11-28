library(stringr)
library(ggplot2)

dt <- read.table("/home/local/USHERBROOKE/juhc3201/BDQC-GEOBON/data/g15_indicators/results/AAC/AAC_final_per_region_land_use.txt")
head(dt)
info_ls <- strsplit(dt$info, "_")

inf <- lapply(info_ls, function(x) {
    year <- substring(x[1], 3, 6)
    reg <- x[7]

    obj <- data.frame(year, reg)
})
info <- do.call("rbind", inf)

dt2 <- cbind(dt, info)

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

df_colors <- data.frame(class = unique(dt3$class), color = c("brown2", "chocolate4", "deepskyblue3", "darkolivegreen", "aquamarine4"))
df_colors2 <- data.frame(class2 = unique(dt3$class2), color = c("brown2", "aquamarine4"))

# ----- #
# Viz per region per class1

dt_reg <- split(dt3, dt3$reg)
length(dt_reg) # 20 regions

reg_cl1 <- lapply(dt_reg, function(x) {
    reg <- unique(x$reg)
    j <- x |>
        group_by(class, year) |>
        summarise(sum = sum(count))
    j$reg <- reg
    j <- left_join(j, df_colors, c("class"))
    j
})

reg_cl2 <- lapply(dt_reg, function(x) {
    reg <- unique(x$reg)
    j <- x |>
        group_by(class2, year) |>
        summarise(sum = sum(count))
    j$reg <- reg
    j$year <- as.numeric(j$year)
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
