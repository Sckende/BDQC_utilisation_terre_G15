# ================================================================================
# Chargement packages & data
# ================================================================================

#### Packages ####
# -------------- #
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyhelper)

library(sf)
library(dplyr)
library(stringr)
library(knitr)
library(plotly)
library(ggplot2)
library(terra)

#### Local data ####
# ---------------- #
# utilisation des terres
comp <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/g15_indicators/results/ESA/2010-2020_comp2010_ISQ_reg.rds")

# Several Polygons for Qc
# -----------------------
# ---- data pour decoupage admin
dec <- st_read("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/Data/QUEBEC_regions/BDGA_1M(adm)_SHP/regio_s.shp")
# selection des regions d'interet
admin <- dec[dec$RES_NM_REG %in% c(
    "Bas-Saint-Laurent",
    "Capitale-Nationale",
    "Mauricie",
    "Estrie",
    "Montréal",
    "Outaouais",
    "Abitibi-Témiscamingue",
    "Gaspésie–Îles-de-la-Madeleine",
    "Chaudière-Appalaches",
    "Laval",
    "Lanaudière",
    "Laurentides",
    "Centre-du-Québec"
), ]


server <- function(input, output, session) {
    observe_helpers()


    # Ref map plot
    output$map_ref <- renderPlot({
        par(mar = rep(0, 4))
        plot(st_geometry(dec), col = ifelse(dec$RES_NM_REG == input$admin, "red", "grey"))
    })
}

ui <- navbarPage(
    "Évolution de l'utilisation des terres au Qc",
    sidebarLayout(
        sidebarPanel(
            h4("Région administrative"),
            selectInput("admin_select",
                label = "",
                choices = admin$RES_NM_REG[order(admin$RES_NM_REG)]
            )
        ),
        plotOutput("map_ref", width = "100%"),
    ),
    mainPanel(
        # First row
        # fluidRow(
        #     box(
        #         title = "Pie chart",
        #         width = 4,
        #         plotOutput()
        #     ),
        #     box(
        #         title = "Trend",
        #         width = 4,
        #         plotOutput()
        #     )
        # )
    )
)

shinyApp(ui = ui, server = server)
