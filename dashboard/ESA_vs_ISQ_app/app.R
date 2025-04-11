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
    "Estrie",
    "Montréal",
    "Outaouais",
    "Abitibi-Témiscamingue",
    "Gaspésie–Îles-de-la-Madeleine",
    "Chaudière-Appalaches",
    "Laval",
    "Lanaudière",
    "Laurentides",
    "Montérégie"
), ]


server <- function(input, output, session) {
    observe_helpers()


    # Ref map plot
    output$map_ref <- renderPlot({
        par(mar = rep(0, 4))
        plot(st_geometry(dec), col = ifelse(dec$RES_NM_REG %in% admin$RES_NM_REG, "grey", "#bebebe67"))
        plot(st_geometry(dec[dec$RES_NM_REG == input$admin_select, ]), col = "red", add = TRUE)
    })

    # trend plot
    output$trend_plot <- renderPlot({
        x <- comp[comp$region == input$admin_select, ]

        ggplot(
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
            )
        # +
        # scale_x_continuous(name = "Année", limits = c(2010, 2020), breaks = 2010:2020) +
        # scale_y_continuous(name = "Variation (%)", limits = c(-35, 35))
    })

    # pie chart
    output$pie <- renderPlotly({
        data <- comp[comp$region == input$admin_select & comp$year == 2010, ]
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
    })
}

ui <- navbarPage(
    "Évolution de l'utilisation des terres au Qc",
    sidebarLayout(
        sidebarPanel(
            h4("Région administrative"),
            selectInput(
                inputId = "admin_select",
                label = "",
                choices = admin$RES_NM_REG[order(admin$RES_NM_REG)]
            )
            # )
            ,
            plotOutput("map_ref", width = "100%")
        ),
        mainPanel(
            # First row
            box(
                title = "Trend",
                width = 6,
                plotOutput("trend_plot")
            ),
            box(
                title = "Pie chart",
                width = 6,
                plotlyOutput("pie")
            )
        )
    )
)
shinyApp(ui = ui, server = server)
