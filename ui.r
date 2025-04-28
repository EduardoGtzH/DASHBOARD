library(shiny)
library(shinythemes)
library(leaflet)

# sólo definimos 'ui', no shinyApp()
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Cambio Climático"),
  fluidRow(
    column(6, plotOutput("plotTempCO2")),
    column(6, plotOutput("plotTrendTemp")),
    column(6, plotOutput("plotTrendCO2")),
    column(6, plotOutput("plotTopTemp")),
    column(6, plotOutput("plotTopCO2")),
    column(6, plotOutput("plotSeaLevelRise")),
    column(6, plotOutput("plotHumidityPrecip")),
    column(6, plotOutput("plotWindSpeed")),
    column(6, plotOutput("plotRecordsPerLocation")),
    column(12, leafletOutput("map", height = "600px"))
  )
)
