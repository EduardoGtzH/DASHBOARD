library(shiny)
library(shinythemes)
library(leaflet)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .plot-box {
        padding: 15px;
        margin-bottom: 20px;
        background: #ffffff;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    "))
  ),
  titlePanel("Cambio Climático"),
  fluidRow(
    column(6, div(class="plot-box", plotOutput("plotTempCO2",        height="300px"))),
    column(6, div(class="plot-box", plotOutput("plotTrendTemp",      height="300px")))
  ),
  fluidRow(
    column(6, div(class="plot-box", plotOutput("plotTrendCO2",       height="300px"))),
    column(6, div(class="plot-box", plotOutput("plotTopTemp",        height="300px")))
  ),
  fluidRow(
    column(6, div(class="plot-box", plotOutput("plotTopCO2",         height="300px"))),
    column(6, div(class="plot-box", plotOutput("plotSeaLevelRise",   height="300px")))
  ),
  fluidRow(
    column(6, div(class="plot-box", plotOutput("plotHumidityPrecip", height="300px"))),
    column(6, div(class="plot-box", plotOutput("plotWindSpeed",      height="300px")))
  ),
  fluidRow(
    column(6, div(class="plot-box", plotOutput("plotRecordsPerLocation", height="300px"))),
    column(6, div(class="plot-box", leafletOutput("map", height="600px")))
  )
)
