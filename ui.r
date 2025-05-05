library(shiny)
library(shinythemes)
library(plotly)
library(leaflet)

ui <- fluidPage(
  theme = shinytheme("flatly"),

  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap",
      rel   = "stylesheet"
    ),
    tags$style(HTML("
      .main-wrapper { display: grid; grid-template-columns: 280px 1fr; min-height: 100vh; }
      .sidebar {
        background: #2d3436;
        color: white;
        padding: 20px;
        display: flex;
        flex-direction: column;
        gap: 15px;
        font-family: 'Inter', sans-serif;
      }
      .sidebar h2 {
        margin: 0;
        font-weight: bold;
        font-size: 1.3em;
      }
      .sidebar h3 {
        margin: 4px 0;
        font-size: 1.1em;
        font-weight: normal;
      }
      .sidebar .small-label {
        font-size: 0.85em;
        margin-top: 12px;
        color: #cccccc;
      }
      .sidebar .tech-list {
        margin: 4px 0 12px 15px;
        line-height: 1.4;
      }
      .sidebar hr {
        border-color: rgba(255,255,255,0.2);
      }
      .main-content {
        padding: 20px;
        overflow-y: auto;
      }
      .plot-card {
        margin-bottom: 20px;
      }
      @media (max-width: 768px) {
        .main-wrapper { grid-template-columns: 1fr; }
      }
    ")),
    uiOutput("dynamicCSS")
  ),

  div(class = "main-wrapper",

    ## SIDEBAR
div(class = "sidebar",
  h2("Facultad de Ingeniería"),
  h3("Bases de datos avanzadas"),
  h3("Proyecto final"),

  div(class = "small-label", style = "color: #888888; font-size: 0.75em;", "Tecnología usada:"),
  tags$ul(class = "tech-list",
    tags$li(strong("Datos:"),       " DBI, RMariaDB, dplyr"),
    tags$li(strong("Backend:"),     " Shiny, DBI"),
    tags$li(strong("Frontend:"),    " shinythemes, plotly, leaflet"),
    tags$li(strong("Plantilla:"),   " CSS Grid, Google Font Inter"),
    tags$li(strong("Arquitectura:")," ui.R & server.R, reactividad"),
    tags$li(strong("Graficadores:")," ggplot2, viridis, sf, rnaturalearth")
  ),
  hr(),

      selectInput("filter_pais_1",  "Gráf.1: País",   choices = NULL),
      selectInput("filter_pais_2",  "Gráf.2: País",   choices = NULL),
      selectInput("filter_pais_3",  "Gráf.3: País",   choices = NULL),
      selectInput("filter_pais_78", "Gráf.7-8: País", choices = NULL),
      sliderInput("map_year",       "Mapa: Año",      min = 1900, max = 2100, value = 2000, sep = ""),
      radioButtons("map_var",       "Variable mapa:",
                   choices  = c("Temperatura" = "Temperature", "CO₂" = "CO2_Emissions"),
                   selected = "Temperature"),
      actionButton("btn_bg", "Cambiar color", icon = icon("adjust"), width = "100%")
    ),

    ## MAIN CONTENT
    div(class = "main-content",
      h1("Dashboard Cambio Climático", style = "margin-bottom: 15px;"),

      fluidRow(
        column(6, div(class = "plot-card", plotlyOutput("plot1", height = "300px"))),
        column(6, div(class = "plot-card", plotlyOutput("plot2", height = "300px")))
      ),
      fluidRow(
        column(6, div(class = "plot-card", plotlyOutput("plot3", height = "300px"))),
        column(6, div(class = "plot-card", plotlyOutput("plot4", height = "300px")))
      ),
      fluidRow(
        column(6, div(class = "plot-card", plotlyOutput("plot5", height = "300px"))),
        column(6, div(class = "plot-card", plotlyOutput("plot6", height = "300px")))
      ),
      fluidRow(
        column(6, div(class = "plot-card", plotlyOutput("plot7", height = "300px"))),
        column(6, div(class = "plot-card", plotlyOutput("plot8", height = "300px")))
      ),
      div(class = "plot-card",
          leafletOutput("map", height = "500px")
      )
    )
  )
)
