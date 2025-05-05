# ui.R
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
      .sidebar h2 { margin: 0; font-weight: bold; font-size: 1.3em; }
      .sidebar h3 { margin: 4px 0; font-size: 1.1em; font-weight: normal; }
      .sidebar .small-label { font-size: 0.75em; margin-top: 12px; color: #888888; }
      .sidebar .tech-list { margin: 4px 0 12px 15px; line-height: 1.4; font-size: 0.75em; }
      .sidebar hr { border-color: rgba(255,255,255,0.2); }
      .main-content { padding: 20px; overflow-y: auto; }
      .plot-card { margin-bottom: 20px; }
      @media (max-width: 768px) { .main-wrapper { grid-template-columns: 1fr; } }
    ")),
    uiOutput("dynamicCSS")
  ),

  div(class = "main-wrapper",

    ## SIDEBAR
    div(class = "sidebar",
      h2("Facultad de Ingeniería"),
      h3("Bases de datos avanzadas"),
      h3("Proyecto final"),

      div(class = "small-label", "Tecnología usada:"),
      tags$ul(class = "tech-list",
        tags$li(strong("Datos:"),       " DBI, RMariaDB, dplyr"),
        tags$li(strong("Backend:"),     " Shiny, DBI"),
        tags$li(strong("Frontend:"),    " shinythemes, plotly, leaflet"),
        tags$li(strong("Plantilla:"),   " CSS Grid, Google Font Inter"),
        tags$li(strong("Arquitectura:")," ui.R & server.R, reactividad"),
        tags$li(strong("Graficadores:")," ggplot2, viridis, sf, rnaturalearth")
      ),
      hr(),

      selectInput("filter_pais_1",    "Temperatura vs CO₂: País",        choices = NULL),
      selectInput("filter_pais_2",    "Tendencia Temp Trimestral: País", choices = NULL),
      selectInput("filter_pais_3",    "Tendencia CO₂ Mensual: País",     choices = NULL),
      selectInput("filter_pais_78",   "Humedad/Viento: País",            choices = NULL),

      selectInput("top_temp_year",    "Top 10 Temp: Año",                choices = NULL),
      selectInput("top_co2_year",     "Top 10 CO₂: Año",                 choices = NULL),
      hr(),

      sliderInput("map_year",         "Mapa: Años",                      min = 1900, max = 2100, value = c(1900, 2100), sep = ""),
      selectInput("map_var",          "Variable mapa:",
                   choices = c(
                     "Temperatura"     = "Temperature",
                     "CO₂"              = "CO2_Emissions",
                     "Nivel del mar"    = "Sea_Level_Rise",
                     "Humedad"          = "Humidity",
                     "Precipitación"    = "Precipitation",
                     "Velocidad viento" = "Wind_Speed"
                   ),
                   selected = "Temperature"
      ),
      actionButton("btn_bg", "Cambiar color", icon = icon("adjust"), width = "100%"),
      hr(),

      numericInput(
        "pred_year",
        "Año a predecir:",
        value = NA,    # se actualizará en server
        min   = NA,
        max   = NA,
        step  = 1
      )
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
        column(6, div(class = "plot-card", plotlyOutput("plot6", height = "300px")))
      ),
      fluidRow(
        column(6, div(class = "plot-card", plotlyOutput("plot7", height = "300px"))),
        column(6, div(class = "plot-card", plotlyOutput("plot8", height = "300px")))
      ),
      fluidRow(
        column(6, div(class = "plot-card", plotlyOutput("plot4", height = "300px"))),
        column(6, div(class = "plot-card", plotlyOutput("plot5", height = "300px")))
      ),

      div(class = "plot-card", leafletOutput("map", height = "500px")),

      div(class = "plot-card", plotlyOutput("plot_pred", height = "300px")),
      div(style = "margin-bottom:20px;",
          strong("Temperatura predicha:"), textOutput("pred_text")
      )
    )
  )
)
