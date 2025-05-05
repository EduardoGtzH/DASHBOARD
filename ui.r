#shiny::runApp("C:/Users/gabos/Documents/GitHub/DASHBOARD")
#shiny::runApp("C:/Users/Eduar/OneDrive/Escritorio/DBOARD/DASHBOARD/app.R") # nolint

library(shiny)
library(shinythemes)
library(leaflet)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap", rel="stylesheet"),
    tags$style(HTML("
      /* Layout principal */
      .main-wrapper {
        display: grid;
        grid-template-columns: 280px 1fr;
        min-height: 100vh;
      }

      /* Barra lateral */
      .sidebar {
        background: #2d3436;
        padding: 25px;
        display: flex;
        flex-direction: column;
        gap: 20px;
        border-right: 1px solid #4a4a4a;
      }

      .logo-container {
        text-align: center;
        padding: 15px;
      }

      .logo {
        width: 180px;
        height: auto;
      }

      .greeting {
        color: #74b9ff;
        text-align: center;
        font-size: 1.1em;
        padding: 15px;
        margin: 0;
        background: rgba(255,255,255,0.05);
        border-radius: 8px;
      }

      /* Botones corregidos */
      .nav-button {
        display: block;
        width: 100%;
        padding: 15px 25px;
        border-radius: 8px;
        background: transparent;
        color: #ffffff !important;
        border: 2px solid #74b9ff;
        transition: all 0.3s ease;
        text-align: left;
        margin: 8px 0;
        cursor: pointer;
      }

      .nav-button:hover {
        background: rgba(116,185,255,0.1);
        transform: translateX(10px);
      }

      /* Contenido principal completo */
      .main-content {
        padding: 40px;
        background: #f8f9fa;
        height: 100vh;
        overflow-y: auto;
      }

      .plot-card {
        background: white;
        border-radius: 12px;
        padding: 25px;
        margin-bottom: 30px;
        box-shadow: 0 3px 12px rgba(0,0,0,0.05);
        min-height: 300px;
      }

      @media (max-width: 768px) {
        .main-wrapper {
          grid-template-columns: 1fr;
        }
        
        .sidebar {
          border-right: none;
          border-bottom: 1px solid #4a4a4a;
        }
        
        .main-content {
          padding: 25px;
          height: auto;
        }
      }
    "))
  ),
  
  div(class = "main-wrapper",
      div(class = "sidebar",
          div(class = "logo-container",
              tags$img(src = "Logo.png", class = "logo", alt = "Logo")
          ),
          div(class = "greeting", "Facultad de Ingieniera\nBases de datos avanzadas\nTecnologia usada:\nTodo el proyecto fue realizado en R con las siguiente bibliotecas:\nshiny\nDBI\nRMariaDB\ndplyr\nggplot2\nleaflet\nshinythemes\nrnaturalearth\nsf"),
          
      ),
      
      div(class = "main-content",
          h1("Dashboard Climático Interactivo", 
             style = "color: #2d3436; margin-bottom: 40px;"),
          
          div(id = "temp-co2", class = "plot-card", plotOutput("plotTempCO2", height = "300px")),
          div(id = "trend-temp", class = "plot-card", plotOutput("plotTrendTemp", height = "300px")),
          div(id = "trend-co2", class = "plot-card", plotOutput("plotTrendCO2", height = "300px")),
          div(id = "top-temp", class = "plot-card", plotOutput("plotTopTemp", height = "300px")),
          div(id = "top-co2", class = "plot-card", plotOutput("plotTopCO2", height = "300px")),
          div(id = "sea-level", class = "plot-card", plotOutput("plotSeaLevelRise", height = "300px")),
          div(id = "humidity-precip", class = "plot-card", plotOutput("plotHumidityPrecip", height = "300px")),
          div(id = "wind-speed", class = "plot-card", plotOutput("plotWindSpeed", height = "300px")),
          div(id = "map", class = "plot-card", leafletOutput("map", height = "650px"))
      )
  ),
  
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      // Mapeo de navegación mejorado
      const navConfig = {
        nav_temp_co2: { target: '#temp-co2', offset: -80 },
        nav_trend_temp: { target: '#trend-temp', offset: -80 },
        nav_trend_co2: { target: '#trend-co2', offset: -80 },
        nav_top_temp: { target: '#top-temp', offset: -80 },
        nav_top_co2: { target: '#top-co2', offset: -80 },
        nav_sea_level: { target: '#sea-level', offset: -80 },
        nav_humidity: { target: '#humidity-precip', offset: -80 },
        nav_wind: { target: '#wind-speed', offset: -80 },
        nav_records: { target: '#records', offset: -80 },
        nav_map: { target: '#map', offset: -80 }
      };

      Object.entries(navConfig).forEach(([buttonId, config]) => {
        const btn = document.getElementById(buttonId);
        const target = document.querySelector(config.target);
        
        if (btn && target) {
          btn.addEventListener('click', () => {
            const targetPosition = target.getBoundingClientRect().top + window.pageYOffset + config.offset;
            window.scrollTo({
              top: targetPosition,
              behavior: 'smooth'
            });
          });
        }
      });
    });
  "))
)
