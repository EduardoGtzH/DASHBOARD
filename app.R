# app.R

library(shiny)
library(shinythemes)
library(DBI)
library(RMariaDB)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(viridis)
library(lubridate)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap", rel = "stylesheet"),
    tags$style(HTML(
      ".main-wrapper { display: grid; grid-template-columns: 280px 1fr; min-height: 100vh; }
       .sidebar { background: #2d3436; color: white; padding: 20px; }
       .sidebar h3 { color: #74b9ff; }
       .main-content { padding: 20px; overflow-y: auto; }
       .plot-card { margin-bottom: 20px; }
       @media (max-width: 768px) { .main-wrapper { grid-template-columns: 1fr; } }"
    ))
  ),
  div(class = "main-wrapper",
      div(class = "sidebar",
          h3("Filtros"),
          # Filtros individuales para gráficas 1-3
          selectInput("filter_pais_1", "Gráf.1: País", choices = NULL),
          selectInput("filter_pais_2", "Gráf.2: País", choices = NULL),
          selectInput("filter_pais_3", "Gráf.3: País", choices = NULL),
          # Filtro para gráficas 7-8 (único país)
          selectInput("filter_pais_78", "Gráf.7-8: País", choices = NULL),
          # Filtros mapa
          sliderInput("map_year", "Mapa: Año", min = 1900, max = 2100, value = 2000, sep = ""),
          radioButtons("map_var", "Variable mapa:", choices = c("Temperatura" = "Temperature", "CO2" = "CO2_Emissions"), selected = "Temperature")
      ),
      div(class = "main-content",
          h1("Dashboard Climático Interactivo", style = "font-family: 'Inter', sans-serif; margin-bottom: 20px;"),
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
          div(class = "plot-card", leafletOutput("map", height = "500px"))
      )
  )
)

server <- function(input, output, session) {
  # Conexión
  conn <- dbConnect(RMariaDB::MariaDB(), dbname = "bd", host = "127.0.0.1", user = "root", password = "root", port = 3306)
  onSessionEnded(function() dbDisconnect(conn))

  # Obtener lista de países
  paises <- dbGetQuery(conn, "SELECT DISTINCT Country FROM ubicaciones")$Country
  updateSelectInput(session, "filter_pais_1", choices = paises, selected = paises[1])
  updateSelectInput(session, "filter_pais_2", choices = paises, selected = paises[1])
  updateSelectInput(session, "filter_pais_3", choices = paises, selected = paises[1])
  updateSelectInput(session, "filter_pais_78", choices = paises, selected = paises[1])

  # Data base all
  base_data <- reactive({
    dbGetQuery(conn, "SELECT d.*, f.Date, u.Country
                 FROM datos_clima d
                 JOIN fechas f ON d.ID=f.ID
                 JOIN ubicaciones u ON d.ID_ubi=u.ID_ubi")
  })

  # Helper
  validate_data <- function(df) validate(need(nrow(df)>0, "No hay datos para estos filtros"))

  # 1. Temp vs CO2 por país
  output$plot1 <- renderPlotly({
    df <- base_data() %>% filter(Country==input$filter_pais_1)
    validate_data(df)
    p <- ggplot(df, aes(x=CO2_Emissions, y=Temperature)) + geom_point() +
      labs(title="Temp vs CO2", x="CO2 ppm", y="Temp °C") + theme_minimal()
    ggplotly(p)
  })
  # 2. Tendencia Temp trimestral por país
  output$plot2 <- renderPlotly({
    df <- base_data() %>% filter(Country==input$filter_pais_2) %>%
      mutate(Y=year(Date), Q=quarter(Date)) %>%
      group_by(Y,Q)%>%summarise(Avg=mean(Temperature,na.rm=TRUE),.groups='drop')
    validate_data(df)
    df <- df %>% mutate(Period=paste0(Y,'-Q',Q))
    p <- ggplot(df,aes(Period,Avg,group=Y,color=factor(Y)))+geom_line()+geom_point()+
      labs(title="Tendencia Temp Trimestral", x="Periodo", y="Avg Temp °C") + theme_minimal()
    ggplotly(p)
  })
  # 3. Tendencia CO2 mensual por país
  output$plot3 <- renderPlotly({
    df <- base_data() %>% filter(Country==input$filter_pais_3) %>%
      mutate(Y=year(Date), M=month(Date)) %>%
      group_by(Y,M)%>%summarise(Avg=mean(CO2_Emissions,na.rm=TRUE),.groups='drop')
    validate_data(df)
    df <- df %>% mutate(Period=sprintf('%d-%02d',Y,M))
    p <- ggplot(df,aes(Period,Avg,group=Y,color=factor(Y)))+geom_line()+geom_point()+
      labs(title="Tendencia CO2 Mensual", x="Periodo", y="Avg CO2 ppm") + theme_minimal()
    ggplotly(p)
  })
  # 4. Top10 Temp prom
  output$plot4 <- renderPlotly({
    df <- base_data() %>% group_by(Country)%>%summarise(Avg=mean(Temperature,na.rm=TRUE),.groups='drop') %>%
      arrange(desc(Avg)) %>% head(10)
    validate_data(df)
    p <- ggplot(df,aes(reorder(Country,Avg),Avg,fill=Avg))+geom_col()+coord_flip()+
      labs(title="Top10 Temp Prom", x="País", y="Temp °C")+theme_minimal()
    ggplotly(p)
  })
  # 5. Top10 CO2 total
  output$plot5 <- renderPlotly({
    df <- base_data() %>% group_by(Country)%>%summarise(Tot=sum(CO2_Emissions,na.rm=TRUE),.groups='drop') %>%
      arrange(desc(Tot)) %>% head(10)
    validate_data(df)
    p <- ggplot(df,aes(reorder(Country,Tot),Tot,fill=Tot))+geom_col()+coord_flip()+
      labs(title="Top10 CO2 Total", x="País", y="CO2 ppm")+theme_minimal()
    ggplotly(p)
  })
  # 6. Evol Nivel mar
  output$plot6 <- renderPlotly({
    df <- base_data() %>% mutate(Y=year(Date))%>% group_by(Y)%>%summarise(Avg=mean(Sea_Level_Rise,na.rm=TRUE),.groups='drop')
    validate_data(df)
    p <- ggplot(df,aes(Y,Avg))+geom_area(fill='#00cec9',alpha=0.4)+geom_line()+
      labs(title="Nivel Mar Anual", x="Año", y="Mar (m)")+theme_minimal()
    ggplotly(p)
  })
  # 7. Humedad por un país (evitar saturación)
  output$plot7 <- renderPlotly({
    df <- base_data() %>% filter(Country==input$filter_pais_78)
    validate_data(df)
    p <- ggplot(df,aes(x=month(Date,label=TRUE),y=Humidity))+geom_boxplot()+
      labs(title="Humedad Mensual", x="Mes", y="Humedad (%)")+theme_minimal()
    ggplotly(p)
  })
  # 8. Viento por un país
  output$plot8 <- renderPlotly({
    df <- base_data() %>% filter(Country==input$filter_pais_78)
    validate_data(df)
    p <- ggplot(df,aes(x=month(Date,label=TRUE),y=Wind_Speed))+geom_violin()+
      labs(title="Viento Mensual", x="Mes", y="Vel (km/h)")+theme_minimal()
    ggplotly(p)
  })
  # 9. Mapa con filtro año y var
  output$map <- renderLeaflet({
    var <- input$map_var
    df <- base_data() %>% filter(year(Date)==input$map_year) %>%
      group_by(Country)%>%summarise(Value=mean(.data[[var]],na.rm=TRUE),.groups='drop')
    world <- ne_countries(returnclass='sf')
    md <- left_join(world, df, by=c('name'='Country'))
    pal <- colorNumeric('YlOrRd', domain=md$Value, na.color='gray')
    leaflet(md)%>%addTiles()%>%addPolygons(fillColor=~pal(Value),weight=0.5,color='white',fillOpacity=0.8,highlight=highlightOptions(weight=1.5,bringToFront=TRUE),
      label=~paste0(name,": ",round(Value,1))) %>% addLegend('bottomright',pal=pal,values=~Value,title=input$map_var)
  })
}

shinyApp(ui, server)
