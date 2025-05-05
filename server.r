library(shiny)
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

server <- function(input, output, session) {

  # 1) Conexión y carga inicial
  conn <- dbConnect(RMariaDB::MariaDB(),
                    dbname   = "bd",
                    host     = "127.0.0.1",
                    user     = "root",
                    password = "root",
                    port     = 3306)
  session$onSessionEnded(function() dbDisconnect(conn))

  base_q <- "
    SELECT d.*, f.Date, u.Country
    FROM datos_clima d
    JOIN fechas f        ON d.ID     = f.ID
    JOIN ubicaciones u   ON d.ID_ubi = u.ID_ubi
  "
  clima_raw <- reactive({
    dbGetQuery(conn, base_q) %>%
      mutate(
        Date = as.Date(Date),
        Year = year(Date)
      )
  })

  # 2) Poblado inicial de filtros
  observe({
    df     <- clima_raw()
    años   <- sort(unique(df$Year))
    países <- sort(unique(df$Country))

    updateSliderInput(session, "map_year",
                      min   = min(años),
                      max   = max(años),
                      value = min(años)
    )
    for(id in c("filter_pais_1","filter_pais_2","filter_pais_3","filter_pais_78")) {
      updateSelectInput(session, id,
                        choices  = países,
                        selected = países[1]
      )
    }
  })

  # 3) Reactivos por filtro individual
  df1   <- reactive({ req(input$filter_pais_1);  clima_raw() %>% filter(Country==input$filter_pais_1) })
  df2   <- reactive({ req(input$filter_pais_2);  clima_raw() %>% filter(Country==input$filter_pais_2) })
  df3   <- reactive({ req(input$filter_pais_3);  clima_raw() %>% filter(Country==input$filter_pais_3) })
  df78  <- reactive({ req(input$filter_pais_78); clima_raw() %>% filter(Country==input$filter_pais_78) })
  dfMap <- reactive({
    req(input$map_year, input$map_var)
    clima_raw() %>%
      filter(Year == input$map_year) %>%
      group_by(Country) %>%
      summarise(Value = mean(.data[[input$map_var]], na.rm = TRUE), .groups='drop')
  })

  validate_data <- function(d) validate(need(nrow(d)>0, "No hay datos para estos filtros"))

  # 4) Gráficas 1–8
  output$plot1 <- renderPlotly({
    df <- df1(); validate_data(df)
    p <- ggplot(df, aes(CO2_Emissions, Temperature)) +
      geom_point(color="#1f77b4", alpha=0.7) +
      labs(title="Temp vs CO₂", x="CO₂ (ppm)", y="Temp (°C)") +
      theme_minimal(base_size=14)
    ggplotly(p)
  })

  output$plot2 <- renderPlotly({
    df <- df2() %>%
      mutate(Y=year(Date), Q=quarter(Date)) %>%
      group_by(Y,Q) %>%
      summarise(Avg=mean(Temperature,na.rm=TRUE), .groups='drop') %>%
      mutate(Period=paste0(Y,'-Q',Q))
    validate_data(df)
    p <- ggplot(df, aes(Period, Avg, group=Y, color=factor(Y))) +
      geom_line(size=1) + geom_point(size=2) +
      labs(title="Tendencia Temp Trimestral", x="Periodo", y="Temp (°C)") +
      theme_minimal(base_size=14) +
      theme(axis.text.x = element_text(angle=45,hjust=1))
    ggplotly(p)
  })

  output$plot3 <- renderPlotly({
    df <- df3() %>%
      mutate(Y=year(Date), M=month(Date)) %>%
      group_by(Y,M) %>%
      summarise(Avg=mean(CO2_Emissions,na.rm=TRUE), .groups='drop') %>%
      mutate(Period=sprintf('%d-%02d',Y,M))
    validate_data(df)
    p <- ggplot(df, aes(Period, Avg, group=Y, color=factor(Y))) +
      geom_line(size=1) + geom_point(size=2) +
      labs(title="Tendencia CO₂ Mensual", x="Periodo", y="CO₂ (ppm)") +
      theme_minimal(base_size=14) +
      theme(axis.text.x = element_text(angle=45,hjust=1))
    ggplotly(p)
  })

  output$plot4 <- renderPlotly({
    df <- clima_raw() %>%
      group_by(Country) %>%
      summarise(Avg=mean(Temperature,na.rm=TRUE), .groups='drop') %>%
      arrange(desc(Avg)) %>% head(10)
    validate_data(df)
    p <- ggplot(df, aes(reorder(Country,Avg), Avg, fill=Avg)) +
      geom_col() + coord_flip() +
      labs(title="Top 10 Temp Prom", x="País", y="Temp (°C)") +
      scale_fill_viridis_c() +
      theme_minimal(base_size=14)
    ggplotly(p)
  })

  output$plot5 <- renderPlotly({
    df <- clima_raw() %>%
      group_by(Country) %>%
      summarise(Tot=sum(CO2_Emissions,na.rm=TRUE), .groups='drop') %>%
      arrange(desc(Tot)) %>% head(10)
    validate_data(df)
    p <- ggplot(df, aes(reorder(Country,Tot), Tot, fill=Tot)) +
      geom_col() + coord_flip() +
      labs(title="Top 10 CO₂ Total", x="País", y="CO₂ (ppm)") +
      scale_fill_viridis_c() +
      theme_minimal(base_size=14)
    ggplotly(p)
  })

  output$plot6 <- renderPlotly({
    df <- clima_raw() %>%
      group_by(Year) %>%
      summarise(Avg=mean(Sea_Level_Rise,na.rm=TRUE), .groups='drop')
    validate_data(df)
    p <- ggplot(df, aes(Year, Avg)) +
      geom_area(fill="#00cec9", alpha=0.4) +
      geom_line(color="#00b894", size=1) +
      labs(title="Elevación nivel del mar", x="Año", y="Nivel (m)") +
      theme_minimal(base_size=14)
    ggplotly(p)
  })

  output$plot7 <- renderPlotly({
    df <- df78()
    validate_data(df)
    p <- ggplot(df, aes(x=month(Date,label=TRUE), y=Humidity)) +
      geom_boxplot(fill="#74b9ff", alpha=0.6) +
      labs(title="Humedad Mensual", x="Mes", y="Humedad (%)") +
      theme_minimal(base_size=14) +
      theme(axis.text.x=element_text(angle=45,hjust=1))
    ggplotly(p)
  })

  output$plot8 <- renderPlotly({
    df <- df78()
    validate_data(df)
    p <- ggplot(df, aes(x=month(Date,label=TRUE), y=Wind_Speed)) +
      geom_violin(fill="#fd79a8", alpha=0.6) +
      labs(title="Viento Mensual", x="Mes", y="Vel (km/h)") +
      theme_minimal(base_size=14) +
      theme(axis.text.x=element_text(angle=45,hjust=1))
    ggplotly(p)
  })

  # 5) Mapa coroplético
  output$map <- renderLeaflet({
    df <- dfMap()
    validate_data(df)
    world <- ne_countries(returnclass="sf")
    md    <- left_join(world, df, by=c("name"="Country"))
    pal   <- colorNumeric("YlOrRd", domain=md$Value, na.color="lightgray")
    leaflet(md) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = ~pal(Value),
        weight      = 0.5,
        color       = "white",
        fillOpacity = 0.8,
        highlight   = highlightOptions(weight=2, color="#666", fillOpacity=0.9),
        label       = ~paste0(name, ": ", round(Value,1))
      ) %>%
      addLegend("bottomright", pal=pal, values=~Value, title=input$map_var)
  })

  # 6) Toggle de tema
  esquemas <- list(
    claro = list(main="#f8f9fa", sidebar="#2d3436", text="#2d3436"),
    medio = list(main="#ffffff", sidebar="#34495e", text="#ecf0f1"),
    oscuro= list(main="#2c3e50", sidebar="#1a252f", text="#ecf0f1")
  )
  tema_activo <- reactiveVal("claro")

  observeEvent(input$btn_bg, {
    tema_activo(
      switch(tema_activo(),
             claro  = "medio",
             medio  = "oscuro",
             oscuro = "claro")
    )
  })

  output$dynamicCSS <- renderUI({
    t <- esquemas[[tema_activo()]]
    tags$style(HTML(sprintf("
      .main-content { background:%s !important; color:%s; }
      .sidebar      { background:%s !important; }
    ", t$main, t$text, t$sidebar)))
  })
}
