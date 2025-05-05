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

server <- function(input, output, session) {
  # Conexión a la base de datos
  conn <- dbConnect(
    RMariaDB::MariaDB(),
    dbname = "bd",
    host = "127.0.0.1",
    user = "root",
    password = "root",
    port = 3306
  )
  # Asegurar collation uniforme
  dbExecute(conn, "SET NAMES 'utf8mb4' COLLATE 'utf8mb4_0900_ai_ci'")
  session$onSessionEnded(function() dbDisconnect(conn))

  # Helper para validar datos
  validate_data <- function(df) {
    validate(
      need(!is.null(df) && nrow(df) > 0, "No hay datos disponibles")
    )
  }

  # 1. Relación Temperatura vs CO2
  output$plotTempCO2 <- renderPlotly({
    df <- dbGetQuery(conn,
      "SELECT Temperature, CO2_Emissions AS CO2, Sea_Level_Rise AS SeaLevel, Humidity
       FROM datos_clima"
    )
    validate_data(df)
    p <- ggplot(df, aes(x = CO2, y = Temperature, color = SeaLevel, size = Humidity)) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_c(name = "Nivel Mar (m)") +
      labs(title = 'Temperatura vs CO2', x = 'CO2 (ppm)', y = 'Temperatura (°C)') +
      theme_minimal()
    ggplotly(p, tooltip = c('x','y','color','size'))
  })

  # 2. Tendencia de Temperatura por Trimestre
  output$plotTrendTemp <- renderPlotly({
    df <- dbGetQuery(conn,
      "SELECT YEAR(f.Date) AS Year, QUARTER(f.Date) AS Quarter,
              AVG(d.Temperature) AS AvgTemp
       FROM datos_clima d
       JOIN fechas f ON d.ID = f.ID
       GROUP BY Year, Quarter
       ORDER BY Year, Quarter"
    )
    validate_data(df)
    df <- df %>% mutate(Period = paste0(Year, '-Q', Quarter))
    p <- ggplot(df, aes(x = Period, y = AvgTemp, group = Year, color = factor(Year))) +
      geom_line(linewidth = 1) + geom_point() +
      labs(title = 'Tendencia de Temperatura', x = 'Periodo', y = 'Temp. Promedio (°C)', color = 'Año') +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = c('x','y'))
  })

  # 3. Tendencia Mensual de CO2
  output$plotTrendCO2 <- renderPlotly({
    df <- dbGetQuery(conn,
      "SELECT YEAR(f.Date) AS Year, MONTH(f.Date) AS Month,
              AVG(d.CO2_Emissions) AS AvgCO2
       FROM datos_clima d
       JOIN fechas f ON d.ID = f.ID
       GROUP BY Year, Month
       ORDER BY Year, Month"
    )
    validate_data(df)
    df <- df %>% mutate(Period = sprintf('%d-%02d', Year, Month))
    p <- ggplot(df, aes(x = Period, y = AvgCO2, group = Year, color = factor(Year))) +
      geom_line(linewidth = 1) + geom_point() +
      labs(title = 'Tendencia Mensual de CO2', x = 'Periodo', y = 'CO2 Promedio (ppm)', color = 'Año') +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = c('x','y'))
  })

  # 4. Top 10 Países por Temperatura Promedio
  output$plotTopTemp <- renderPlotly({
    df <- dbGetQuery(conn,
      "SELECT u.Country AS Country, AVG(d.Temperature) AS AvgTemp
       FROM datos_clima d
       JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi
       GROUP BY u.Country
       ORDER BY AvgTemp DESC
       LIMIT 10"
    )
    validate_data(df)
    p <- ggplot(df, aes(x = reorder(Country, AvgTemp), y = AvgTemp, fill = AvgTemp)) +
      geom_col() + coord_flip() +
      labs(title = 'Top 10 Países Temperatura Promedio', x = 'País', y = 'Temp. Promedio (°C)') +
      scale_fill_viridis_c() + theme_minimal()
    ggplotly(p, tooltip = c('x','y'))
  })

  # 5. Top 10 Países por Emisiones Totales de CO2
  output$plotTopCO2 <- renderPlotly({
    df <- dbGetQuery(conn,
      "SELECT u.Country AS Country, SUM(d.CO2_Emissions) AS TotalCO2
       FROM datos_clima d
       JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi
       GROUP BY u.Country
       ORDER BY TotalCO2 DESC
       LIMIT 10"
    )
    validate_data(df)
    p <- ggplot(df, aes(x = reorder(Country, TotalCO2), y = TotalCO2, fill = TotalCO2)) +
      geom_col() + coord_flip() +
      labs(title = 'Top 10 Países CO2 Total', x = 'País', y = 'CO2 Total (ppm)') +
      scale_fill_viridis_c() + theme_minimal()
    ggplotly(p, tooltip = c('x','y'))
  })

  # 6. Evolución Nivel del Mar Global
  output$plotSeaLevelRise <- renderPlotly({
    df <- dbGetQuery(conn,
      "SELECT YEAR(f.Date) AS Year, AVG(d.Sea_Level_Rise) AS AvgSea
       FROM datos_clima d
       JOIN fechas f ON d.ID = f.ID
       GROUP BY Year
       ORDER BY Year"
    )
    validate_data(df)
    p <- ggplot(df, aes(x = Year, y = AvgSea)) +
      geom_area(fill = '#00cec9', alpha = 0.4) + geom_line(color = '#00b894', linewidth = 1) +
      labs(title = 'Nivel del Mar por Año', x = 'Año', y = 'Nivel Mar (m)') +
      theme_minimal()
    ggplotly(p, tooltip = c('x','y'))
  })

  # 7. Distribución Humedad y Precipitación por País
  output$plotHumidityPrecip <- renderPlotly({
    df <- dbGetQuery(conn,
      "SELECT u.Country AS Country, d.Humidity, d.Precipitation
       FROM datos_clima d
       JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi"
    )
    validate_data(df)
    p <- ggplot(df, aes(x = Country, y = Humidity)) +
      geom_boxplot(fill = '#74b9ff', alpha = 0.6) +
      labs(title = 'Humedad por País', x = 'País', y = 'Humedad (%)') +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })

  # 8. Distribución Velocidad del Viento por País
  output$plotWindSpeed <- renderPlotly({
    df <- dbGetQuery(conn,
      "SELECT u.Country AS Country, d.Wind_Speed AS WindSpeed
       FROM datos_clima d
       JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi"
    )
    validate_data(df)
    p <- ggplot(df, aes(x = Country, y = WindSpeed)) +
      geom_violin(fill = '#fd79a8', alpha = 0.6) +
      labs(title = 'Viento por País', x = 'País', y = 'Velocidad (km/h)') +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })

  # 9. Mapa Interactivo de Temperatura Media por País
  output$map <- renderLeaflet({
    df <- dbGetQuery(conn,
      "SELECT u.Country AS Country, AVG(d.Temperature) AS AvgTemp
       FROM datos_clima d
       JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi
       GROUP BY u.Country"
    )
    world <- ne_countries(scale = "medium", returnclass = "sf")
    map_data <- left_join(world, df, by = c("name" = "Country"))
    pal <- colorNumeric('YlOrRd', domain = map_data$AvgTemp, na.color = 'gray')
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(AvgTemp),
        weight = 0.5,
        color = 'white',
        fillOpacity = 0.8,
        highlight = highlightOptions(
          weight = 1.5,
          color = '#666',
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(name, ': ', round(AvgTemp,1), '°C')
      ) %>%
      addLegend('bottomright', pal = pal, values = ~AvgTemp,
                title = 'Temp. Media (°C)')
  })
}
