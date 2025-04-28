library(shiny)
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(leaflet)
library(shinythemes)
library(rnaturalearth)

# sólo definimos 'server', sin shinyApp()
server <- function(input, output, session) {

  # 1) Conexión a MySQL
  conn <- dbConnect(RMySQL::MySQL(),
                    dbname   = "baseclima",
                    host     = "127.0.0.1",
                    user     = "root",
                    password = "root",
                    port     = 3306)

  # cerramos al terminar la sesión
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })

  # 2) Carga de datos y chequeo
  clima <- dbGetQuery(conn, "
    SELECT d.*, f.Date, u.Country, u.Location
    FROM datos_clima d
    JOIN fechas f       ON d.ID     = f.ID
    JOIN ubicaciones u  ON d.ID_ubi = u.ID_ubi
  ")
  clima$Date <- as.Date(clima$Date)

  if (nrow(clima) == 0) {
    showNotification("No hay datos en 'baseclima'.", type = "error")
    return()
  }

  # 3) Gráfica 1: Temperatura vs CO2
  output$plotTempCO2 <- renderPlot({
    ggplot(clima, aes(x = CO2_Emissions, y = Temperature)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Temperatura vs CO2",
           x     = "CO2 (ppm)",
           y     = "Temp (°C)") +
      theme_minimal()
  })

  # 4) Gráfica 2: Tendencia anual de temperatura
  output$plotTrendTemp <- renderPlot({
    clima %>%
      mutate(Year = format(Date, "%Y")) %>%
      group_by(Year) %>%
      summarise(Temp = mean(Temperature, na.rm = TRUE)) %>%
      ggplot(aes(as.integer(Year), Temp)) +
      geom_line(color = "orange") +
      labs(title = "Tendencia anual de temperatura",
           x = "Año", y = "°C") +
      theme_minimal()
  })

  # 5) Gráfica 3: Tendencia anual de CO2
  output$plotTrendCO2 <- renderPlot({
    clima %>%
      mutate(Year = format(Date, "%Y")) %>%
      group_by(Year) %>%
      summarise(CO2 = mean(CO2_Emissions, na.rm = TRUE)) %>%
      ggplot(aes(as.integer(Year), CO2)) +
      geom_line(color = "darkgreen") +
      labs(title = "Tendencia anual de CO2",
           x = "Año", y = "ppm") +
      theme_minimal()
  })

  # 6) Top 10 países por temperatura y CO2
  output$plotTopTemp <- renderPlot({
    clima %>%
      group_by(Country) %>%
      summarise(Temp = mean(Temperature, na.rm = TRUE)) %>%
      top_n(10, Temp) %>%
      ggplot(aes(reorder(Country, Temp), Temp)) +
      geom_col(fill = "firebrick") + coord_flip() +
      labs(title = "Top 10 países por temperatura") +
      theme_minimal()
  })
  output$plotTopCO2 <- renderPlot({
    clima %>%
      group_by(Country) %>%
      summarise(CO2 = mean(CO2_Emissions, na.rm = TRUE)) %>%
      top_n(10, CO2) %>%
      ggplot(aes(reorder(Country, CO2), CO2)) +
      geom_col(fill = "purple") + coord_flip() +
      labs(title = "Top 10 países por CO2") +
      theme_minimal()
  })

  # 7) Elevación del nivel del mar
  output$plotSeaLevelRise <- renderPlot({
    clima %>%
      mutate(Year = format(Date, "%Y")) %>%
      group_by(Year) %>%
      summarise(Sea = mean(Sea_Level_Rise, na.rm = TRUE)) %>%
      ggplot(aes(as.integer(Year), Sea)) +
      geom_line(color = "steelblue") +
      labs(title = "Elevación anual del nivel del mar (mm)") +
      theme_minimal()
  })

  # 8) Humedad vs Precipitación
  output$plotHumidityPrecip <- renderPlot({
    ggplot(clima, aes(x = Humidity, y = Precipitation)) +
      geom_point(alpha = 0.5, color = "darkcyan") +
      labs(title = "Humedad vs Precipitación") +
      theme_minimal()
  })

  # 9) Distribución de velocidad del viento
  output$plotWindSpeed <- renderPlot({
    ggplot(clima, aes(x = Wind_Speed)) +
      geom_histogram(bins = 15, fill = "gold", color = "black") +
      labs(title = "Velocidad del viento (km/h)") +
      theme_minimal()
  })

  # 10) Top 10 ubicaciones por número de registros
  output$plotRecordsPerLocation <- renderPlot({
    clima %>%
      count(Location) %>%
      top_n(10, n) %>%
      ggplot(aes(reorder(Location, n), n)) +
      geom_col(fill = "darkgray") + coord_flip() +
      labs(title = "Top 10 ubicaciones con más registros") +
      theme_minimal()
  })

  # 11) Mapa interactivo de conteo por país
  output$map <- renderLeaflet({
    counts <- clima %>% count(Country)
    world   <- ne_countries(returnclass = "sp")
    world@data <- left_join(world@data, counts, by = c("admin" = "Country"))
    pal <- colorNumeric("YlOrRd", domain = world@data$n, na.color = "transparent")

    leaflet(world) %>%
      addTiles() %>%
      addPolygons(fillColor   = ~pal(n),
                  fillOpacity = 0.7,
                  color       = "white",
                  weight      = 0.5,
                  label       = ~paste0(admin, ": ", ifelse(is.na(n), 0, n)))
  })
}
