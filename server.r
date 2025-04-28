library(shiny)
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(leaflet)
library(shinythemes)
library(rnaturalearth)
library(sf)

server <- function(input, output, session) {

  conn <- dbConnect(RMySQL::MySQL(),
                    dbname   = "baseclima",
                    host     = "127.0.0.1",
                    user     = "root",
                    password = "root",
                    port     = 3306)
  session$onSessionEnded(function() dbDisconnect(conn))

  clima <- dbGetQuery(conn, "
    SELECT d.*, f.Date, u.Country, u.Location
    FROM datos_clima d
    JOIN fechas f       ON d.ID     = f.ID
    JOIN ubicaciones u  ON d.ID_ubi = u.ID_ubi
  ")
  clima$Date <- as.Date(clima$Date)
  if (nrow(clima)==0) stop("No hay datos en la DB")

  base_theme <- theme_minimal(base_size=14) +
    theme(
      plot.title   = element_text(face="bold", size=16, hjust=0.5),
      axis.title   = element_text(size=12),
      plot.margin  = margin(10,10,10,10)
    )

  output$plotTempCO2 <- renderPlot({
    ggplot(clima, aes(CO2_Emissions, Temperature)) +
      geom_point(alpha=0.5) +
      geom_smooth(method="lm", se=TRUE, color="red") +
      labs(title="Temperatura vs CO2", x="CO2 (ppm)", y="Temp (°C)") +
      base_theme
  })

  output$plotTrendTemp <- renderPlot({
    clima %>%
      mutate(Year=format(Date,"%Y")) %>%
      group_by(Year) %>%
      summarise(Temp=mean(Temperature,na.rm=TRUE)) %>%
      ggplot(aes(as.integer(Year),Temp)) +
      geom_line(color="orange", size=1) +
      labs(title="Tendencia anual de temperatura", x="Año", y="°C") +
      base_theme
  })

  output$plotTrendCO2 <- renderPlot({
    clima %>%
      mutate(Year=format(Date,"%Y")) %>%
      group_by(Year) %>%
      summarise(CO2=mean(CO2_Emissions,na.rm=TRUE)) %>%
      ggplot(aes(as.integer(Year),CO2)) +
      geom_line(color="darkgreen", size=1) +
      labs(title="Tendencia anual de CO2", x="Año", y="ppm") +
      base_theme
  })

  output$plotTopTemp <- renderPlot({
    clima %>%
      group_by(Country) %>%
      summarise(Temp=mean(Temperature,na.rm=TRUE)) %>%
      top_n(10,Temp) %>%
      ggplot(aes(reorder(Country,Temp),Temp)) +
      geom_col(fill="firebrick") + coord_flip() +
      labs(title="Top 10 países por temperatura", x=NULL, y="°C") +
      base_theme
  })

  output$plotTopCO2 <- renderPlot({
    clima %>%
      group_by(Country) %>%
      summarise(CO2=mean(CO2_Emissions,na.rm=TRUE)) %>%
      top_n(10,CO2) %>%
      ggplot(aes(reorder(Country,CO2),CO2)) +
      geom_col(fill="purple") + coord_flip() +
      labs(title="Top 10 países por CO2", x=NULL, y="ppm") +
      base_theme
  })

  output$plotSeaLevelRise <- renderPlot({
    clima %>%
      mutate(Year=format(Date,"%Y")) %>%
      group_by(Year) %>%
      summarise(Sea=mean(Sea_Level_Rise,na.rm=TRUE)) %>%
      ggplot(aes(as.integer(Year),Sea)) +
      geom_line(color="steelblue", size=1) +
      labs(title="Elevación nivel del mar (mm)", x="Año", y="mm") +
      base_theme
  })

  output$plotHumidityPrecip <- renderPlot({
    ggplot(clima, aes(Humidity, Precipitation)) +
      geom_point(alpha=0.5, color="darkcyan") +
      labs(title="Humedad vs Precipitación", x="% Humedad", y="Precipitación") +
      base_theme
  })

  output$plotWindSpeed <- renderPlot({
    ggplot(clima, aes(Wind_Speed)) +
      geom_histogram(bins=15, fill="gold", color="black") +
      labs(title="Vel. del viento (km/h)", x="km/h", y="Frecuencia") +
      base_theme
  })

  output$plotRecordsPerLocation <- renderPlot({
    clima %>%
      count(Location) %>%
      top_n(10,n) %>%
      ggplot(aes(reorder(Location,n),n)) +
      geom_col(fill="darkgray") + coord_flip() +
      labs(title="Top 10 ubicaciones por registros", x=NULL, y="Registros") +
      base_theme
  })

  output$map <- renderLeaflet({
    counts   <- clima %>% count(Country)
    world_sf <- ne_countries(scale="medium", returnclass="sf")
    world_sf <- left_join(world_sf, counts, by=c("admin"="Country"))
    pal      <- colorNumeric("YlOrRd", domain=world_sf$n, na.color="transparent")

    leaflet(world_sf) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = ~pal(n),
        fillOpacity = 0.7,
        color       = "white",
        weight      = 0.5,
        label       = ~paste0(admin, ": ", ifelse(is.na(n),0,n))
      )
  })
}
