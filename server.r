# server.R
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
library(RColorBrewer)
library(rnaturalearthdata)

server <- function(input, output, session) {

  # 1) Conexión y carga cruda para grfs 7 & 8
  conn <- dbConnect(
    RMariaDB::MariaDB(),
    dbname   = "bd",
    host     = "127.0.0.1",
    user     = "root",
    password = "root",
    port     = 3306
  )
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

  # 2) Inicializar filtros
  años   <- dbGetQuery(conn, "SELECT DISTINCT YEAR(Date) AS Year FROM fechas")$Year %>% sort()
  países <- dbGetQuery(conn, "SELECT DISTINCT Country FROM ubicaciones ORDER BY Country")$Country

  updateSelectInput(session, "filter_pais_1",  choices = países, selected = países[1])
  updateSelectInput(session, "filter_pais_2",  choices = países, selected = países[1])
  updateSelectInput(session, "filter_pais_3",  choices = países, selected = países[1])
  updateSelectInput(session, "filter_pais_78", choices = países, selected = países[1])
  updateSelectInput(session, "top_temp_year",  choices = años,   selected = max(años))
  updateSelectInput(session, "top_co2_year",   choices = años,   selected = max(años))
  updateSliderInput(session, "map_year",
                    min   = min(años),
                    max   = max(años),
                    value = c(min(años), max(años)))

  # 3) Reactivo para grfs 7 & 8
  df78 <- reactive({
    req(input$filter_pais_78)
    clima_raw() %>% filter(Country == input$filter_pais_78)
  })

  validate_data <- function(df) validate(need(nrow(df) > 0, "No hay datos para estos filtros"))

  # 4) Gráfica 1: Temp vs CO₂ (SQL)
  output$plot1 <- renderPlotly({
    sql <- sprintf("
      SELECT d.CO2_Emissions AS CO2, d.Temperature
      FROM datos_clima d
      JOIN fechas f       ON d.ID     = f.ID
      JOIN ubicaciones u  ON d.ID_ubi = u.ID_ubi
      WHERE u.Country = %s
    ",
      dbQuoteString(conn, input$filter_pais_1)
    )
    df <- dbGetQuery(conn, sql); validate_data(df)
    p <- ggplot(df, aes(CO2, Temperature)) +
      geom_point(color = "#1f77b4", alpha = 0.7) +
      labs(title="Temp vs CO₂", x="CO₂ (ppm)", y="Temp (°C)") +
      theme_minimal(base_size=14)
    ggplotly(p)
  })

  # 5) Gráfica 2: Tendencia Temp Trimestral (SQL)
  output$plot2 <- renderPlotly({
    sql <- sprintf("
      SELECT YEAR(f.Date) AS Y, QUARTER(f.Date) AS Q, AVG(d.Temperature) AS Avg
      FROM datos_clima d
      JOIN fechas f       ON d.ID     = f.ID
      JOIN ubicaciones u  ON d.ID_ubi = u.ID_ubi
      WHERE u.Country = %s
      GROUP BY Y, Q
      ORDER BY Y, Q
    ",
      dbQuoteString(conn, input$filter_pais_2)
    )
    df <- dbGetQuery(conn, sql) %>% mutate(Period = paste0(Y, "-Q", Q))
    validate_data(df)
    p <- ggplot(df, aes(Period, Avg, group=factor(Y), color=factor(Y))) +
      geom_line(size=1) + geom_point(size=2) +
      labs(title="Tendencia Temp Trimestral", x="Periodo", y="Temp (°C)") +
      theme_minimal(base_size=14) +
      theme(axis.text.x = element_text(angle=45, hjust=1))
    ggplotly(p)
  })

  # 6) Gráfica 3: Tendencia CO₂ Mensual (SQL)
  output$plot3 <- renderPlotly({
    sql <- sprintf("
      SELECT YEAR(f.Date) AS Y, MONTH(f.Date) AS M, AVG(d.CO2_Emissions) AS Avg
      FROM datos_clima d
      JOIN fechas f      ON d.ID     = f.ID
      JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi
      WHERE u.Country = %s
      GROUP BY Y, M
      ORDER BY Y, M
    ",
      dbQuoteString(conn, input$filter_pais_3)
    )
    df <- dbGetQuery(conn, sql) %>% mutate(Period = sprintf('%d-%02d', Y, M))
    validate_data(df)
    p <- ggplot(df, aes(Period, Avg, group=factor(Y), color=factor(Y))) +
      geom_line(size=1) + geom_point(size=2) +
      labs(title="Tendencia CO₂ Mensual", x="Periodo", y="CO₂ (ppm)") +
      theme_minimal(base_size=14) +
      theme(axis.text.x = element_text(angle=45, hjust=1))
    ggplotly(p)
  })

  # 7) Humedad Mensual (dplyr sobre clima_raw)
  output$plot7 <- renderPlotly({
    df <- df78(); validate_data(df)
    p <- ggplot(df, aes(x = month(Date, label = TRUE), y = Humidity)) +
      geom_boxplot(fill = "#74b9ff", alpha = 0.6) +
      labs(title = "Humedad Mensual", x = "Mes", y = "Humedad (%)") +
      theme_minimal(base_size=14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })

  # 8) Viento Mensual (dplyr sobre clima_raw)
  output$plot8 <- renderPlotly({
    df <- df78(); validate_data(df)
    p <- ggplot(df, aes(x = month(Date, label = TRUE), y = Wind_Speed)) +
      geom_violin(fill = "#fd79a8", alpha = 0.6) +
      labs(title = "Viento Mensual", x = "Mes", y = "Vel (km/h)") +
      theme_minimal(base_size=14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })

  # 9) Elevación nivel del mar (Gráfica 6, SQL)
  output$plot6 <- renderPlotly({
    df <- dbGetQuery(conn, "
      SELECT YEAR(f.Date) AS Year, AVG(d.Sea_Level_Rise) AS Avg
      FROM datos_clima d
      JOIN fechas f ON d.ID = f.ID
      GROUP BY Year
      ORDER BY Year
    ")
    validate_data(df)
    p <- ggplot(df, aes(Year, Avg)) +
      geom_area(fill="#00cec9", alpha=0.4) +
      geom_line(color="#00b894", size=1) +
      labs(title="Elevación nivel del mar", x="Año", y="Nivel (m)") +
      theme_minimal(base_size=14)
    ggplotly(p)
  })

  # 10) Top 10 Temp Prom (Gráfica 4, SQL)
  output$plot4 <- renderPlotly({
    sql <- sprintf("
      SELECT u.Country, AVG(d.Temperature) AS Avg
      FROM datos_clima d
      JOIN fechas f      ON d.ID     = f.ID
      JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi
      WHERE YEAR(f.Date) = %s
      GROUP BY u.Country
      ORDER BY Avg DESC
      LIMIT 10
    ",
      dbQuoteString(conn, as.character(input$top_temp_year))
    )
    df <- dbGetQuery(conn, sql); validate_data(df)
    p <- ggplot(df, aes(reorder(Country, Avg), Avg, fill=Avg)) +
      geom_col() + coord_flip() +
      labs(title=sprintf("Top 10 Temp Prom (%s)", input$top_temp_year),
           x="País", y="Temp (°C)") +
      scale_fill_viridis_c() +
      theme_minimal(base_size=14)
    ggplotly(p)
  })

  # 11) Top 10 CO₂ Prom (Gráfica 5, SQL)
  output$plot5 <- renderPlotly({
    sql <- sprintf("
      SELECT u.Country, AVG(d.CO2_Emissions) AS Tot
      FROM datos_clima d
      JOIN fechas f      ON d.ID     = f.ID
      JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi
      WHERE YEAR(f.Date) = %s
      GROUP BY u.Country
      ORDER BY Tot DESC
      LIMIT 10
    ",
      dbQuoteString(conn, as.character(input$top_co2_year))
    )
    df <- dbGetQuery(conn, sql); validate_data(df)
    p <- ggplot(df, aes(reorder(Country, Tot), Tot, fill=Tot)) +
      geom_col() + coord_flip() +
      labs(title=sprintf("Top 10 CO₂ Prom (%s)", input$top_co2_year),
           x="País", y="CO₂ (ppm)") +
      scale_fill_viridis_c() +
      theme_minimal(base_size=14)
    ggplotly(p)
  })
# Corrección mapa diferencial años seleccionados
output$map <- renderLeaflet({
  # 1) Años como enteros y variable
  a1  <- as.integer(input$map_year[1])
  a2  <- as.integer(input$map_year[2])
  var <- input$map_var

  # 2) Traer diff desde la base (sin comillas en los años)
  sql <- sprintf("
    SELECT t2.Country,
           (t2.v2 - t1.v1) AS diff
    FROM (
      SELECT u.Country, AVG(d.%s) AS v1
      FROM datos_clima d
      JOIN fechas f ON d.ID = f.ID
      JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi
      WHERE YEAR(f.Date) = %d
      GROUP BY u.Country
    ) t1
    JOIN (
      SELECT u.Country, AVG(d.%s) AS v2
      FROM datos_clima d
      JOIN fechas f ON d.ID = f.ID
      JOIN ubicaciones u ON d.ID_ubi = u.ID_ubi
      WHERE YEAR(f.Date) = %d
      GROUP BY u.Country
    ) t2 ON t1.Country = t2.Country
  ",
    var, a1,
    var, a2
  )

  df <- dbGetQuery(conn, sql)
  validate(need(nrow(df) > 0, "No hay datos para estos filtros"))

  # 3) Forzar diff numérico en el dataframe de la consulta
  df$diff <- as.numeric(df$diff)
  if (!is.numeric(df$diff)) stop("ERROR: df$diff sigue sin ser numérico")

  # 4) Cargar geometrías de países
  world <- ne_countries(scale = "medium", returnclass = "sf")

  # 5) Hacer el join y volver a forzar numérico
  md <- left_join(world, df, by = c("name" = "Country"))
  md$diff <- as.numeric(md$diff)
  if (!is.numeric(md$diff)) stop("ERROR: md$diff sigue sin ser numérico")

  # 6) Calcular el rango máximo para la paleta
  mv <- max(abs(md$diff), na.rm = TRUE)
  pal <- colorNumeric(
    palette   = rev(brewer.pal(11, "RdBu")),
    domain    = c(-mv, mv),
    na.color  = "lightgray"
  )

  # 7) Crear breaks y labels numéricos para la leyenda
  brks  <- pretty(c(-mv, mv), n = 7)
  labs  <- sprintf("%+.2f", brks)
  cols  <- pal(brks)

  # 8) Renderizar el mapa con leyenda manual
  leaflet(md) %>%
    addTiles() %>%
    addPolygons(
      fillColor   = ~pal(diff),
      weight      = 0.5,
      color       = "white",
      fillOpacity = 0.8,
      highlight   = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9),
      label       = ~sprintf("%s: %+.2f", name, diff)
    ) %>%
    addLegend(
      position = "bottomright",
      colors   = cols,
      labels   = labs,
      title    = sprintf("%s Δ (%d → %d)", var, a1, a2),
      opacity  = 0.8
    )
})

  # 13) Toggle de tema dinámico
  esquemas <- list(
    claro  = list(main="#f8f9fa", sidebar="#2d3436", text="#2d3436"),
    medio  = list(main="#ffffff", sidebar="#34495e", text="#ecf0f1"),
    oscuro = list(main="#2c3e50", sidebar="#1a252f", text="#ecf0f1")
  )
  tema_activo <- reactiveVal("claro")
  observeEvent(input$btn_bg, {
    tema_activo(switch(tema_activo(),
      claro  = "medio",
      medio  = "oscuro",
      oscuro = "claro"
    ))
  })
  output$dynamicCSS <- renderUI({
    t <- esquemas[[tema_activo()]]
    tags$style(HTML(sprintf("
      .main-content { background:%s !important; color:%s; }
      .sidebar      { background:%s !important; }
    ", t$main, t$text, t$sidebar)))
  })
}

# Para ejecutar:
# shiny::runApp("C:/Users/gabos/Documents/GitHub/DASHBOARD", port = 3838, launch.browser = TRUE)
