# Instalar paquetes si no existen (solo ejecutar una vez)
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("RMariaDB", quietly = TRUE)) install.packages("RMariaDB")
if (!requireNamespace("conflicted", quietly = TRUE)) install.packages("conflicted")

# Cargar librerías silenciando mensajes
suppressPackageStartupMessages({
  library(tidyverse)
  library(RMariaDB)
  library(conflicted)
})




#  Conexión a Base de Datos MySQL

BD <- dbConnect(
    RMariaDB::MariaDB(),
    host = "127.0.0.1",  # Usar IP evita problemas de DNS
    user = "root",
    password = "root",
    dbname = "bd",
    port = 3306
)
 


# Queries

queries <- list(
  q1 = "SELECT DC.Temperature AS Tem, DC.CO2_Emissions AS CO2 
         FROM DATOS_CLIMA DC 
         JOIN UBICACIONES U ON DC.ID_ubi = U.ID_ubi;",
  
  q2 = "SELECT U.Country, AVG(DC.Temperature) AS Avg_Temperature 
         FROM DATOS_CLIMA DC 
         JOIN UBICACIONES U ON DC.ID_ubi = U.ID_ubi 
         GROUP BY U.Country;",
  
  q3 = "SELECT U.Location, U.Country, DC.CO2_Emissions 
         FROM DATOS_CLIMA DC 
         JOIN UBICACIONES U ON DC.ID_ubi = U.ID_ubi 
         ORDER BY DC.CO2_Emissions DESC 
         LIMIT 10;",
  
  q4 = "SELECT U.Location, COUNT(*) AS Num_Records 
         FROM DATOS_CLIMA DC 
         JOIN UBICACIONES U ON DC.ID_ubi = U.ID_ubi 
         GROUP BY U.Location;"
)

# Ejecutar todas las queries y almacenar resultados
resultados <- lapply(queries, function(q) {
  tryCatch({
    dbGetQuery(BD, q)
  }, error = function(e) {
    message("Error en query: ", substr(q, 1, 50), "...")
    return(NULL)
  })
})


# 3. Visualización de Datos

if (!is.null(resultados$q1)) {
  # Gráfico de línea para Temperatura vs CO2
  grafico_co2 <- ggplot(resultados$q1, aes(x = Tem, y = CO2)) +
    geom_line(color = "#1b7bc5", linewidth = 1, alpha = 0.8) +
    geom_point(color = "#ce0b0f", size = 3, alpha = 0.6) +
    labs(
      title = "Relación Temperatura vs. Emisiones de CO2",
      subtitle = "Datos climáticos históricos",
      x = "Temperatura (°C)",
      y = "CO2 (ppm)",
      caption = "Fuente: Base de Datos Climáticos"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(size = 12)
    )
}
  # Mostrar gráfico
  print(grafico_co2)
  



if (!is.null(resultados$q2)) {
  # Top 5 países con mayor temperatura promedio
  top_paises <- resultados$q2 %>%
    arrange(desc(Avg_Temperature)) %>%
    head(5)
  
  cat("\n🌡️ Top 5 países más cálidos:\n")
  print(top_paises)
}


dbDisconnect(BD)
rm(list = ls())
gc()