library(RMariaDB)
library(tidyverse)

BD <- dbConnect( 
  RMariaDB::MariaDB(),
  host = "localhost",
  user = "root",
  password = "root",
  dbname = "bd",
  port = 3306
)

query_1 <- "SELECT DC.Temperature AS Tem, DC.CO2_Emissions AS CO2 FROM DATOS_CLIMA DC JOIN UBICACIONES U ON DC.ID_ubi = U.ID_ubi;"
query_2 <- "SELECT U.Country, AVG(DC.Temperature) AS Avg_Temperature FROM DATOS_CLIMA DC JOIN UBICACIONES U ON DC.ID_ubi = U.ID_ubi GROUP BY U.Country;"
query_3 <- "SELECT U.Location, U.Country, DC.CO2_Emissions FROM DATOS_CLIMA DC JOIN UBICACIONES U ON DC.ID_ubi = U.ID_ubi ORDER BY DC.CO2_Emissions DESC LIMIT 10;"
query_4 <- "SELECT U.Location, COUNT(*) AS Num_Records FROM DATOS_CLIMA DC JOIN UBICACIONES U ON DC.ID_ubi = U.ID_ubi GROUP BY U.Location;"

rest_1 <- dbGetQuery(BD, query_1)
rest_2 <- dbGetQuery(BD, query_2)
rest_3 <- dbGetQuery(BD, query_3)
rest_4 <- dbGetQuery(BD, query_4)

ggplot(rest_1, aes(x = Tem, y = CO2)) +
    geom_line(color = "#2c7bb6", linewidth = 1) +
    geom_point(color = "#d7191c", size = 2) +
    labs(title = "Tem/CO2", x = "Temperatura", y = "CO2 Emisiones") 
