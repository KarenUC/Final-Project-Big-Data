# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 28-07-2022
###----------------- Project Set 3----------###

##### ---Limpiar Ambiente --- ######

rm(list = ls())

##### ---Cargar paquetes --- ######
require(pacman)
p_load(rio) # Librer?a para importar datos 
p_load(tidyverse) # Librer?a para limpiar datos
p_load(e1071) # Tiene la funci?n para calcular skewness
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(skimr, # summary data
       caret, # Classification And REgression Training
       rvest,
       stringr,
       dplyr,
       robotstxt,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, #visualizci√≥n
       tmaptools, #geocode
       osmdata,
       expss,
       ggmap,
       readr# Get OSM data
)


# Geocoding a csv column of "addresses" in R

#load ggmap
library(ggmap)

setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/Final-Project-Big-Data")

Base_2017_VIC <- read_csv("stores/Base_2017_VIC.csv")
Base_2017_COND <- read_csv("stores/Base_2017_COND.csv")
Base_2017_ACC <- read_csv("stores/Base_2017_ACC.csv")

#Validar que variables nos sirven de cada base

#Validando la base, no podemos identificar el culpable. Propongo cambiar culpable por conductor y ya. 

#Incluir N˙mero de vehÌculos involucrados, edad promedio conductor y edad promedio vÌctima y para el gÈnero contar. 

#La de conductores
colnames(Base_2017_COND)

summary(Base_2017_COND$Vehiculo)

Base_2017_COND$num_conductores <- Base_2017_COND %>% 
  group_by(id) %>% 
  summarise(Vehiculo = n())

Base_2017_COND$vEHI2 = Base_2017_COND %>% 
  group_by(id)%>% 
  count(Vehiculo)

Base_2017_COND$num_conductores <- Base_2017_COND %>% 
  group_by(id) %>% 
  summarise(vEHI2 = n())

summarise(Base_2017_COND$num_conductores)

class(Base_2017_ACC$Latitud)
table(Base_2017_ACC$Latitud)


df_2017 <- right_join(Base_2017_ACC, Base_2017_COND, by = "id")
base_2017_total <- full_join(df_2017, Base_2017_VIC, by = "id")



origAddress <- read.csv("stores/Base_2017_VIC", stringsAsFactors = FALSE)

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$addresses[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}
# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "geocoded.csv", row.names=FALSE)