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
       leaflet, #visualizciÃ³n
       tmaptools, #geocode
       osmdata, # Get OSM data
       expss,
       ggmap,
       readr, 
       readxl
)



setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/Final-Project-Big-Data")

# 2017 
base_2017_victimas <- read_excel("stores/Base_2017.xlsx", sheet = "VICTIMAS")
base_2017_conductores <- read_excel("stores/Base_2017.xlsx", sheet = "CONDUCTORES")
base_2017_siniestros <- read_excel("stores/Base_2017.xlsx", sheet = "ACCIDENTES")

class(base_2017_victimas)

# 2018 
base_2018_victimas <- read_excel("stores/Base_2018.xlsx", sheet = "VICTIMAS")
base_2018_conductores <- read_excel("stores/Base_2018.xlsx", sheet = "CONDUCTORES")
base_2018_siniestros <- read_excel("stores/Base_2018.xlsx", sheet = "ACCIDENTES")

# 2019
base_2019_victimas <- read_excel("stores/Base_2019.xlsx", sheet = "VICTIMAS")
base_2019_conductores <- read_excel("stores/Base_2019.xlsx", sheet = "CONDUCTORES")
base_2019_siniestros <- read_excel("stores/Base_2019.xlsx", sheet = "ACCIDENTES")


 ############### ========= CREACION VARIABLES ============ ##############

#Validando la base, no podemos identificar el culpable. Propongo cambiar culpable por conductor. 
#Incluir Número de vehículos involucrados, edad promedio conductor y edad promedio víctima y para el género contar el número de mujeres y hombres involucrados. 
#Para fecha: as.Date(Base$variable, format = '%m/%d/%Y')


#======= BASE 2017 ======#
### Crear numero de vehículos involucrados por accidente

num_vehic <- base_2017_conductores %>% 
  group_by(id)%>% 
  summarise(Vehiculo = n())

base_2017_siniestros <-base_2017_siniestros %>% left_join(num_vehic,by="id")

sum(is.na(base_2017_siniestros$Vehiculo))

### Crear numero de victimas por accidente
colnames(base_2017_victimas)

num_vic <- base_2017_victimas %>% 
  group_by(id)%>% 
  summarise(Numero = n())

base_2017_siniestros <-base_2017_siniestros %>% left_join(num_vic,by="id")
base_2017_siniestros$SD <- (base_2017_siniestros$GravedadNombre)

sum(is.na(base_2017_siniestros$Numero))

#Asumir que no existen víctimas si existe NA

base_2017_siniestros$Numero <- ifelse(is.na(base_2017_siniestros$Numero),
                                       yes = 0,
                                       no = base_2017_siniestros$Numero)

table(base_2017_siniestros$Numero, base_2017_siniestros$SD)

### Crear edad promedio conductores

class(base_2017_conductores$EDAD_PROCESADA)

m_edad_c <- base_2017_conductores %>% 
  group_by(id)%>% 
  summarise(m_edad_c = round(mean(EDAD_PROCESADA), digits = 0))

base_2017_siniestros <-base_2017_siniestros %>% left_join(m_edad_c,by="id")

### Crear edad promedio victimas

class(base_2017_victimas$EDAD_PROCESADA)

m_edad_v <- base_2017_victimas %>% 
  group_by(id)%>% 
  summarise(m_edad_v = round(mean(EDAD_PROCESADA), digits = 0))

base_2017_siniestros <-base_2017_siniestros %>% left_join(m_edad_v,by="id")
table(base_2017_siniestros$Numero, base_2017_siniestros$SD)

### Variable género ********** OJO REVISAR
# Hay dos opciones: contar el numero de mujeres y hombres o dummy mayoría mujeres =1 y 0 de lo contrario

# == Numero de mujeres y hombres

# Conductores

num_hombres_c <- base_2017_conductores %>% 
  group_by(id) %>% 
  summarise(num_hombres_c = sum(Sexo == "MASCULINO"))

num_mujeres_c <- base_2017_conductores %>% 
  group_by(id) %>% 
  summarise(num_mujeres_c = sum(Sexo == "FEMENINO"))

num_otro_c <- base_2017_conductores %>% 
  group_by(id) %>% 
  summarise(num_otro_c = sum(Sexo == "NO APLICA"))


base_2017_siniestros <-base_2017_siniestros %>% left_join(num_hombres_c,by="id")
base_2017_siniestros <-base_2017_siniestros %>% left_join(num_mujeres_c,by="id")

table(base_2017_siniestros$SD, base_2017_siniestros$num_hombres_c)
table(base_2017_siniestros$SD, base_2017_siniestros$num_mujeres_c)

# Victimas

num_hombres_v <- base_2017_victimas %>% 
  group_by(id) %>% 
  summarise(num_hombres_v = sum(Sexo == "MASCULINO"))

num_mujeres_v <- base_2017_victimas %>% 
  group_by(id) %>% 
  summarise(num_mujeres_v = sum(Sexo == "FEMENINO"))

base_2017_siniestros <-base_2017_siniestros %>% left_join(num_hombres_v,by="id")
base_2017_siniestros <-base_2017_siniestros %>% left_join(num_mujeres_v,by="id")


############### ========= ORGANIZACION VARIABLES RELEVANTES ============ ##############

## Variables relevantes: id, Dia, Fecha, GravedadCod, GravedadNombre, ClaseCodigo, ClaseNombre, Latitud, Longitud, Direccion,
                      #  Localidad, HoraOcurrencia, TipoTiempo, Vehiculo, Numero, m_edad_c, m_edad_v, num_hombres_c, num_mujeres_c, 
                      #   num_hombres_v, num_mujeres_v

# Variable Dependiente: Grado de severidad del choque (1 con muertos, 2 con heridos, 3 con daños) - Dejarla en 1 severo 0 no severo


class(base_2017_siniestros$GravedadNombre) #Preferiria dejar el mayor numero como el mas severo y solo dos categorias

base_2017_siniestros$GravedadNombre[base_2017_siniestros$GravedadNombre== "Con Heridos"] <- "Con Muertos" # Dejar solo dos categorias

base_2017_siniestros$GravedadNombre <- factor(base_2017_siniestros$GravedadNombre, 
                                                 levels = c("Con Muertos", "Solo Daños"),
                                                 labels = c("Severo", "Leve"))



class(base_2017_siniestros$GravedadNombre)
levels(base_2017_siniestros$GravedadNombre)
table(base_2017_siniestros$GravedadNombre)











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