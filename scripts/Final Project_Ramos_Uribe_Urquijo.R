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

# Unir bases en una sola

# 2017
base_2017_victimas$Year <- "2017"
base_2017_victimas$idFormulario <- base_2017_victimas$id
base_2017_conductores$Year <- "2017"
base_2017_conductores$idFormulario <- base_2017_conductores$id
base_2017_siniestros$Year <- "2017"
base_2017_siniestros$idFormulario <- base_2017_siniestros$id

# 2018
base_2018_victimas$Year <- "2018"
base_2018_conductores$Year <- "2018"
base_2018_siniestros$Year <- "2018"


glimpse(base_2017_siniestros)
glimpse(base_2018_conductores)
glimpse(base_2017_victimas)


########## Seleccion Variables

#2017

base_2017_siniestros <- base_2017_siniestros %>% 
                        select(idFormulario, Dia, Fecha, GravedadNombre, ClaseNombre, Latitud, 
                               Longitud, Direccion, TipoDisenno, TipoTiempo, CON_BICICLETA,
                               CON_CARGA, CON_EMBRIAGUEZ, CON_HUECOS, CON_MENORES, CON_MOTO,
                               CON_PEATON, CON_PERSONA_MAYOR, CON_RUTAS, CON_TPI, CON_VELOCIDAD, Year)

base_2017_conductores <- base_2017_conductores %>%
                        select (idFormulario, Vehiculo, EDAD_PROCESADA, LLevaCinturon, LLevaChaleco, LLevaCasco,
                                Sexo, PortaLicencia, ClaseVehiculo, ServicioVehiculo, Year)


base_2017_victimas <- base_2017_victimas  %>%
                      select (idFormulario, Numero, EDAD_PROCESADA, LLevaCinturon, LlevaChaleco, LLevaCasco,
                              Sexo, VEHICULO_VIAJABA, Year)
  
#2018

base_2018_siniestros <- base_2018_siniestros %>% 
  select(idFormulario, Dia, Fecha, GravedadNombre, ClaseNombre, Latitud, 
         Longitud, Direccion, TipoDisenno, TipoTiempo, CON_BICICLETA,
         CON_CARGA, CON_EMBRIAGUEZ, CON_HUECOS, CON_MENORES, CON_MOTO,
         CON_PEATON, CON_PERSONA_MAYOR, CON_RUTAS, CON_TPI, CON_VELOCIDAD, Year)

base_2018_conductores <- base_2018_conductores %>%
  select (idFormulario, Vehiculo, EDAD_PROCESADA, LLevaCinturon, LLevaChaleco, LLevaCasco,
          Sexo, PortaLicencia, ClaseVehiculo, ServicioVehiculo, Year)


base_2018_victimas <- base_2018_victimas  %>%
  select (idFormulario, Numero, EDAD_PROCESADA, LLevaCinturon, LlevaChaleco, LLevaCasco,
          Sexo, VEHICULO_VIAJABA, Year)


### Unir Bases

# Base Siniestros
base_siniestros <- bind_rows(base_2017_siniestros, base_2018_siniestros)



# Base Conductores
glimpse(base_2017_conductores)
glimpse(base_2018_conductores)
base_2018_conductores$Vehiculo <- as.double(base_2018_conductores$Vehiculo)
base_2018_conductores$EDAD_PROCESADA <- as.double(base_2018_conductores$EDAD_PROCESADA)


base_conductores <- bind_rows(base_2017_conductores, base_2018_conductores)

# Base Victimas

glimpse(base_2017_victimas)
glimpse(base_2018_victimas)

base_2018_victimas$EDAD_PROCESADA <- as.double(base_2018_victimas$EDAD_PROCESADA)

base_victimas <- bind_rows(base_2017_victimas, base_2018_victimas)



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

### DEPENDENT VARIABLE

# Variable Dependiente: Grado de severidad del siniestro (1 con muertos, 2 con heridos, 3 con daños) - Dejarla en 1 severo 0 leve


class(base_2017_siniestros$GravedadNombre) #Preferiria dejar solo dos categorias

base_2017_siniestros$GravedadNombre[base_2017_siniestros$GravedadNombre== "Con Heridos"] <- "Con Muertos" 

base_2017_siniestros$GravedadNombre <- factor(base_2017_siniestros$GravedadNombre, 
                                                 levels = c("Con Muertos", "Solo Daños"),
                                                 labels = c("Severo", "Leve"))  ## Poner variable como categorica
class(base_2017_siniestros$GravedadNombre)
levels(base_2017_siniestros$GravedadNombre)
table(base_2017_siniestros$GravedadNombre)

### HUMAN FACTOR

# Accident Type: ClaseNombre

class(base_2017_siniestros$ClaseNombre)
table(base_2017_siniestros$ClaseNombre)

base_2017_siniestros$TipoAccidente <- factor(base_2017_siniestros$ClaseNombre, 
                                           levels = c("Atropello", "Autolesion", "Caida Ocupante", "Choque",
                                                      "Incendio", "Otro", "Volcamiento"),
                                           labels = c("Atropello", "Autolesion", "Caida Ocupante", "Choque",
                                                      "Incendio", "Otro", "Volcamiento"))  ## Poner variable como categorica
class(base_2017_siniestros$TipoAccidente)
table(base_2017_siniestros$TipoAccidente)

# Violation of law: 

#1. Embriaguez

class(base_2017_siniestros$CON_EMBRIAGUEZ)
table(base_2017_siniestros$CON_EMBRIAGUEZ)

base_2017_siniestros$CON_EMBRIAGUEZ <- factor(base_2017_siniestros$CON_EMBRIAGUEZ, 
                                              levels = c("SI", "NO"),
                                              labels = c("SI", "NO"))  ## Poner variable como categorica



class(base_2017_siniestros$CON_EMBRIAGUEZ)
table(base_2017_siniestros$CON_EMBRIAGUEZ)

#2. Velocidad

class(base_2017_siniestros$CON_VELOCIDAD)
table(base_2017_siniestros$CON_VELOCIDAD)

base_2017_siniestros$CON_VELOCIDAD <- factor(base_2017_siniestros$CON_VELOCIDAD, 
                                              levels = c("SI", "NO"),
                                              labels = c("SI", "NO"))  ## Poner variable como categorica



class(base_2017_siniestros$CON_VELOCIDAD)
table(base_2017_siniestros$CON_VELOCIDAD)


#3. Otro: No Porta Licencia, No lleva cinturon, No lleva caso, No lleva Chaleco









# Gender ****** PENDIENTE

# Age






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