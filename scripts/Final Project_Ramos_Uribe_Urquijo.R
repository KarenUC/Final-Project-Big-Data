# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 29-07-2022
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
       leaflet, #visualizción
       tmaptools, #geocode
       osmdata, # Get OSM data
       expss,
       ggmap,
       readr, 
       readxl
)


setwd("C:/Users/pau_9/Documents/GitHub/Final-Project_Big-Data_Ramos-Uribe-Urquijo")
#setwd("C:/Users/kurib/OneDrive - Universidad de los Andes/Documentos/MECA/Github/Final-Project-Big-Data")

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
#Incluir N?mero de veh?culos involucrados, edad promedio conductor y edad promedio v?ctima y para el g?nero contar el n?mero de mujeres y hombres involucrados. 
#Para fecha: as.Date(Base$variable, format = '%m/%d/%Y')


#======= BASE ======#
### Crear numero de veh?culos involucrados por accidente

num_vehic <- base_conductores %>% 
  group_by(idFormulario)%>% 
  summarise(Vehiculo = n())

base_siniestros <-base_siniestros %>% left_join(num_vehic,by="idFormulario")

sum(is.na(base_siniestros$Vehiculo))

### Crear numero de victimas por accidente
colnames(base_victimas)

num_vic <- base_victimas %>% 
  group_by(idFormulario)%>% 
  summarise(Numero = n())

base_siniestros <-base_siniestros %>% left_join(num_vic,by="idFormulario")
base_siniestros$SD <- (base_siniestros$GravedadNombre)

sum(is.na(base_siniestros$Numero))

#Asumir que no existen v?ctimas si existe NA

base_siniestros$Numero <- ifelse(is.na(base_siniestros$Numero),
                                       yes = 0,
                                       no = base_siniestros$Numero)
table(base_siniestros$Numero, base_siniestros$GravedadNombre)
##Dado que existen accidentes con heridos y con muertos pero sin víctimas, se asume que si tienen heridos o victimas, al menos tienen una víctima
base_siniestros$Numero <- ifelse((base_siniestros$GravedadNombre == "Con Heridos" | base_siniestros$GravedadNombre == "Con Muertos")& (base_siniestros$Numero == 0) ,
                                 yes = 1,
                                 no = base_siniestros$Numero)

table(base_siniestros$Numero, base_siniestros$GravedadNombre)


### Crear edad promedio conductores

class(base_conductores$EDAD_PROCESADA)

m_edad_c <- base_conductores %>% 
  group_by(idFormulario)%>% 
  summarise(m_edad_c = round(mean(EDAD_PROCESADA), digits = 0))

base_siniestros <-base_siniestros %>% left_join(m_edad_c,by="idFormulario")

sum(is.na(base_siniestros$m_edad_c))

base_siniestros = base_siniestros %>% group_by(idFormulario) %>% 
  mutate(m_edad_c = ifelse(is.na(m_edad_c),
                                 yes = median(base_conductores$EDAD_PROCESADA, na.rm = TRUE),
                                 no = m_edad_c))

### Crear edad promedio victimas

class(base_victimas$EDAD_PROCESADA)

m_edad_v <- base_victimas %>% 
  group_by(idFormulario)%>% 
  summarise(m_edad_v = round(mean(EDAD_PROCESADA), digits = 0))

base_siniestros <-base_siniestros %>% left_join(m_edad_v,by="idFormulario")

sum(is.na(base_siniestros$m_edad_v))

base_siniestros = base_siniestros %>% group_by(idFormulario) %>% 
  mutate(m_edad_v = ifelse(is.na(m_edad_v),
                           yes = median(base_victimas$EDAD_PROCESADA, na.rm = TRUE),
                           no = m_edad_v))

### Variable g?nero ********** OJO REVISAR
# Hay dos opciones: contar el numero de mujeres y hombres o dummy mayor?a mujeres =1 y 0 de lo contrario

# == Numero de mujeres y hombres

# Conductores

num_hombres_c <- base_conductores %>% 
  group_by(idFormulario) %>% 
  summarise(num_hombres_c = sum(Sexo == "MASCULINO"))

num_mujeres_c <- base_conductores %>% 
  group_by(idFormulario) %>% 
  summarise(num_mujeres_c = sum(Sexo == "FEMENINO"))

num_otro_c <- base_conductores %>% 
  group_by(idFormulario) %>% 
  summarise(num_otro_c = sum(Sexo == "NO APLICA"))

sum(base_conductores$num_otro_c)
base_siniestros <-base_siniestros %>% left_join(num_hombres_c,by="idFormulario")
base_siniestros <-base_siniestros %>% left_join(num_mujeres_c,by="idFormulario")
base_siniestros <-base_siniestros %>% left_join(num_otro_c,by="idFormulario")

table(base_siniestros$SD, base_siniestros$num_hombres_c)
table(base_siniestros$SD, base_siniestros$num_mujeres_c)
table(base_siniestros$SD, base_siniestros$num_otro_c)

sum(base_siniestros$num_otro_c, na.rm = TRUE)

# Victimas

num_hombres_v <- base_victimas %>% 
  group_by(idFormulario) %>% 
  summarise(num_hombres_v = sum(Sexo == "MASCULINO"))

num_mujeres_v <- base_victimas %>% 
  group_by(idFormulario) %>% 
  summarise(num_mujeres_v = sum(Sexo == "FEMENINO"))

base_siniestros <-base_siniestros %>% left_join(num_hombres_v,by="idFormulario")
base_siniestros <-base_siniestros %>% left_join(num_mujeres_v,by="idFormulario")

table(base_siniestros$num_hombres_v, base_siniestros$GravedadNombre )
table(base_siniestros$num_mujeres_v,base_siniestros$GravedadNombre )

base_siniestros = base_siniestros %>% group_by(idFormulario) %>% 
  mutate(num_hombres_v = ifelse(is.na(num_hombres_v),
                           yes = 0,
                           no = num_hombres_v))

base_siniestros = base_siniestros %>% group_by(idFormulario) %>% 
  mutate(num_mujeres_v = ifelse(is.na(num_mujeres_v),
                                yes = 0,
                                no = num_mujeres_v))

############### ========= ORGANIZACION VARIABLES RELEVANTES ============ ##############


## Variables relevantes: id, Dia, Fecha, GravedadCod, GravedadNombre, ClaseCodigo, ClaseNombre, Latitud, Longitud, Direccion,
                      #  Localidad, HoraOcurrencia, TipoTiempo, Vehiculo, Numero, m_edad_c, m_edad_v, num_hombres_c, num_mujeres_c, 
                      #   num_hombres_v, num_mujeres_v

### DEPENDENT VARIABLE

# Variable Dependiente: Grado de severidad del siniestro (1 con muertos, 2 con heridos, 3 con da?os) - Dejarla en 1 severo 0 leve
glimpse(base_siniestros)


class(base_siniestros$GravedadNombre) #Preferiria dejar solo dos categorias

base_siniestros$GravedadNombre[base_siniestros$GravedadNombre== "Con Heridos"] <- "Con Muertos" 

base_siniestros$GravedadNombre <- factor(base_siniestros$GravedadNombre, 
                                                 levels = c("Con Muertos", "Solo Daños"),
                                                 labels = c("Severo", "Leve"))  ## Poner variable como categorica
class(base_siniestros$GravedadNombre)
levels(base_siniestros$GravedadNombre)
table(base_siniestros$GravedadNombre)

### HUMAN FACTOR

# Accident Type: ClaseNombre

class(base_siniestros$ClaseNombre)
table(base_siniestros$ClaseNombre)

base_siniestros$TipoAccidente <- factor(base_siniestros$ClaseNombre, 
                                           levels = c("Atropello", "Autolesion", "Caida Ocupante", "Choque",
                                                      "Incendio", "Otro", "Volcamiento"),
                                           labels = c("Atropello", "Autolesion", "Caida Ocupante", "Choque",
                                                      "Incendio", "Otro", "Volcamiento"))  ## Poner variable como categorica
class(base_siniestros$TipoAccidente)
table(base_siniestros$TipoAccidente)

# Violation of law: 

#1. Embriaguez

class(base_siniestros$CON_EMBRIAGUEZ)
table(base_siniestros$CON_EMBRIAGUEZ)

base_siniestros$CON_EMBRIAGUEZ <- factor(base_siniestros$CON_EMBRIAGUEZ, 
                                              levels = c("SI", "NO"),
                                              labels = c("SI", "NO"))  ## Poner variable como categorica

class(base_siniestros$CON_EMBRIAGUEZ)
table(base_siniestros$CON_EMBRIAGUEZ)

#2. Velocidad

class(base_siniestros$CON_VELOCIDAD)
table(base_siniestros$CON_VELOCIDAD)

base_siniestros$CON_VELOCIDAD <- factor(base_siniestros$CON_VELOCIDAD, 
                                              levels = c("SI", "NO"),
                                              labels = c("SI", "NO"))  ## Poner variable como categorica



class(base_siniestros$CON_VELOCIDAD)
table(base_siniestros$CON_VELOCIDAD)


#3. Otro: No Porta Licencia, No lleva cinturon, No lleva caso, No lleva Chaleco

##Dado que existen otro tipo de "infracciones", se crea una categoría adicional

base_conductores$OtraInfrac<-ifelse((base_conductores$PortaLicencia == "N" | base_conductores$LLevaCinturon == "N" |
                                        base_conductores$LLevaCasco == "N" | base_conductores$LLevaChaleco == "N"), 1, 0)
OtraInfrac<-base_conductores %>% group_by(idFormulario) %>% summarise(OtraInfrac = sum(OtraInfrac))

base_victimas$OtraInfraV <-ifelse((base_victimas$LLevaCinturon == "N" | base_victimas$LLevaCasco == "N" |
                                   base_victimas$LlevaChaleco == "N"),1, 0)
OtraInfraV<-base_victimas %>% group_by(idFormulario) %>% summarise(OtraInfrav = sum (OtraInfraV))

OI <-left_join(OtraInfrac,OtraInfraV, by="idFormulario")
OI$Otrainfra_c_v<-ifelse((OI$OtraInfrac>=1 | OI$OtraInfrav>=1 ),1, 0)
OI <- OI%>% select (1,4) 
base_siniestros <-left_join(base_siniestros,OI, by="idFormulario")


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