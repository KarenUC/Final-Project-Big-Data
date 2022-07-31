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
       leaflet, #visualizci贸n
       tmaptools, #geocode
       osmdata, # Get OSM data
       expss,
       ggmap,
       readr, 
       readxl
)

#setwd("/Users/jdaviduu96/Documents/MECA 2022/Big Data y Machine Learning 2022-13/Final Project/Final-Project_Big-Data_Ramos-Uribe-Urquijo")
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
base_2017_conductores$Year <- "2017"
base_2017_siniestros$Year <- "2017"

# 2018
base_2018_victimas$Year <- "2018"
base_2018_conductores$Year <- "2018"
base_2018_siniestros$Year <- "2018"


glimpse(base_2018_siniestros)
glimpse(base_2018_conductores)
glimpse(base_2017_victimas)


########## Seleccion Variables

#2017

base_2017_siniestros <- base_2017_siniestros %>% 
                        select(idFormulario, Dia, Fecha, GravedadNombre, ClaseNombre, Latitud, 
                               Longitud, Direccion, TipoDisenno, TipoTiempo, CON_BICICLETA,
                               CON_CARGA, CON_EMBRIAGUEZ, CON_HUECOS, CON_MENORES, CON_MOTO,
                               CON_PEATON, CON_PERSONA_MAYOR, CON_RUTAS, CON_TPI, CON_VELOCIDAD, Year, HORA_PROCESADA )

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
         CON_PEATON, CON_PERSONA_MAYOR, CON_RUTAS, CON_TPI, CON_VELOCIDAD, Year, HORA_PROCESADA)

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
##Dado que existen accidentes con heridos y con muertos pero sin v铆ctimas, se asume que si tienen heridos o victimas, al menos tienen una v铆ctima
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
table(base_siniestros$GravedadNombre)

base_siniestros$GravedadNombre[base_siniestros$GravedadNombre== "Con Heridos"] <- "Con Muertos" 

#Ojo cambiar variable da?os para que quede bien

base_siniestros$GravedadNombre <- factor(base_siniestros$GravedadNombre, 
                                                 levels = c("Con Muertos", "Solo Da帽os"),
                                                 labels = c("Severo", "Leve"))  ## Poner variable como categorica
class(base_siniestros$GravedadNombre)
levels(base_siniestros$GravedadNombre)
table(base_siniestros$GravedadNombre)
sum(is.na(base_siniestros$GravedadNombre))

### HUMAN FACTOR

# Accident Type: ClaseNombre

class(base_siniestros$ClaseNombre)
table(base_siniestros$ClaseNombre)

base_siniestros$TipoAccidente <- factor(base_siniestros$ClaseNombre, 
                                           levels = c("Atropello", "Autolesion", "Caida de ocupante", "Choque",
                                                      "Incendio", "Otro", "Volcamiento"),
                                           labels = c("Atropello", "Autolesion", "Caida de ocupante", "Choque",
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

##Dado que existen otro tipo de "infracciones", se crea una categor铆a adicional

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
base_siniestros$Otrainfra_c_v <- ifelse(is.na(base_siniestros$Otrainfra_c_v), 0, base_siniestros$Otrainfra_c_v)
base_siniestros$Otrainfra_c_v <- factor(base_siniestros$Otrainfra_c_v)

# Gender ****** 

# ------------- Conductores
class(base_siniestros$num_hombres_c)
class(base_siniestros$num_mujeres_c)

sum(is.na(base_siniestros$num_hombres_c))
sum(is.na(base_siniestros$num_mujeres_c))

#Si existen NAs (es solo uno entonces se elimina)

base_siniestros <- base_siniestros[!is.na(base_siniestros$num_hombres_c),]
base_siniestros <- base_siniestros[!is.na(base_siniestros$num_mujeres_c),]

sum(is.na(base_siniestros$num_hombres_c))
sum(is.na(base_siniestros$num_mujeres_c)) ## Ya no quedan NAs

# --------------- Victimas

class(base_siniestros$num_hombres_v)
class(base_siniestros$num_mujeres_v)

sum(is.na(base_siniestros$num_hombres_v))
sum(is.na(base_siniestros$num_mujeres_v))

# Age

class(base_siniestros$m_edad_c)
class(base_siniestros$m_edad_v)

sum(is.na(base_siniestros$m_edad_c))
sum(is.na(base_siniestros$m_edad_v)) # Esta bien no existen NAs

## ===== crear grupos de edad === #

# Conductores

base_siniestros$categorias_edad_c <- ifelse(base_siniestros$m_edad_c<=18,
                                            yes = "menor edad",
                                            no = NA)

base_siniestros$categorias_edad_c <- ifelse(base_siniestros$m_edad_c>18 & base_siniestros$m_edad_c<=34,
                                            yes = "joven",
                                            no = base_siniestros$categorias_edad_c)

base_siniestros$categorias_edad_c <- ifelse(base_siniestros$m_edad_c>34 & base_siniestros$m_edad_c<=49,
                                            yes = "adulto",
                                            no = base_siniestros$categorias_edad_c)

base_siniestros$categorias_edad_c <- ifelse(base_siniestros$m_edad_c>49 & base_siniestros$m_edad_c<=64,
                                            yes = "mayor",
                                            no = base_siniestros$categorias_edad_c)

base_siniestros$categorias_edad_c <- ifelse(base_siniestros$m_edad_c>64,
                                            yes = "anciano",
                                            no = base_siniestros$categorias_edad_c)

table(base_siniestros$categorias_edad_c)

table(base_siniestros$categorias_edad_c, base_siniestros$GravedadNombre)

sum(is.na(base_siniestros$categorias_edad_c))

base_siniestros$categorias_edad_c <- as.factor(base_siniestros$categorias_edad_c)
class(base_siniestros$categorias_edad_c)

# Victimas

base_siniestros$categorias_edad_v <- ifelse(base_siniestros$m_edad_v<=18,
                                            yes = "menor edad",
                                            no = NA)

base_siniestros$categorias_edad_v <- ifelse(base_siniestros$m_edad_v>18 & base_siniestros$m_edad_v<=34,
                                            yes = "joven",
                                            no = base_siniestros$categorias_edad_v)

base_siniestros$categorias_edad_v <- ifelse(base_siniestros$m_edad_v>34 & base_siniestros$m_edad_v<=49,
                                            yes = "adulto",
                                            no = base_siniestros$categorias_edad_v)

base_siniestros$categorias_edad_v <- ifelse(base_siniestros$m_edad_v>49 & base_siniestros$m_edad_v<=64,
                                            yes = "mayor",
                                            no = base_siniestros$categorias_edad_v)

base_siniestros$categorias_edad_v <- ifelse(base_siniestros$m_edad_v>64,
                                            yes = "anciano",
                                            no = base_siniestros$categorias_edad_v)

table(base_siniestros$categorias_edad_v)

table(base_siniestros$categorias_edad_v, base_siniestros$GravedadNombre)

sum(is.na(base_siniestros$categorias_edad_v))

base_siniestros$categorias_edad_v <- as.factor(base_siniestros$categorias_edad_v)
class(base_siniestros$categorias_edad_v)

### ROAD FACTOR

# Road Surface

#Con Huecos

class(base_siniestros$CON_HUECOS)
table(base_siniestros$CON_HUECOS)

base_siniestros$CON_HUECOS <- factor(base_siniestros$CON_HUECOS, 
                                         levels = c("SI", "NO"),
                                         labels = c("SI", "NO"))  ## Poner variable como categorica


class(base_siniestros$CON_HUECOS)
table(base_siniestros$CON_HUECOS)


# Road Type

class(base_siniestros$TipoDisenno)
table(base_siniestros$TipoDisenno)
summary(base_siniestros$TipoDisenno)

base_siniestros$TipoDisenno <- factor(base_siniestros$TipoDisenno, 
                                     levels = c("Cicloruta", "Glorieta", "Interseccion", "Lote o predio",
                                                "Paso a nivel", "Paso elevado", "Paso inferior", "Ponton",
                                                "Puente", "Tramo de Via"),
                                     labels = c("Cicloruta", "Glorieta", "Interseccion", "Lote o predio",
                                                "Paso a nivel", "Paso elevado", "Paso inferior", "Ponton",
                                                 "Puente", "Tramo de Via"))  ## Poner variable como categorica

class(base_siniestros$TipoDisenno)
table(base_siniestros$TipoDisenno)

base_siniestros <- base_siniestros[!is.na(base_siniestros$TipoDisenno),]

# Vehicles type Conductores

table(base_conductores$ClaseVehiculo)

### Agrupar tipo de vehiculo por: Autos, Carga, Servicio p煤blico, moto y bici

num_autos_c <- base_conductores %>% 
              group_by(idFormulario) %>%
              summarise(num_autos_c = sum(ClaseVehiculo == "Automovil"|
                                            ClaseVehiculo== "Camioneta"|
                                             ClaseVehiculo=="Campero"))

num_serv_pub_c <- base_conductores %>% 
  group_by(idFormulario) %>%
  summarise(num_serv_pub_c = sum(ClaseVehiculo == "Bus"|
                                ClaseVehiculo== "Buseta"|
                                  ClaseVehiculo=="Microbus"))

num_carga_c <- base_conductores %>% 
                  group_by(idFormulario) %>%
                  summarise(num_carga_c = sum(ClaseVehiculo == "Camion, Furgon"|
                                                ClaseVehiculo== "Volqueta"|
                                                 ClaseVehiculo=="Tractocamion"))

num_moto_c <- base_conductores %>% 
  group_by(idFormulario) %>%
  summarise(num_moto_c = sum(ClaseVehiculo == "Motocarro"|
                                  ClaseVehiculo== "Motocicleta"|
                                   ClaseVehiculo=="Motociclo"|
                                    ClaseVehiculo=="Cuatrimoto"))

num_bici_c <- base_conductores %>% 
  group_by(idFormulario) %>%
  summarise(num_bici_c = sum(ClaseVehiculo == "Bicicleta"))

num_otro_vehi_c <- base_conductores %>% 
  group_by(idFormulario) %>%
  summarise(num_otro_vehi_c = sum(ClaseVehiculo == "Bicitaxi"|
                                   ClaseVehiculo == "null" ))



# Vehicles type Victimas

table(base_victimas$VEHICULO_VIAJABA)

### Agrupar tipo de vehiculo por: Autos, Carga, Servicio p煤blico, moto y bici

num_autos_v <- base_victimas %>% 
  group_by(idFormulario) %>%
  summarise(num_autos_v = sum(VEHICULO_VIAJABA == "AUTOMOVIL"|
                                VEHICULO_VIAJABA== "CAMIONETA"|
                                VEHICULO_VIAJABA=="CAMPERO"))

num_serv_pub_v <- base_victimas %>% 
  group_by(idFormulario) %>%
  summarise(num_serv_pub_v = sum(VEHICULO_VIAJABA == "BUS"|
                                   VEHICULO_VIAJABA== "BUSETA"|
                                   VEHICULO_VIAJABA=="MICROBUS"))


num_carga_v <- base_victimas %>% 
  group_by(idFormulario) %>%
  summarise(num_carga_v = sum(VEHICULO_VIAJABA == "CAMION, FURGON"|
                                VEHICULO_VIAJABA== "VOLQUETA"|
                                VEHICULO_VIAJABA=="TRACTOCAMION"))

num_moto_v <- base_victimas %>% 
  group_by(idFormulario) %>%
  summarise(num_moto_v = sum(VEHICULO_VIAJABA == "MOTOCARRO"|
                               VEHICULO_VIAJABA== "MOTOCICLETA"|
                               VEHICULO_VIAJABA=="CUATRIMOTO"))


num_bici_v <- base_victimas %>% 
  group_by(idFormulario) %>%
  summarise(num_bici_v = sum(VEHICULO_VIAJABA == "BICICLETA"))


num_otro_vehi_v <- base_victimas %>% 
  group_by(idFormulario) %>%
  summarise(num_otro_vehi_v = sum(VEHICULO_VIAJABA == "BICITAXI"|
                                    VEHICULO_VIAJABA == "NULL" ))

num_peatones_v <- base_victimas %>% 
  group_by(idFormulario) %>%
  summarise(num_peatones_v = sum(VEHICULO_VIAJABA == "PEATON" ))


### Pegar a base de siniestros

base_siniestros<-left_join(base_siniestros,num_autos_c, by="idFormulario")
base_siniestros <-left_join(base_siniestros,num_serv_pub_c, by="idFormulario")
base_siniestros <-left_join(base_siniestros,num_carga_c, by="idFormulario")
base_siniestros<-left_join(base_siniestros,num_moto_c, by="idFormulario")
base_siniestros<-left_join(base_siniestros,num_bici_c, by="idFormulario")
base_siniestros<-left_join(base_siniestros,num_otro_vehi_c, by="idFormulario")

base_siniestros<-left_join(base_siniestros,num_autos_v, by="idFormulario")
base_siniestros <-left_join(base_siniestros,num_serv_pub_v, by="idFormulario")
base_siniestros <-left_join(base_siniestros,num_carga_v, by="idFormulario")
base_siniestros<-left_join(base_siniestros,num_moto_v, by="idFormulario")
base_siniestros <-left_join(base_siniestros,num_bici_v, by="idFormulario")
base_siniestros <-left_join(base_siniestros,num_otro_vehi_v, by="idFormulario")
base_siniestros<-left_join(base_siniestros,num_peatones_v, by="idFormulario")


base_siniestros$num_autos_v  <- ifelse(is.na(base_siniestros$num_autos_v),0,base_siniestros$num_autos_v)
base_siniestros$num_serv_pub_v <- ifelse(is.na(base_siniestros$num_serv_pub_v),0,base_siniestros$num_serv_pub_v)
base_siniestros$num_carga_v <- ifelse(is.na(base_siniestros$num_carga_v),0,base_siniestros$num_carga_v)
base_siniestros$num_moto_v <- ifelse(is.na(base_siniestros$num_moto_v),0,base_siniestros$num_moto_v)
base_siniestros$num_bici_v <- ifelse(is.na(base_siniestros$num_bici_v),0,base_siniestros$num_bici_v)
base_siniestros$num_otro_vehi_v <- ifelse(is.na(base_siniestros$num_otro_vehi_v),0,base_siniestros$num_otro_vehi_v)
base_siniestros$num_peatones_v <- ifelse(is.na(base_siniestros$num_peatones_v),0,base_siniestros$num_peatones_v)


#########--------Enviromental factors ---------#######################################

table(base_siniestros$TipoTiempo)

base_siniestros = base_siniestros %>% 
  mutate(clima = ifelse(TipoTiempo=="Normal"|TipoTiempo=="Normal/Normal"|
                          TipoTiempo=="Normal/Viento"|TipoTiempo=="Viento"|TipoTiempo=="Viento/Normal",
                           yes = "Soleado",
                           no = TipoTiempo))

base_siniestros = base_siniestros %>% 
  mutate(clima = ifelse(TipoTiempo=="Granizo"|TipoTiempo=="Lluvia"|TipoTiempo=="Lluvia/Lluvia"|
                          TipoTiempo=="Lluvia/Normal"|TipoTiempo=="Viento/Lluvia",
                        yes = "Lluvioso",
                        no = clima))

base_siniestros = base_siniestros %>% 
  mutate(clima = ifelse(TipoTiempo=="Niebla"|TipoTiempo=="Niebla/Normal",
                        yes = "Niebla",
                        no = clima))

######## ----- Tiempo del siniestro ----- #############

table(base_siniestros$HORA_PROCESADA)
class(base_siniestros$HORA_PROCESADA)

base_siniestros$tiempo = ifelse(base_siniestros$HORA_PROCESADA<=6 & base_siniestros$HORA_PROCESADA>=0,
                        yes = "Madrugada",
                        no = base_siniestros$HORA_PROCESADA)

base_siniestros$tiempo = ifelse(base_siniestros$HORA_PROCESADA<=18 & base_siniestros$HORA_PROCESADA>6,
                         yes = "Dia",
                         no = base_siniestros$tiempo)

base_siniestros$tiempo = ifelse(base_siniestros$HORA_PROCESADA<=24 & base_siniestros$HORA_PROCESADA>18,
                         yes = "Noche",
                         no = base_siniestros$tiempo)


base_siniestros$Dia <- factor(base_siniestros$Dia)
base_siniestros$clima <- factor(base_siniestros$clima)
base_siniestros$tiempo <- factor(base_siniestros$tiempo)
base_siniestros$SD <- factor(base_siniestros$SD)
base_siniestros$Year <- factor(base_siniestros$Year)



############ Revisar NAS base_siniestros ######################
summary(base_siniestros)
glimpse(base_siniestros)

######################################################################################
############################# Dividir base ###########################################
######################################################################################

# Partimos nuestra base aleatoriamente. As铆 garantizamos que se conserve la distribuci贸n en las marcas.
set.seed(123)
n <- 0.8*nrow(base_siniestros)
n <- round(n, 0)
index <- sample(1:nrow(base_siniestros), n)
train_base_siniestros <- base_siniestros[index,]
test_base_siniestros <- base_siniestros[-index,]
#Verificamos que la partici贸n sea adecuada
prop.table(table(train_base_siniestros$GravedadNombre))
prop.table(table(test_base_siniestros$GravedadNombre))
prop.table(table(base_siniestros$GravedadNombre))

# ######################################################################################
# #################-------Balancear muestra----------###################################
# ######################################################################################
table(base_siniestros$GravedadNombre)
#Upsampling
set.seed(123)
train_upSampled_base <- upSample(x = train_base_siniestros,
                           y = train_base_siniestros$GravedadNombre,
                           yname = "GravedadNombre")

dim(train_base_siniestros)
dim(train_upSampled_base)
table(train_upSampled_base$GravedadNombre)
table(base_siniestros$GravedadNombre)

#Downsampling
set.seed(123)
train_downSampled_base <- downSample(x = train_base_siniestros,
                                 y = train_base_siniestros$GravedadNombre,
                                 yname = "GravedadNombre")

dim(train_base_siniestros)
dim(train_downSampled_base)
table(train_downSampled_base$GravedadNombre)
table(base_siniestros$GravedadNombre)


######################################################################################
#############################----MODELOS----##########################################
######################################################################################

######--- LOGIT ----####

set.seed(123)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- trainControl(method = "cv",
                           number = 5,
                           summaryFunction = fiveStats,
                           classProbs = TRUE,
                           verbose=FALSE,
                           savePredictions = T)
lambda_grid <- 10^seq(-4, 0.01, length = 100)


logit_lasso_upsample <- train(GravedadNombre ~ TipoAccidente + CON_EMBRIAGUEZ + 
                                                 CON_VELOCIDAD + Otrainfra_c_v + 
                                                num_hombres_c + num_mujeres_c + num_hombres_v+
                                                num_mujeres_v + categorias_edad_c +categorias_edad_v+
                                                CON_HUECOS + TipoDisenno + num_autos_c + num_serv_pub_c+
                                                num_carga_c+num_moto_c+num_bici_c+num_otro_vehi_c+num_autos_v+
                                                num_serv_pub_v+num_carga_v+num_moto_v+num_bici_v+num_otro_vehi_v+num_peatones_v+
                                                num_otro_c+tiempo+Dia+clima,
                                data = train_upSampled_base,
                                method = "glmnet",
                                trControl = ctrl,
                                family = "binomial",
                                metric = "ROC",
                                tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                                preProcess = c("center", "scale")
)


logit_lasso_upsample
logit_lasso_upsample[["bestTune"]]

result_lassoupsample <- logit_lasso_upsample[["results"]][60,-1]

#########--Predicci贸n --- ##########################################################################################################################

train_base_siniestros$lasso_upsample<- predict(logit_lasso_upsample,
                                     newdata = train_base_siniestros,
                                     type = "prob")[,1]

summary(train_base_siniestros$GravedadNombre)
summary(train_base_siniestros$lasso_upsample)

######## Determinar cutoff ###########################################################################################################################

p_load(pROC)
rfROC <- roc(train_base_siniestros$GravedadNombre, train_base_siniestros$lasso_upsample, levels = rev(levels(train_base_siniestros$GravedadNombre)))
rfROC
rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh

#### el corte es  0.5250239

train_base_siniestros$hat_gravedad_05=ifelse(train_base_siniestros$lasso_upsample>0.5,"Severo","Leve")

train_base_siniestros$hat_gravedad_rfThresh=ifelse(train_base_siniestros$lasso_upsample>rfThresh$threshold,"Severo","Leve")

##### Matriz de confusi贸n ################

with(train_base_siniestros,table(GravedadNombre,hat_gravedad_05))
with(train_base_siniestros,table(GravedadNombre,hat_gravedad_rfThresh))

#Preferimos identificar los siniestros severos que es reducir el erro tipo 2. Por lo que preferimos usar el cutoff en 0.5

#########--Predicci贸n con test--- ##########################################################################################################################

test_base_siniestros$lasso_upsample<- predict(logit_lasso_upsample,
                                               newdata = test_base_siniestros,
                                               type = "prob")[,1]

summary(test_base_siniestros$GravedadNombre)
summary(test_base_siniestros$lasso_upsample)

test_base_siniestros$hat_gravedad_05=ifelse(test_base_siniestros$lasso_upsample>0.5,"Severo","Leve")

##### Matriz de confusi贸n ################

with(test_base_siniestros,table(GravedadNombre,hat_gravedad_05))


# Identificamos cu?ntos cores tiene nuestra m?quina

p_load(tidyverse, ggplot2, doParallel, rattle, MLmetrics,
       janitor, fastDummies, tidymodels, caret)
n_cores <- detectCores()
print(paste("Mi PC tiene", n_cores, "nucleos"))

# # Vamos a usar n_cores - 2 procesadores para esto
# cl <- makePSOCKcluster(n_cores - 6) 
# registerDoParallel(cl)
# stopCluster(cl)


######--- XGBOOST ---######

#Paquetes

p_load(xgboost)
p_load(caret)

# Semilla
set.seed(123)

require("xgboost")

grid<- expand.grid(nrounds = c(250,500),
                         max_depth = c(4,6,8),
                         eta = c(0.01,0.3,0.5),
                         gamma = c(0,1),
                         min_child_weight = c(10, 25,50),
                         colsample_bytree = c(0.7),
                         subsample = c(0.6))
set.seed(1410)

xgboost <- train(
  GravedadNombre ~ TipoAccidente + CON_EMBRIAGUEZ + 
    CON_VELOCIDAD + Otrainfra_c_v + 
    num_hombres_c + num_mujeres_c + num_hombres_v+
    num_mujeres_v + categorias_edad_c +categorias_edad_v+
    CON_HUECOS + TipoDisenno + num_autos_c + num_serv_pub_c+
    num_carga_c+num_moto_c+num_bici_c+num_otro_vehi_c+num_autos_v+
    num_serv_pub_v+num_carga_v+num_moto_v+num_bici_v+num_otro_vehi_v+num_peatones_v+
    num_otro_c+tiempo+Dia+clima,
  data = train_base_siniestros,
  method = "xgbTree",
  trControl = ctrl,
  metric = "Sens",
  tuneGrid = grid,
  preProcess = c("center", "scale")
)

xgboost$bestTune
xgboost_Results <- xgboost$results

#---Variable Importance

var_imp<- varImp(xgboost, scale = FALSE)
plot(var_imp)

var_imp_xgb <- var_imp$importance[1:20,]
var_imp_xgb<- as.data.frame(var_imp_xgb)
class(var_imp_xgb)

rownames(var_imp_xgb) = c("No.Motos Conductor", "Tipo Accidente = Choque", "No. Bicis Conductor", 
                              "No. Peatones V韈timas", "No. Autos Conductores", "Victima Joven", "No. Mujeres Victimas",
                              "No. Hombres Victimas", "No. Motos Victimas", "No. Autos Victimas", "No. Servicio Pub. Conductor",
                              "No. Carga Conductor", "No. Hombres Conductor", "Intersecci髇", "Madrugada", "Conductor Joven", "Tramo V韆",
                              "Mujeres Conductor", "Otra Infracci髇", "Viernes")
                          
                          
var_imp_xgb$varnames<- rownames(var_imp_xgb)


ggplot(var_imp_xgb, aes(x=reorder(varnames, var_imp_xgb), y=var_imp_xgb)) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=var_imp_xgb), color = "#56B4E9") +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip()

# Predictions

######--- DBSCAN ---######

p_load(dbscan)
colnames(base_siniestros)

df_train <- train_base_siniestros[,-4]
df_train <- df_train[,-1]
colnames(df_train)

df_train<- select(df_train, TipoAccidente,CON_EMBRIAGUEZ ,
                     CON_VELOCIDAD , Otrainfra_c_v,
                     num_hombres_c , num_mujeres_c , num_hombres_v,
                     num_mujeres_v , categorias_edad_c ,categorias_edad_v,
                     CON_HUECOS , TipoDisenno , num_autos_c , num_serv_pub_c,
                     num_carga_c, num_moto_c, num_bici_c, num_otro_vehi_c, num_autos_v,
                     num_serv_pub_v, num_carga_v, num_moto_v, num_bici_v, num_otro_vehi_v,
                     num_peatones_v, num_otro_c, tiempo, Dia, clima)

df_train <- data.matrix(df_train)
class(df_train)
df_train  <-scale(df_train)

#df: dataset sin variable "species"
#k: el n?mero m?nimo de puntos que elegimos

kNNdist(df_train , k = 31)
kNNdistplot(df_train, k = 31)
abline(h = 10, lty = 2)

#Con eps=18.2
cl<-dbscan(df_train,eps=10,MinPts = 62)
head(cbind(df_train,cl$cluster))

# DBSCAN clustering for 57569 objects.
# Parameters: eps = 10, minPts = 62
# The clustering contains 4 cluster(s) and 150 noise points.
# 
# 0     1     2     3     4 
# 150 56465   399   480    75 
# 
# Available fields: cluster, eps, minPts
p_load(factoextra)

fviz_cluster(cl, df_train, geom = "point")

