########## Objetos vectoriales en R: sp y sf ########################
########## Ecologia Regional 2022            ########################
########## Autores: Regino Cavia             ########################

# Usaremos Mayuscula para los objetos creados por nosotros (apareceran en "Global Environment") para distinguirlos de funciones existentes en R
# Lea la nota a la derecha del simbolo "#" antes de correr la linea, puede haber una intruccion para ud para que realice antes de correr dicha linea o puede estar explicandole que es lo que hace la intruccion. EL uso de tildes (o caracteres raros) fue evitado porque suelen desconfigurarse.
# Para evitar "scrollear" a la derecha para continuar leyendo, puede configurar el ajuste de lineas (recomendamos): Tools > Global options > Code > tildar "Soft-wrap R source files". Es posible que le pida reiniciar RStudio

rm(list=ls()) # Remueve los objetos de la memoria de R. En Rstudio estos aparecen en "Global environment"
# tambien se puede hacerse con la **escoba** de la solapa Environment.


setwd("D:\\EcoRegional2021\\TP5\\") #configurar el directorio donde estan los datos y donde se guardan los datos que se escriben. En RStudio, se puede hacer a partir de la solapa Files, navegando al directorio deseado y luego con el boton More > Set as working directory.
#setwd("d:\\basesgis\\Regional\\TP5\\TP_5_Datos_espaciales_e_interpolacion\\") dir() # que hay en el directorio de trabajo (o la carpeta del proyecto)
getwd() #cual es el directorio de trabajo ("working directory")

library(gstat) 
library(sp)
library(rgdal)
library(leaflet)
library(sf)
library(dismo)
library(tmap)

### Ejercicio 1: manejos de objetos vectoriales de la libreria sp 
# Cargamos un archivo de texto delimitado por tabulaciones
# Descargue el archivo xls y guardelo como "Texto (delimitado por tabulaciones)(*.txt)" en el directorio de trabajo
# Cargamos el archivo

Puntos_CU <- read.delim("Ciudad_Universitaria.txt")
class(Puntos_CU)
str(Puntos_CU)
plot(Puntos_CU$Long, Puntos_CU$Lat)

# Convertimos el "data.frame" a un "SpatialPointsDataFrame" objeto del paquete sp
sp_Puntos_CU <- Puntos_CU #copiamos el objeto
coordinates(sp_Puntos_CU) <- ~Long + Lat # lo convertimos (tambien puede usar la forma Puntos_CU_sp[ ,2:3])
class(sp_Puntos_CU)
str(sp_Puntos_CU)

plot(sp_Puntos_CU) # observe la diferencia con el grafico anterior
plot(sp_Puntos_CU, axes=TRUE) # agragamos las coordenadas
spplot(sp_Puntos_CU, "Id")

# Asignamos un sisteman de referencia geografico a nuestro SpatialPointsDataFrame, en este ejemplo a WGS84 que es el sistema de coordenadas de los datos.
sp_Puntos_CU
proj4string(sp_Puntos_CU) #consultamos el sistema de referencia
proj4string(sp_Puntos_CU) <- CRS("EPSG:4326") # o usando la forma: CRS("+proj=longlat +datum=WGS84 +no_defs") o WGS84 = CRS("+init=epsg:4326")
proj4string(sp_Puntos_CU) #consultamos el sistema de referencia

# Guardamos el objeto como archivo .shp (elija un directorio y nombre para el archivo-> layer)
writeOGR(sp_Puntos_CU, dsn= "D:\\basesgis\\Regional\\TP5\\TP_5_Datos_espaciales_e_interpolacion\\", layer="Ciudad_Universitaria",  driver="ESRI Shapefile")

# Ahora podemos leer un archivo .shp 
sp_Puntos_CU_1<-readOGR("Ciudad_Universitaria.shp")
sp_Puntos_CU_1

# Vamos a reproyectar a una faja de POSGAR, podemos escribir los parametros o buscar el codigo EPSG en https://epsg.io/

# POSGAR 2007 / Argentina 3: EPSG:5345
# POSGAR 2007 / Argentina 5: EPSG:5347

POSGAR_2005_FAJA5 <- CRS("+init=epsg:5347")

sp_Puntos_CU_5 <- spTransform(sp_Puntos_CU_1, POSGAR_2005_FAJA5)
sp_Puntos_CU_5

plot(sp_Puntos_CU_5, axes=TRUE)

### Ejercicio 2: manejos de objetos vectoriales de la libreria sf: library(sf) 

# Tenemos varios metodos posibles, aqui a partir de un data.frame, de un objeto "sp" o de un archivo

# A partir de un data.frame
sf_Puntos_CU_1 <- st_as_sf(Puntos_CU, coords = c("Long", "Lat"), crs = <el numero que correspoda>) # Busque el de WGS84
sf_Puntos_CU_1 <- st_as_sf(Puntos_CU, coords = c("Long", "Lat"), crs = 4326) # Busque el de WGS84

str(sf_Puntos_CU_1)
sf_Puntos_CU_1
plot(sf_Puntos_CU_1)

# A partir de un objeto "sp"
sf_Puntos_CU_2 <- st_as_sf(sp_Puntos_CU_1) # Busque el de WGS84
str(sf_Puntos_CU_2)
sf_Puntos_CU_2

# A partir de un archivo .shp
sf_Puntos_CU_3 <- st_read("Ciudad_Universitaria.shp")
str(sf_Puntos_CU_3)
sf_Puntos_CU_3

### Ejercicio 3: Mapas con la libreria tmap: library(tmap)

# Activamos la vista interactiva de tmap
tmap_mode("view") 

# tmap trabaja con capas similar a ggplot, la primera capa se refiere al objeto a mapear y la segunda a la simbología a utilizar
tm_shape(sf_Puntos_CU_1) + tm_dots() # Despliegue las capas debajo del +/-

# Probamos cambiar el tamaño, color y transparencia
#col = "red"
#col = "#E2E2E2", col = "red"
#alpha = 0.3
#size = 0.3

tm_shape(sf_Puntos_CU_1) + tm_dots(col = "red", size = 0.5, alpha = 0.3) 

# Por ultimo usamos una rampa de colores para un campo, en este caso el Id

tm_shape(sf_Puntos_CU_1) + tm_dots("Id", palette = sf.colors(7)) 


# Puede consultar mas ejemplos para graficar objetos sp en
# https://edzer.github.io/sp/

# Puede consultar mas ejemplos para graficar con tmap en
# https://bookdown.org/nicohahn/making_maps_with_r5/docs/tmap.html

# Un libro mas que recomendable es el último libro de Pebesma & Bivard, autores de estas librerías:
# https://r-spatial.org/book/sp-raster.html

# Una version en castellano: Estadística Espacial con R (Casal y Yañez) 
# https://rubenfcasal.github.io/estadistica_espacial/