########## Semivariogramas y Kriging ########################
########## Ecologia Regional 2017 >> 2021 ##########################
########## Autores: Regino Cavia y Natalia Morandeira; ######
########## sobre datos de Anibal Carbajo #################### 
# Usaremos Mayuscula para los objetos creados por nosotros (apareceran en "Global Environment") para distinguirlos de funciones existentes en R
# Lea la nota a la derecha del simbolo "#" antes de correr la linea, puede haber una intruccion para ud para que realice antes de correr dicha linea o puede estar explicandole que es lo que hace la intruccion. EL uso de tildes (o caracteres raros) fue evitado porque suelen desconfigurarse.
# Para evitar "scrollear" a la derecha para continuar leyendo, puede configurar el ajuste de lineas (recomendamos): Tools > Global options > Code > tildar "Soft-wrap R source files". Es posible que le pida reiniciar RStudio

rm(list=ls()) # Remueve los objetos de la memoria de R. En Rstudio estos aparecen en "Global environment"
              # tambien se puede hacerse con la **escoba** de la solapa Environment.

# setwd("D:/EcoRegional2021/TP5/")
setwd("d:\\basesgis\\Regional\\TP_5\\TP_5_Datos_espaciales_e_interpolacion\\") #configurar el directorio donde estan los datos y donde se guardan los datos que se escriben. En RStudio, se puede hacer a partir de la solapa Files, navegando al directorio deseado y luego con el boton More > Set as working directory.
dir() # que hay en el directorio de trabajo (o la carpeta del proyecto)
getwd() #cual es el directorio de trabajo ("working directory")

library(gstat) 
library(sp)
library(rgdal)
library(leaflet)
library(sf)
library(dismo)
library(tmap)

### Ejercicio 1: Cargamos un archivo de texto delimitado por tabulaciones
# Descargue el archivo xls y guardelo como "Texto (delimitado por tabulaciones)(*.txt)" en el directorio de trabajo
# Cargamos el archivo

Puntos_CU <- read.delim("Ciudad_Universitaria.txt")
class(Puntos_CU)
str(Puntos_CU)
plot(Puntos_CU$Long, Puntos_CU$Lat)

# Libreria(sp): Convertimos el "data.frame" a un "SpatialPointsDataFrame" objeto del paquete sp
Puntos_CU_sp <- Puntos_CU #copiamos el objeto
coordinates(Puntos_CU) <- ~Long + Lat # lo convertimos (tambien puede usar la forma Puntos_CU_sp[ ,2:3])
class(Puntos_CU)
str(Puntos_CU)

plot(Puntos_CU) #observe la diferencia con el grafico anterior
plot(Puntos_CU, axes=TRUE) #
spplot(Puntos_CU, "Id", colorkey = TRUE)

# Asignamos un sisteman de referencia geográfico a nuestro SpatialPointsDataFrame

proj4string(Puntos_CU) #consultamos el sistema de referencia

proj4string(Puntos_CU) <- CRS("EPSG:4326") # o usando la forma: CRS("+proj=longlat +datum=WGS84 +no_defs") o WGS84 = CRS("+init=epsg:4326")

proj4string(Puntos_CU) #consultamos el sistema de referencia

## spTransform(meuse, WGS84)


?CRS()
st_as_sf(datos, coords = c("x", "y"), crs = <el numero que correspoda>)

# Guardamos el objeto como archivo .shp
writeOGR(Puntos_CU, "Puntos_Ciudad_Universitaria.shp", layer= Puntos_CU, driver="ESRI Shapefile")
readOGR("cotas.shp")

# Ejercicio 1 ### Cargamos datos vectoriales desde un archivo shp con la libreria rgdal ####
??readOGR
Datos <- readOGR("cotas.shp")
Datos
proj4string(Datos)
summary(Datos)
# Analice la informacion que devolvieron las lineas 29, 30 y 31

# Ejercicio 2 ### Graficamos el shp ####

plot(Datos)

bubble(Datos, maxsize = 1.5)

# Ejercicio 3 ### Construimos los poligonos de Tiessen y los graficamos ####

Poli_Tiessen <- voronoi(Datos)
Poli_Tiessen

plot(Poli_Tiessen)
plot(Datos, col="blue", pch=20,  add=T)

### Vamos a convertir el objeto "SpatialPolygonsDataFrame" a un objeto de la libreria sf (simple features) y graficamos nuevamente ####
### Esta libreria tiene una mayor plasticidad de usos y vinculos con otras librerias como tmap y leaflet #

sf_Poli_Tiessen <- st_as_sf(Poli_Tiessen)
sf_Poli_Tiessen

summary(sf_Poli_Tiessen)
names(sf_Poli_Tiessen)
plot(sf_Poli_Tiessen[1],  axes = TRUE) ## Muestra coordenadas originales
plot(sf_Poli_Tiessen[1],  axes = TRUE, graticule = TRUE) ## cambia la forma de la representacion, con las coordenadas geograficas

sf_Datos<- st_read("cotas.shp") # Aqui leemos el archivo shp 
plot(sf_Datos[1], graticule = TRUE, axes = TRUE) # si lo desea agregue graticule = TRUE,

tmap_mode("view") # Con este comando activamos la vista interactiva de tmap (aunque no hace nada, continue en la siguiente linea)

tm_shape(sf_Poli_Tiessen) + tm_fill("MENS_COTA", palette = sf.colors(7))
# Ahora pruebe tildar y destildar las capas (debajo del zoom del mapa)

tm_shape(sf_Datos) + tm_dots("MENS_COTA", palette = sf.colors(7))
# Ahora pruebe tildar y destildar las capas (debajo del zoom del mapa)

# Podemos sumar capas con "+" en forma similar al paquete ggplot2
tm_shape(sf_Poli_Tiessen) + tm_fill("MENS_COTA", palette = sf.colors(7))+tm_shape(sf_Datos) + tm_dots("MENS_COTA", palette = sf.colors(7))


#### Ahora vamos a interpolar usando la libreria sp, para eso vamos a cargar la informacion desde una tabla de texto. Es un poco mas artesanal el proceso ####
#### Cargar datos ####
Datos <- read.delim("cotas.csv", header=TRUE, sep=",") # tambien puede hacerlo a traves de "Import dataset"
Datos
head(Datos) # muestra las primeras filas
names(Datos) # muestra los nombres de las columnas
str(Datos)

#### Crear datos espaciales: requeridos por la libreria "sp" #### 
DatosEsp <- data.frame(Datos$MENS_COTA, Datos$X,Datos$Y)
head(DatosEsp)
str(DatosEsp) # muestra la estructura del "data.frame"
coordinates(DatosEsp) <- DatosEsp[, 2:3] # Asignamos coordenadas a los datos, que esas coordenadas sean las columnas 2 y 3 (X e Y).
str(DatosEsp) # Observar que ahora "DatosEsp" aparece como un "SpatialPointDataFrame" como si lo hubiesemos cargado con readOGR() desde "cotas.shp"
DatosEsp

#### Graficos de burbujas ####
bubble(DatosEsp, col = "red", main = "Altitud (m s.n.m.)", xlab = "Longitud Oeste",ylab = "Latitud Sur", maxsize = 1.2) # 

# Ejercicio 4 #### Interpolación por inversa distancia ponderada y krigging #####
# Requiere library(gstat) y library(sp) 3

### El primero hacemos una grilla "en blanco". Esta grilla en necesaria para luego "llenarlas" de valores con la interpolación espacial. Es un poco artesanal pero tiene la ventaja de que al ser artesanal no se crea por defecto como en algunos paquetes enlatados y no logramos hacer lo que nosotros queremos sino lo que el programador penso (que en la mayoria de los casos no se adecua a nuestras necesidades)

# Primero analizar la extension del area a interpolar
summary(Datos) # estudiar la extension de los datos en X e Y. LIMITES a considerar (valores minimos y maximos, redondear a multiplos de 100) 

?seq # vamos a usar la función "secuencia" para para armar la grilla
X1 <-seq(from=6359000, to=6376000, by=100)
X1 # Simplemente es un listado de las coordenadas X, cada tantos metros como el tamanio de celda.
length(X1) # cuantas columnas tiene el vector?

Y1 <- seq(from=6160000, to=6178000, by=100) # Lo mismo para la coordenada Y
Y1 # Es un listado de las coordenadas Y, cada tantos metros como el tama?o de celda.
length(Y1) # cuantas filas tiene la matriz?

XY <- expand.grid(X1, Y1) # generamos el listado de coordenadas combinando cada X con cada Y
head(XY)
colnames(XY) <- c("X", "Y") # cambio de nombre de las columnas
head(XY) # head muestra las primeras columnas de la matriz, para tener una idea de cÃ³mo es

#### Convertir en una grilla, requiere library("sp") ####
Grilla <- data.frame(XY$X, XY$Y)
coordinates(Grilla) <- ~XY.X + XY.Y
str(Grilla)

gridded(Grilla) = TRUE
str(Grilla) # la estructura de "Grilla" es de grilla ;)
head(Grilla)


# Ejercicio 5 #### Ahora si interpolamos por inversa distancia ponderada. Revise el apunte teorico y la clase teorica para recordar que es la potencia (r o idp: inverse distance power) en este método.

IDP_05 <- idw(Datos$MENS_COTA ~ 1, DatosEsp, newdata=Grilla, idp = 0.5) ## idp=inverse distance power (r, potencia)
?idw
names(IDP_05)
plot(IDP_05["var1.pred"])
class(IDP_05)

# Cambiamos ahora la potencia
IDP_1 <- idw(Datos$MENS_COTA ~ 1, DatosEsp, newdata=Grilla, idp = 1)
IDP_2 <- idw(Datos$MENS_COTA ~ 1, DatosEsp, newdata=Grilla, idp = 2)

Apilado <- stack(raster(IDP_05),raster(IDP_1),raster(IDP_2)) # Armamos un "raster stack" o un apilado de capas rasters
names(Apilado)

names(Apilado)<-c("IDP_05", "IDP_1", "IDP_2" )
names(Apilado)

windows(9,6)
plot(Apilado)

# Analice, ¿cual es la potencia mas adecuada? ¿Por que? ## exploren otros valores de potencia.

## Si tiene tiempo puede hacer los mapas interactivos con tmap
### Para hacer el mapa interactivo en tmap es mas facil pasar a raster
#proj4string(Apilado) # No tenemos CRS, debemos definirlo

#proj4string(Apilado) <- CRS("+proj=tmerc +lat_0=-90 +lon_0=-57 +k=1 +x_0=6500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")

#tm_shape(Apilado)+tm_raster(palette = "Reds")+tm_facets(as.layers = TRUE) ## despliegue cada capa, para ver los distintos idp

#https://mran.microsoft.com/snapshot/2017-01-20/web/packages/tmap/vignettes/tmap-nutshell.html  

###############################################################################################
###############################################################################################
###############################################################################################
###################  SEGUNDA PARTE ########################

#1### VARIOGRAMA. Requiere "library(gstat)" ####
Vario0 <- variogram(Datos$MENS_COTA ~ Datos$X + Datos$Y, locations = DatosEsp, width=200, cutoff=8000, cloud=F) #formula: Variable ~ x + y. Ademas se elijen distancias de separacion a evaluar y corte (maxima distancia evaluada)
plot(Vario0, main = "Semivarianza en funcion de h") # Fig 1. 

Vario0_nube <- variogram(Datos$MENS_COTA ~ Datos$X + Datos$Y, locations = DatosEsp, width=200, cutoff=8000, cloud=T)  #Mostrar la nube de puntos (no solo la media para cada distancia de separaci?n)
plot(Vario0_nube, main = "Semivarianza en funcion de h") # Fig 2. 

#### Para hacer varios variogramas y compararlos, podemos cambiar a mano la 
# distancia de separacion dentro de cada formula. O bien definir una variable 
# antes (para no buscar dentro de la formula), ejemplo "distancia" y "corte" 
# (y luego en la formula referenciar estas variables).

# Reemplace "h?" por un valor de h (distancia) entre puntos que crea conveniente para estos datos  
# Reemplace "Max?" por un valor de distancia maxima que crea convenientre. 
# En general 1/2 de la distancia maxima entre puntos es un buen inicio

Distancia <-  h?
Corte <- Max? 
Vario <- variogram(Datos$MENS_COTA ~ Datos$X + Datos$Y, DatosEsp, width=Distancia, cutoff=Corte)
plot(Vario, xlab="Distancia de separacion (h en mts)", ylab="Semivarianza") # Fig 3. 

### 1 TAREA: 1.1- Comparar varias distancias de separacion y/o maxima distancia a separar. Definir estos parametros.

# 2 ## Ajuste de un modelo al semivariograma ####
# ajuste manual de un modelo esferico: elegir sill, rango, nugget y tipo de modelo a ajustar
Fn.Vario0_Sph <- vgm(psill= ??,range = ??,nugget = ??, model="Sph")
plot(Vario, Fn.Vario0_Sph, main = "Semivariograma ajustado a ojo") # Fig 4. 

# a partir de los valores pre-definidos de sill, rango y nugget, ajustar automaticamente un modelo esferico
library(automap)
Fn.Vario0_Sph_auto = autofitVariogram(Vario, formula = Datos$MENS_COTA ~ Datos$X + Datos$Y, input_data = DatosEsp, model="Sph")

plot(Vario, Fn.Vario0_Sph_auto$var_model) # Fig 5.
Fn.Vario0_Sph_auto$var_model # ver (y anotar) los valores ajustados en el modelo
Fn.Vario0_Sph_auto$exp_var # aca podemos ver todos los datos del semivariograma (distancias evaluadas, cantidad de puntos, semivarianza)

plot(Fn.Vario0_Sph_auto) # (Fig 6) Comparar modelo con ajuste automatico. 
                      # Los numeros indican cuantos puntos se usaron para calcular cada valor, analicelos.

# finalmente, guardamos lo ajustado automaticamente en un modelo de tipo "vgm" (asi lo pide luego la instruccion que realiza el Kriging)
Fn.Vario0_Sph <- vgm(psill= ?,range = ?,nugget = ?, model="Sph")
## se construye la funcion para luego poder interpolar


# 2 TAREA. Discutir:
# 2.1- hay dependencia espacial?, de que orden? (tendencia o autocorrelacion?)

#########################################################################
##### Kriging ###########################################################



# 3 ### Interpolacion por kriging ####
Modelo <- Fn.Vario0_Sph ## Aca debemos elegir cual es el mejor modelo ajustado

Altitud.Kriging <- krige(Datos$MENS_COTA ~ 1, DatosEsp, newdata=Grilla, model= Modelo) # Ordinary Kriging

summary(Altitud.Kriging) # informa: el tamanio de la grilla, la cantidad de celdas, las estadisticas de los valores predichos (interpolados) y las estadisticas de sus varianzas.
## Fig 7
spplot(Altitud.Kriging["var1.pred"], main = "Altitud (m s.n.m.) - Interpolacion por Kriging") 

# se puede limitar la cantidad de vecinos y la distancia maxima
Altitud.Kriging_5 <- krige(Datos$MENS_COTA~1, DatosEsp, newdata=Grilla, model= Modelo, nmax=5, nmin = 1, maxdist = 1000) # Hasta 5 vecinos y 1000m
summary(Altitud.Kriging_5)
## Fig 8
spplot(Altitud.Kriging_5["var1.pred"], main = "Altitud (m s.n.m.) - Interpolacion por Kriging")
str(Altitud.Kriging_5)
names(Altitud.Kriging_5)

#### GUARDAR SALIDAS #####
library(raster)
Altitud.Kriging.r.pred = raster(Altitud.Kriging["var1.pred"])
writeRaster(Altitud.Kriging.r.pred, "kriging_predichos.tif", "GTiff")

Altitud.Kriging.r.var = raster(Altitud.Kriging["var1.var"])
writeRaster(Altitud.Kriging.r.var, "kriging_var.tif", "GTiff")

Altitud.Kriging_5.r.pred = raster(Altitud.Kriging_5["var1.pred"])
writeRaster(Altitud.Kriging_5.r.pred, "kriging_predichos_5.tif", "GTiff")

Altitud.Kriging_5.r.var = raster(Altitud.Kriging_5["var1.var"])
writeRaster(Altitud.Kriging_5.r.var, "kriging_var_5.tif", "GTiff")

## Se animan a plotear estos cuatro rásters en tmap? Si no se anima, puede abrirlos en 
# Seguir en QGIS... Comparar las predicciones de Kriging con y sin limite de vecinos. Analizar la informacion provista por la capa de varianzas. Comparar con las predicciones del modelo de inversa distancia ponderada.


#############################


#4### VARIOGRAMAS EN MULTIPLES DIRECCIONES (para evaluar anisotropia) ####
Distancia <- h? # elegir "h?" segun se ha definido previamente
Corte <- Max? #?dem elegir "Max?"
  
Vario0_ani <- variogram(Datos$MENS_COTA ~ Datos$X + Datos$Y, DatosEsp, width=Distancia, cutoff=Corte, alpha =c(0,45,90,135))

# alpha indica las direcciones: 0 indica direccion Norte-Sur, 90 indica direccion Este-Oeste; 
# los angulos aumentan en sentido horario. 
# En este caso en que evaluamos 4 direcciones,
# la tolerancia por defecto es 22.5? (90/cantidad de direcciones)

plot(Vario0_ani, xlab="Distancia de separacion (m)", ylab="Semivarianza") # Fig 9

# Podemos repetirlo con las dos direcciones extremas
Distancia <- 200 # Cambiar "200" si corresponde 
Corte <- 8000 # Cambiar "8000" si corresponde

# ej, c(45, 135) o (0, 90) (dos direcciones ortogonales)
Vario0_ani <- variogram(Datos$MENS_COTA ~ Datos$X + Datos$Y, DatosEsp, width=Distancia, cutoff=Corte, alpha =c(45, 135)) 


plot(Vario0_ani, xlab="Distancia de separaci?n (m)", ylab="Semivarianza") # Fig 10


Fn.Vario0_an<- vgm(psill= 34,range = 6000,nugget = 0, model="Sph", anis = c(45, 0.20))  # Aqui indicamos la direccion del rango mayor y la proporcion del rango menor
## cual tendria que ser la proporcion del rango menor para un mejor ajuste?
plot(Vario0_ani,Fn.Vario0_an, xlab="Distancia de separacion (m)", ylab="Semivarianza") # Fig 11

# 4 TAREA: Comparar los patrones en distintas direcciones: 
# 4.1- El patron de dependencia espacial (si existe), es isotropico o anisotropico? 
# 4.2- Si hay anisotropia, que cambia en cada direccion? Describir.

#5### Interpolacion por kriging anisotropico ####
Modelo.Aniso <- Fn.Vario0_an ## Aca debemos elegir cual es el mejor modelo (anisotripico en este caso)

Altitud.interpolada.kriging.Aniso <- krige(Datos$MENS_COTA ~ 1, DatosEsp, newdata=Grilla, model= Modelo.Aniso) # Kriging ordinario anisotropico

summary(Altitud.interpolada.kriging.Aniso) # informa: el tamanio de la grilla, la cantidad de celdas, las estadisticas de los valores predichos (interpolados) y las estadisticas de sus varianzas.
## Fig 12
spplot(Altitud.interpolada.kriging.Aniso["var1.pred"], main = "Altitud (m s.n.m.) - Interpolacion por Kriging Anisotropico") 

#### GUARDAR SALIDAS #####

r = raster(Altitud.interpolada.kriging.Aniso["var1.pred"])
writeRaster(r, "kriging_aniso_predichos.tif", "GTiff",overwrite=TRUE)

r = raster(Altitud.interpolada.kriging.Aniso["var1.var"])
writeRaster(r, "kriging_aniso_var.tif", "GTiff")


#### OPTATIVO 1: Si hay tiempo, veremos como quitar la TENDENCIA (si es que se detecta tendencia) #####
#### Se puede realizar un modelo de regresion para quitar el patron de primer orden. Luego se pueden modelar los residuos (que dependencia espacial queda despues de todo lo explicado por el modelo de regresion?; es todo error aleatorio o parte se puede explicar por autocorrelacion?)
modelo.regresion=lm(Datos$MENS_COTA ~ Datos$X + Datos$Y) # es un modelo sencillo de regresion, donde la altitud depende de la posicion en X e Y (mas el error aleatorio y, si hay, autocorrelacion). 
summary(modelo.regresion) # devuelve el modelo ajustado (estimadores de los parametros (ordenada y pendientes asociadas a X e Y, y a la interaccion entre X e Y; p-valores indicando la significancia del efecto de cada factor))

# algunos temas de estadistica: verificar cumplimiento de supuestos; evaluar efecto de la interaccion

Datos$residuos=residuals(modelo.regresion) #Para cada uno de los 500 puntos que tenemos, agregamos los residuos del modelo a la tabla de Datos
head(Datos)

# Ahora repetimos el analisis de semivariogramas, pero usando los residuos 
# (en lugar de MENS_COTA). Es decir lo que no es explicado por la superficie "lm(Datos$MENS_COTA ~ Datos$X + Datos$Y)"

Distancia <- 200
Corte <- 8000
Vario_residuos <- variogram(Datos$residuos ~ Datos$X + Datos$Y, DatosEsp, width=Distancia, cutoff=Corte)

plot(Vario_residuos, xlab="Distancia de separacion (m)", ylab="Semivarianza")

Fn.Vario_residuos_Sph <- vgm(psill= 32,range = 4000,nugget = 0, model="Sph")

plot(Vario_residuos, Fn.Vario_residuos_Sph)

Fn.Vario_residuos_Sph_auto = autofitVariogram(Vario_residuos, formula = Datos$residuos ~ Datos$X + Datos$Y, input_data = DatosEsp, model="Sph")

plot(Vario_residuos, Fn.Vario_residuos_Sph_auto$var_model)
Fn.Vario_residuos_Sph_auto$var_model # ver (y anotar) los valores ajustados en el modelo

# anotar los valores ajustados semi-automaticamente en un modelo de formato "vgm"
Fn.Vario_residuos_Sph <- vgm(psill= 32,range = 4000,nugget = 0, model="Sph") 

### ¿Los valores de sill y rango cambiaron respecto al modelo realizado antes de quitar la tendencia?

plot(Fn.Vario_residuos_Sph_auto) # comparar modelo con ajuste automatico. Indica cuantos puntos se usaron para calcular cada valor observado.


## Interpolacion por Kriging de los residuos (luego hay que sumarle el modelo de la tendencia)

modelo = Fn.Vario_residuos_Sph ## Aca debemos elegir cual es el mejor modelo ajustado

Altitud.kriging.residuos <- krige(Datos$residuos ~ 1, DatosEsp, newdata=grilla, model= modelo) 

str(Altitud.kriging.residuos)

summary(Altitud.kriging.residuos) # informa: el tamanio de la grilla, la cantidad de celdas, las estadisticas de los valores predichos (interpolados) y las estadisticas de sus varianzas.

spplot(Altitud.kriging.residuos["var1.pred"], main = "Residuos de altitud (m s.n.m.) - Interpolacion por Kriging") 

# Comparar con el mapa de predichos de altitud. Ver la escala de los colores!

## Repetimos los graficos en la misma escala de colores
spplot(Altitud.kriging.residuos["var1.pred"], main = "Residuos de altitud (m s.n.m.) - Interpolacion por Kriging", at=c(-10,0,5,10,15,20,25,30,35,40)) 

spplot(Altitud.kriging["var1.pred"], main = "Altitud (m s.n.m.) - Interpolacion por Kriging", at=c(-10,0,5,10,15,20,25,30,35,40)) 


# Guardar salidas
r = raster(Altitud.kriging.residuos["var1.pred"])
writeRaster(r, "kriging_residuos_predichos.tif", "GTiff")

r = raster(Altitud.kriging.residuos["var1.var"])
writeRaster(r, "kriging_residuos_var.tif", "GTiff")


# ademas vamos a guardar un raster con las coordenadas X y otro con las coordenadas Y

library(rgdal)
capa<-readGDAL('kriging_residuos_var.tif') # carga raster de interes. Puede ser cualquiera, ya que todos tienen el mismo tamanio de grilla
capa_X <- capa # raster de coordenadas X (por ahora copiamos la estructura de la grilla)
capa_X$band1<-XY$X # sustitucion de values por los valores de X
capa_Y <- capa # raster de coordenadas Y (por ahora copiamos la estructura de la grilla)
capa_Y$band1<-XY$Y # sustitucion de values por los valores de Y

writeGDAL(capa_X,'coordenadas_X.tif',drivername='GTiff') #raster de longitud
writeGDAL(capa_Y,'coordenadas_Y.tif',drivername='GTiff') #raster de latitud

## En QGIS, con la calculadora raster: 
## Primero producimos un mapa con el modelo de la tendencia, a partir de la ordenada y pendientes ajustados con la regresi?n lineal y usando las capas X e Y reci?n exportadas
## Ordenada + Pendiente_X * coordenadas_X + Pendiente_Y * coordenadas_Y
## Guardamos este mapa como tendencia.tif
## Luego generamos el modelo final de altitud como:
## tendencia.tif + altitud.kriging.residuos.tif
## Guardamos este mapa como (por ejemplo) modelo_tendenciaykriging.tif
## Comparamos con el modelo sin considerar la tendencia (altitud.kriging.tif)

#### FIN de OPTATIVO 2 #####






# Ahora hay que hacer el mapa interpolado considerando que modelamos por un lado la tendencia
# y por otro lado la autocorrelacion (kriging).
# Repita el punto #3, salve la interpolacion y utilice las funciones de algebra de mapas para crear el mapa usando la tendencia global y la interpolacion de los residuos.

#### FIN de OPTATIVO 1 #####


#### OPTATIVO 2 ####
###¿Ahora se ve anisotropia analizando los residuos?
#¿Deberia contruir una interpolacion por kriging anisotropico?
# Repita el punto #4 y #5 para los residuos.

#### FIN de OPTATIVO 2 #####

