
# Limpieza del ambiente de trabajo previo
rm(list = ls())
graphics.off()
gc()

# Carga de paquetes necesarios
library(sp)               # Proporciona clases y métodos para datos espaciales
library(maptools)         # Funciones para manipular datos espaciales y geográficos
library(RColorBrewer)     # Esquemas de colores predefinidos y personalizados
library(classInt)         # Ayuda a encontrar intervalos de clase para variables continuas
library(fields)           # Herramientas para interpolación y análisis de datos espaciales
library(s2dverification)  # Utilizado en la verificación de pronósticos y observaciones
library(maps)             # Para trazar mapas y agregar información geográfica
library(pracma)           # Funciones matemáticas prácticas
library(verification)     # Utilizado en la verificación de pronósticos
library(psych)            # Funciones para el análisis psicométrico y estadístico
library(sf)               # Clases y funciones para datos espaciales
library(oce)              # Análisis y visualización de datos oceanográficos
library(raster)           # Clases y métodos para manejo de datos raster
library(ggplot2)          # Crear gráficos en capas
library(terra)            # Funcionalidades para análisis espacial
library(graticule)        # Agregar líneas graticulares a gráficos de mapas
library(rgdal)            # Lectura y escritura de datos geoespaciales
library(rworldmap)        # Funciones para trazar mapas mundiales
library(rnaturalearth)    # Proporciona datos geoespaciales naturales del mundo

# Carga de scripts externos
source("~/script/Common/CorrMIO.R")
source("~/script/Common/ColorBarM.R")
source("~/script/Common/mioplot_global.R")

data(coastlineWorld)

dir_drop <- '~/Chapter3-4SPAIN/Data/'
dir_4drop<- '~/Chapter3-4SPAIN/Data/'
dir_out2 <- '~/Chapter3-4SPAIN/Figures/'

# Carga de coordenadas
load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
lonGPCP <- lon
latGPCP <- lat
ni = length(lon)
nj = length(lat)
## select case study
ilon = lon
ilat = lat
# Parámetros fijos
time_scale <- c(6)

anni <- 1981:2017
mesi <- rep(1:12, length(anni))
mesi_start <- which(mesi == 1)

datasets = c( 'ERA5')
timelead =c(4)

mesi_8 = which(mesi == 9)
anno_for = which(anni == 2017)
anno_case = mesi_8[anno_for]

# Definir proyección y cargar mapa mundial
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
worldmap <- rworldmap::getMap(resolution = "coarse")
worldmap <- sp::spTransform(worldmap, crs)

# Definir coordenadas latitud/longitud y etiquetas para graticules
lati <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135, -90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lati, xline = -180, yline = 90, proj = crs)
lines <- graticule::graticule(lons = long, lats = lati, proj = crs)

# Cargar y transformar máscaras geográficas
ocean <- sf::st_transform(read_sf('~/Chapter3-4SPAIN/Data/mascaras/ne_10m_ocean.shp'), crs = crs)
paises <- read_sf('~/Chapter3-4SPAIN/Data/mascaras/ne_10m_admin_0_countries.shp')
Morroco <- sf::st_transform(paises[27, 1], crs = crs)
Argelia <- sf::st_transform(paises[122, 1], crs = crs)
shapename <- read_sf('~/Chapter3-4SPAIN/Data/mascaras/Europa.shp')
Francia <- sf::st_transform(shapename[11, 1], crs = crs)
Portugal <- sf::st_transform(shapename[32, 1], crs = crs)
rm(Argelia, Francia, Morroco, Portugal, paises, shapename)



for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    for (tlead in 1:length(timelead)) {
      tmld = timelead[tlead]
      
      ## load dati
      load(file.path( dir_drop, paste("SPI6ESP_",tmld,"M_",dataset,".RData", sep = "") ))
      prob = spi6pred
      
      spei6obs = prob[,, anno_for,]
      obs_drought = array(data = NA, dim = c(dim(spei6obs)[1], dim(spei6obs)[2]))
      
      for (i in 1:ni) {
        for (j in 1:nj) {
          obs_drought[i,j]=sum(spei6obs[i,j,]<=-0.8,na.rm=TRUE)/dim(spei6obs)[3];
        }
      }
      
      
      obs = apply(spi6pred, c(1,2,3), mean, na.rm=TRUE)
      dim(obs)
      spei6obs = obs[,, anno_for]
      
    }
  }
}

###########################################################################
# SPI PREDICHO
###########################################################################
spiobs = spei6obs
spi_obs = spiobs*NA
spi_obs[(spiobs >= 2)] = 1
spi_obs[(spiobs < 2) & (spiobs >= 1.6)] = 2
spi_obs[(spiobs < 1.6) & (spiobs >= 1.3)] = 3
spi_obs[(spiobs < 1.3) & (spiobs >= 0.8)] = 4
spi_obs[(spiobs < 0.8) & (spiobs >= 0.5)] = 5
spi_obs[(spiobs < 0.5) & (spiobs > -0.5)] = 6
spi_obs[(spiobs <= -0.5) & (spiobs > -0.8)] = 7
spi_obs[(spiobs <= -0.8) & (spiobs > -1.3)] = 8
spi_obs[(spiobs <= -1.3) & (spiobs > -1.6)] = 9
spi_obs[(spiobs <= -1.6) & (spiobs > -2)] = 10
spi_obs[(spiobs <= -2)] = 11


brk_new  <- seq(0, 11, length.out = 12)
col_s  <- rev(brewer.pal(11, "BrBG"))

tit1 <- paste("Standardized index, September 2017\n Forecast for ", dataset, "; lead time: ",tmld, " months", sep = "")
pdf(paste(dir_out2,"CASO_ESTUDIO_SPI_PRE",sc,
"_", tmld, "M_",dataset,".pdf",sep = ""),
width=12, height=9)

set.panel() 
split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))
split.screen(c(1,1), screen=1)-> ind
zr<- range(-1,1)
screen( ind[1])

par(oma=c(1.5,1,3,1.7),mar=c(1.5,1,3,1.7))
mapPlot(coastlineWorld, col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-9,3),
        latitudelim=c(35, 44.5),
        drawBox = TRUE, main = tit1, cex.main = 1.5,
        line = 0.5, adj = 0.5)

mapImage(lon, lat, spi_obs, col=col_s, breaks = brk_new)
plot(ocean, col = "lightblue", add =TRUE)
plot(argelia, col = "gray", add =TRUE)
plot(morroco, col = "gray", add =TRUE)
plot(francia, add =TRUE, col= "gray")
plot(portugal, add =TRUE, col="gray")
mapGrid(dlongitude = 5, dlatitude = 5, lty = 1,lwd = 1, col="black")

c2<- range(1,11)
screen(2)   
image.plot(
  zlim=c2,
  legend.only=TRUE, 
  smallplot=c(.1,.3, .03,.9),
  col=col_s,
  horizontal = FALSE,
  axis.args = list(at = seq(1:11),
                   labels=c(" > 2", 
                            "1.6 to 2",
                            "1.3 to 1.6",
                            "0.8 to 1.3",
                            "0.5 to 0.8",
                            " -0.5 to  0.5",
                            " -0.5 to -0.8",
                            " -0.8 to -1.3",
                            " -1.3 to -1.6",
                            " -1.6 to -2", 
                            " < -2"))
  
)

close.screen( all=TRUE)
dev.off()

###########################################################################
# PROBABILIDAD DE SEQUÍA MODERADA PREDICHA
###########################################################################
tit2 <- paste("Probability drought, September 2017\n Forecast for ",
              dataset, "; lead time: ",tmld, " months", sep = "")
brk_prob <- c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1)
col_prob  <- brewer.pal(6, "YlOrBr")
col_prob[1]="white"

pdf(paste(dir_out2,"CASO_ESTUDIO_SPI_PRE_PROB_",sc,
          "_", tmld, "M_",dataset,".pdf",sep = ""),
    width=12, height=9)

set.panel() 
split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))
split.screen(c(1,1), screen=1)-> ind
zr<- range(-1,1)
screen( ind[1])
par(oma=c(1.5,1,3,1.7),mar=c(1.5,1,3,1.7))
mapPlot(coastlineWorld, col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-9,3),
        latitudelim=c(35, 44.5),
        drawBox = TRUE, main = tit2, cex.main = 1.5,
        line = 0.5, adj = 0.5)

mapImage(lon, lat, obs_drought, col=col_prob, breaks =brk_prob)
plot(ocean, col = "lightblue", add =TRUE)
plot(argelia, col = "gray", add =TRUE)
plot(morroco, col = "gray", add =TRUE)
plot(francia, add =TRUE, col= "gray")
plot(portugal, add =TRUE, col="gray")
mapGrid(dlongitude = 5, dlatitude = 5, lty = 1,lwd = 1, col="black")


c2<- range(0,1)
screen(2)   
image.plot(
  zlim=c2,
  legend.only=TRUE, 
  smallplot=c(.1,.3, .03,.9),
  col=col_prob,
  breaks = brk_prob,
  horizontal = FALSE
)


close.screen( all=TRUE)
dev.off()



###########################################################################
# SPI OBSEERVADO
###########################################################################

load(file.path( dir_drop, paste("SPI6_AEMET_1981_2017.RData", sep = "") ))
spei6obs = spi6

image.plot(lon, lat, spei6obs)
spiobs = spei6obs
spi_obs = spiobs*NA
spi_obs[(spiobs >= 2)] = 1
spi_obs[(spiobs < 2) & (spiobs >= 1.6)] = 2
spi_obs[(spiobs < 1.6) & (spiobs >= 1.3)] = 3
spi_obs[(spiobs < 1.3) & (spiobs >= 0.8)] = 4
spi_obs[(spiobs < 0.8) & (spiobs >= 0.5)] = 5
spi_obs[(spiobs < 0.5) & (spiobs > -0.5)] = 6
spi_obs[(spiobs <= -0.5) & (spiobs > -0.8)] = 7
spi_obs[(spiobs <= -0.8) & (spiobs > -1.3)] = 8
spi_obs[(spiobs <= -1.3) & (spiobs > -1.6)] = 9
spi_obs[(spiobs <= -1.6) & (spiobs > -2)] = 10
spi_obs[(spiobs <= -2)] = 11


image.plot(lon, lat, spi_obs)

close.screen( all=TRUE)
dev.off()




brk_new  <- seq(0, 11, length.out = 12)
col_s  <- rev(brewer.pal(11, "BrBG"))

tit1 <-  ("Standardized index, September 2017\n Observed AEMET")
pdf(paste(dir_out2,"CASO_ESTUDIO_SPI_OBS.pdf",sep = ""),
    width=12, height=9)

set.panel() 
split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))
split.screen(c(1,1), screen=1)-> ind
zr<- range(-1,1)
screen( ind[1])

par(oma=c(1.5,1,3,1.7),mar=c(1.5,1,3,1.7))
mapPlot(coastlineWorld, col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-9,3),
        latitudelim=c(35, 44.5),
        drawBox = TRUE, main = tit1, cex.main = 1.5,
        line = 0.5, adj = 0.5)

mapImage(lon, lat, spi_obs[,,anno_case], col=col_s, breaks = brk_new)
plot(ocean, col = "lightblue", add =TRUE)
plot(argelia, col = "gray", add =TRUE)
plot(morroco, col = "gray", add =TRUE)
plot(francia, add =TRUE, col= "gray")
plot(portugal, add =TRUE, col="gray")
mapGrid(dlongitude = 5, dlatitude = 5, lty = 1,lwd = 1, col="black")

c2<- range(1,11)
screen(2)   
image.plot(
  zlim=c2,
  legend.only=TRUE, 
  smallplot=c(.1,.3, .03,.9),
  col=col_s,
  horizontal = FALSE,
  axis.args = list(at = seq(1:11),
                   labels=c(" > 2", 
                            "1.6 to 2",
                            "1.3 to 1.6",
                            "0.8 to 1.3",
                            "0.5 to 0.8",
                            " -0.5 to  0.5",
                            " -0.5 to -0.8",
                            " -0.8 to -1.3",
                            " -1.3 to -1.6",
                            " -1.6 to -2", 
                            " < -2"))
  
)

close.screen( all=TRUE)
dev.off()

###########################################################################
# CONDICIONES DE SPI
###########################################################################

load(file.path( dir_drop, paste("SPI6_AEMET_1981_2017.RData", sep = "") ))
spei6obs = spi6

image.plot(lon, lat, spei6obs[,,anno_case])
obs_drought = spei6obs*NA
obs_drought[(spei6obs > -0.5)] = 0
obs_drought[(spei6obs <= -0.5) & (spei6obs > -0.8)] = 1
obs_drought[(spei6obs <= -0.8) & (spei6obs > -1.3)] = 2
obs_drought[(spei6obs <= -1.3) & (spei6obs > -1.6)] = 3
obs_drought[(spei6obs <= -1.6) & (spei6obs > -2)] = 4
obs_drought[(spei6obs <= -2)] = 5

dim(obs_drought)
# image.plot(lon, lat, spi_obs)

close.screen( all=TRUE)
dev.off()

brk <- seq(-1, 5, length.out = 7)
pal.1 = colorRampPalette(c("yellow", "red", "black"), space = "rgb")
col = pal.1(length(brk) - 1)
col[1] = "white"
col_s = col

# brk_new  <- seq(0, 11, length.out = 12)
# col_s  <- rev(brewer.pal(11, "BrBG"))

tit1 <- paste("Drought condition, September 2017\n Observed AEMET", sep = "")
pdf(paste(dir_out2,"CASO_ESTUDIO_SPI_OBS_condition.pdf",sep = ""),
    width=12, height=9)

set.panel() 
split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))
split.screen(c(1,1), screen=1)-> ind
zr<- range(-1,1)
screen( ind[1])

par(oma=c(1.5,1,3,1.7),mar=c(1.5,1,3,1.7))
mapPlot(coastlineWorld, col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-9,3),
        latitudelim=c(35, 44.5),
        drawBox = TRUE, main = tit1, cex.main = 1.5,
        line = 0.5, adj = 0.5)

mapImage(lon, lat, obs_drought[,,anno_case], col=col_s, breaks = brk)
plot(ocean, col = "lightblue", add =TRUE)
plot(argelia, col = "gray", add =TRUE)
plot(morroco, col = "gray", add =TRUE)
plot(francia, add =TRUE, col= "gray")
plot(portugal, add =TRUE, col="gray")
mapGrid(dlongitude = 5, dlatitude = 5, lty = 1,lwd = 1, col="black")

screen(2)   
image.plot(
  zlim=brk,
  # zlim=seq(-2,2,length.out=11),
  # zlim= c(-2.20, -2,-1.6,-0.8,-0.5, 0, 0.5, 0.8, 1.3, 2, 2.20),
  legend.only=TRUE, 
  smallplot=c(.1,.3, .03,.9),
  col=col_s,
  breaks = brk,
  horizontal = FALSE,
  # labels=c( "","0.2", "0.4", "0.6", "0.8", "1"),
  axis.args = list(at = brk,
                   labels=c("","","", "","","",""))
)


close.screen( all=TRUE)
dev.off()

