
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

# Parámetros fijos
time_scale <- c(6)

anni <- 1981:2017
mesi <- rep(1:12, length(anni))
mesi_start <- which(mesi == 1)

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

# Definición de los datasets a analizar
datasets = c('cor', 'mae', 'bias')
# Definición de los meses a analizar
meses = c('02', '05', '08', '11')

# Crear un PDF para guardar los gráficos
pdf(paste(dir_out2, "Verification_ESP_spi6_4M.pdf", sep = ""), width = 9.3, height = 11.5)

# Configurar paneles y pantalla dividida
set.panel()
split.screen(rbind(c(0, 1, .1, 1), c(0, 1, 0, .1)))
split.screen(c(4, 3), screen = 1) -> ind
zr <- range(0, 1)

# Bucle a través de los meses y los datasets
for (imes in 1:length(meses)) {
  mes = meses[imes]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    print(paste(dataset, "=", mes))
    
    if (dataset == "cor") {
      # Cargar los datos de correlación
      load(file.path(dir_out2, paste(dataset, "_esp/", dataset, "_ESP_spi6_", mes, "_4M_ERA5_original.Rdata", sep = "")))
      load(file.path(dir_out2, paste(dataset, "_esp/PVALUE_ESP_spi6_", mes, "_4M_ERA5_original.RData", sep = "")))
      pvalue1 = pvalue_adj
      pvalue1[pvalue1 <= 0.05] = NA
      pvalue1[pvalue1 >= 0.05] = -1
      pvalue1[is.infinite(pvalue1)] = NA
      pvalue1[is.na(pvalue1)] = NA
      colns <- c("#FFE4C4")
      rm(pvalue_adj)
    } else {
      # Cargar otros datasets
      load(file.path(dir_out2, paste(dataset, "_esp/", dataset, "_ESP_spi6_", mes, "_4M_ERA5.Rdata", sep = "")))
    }
    
    corre1 = corre
    
    # Asignar nombre del mes y predicción basado en el índice
    if (meses[imes] == "02") {
      name_mes = 2
    } else if (meses[imes] == "05") {
      name_mes = 5
    } else if (meses[imes] == "08") {
      name_mes = 8
    } else if (meses[imes] == "11") {
      name_mes = 11
    }
    
    # Calcular el número de interacción para dividir la pantalla
    interaction_number <- (imes - 1) * 3 + idata
    screen(ind[interaction_number])
    par(oma = c(0.8, 1, 3, 0.4), mar = c(0.8, 1, 3, 0.4))
    
    # Asignar el panel correspondiente
    if (interaction_number == 1) {
      panel = "a"
    } else if (interaction_number == 2) {
      panel = "b"
    } else if (interaction_number == 3) {
      panel = "c"
    } else if (interaction_number == 4) {
      panel = "d"
    } else if (interaction_number == 5) {
      panel = "e"
    } else if (interaction_number == 6) {
      panel = "f"
    } else if (interaction_number == 7) {
      panel = "g"
    } else if (interaction_number == 8) {
      panel = "h"
    } else if (interaction_number == 9) {
      panel = "i"
    } else if (interaction_number == 10) {
      panel = "j"
    } else if (interaction_number == 11) {
      panel = "k"
    } else if (interaction_number == 12) {
      panel = "l"
    }
    
    # Definir colores y paletas según el dataset
    if (datasets[idata] == "cor") {
      name_dat = "COR"
      brk_cor <- seq(-1, 1, 0.2)
      col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))
      zz <- range(-1, 1, 0.2)
    } else if (datasets[idata] == "mae") {
      name_dat = "MAE"
      brk_cor <- seq(0, 1, 0.1)
      col_cor <- (colorRampPalette(brewer.pal(11, "YlOrBr"))(10))
      zz <- range(0, 1, 0.1)
    } else if (datasets[idata] == "bias") {
      name_dat = "BIAS"
      brk_cor <- seq(-0.1, 0.1, 0.02)
      col_bias <- colorRampPalette(c("dark green", "green", "white", "blue", "dark blue"))(10)
      zz <- c(-0.1, 0.1)
    }
    
    # Asignar el nombre del mes de predicción
    if (meses[imes] == "02") {
      name_pred = 11
    } else if (meses[imes] == "05") {
      name_pred = 2
    } else if (meses[imes] == "08") {
      name_pred = 5
    } else if (meses[imes] == "11") {
      name_pred = 8
    }
    
    # Crear el mapa y gráfico
    mapPlot(coastlineWorld, col = "lightgray",
            projection = "+proj=robin",
            longitudelim = c(-9, 3),
            latitudelim = c(35, 44.5),
            drawBox = TRUE,
            main = paste0(panel, ') ', name_dat, ' for SPI6 in ERA5 (ESP) against AEMET \n Start date: ',
                          month.name[name_pred], '; Forecast date: ', month.name[name_mes], sep = ""),
            cex.main = 0.7, line = 0.6, adj = 0.5)
    
    mapImage(lon, lat, corre1, col = col_cor, breaks = brk_cor)
    
    if (datasets[idata] == "cor") {
      mapImage(lon, lat, pvalue1, col = colns)
    }
    plot(ocean, col = "lightblue", add = TRUE)
    plot(argelia, col = "gray", add = TRUE)
    plot(morroco, col = "gray", add = TRUE)
    plot(francia, add = TRUE, col = "gray")
    plot(portugal, add = TRUE, col = "gray")
    mapGrid(dlongitude = 5, dlatitude = 5, lty = 1, lwd = 1, col = "black")
    
  }
}

# Crear gráficos adicionales y cerrar pantalla dividida
split.screen(c(1, 4), screen = 2) -> ind2

screen(ind2[1])
zr <- range(0, 1)
nombres_paleta <- c("No significative", "No data")
image.plot(zlim = zr, legend.only = TRUE, smallplot = c(0, .95, .45, .7), col = c("#FFE4C4", "gray"), 
           horizontal = TRUE, axis.args = list(at = c(zr, zr[length(zr)] + diff(zr)[1]),
                                               labels = c(nombres_paleta, 'No model')))

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

screen(ind2[2])
c2 <- range(-1, 1)
image.plot(zlim = c2,
           legend.only = TRUE,
           smallplot = c(0, .90, .45, .7),
           col = col_cor,
           breaks = brk_cor,
           cex.legend = 0.7,
           horizontal = T)

brk_mae <- seq(0, 1, 0.1)
col_mae <- (colorRampPalette(brewer.pal(11, "YlOrBr"))(10))

screen(ind2[3])
c2 <- range(-1, 1)
image.plot(zlim = c2,
           legend.only = TRUE,
           smallplot = c(0, .90, .45, .7),
           col = col_mae,
           breaks = brk_mae,
           cex.legend = 0.7,
           horizontal = T)

brk_bias <- seq(-0.1, 0.1, 0.02)
col_bias <- colorRampPalette(c("dark green", "green", "white", "blue", "dark blue"))(10)
screen(ind2[4])
c2 <- range(-0.1, 0.1)
image.plot(zlim = c2,
           legend.only = TRUE,
           smallplot = c(.05, .95, .45, .7),
           col = col_bias,
           breaks = brk_bias,
           cex.legend = 0.7,
           horizontal = T)

# Cerrar pantallas divididas y finalizar PDF
close.screen(all = TRUE)
dev.off()
