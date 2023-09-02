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

dir_drop <- '~/Chapter3-4SPAIN/Data/'
dir_4drop <- '~/Chapter3-4SPAIN/Data/'
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

# Definición de datasets y colores
datasets <- c('CHIRPS', 'EOBS', 'ERA5')


brk_cor <- seq(0, 1, 0.1)
col_cor <- (colorRampPalette(brewer.pal(11, "YlOrBr"))(10))

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

# Definir datasets
datasets <- c("ERA5")

# Bucle para diferentes escalas de tiempo
for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  # Bucle para diferentes datasets
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    # Cargar los datos de MAE
    load(file.path(dir_out2, paste("MAE_OBS_spi", sc, "_", sprintf("%02d", start_date), "_", dataset, ".RData", sep = "")))
    
    # Crear un archivo PDF para el mapa
    pdf(paste(dir_out2, "MAE_OBS_", dataset, ".pdf", sep = ""), width = 13.5, height = 9)
    
    tit <- paste('MAE (', month.name[start_date],
                 " = ", (round(mean(corre, na.rm = TRUE), 2)), ") ", dataset, " against AEMET", sep = ""
    )
    
    # Configuración de la pantalla
    set.panel()
    split.screen(rbind(c(0, .9, 0, 1), c(.9, 1, 0, 1)))
    split.screen(c(1, 1), screen = 1) -> ind
    screen(ind[1])
    par(oma = c(0.8, 1, 3, 0.2), mar = c(0.8, 1, 3, 0.2))
    
    # Crear el mapa base
    mapPlot(coastlineWorld, col = "lightgray",
            projection = "+proj=robin",
            longitudelim = c(-9, 3),
            latitudelim = c(35, 44.5),
            drawBox = TRUE, main = tit, cex.main = 3,
            line = 1, adj = 0.5)
    
    # Agregar capas al mapa
    mapImage(lon, lat, corre, col = col_cor, breaks = brk_cor)
    plot(ocean, col = "lightblue", add = TRUE)
    plot(argelia, col = "gray", add = TRUE)
    plot(morroco, col = "gray", add = TRUE)
    plot(francia, add = TRUE, col = "gray")
    plot(portugal, add = TRUE, col = "gray")
    
    mapGrid(dlongitude = 5, dlatitude = 5, lty = 1, lwd = 1, col = "black")
    
    c2 <- range(0, 1, 0.1)
    
    # Crear el gráfico de colores
    split.screen(c(1, 1), screen = 2) -> ind
    screen(ind[1])
    image.plot(zlim = c2,
               legend.only = TRUE,
               smallplot = c(.01, .5, .02, .89),
               col = col_cor,
               breaks = brk_cor,
               horizontal = FALSE)
    
    close.screen(all = TRUE)
    dev.off()
  }
}
