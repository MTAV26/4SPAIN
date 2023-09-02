# Limpieza del ambiente
rm(list = ls())
graphics.off()
gc()

# Carga de paquetes necesarios
library(ncdf4)            # Leer archivos netcdf
library(sp)               # Proporciona clases y métodos para datos espaciales
library(maptools)         # Funciones para manipular datos espaciales y geográficos
library(RColorBrewer)     # Esquemas de colores predefinidos y personalizados
library(classInt)         # Ayuda a encontrar intervalos de clase para variables continuas
library(fields)           # Herramientas para interpolación y análisis de datos espaciales
library(s2dverification)  # Utilizado en la verificación de pronósticos y observaciones
library(maps)             # Para trazar mapas y agregar información geográfica
library(pracma)           # Funciones matemáticas prácticas
library(verification)     # Utilizado en la verificación de pronósticos

# Carga de datos mundiales simplificados
data(wrld_simpl)

# Directorios
dir_oss = '~/Chapter3-4SPAIN/Data/'
dir_data = '~/Chapter3-4SPAIN/Data/'
dir_out = '~/Chapter3-4SPAIN/Data/'

# Definición de años y meses
anni = 1981:2017
mesi = rep(1:12, length(anni))

# Carga de datos de observaciones de precipitación
fname <- file.path(dir_oss, 'AEMET_025_spain_1981_2017.nc')
obs.nc <- nc_open(fname)
obs = ncvar_get(obs.nc, "precipitation")
lat = ncvar_get(obs.nc, "latitude")
lat = rev(lat)
lon = ncvar_get(obs.nc, "longitude")
obs[obs == "-32767s"] <- NA
obs = obs[, dim(obs)[2]:1,]

# Creación de máscara de áreas con precipitación positiva
inout <- obs
inout[obs > 0] <- 1
inout = inout[, , 444]

# Ciclo para diferentes datasets
datasets = c('CHIRPS', 'ERA5', 'EOBS', 'AEMET')





for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  
  # Carga y procesamiento de los datos según el dataset
  if (dataset == "CHIRPS") {
    fname <- file.path(dir_data, 'CHIRPS_SPAIN_1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precipitation")
    obs[obs == "-32767s"] <- NA
    
  } else if (dataset == "ERA5") {
    fname <- file.path(dir_data, 'ERA5_SPAIN_1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precipitation")
    obs[obs == "-32767s"] <- NA
    
  } else if (dataset == "EOBS") {
    fname <- file.path(dir_data, 'EOBS_SPAIN_1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precipitation")
    obs[obs == "-32767s"] <- NA
    
  } else if (dataset == "AEMET") {
    fname <- file.path(dir_oss, 'AEMET_025_spain_1981_2017.nc')
    obs.nc <- nc_open(fname)
    obs = ncvar_get(obs.nc, "precipitation")
    lat = ncvar_get(obs.nc, "latitude")
    lat = rev(lat)
    lon = ncvar_get(obs.nc, "longitude")
    obs[obs == "-32767s"] <- NA
    obs = obs[, dim(obs)[2]:1,]
    
  } else {
    print('dataset not known')
  }
  
  # Filtrado de los años 1981-2017
  obs = obs[, , 1:444]
  
  # Normalización de los datos y cálculo de SPI6
  for (i in 1:dim(obs)[3]) {
    obs[, , i] = inout * obs[, , i]
  }
  
  aux = apply(obs, c(1, 2), mean, na.rm = TRUE)
  
  for (i in 1:dim(obs)[3]) {
    obs[, , i] = aux / aux * obs[, , i]
  }
  
  spi6 = obs * NA
  
  for (i in 1:length(lon)) {
    print(paste0('grid ', i, ' of ', length(lon)))
    for (j in 1:length(lat)) {
      if (!is.na(aux[i, j] / aux[i, j])) {
        dum <- spi(obs[i, j, ], 6, na.rm = TRUE)
        spi6[i, j, ] = dum$fitted
        rm(dum)
      }
    }
  }
  
  spi6[is.infinite(spi6)] <- NA
  spi6[is.na(spi6)] <- NA
  
  # Guardar resultados
  save(spi6, file = file.path(dir_out, paste0("SPI6_", dataset, "_1981_2017.RData")))
}

# Guardar latitudes, longitudes y máscara de precipitación
save(lon, file = file.path(dir_out, paste0("lon_ESP_1981_2017.RData")))
save(lat, file = file.path(dir_out, paste0("lat_ESP_1981_2017.RData")))
save(inout, file = file.path(dir_out, paste0("inout.RData")))