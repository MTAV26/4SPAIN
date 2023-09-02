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


dir_drop <- '~/Chapter3-4SPAIN/Data/'
dir_4drop <- '~/Chapter3-4SPAIN/Data/'
dir_out2 <- '~/Chapter3-4SPAIN/Data/'

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

# Definición de datasets
datasets <- c('CHIRPS', 'EOBS', 'ERA5')

# Ciclo para diferentes escalas de tiempo
for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  # Ciclo para diferentes datasets
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    # Definición de fechas de inicio
    start_dates = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    
    # Ciclo para diferentes fechas de inicio
    for (idate in 1:length(start_dates)) {
      start_date = start_dates[idate]
      
      # Asignación de meses correspondientes a la fecha de inicio
      if (start_date == 1) {
        mesi_8 = which(mesi == 1)
      } else if (start_date == 2) {
        mesi_8 = which(mesi == 2)
      } else if (start_date == 3) {
        mesi_8 = which(mesi == 3)
      } else if (start_date == 4) {
        mesi_8 = which(mesi == 4)
      } else if (start_date == 5) {
        mesi_8 = which(mesi == 5)
      } else if (start_date == 6) {
        mesi_8 = which(mesi == 6)
      } else if (start_date == 7) {
        mesi_8 = which(mesi == 7)
      } else if (start_date == 8) {
        mesi_8 = which(mesi == 8)
      } else if (start_date == 9) {
        mesi_8 = which(mesi == 9)
      } else if (start_date == 10) {
        mesi_8 = which(mesi == 10)
      } else if (start_date == 11) {
        mesi_8 = which(mesi == 11)
      } else if (start_date == 12) {
        mesi_8 = which(mesi == 12)
      }
      
      # Cargar datos SPI6 para AEMET y el dataset actual
      nam <- paste("spi6", sep = "")
      load(file.path(dir_drop, paste("SPI", sc, "_AEMET_1981_2017.RData", sep = "")))
      ens = spi6[,,mesi_8]
      ens[is.infinite(ens)] = NA
      ens[is.na(ens)] = NA
      
      nam <- paste("spi", sc, sep = "")
      print(nam)
      
      # Cargar datos específicos para el dataset actual
      if (dataset == "EOBS") {
        load(file.path(dir_drop, paste('SPI', sc, "_EOBS_1981_2017.RData", sep = "")))
      } else if (dataset == "CHIRPS") {
        load(file.path(dir_drop, paste('SPI', sc, "_CHIRPS_1981_2017.RData", sep = "")))
      } else if (dataset == "ERA5") {
        load(file.path(dir_drop, paste('SPI', sc, "_ERA5_1981_2017.RData", sep = "")))
      } else {
        print('dataset not known')
      }
      
      # Filtrado y procesamiento de los datos
      data = spi6[,,mesi_8]
      data[is.infinite(data)] = NA
      data[is.na(data)] = NA
      
      ni = dim(data)[1]
      nj = dim(data)[2]
      nt = dim(data)[3]
      
      corre <- matrix(data = NA, nrow = ni, ncol = nj)
      
      data[is.infinite(data)] = NA
      
      for (i in 1:ni) {
        for (j in 1:nj) {
          # Verificación de datos completos
          OK <- complete.cases(ens[i, j,], data[i, j,])
          x <- ens[i, j, OK]
          y <- data[i, j, OK]
          n <- length(x)
          
          # Cálculo de MAE solo si los datos son suficientes
          if (n >= nt * 0.9) {
            corre[i, j] = mae(x, y)
          }
          
          rm(OK, n, x, y)
        }
      }
      
      # Guardar resultados
      save(corre, file = file.path(dir_out2, paste("MAE_OBS_spi", sc, "_", sprintf("%02d", start_date), "_", dataset, ".RData", sep = "")))
    }
  }
}
