
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

# Carga de scripts externos
source("~/script/Common/CorrMIO.R")
source("~/script/Common/ColorBarM.R")
source("~/script/Common/mioplot_global.R")

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

# Ciclo para cada valor en 'time_scale'
for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  # Ciclo para cada dataset en 'datasets'
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    start_dates = c(1,2,3,4,5,6,7,8,9,10,11,12)
    
    # Ciclo para cada 'start_date' en 'start_dates'
    for (idate in 1:length(start_dates)) {
      start_date = start_dates[idate]
      
      # Definición de 'mesi_8' basado en 'start_date'
      if (start_date == '1') {
        mesi_8 = which(mesi == 01)
      } else if (start_date == '2') {
        mesi_8 = which(mesi == 02)
      } else if (start_date == '3') {
        mesi_8 = which(mesi == 03)
      } else if (start_date == '4') {
        mesi_8 = which(mesi == 04)
      } else if (start_date == '5') {
        mesi_8 = which(mesi == 05)
      } else if (start_date == '6') {
        mesi_8 = which(mesi == 06)
      } else if (start_date == '7') {
        mesi_8 = which(mesi == 07)
      } else if (start_date == '8') {
        mesi_8 = which(mesi == 08)
      } else if (start_date == '9') {
        mesi_8 = which(mesi == 09)
      } else if (start_date == '10') {
        mesi_8 = which(mesi == 10)
      } else if (start_date == '11') {
        mesi_8 = which(mesi == 11)
      } else if (start_date == '12') {
        mesi_8 = which(mesi == 12)
      }
      
      # Carga de datos 'spi6' y 'data'
      nam <- paste("spi6", sep = "")
      load(file.path(dir_drop, paste("SPI", sc, "_AEMET_1981_2017.RData", sep = "")))
      ens = spi6[,,mesi_8]
      ens[is.infinite(ens)] <- NA
      ens[is.na(ens)] <- NA
      
      nam <- paste("spi", sc, sep = "")
      print(nam)
      
      # Carga de datos según 'dataset'
      if (dataset == "EOBS") {
        load(file.path(dir_drop, paste('SPI', sc, "_EOBS_1981_2017.RData", sep = "")))
      } else if (dataset == "CHIRPS") {
        load(file.path(dir_drop, paste('SPI', sc, "_CHIRPS_1981_2017.RData", sep = "")))
      } else if (dataset == "ERA5") {
        load(file.path(dir_drop, paste('SPI', sc, "_ERA5_1981_2017.RData", sep = "")))
      } else {
        print('dataset not known')
      }
      
      data = spi6[,,mesi_8]
      data[is.infinite(data)] <- NA
      data[is.na(data)] <- NA
      
      ni = dim(data)[1]
      nj = dim(data)[2]
      nt = dim(data)[3]
      
      corre <- matrix(data = NA, nrow = ni, ncol = nj)
      pvalue <- matrix(data = NA, nrow = ni, ncol = nj)
      
      # Ciclo para calcular correlaciones y p-values
      for (i in 1:ni) {
        for (j in 1:nj) {
          # Filtra valores válidos en 'ens' y 'data'
          OK <- complete.cases(ens[i, j,], data[i, j,])
          x <- ens[i, j, OK]
          y <- data[i, j, OK]
          n <- length(x)
          
          # Calcula correlaciones y p-values si se cumplen condiciones
          if (n >= nt * 0.9) {
            dum = CorrMIO((y), (x), method = 'pearson', pval = TRUE)
            corre[i, j] = as.numeric(dum)[1]
            pvalue[i, j] <- as.numeric(dum)[4]
            rm(dum)
          }
          
          rm(OK, n, x, y)
        }
      }
      pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
      pvalue_adj = matrix(pvalue_adj, nrow = ni, ncol = nj)
      
      # Guarda resultados
      save(corre, file = file.path(dir_out2, paste("COR_OBS_spi", sc, "_", sprintf("%02d", mesi_8[1]), "_", dataset, ".RData", sep = "")))
      save(pvalue_adj, file = file.path(dir_out2, paste("PVALUE_OBS_spi", sc, "_", sprintf("%02d", mesi_8[1]), "_", dataset, ".RData", sep = "")))
    }
  }
}

