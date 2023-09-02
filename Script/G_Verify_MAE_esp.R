
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
datasets = c( 'ERA5')
start_dates = c(2,3,4)

# Bucle para cada escala de tiempo en la variable time_scale
for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  # Bucle para cada conjunto de datos en la variable datasets
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    # Bucle para cada fecha de inicio en la variable start_dates
    for (istart_date in 1:length(start_dates)) {
      start_date = start_dates[istart_date]
      
      # Definición de fechas de pronóstico
      forecast_dates = c(1,2,3,4,5,6,7,8,9,10,11,12)
      
      # Bucle para cada fecha de pronóstico en forecast_dates
      for (ifdate in 1:length(forecast_dates)) {
        f_date = forecast_dates[ifdate]
        
        # Asignación del mes correspondiente a mesi_8 según la fecha de pronóstico
        if (f_date == '1') {
          mesi_8 = which(mesi== 01)
        } else if (f_date == '2') {
          mesi_8 = which(mesi == 02)
        } else if (f_date == '3') {
          mesi_8 = which(mesi == 03)
        } else if (f_date == '4') {
          mesi_8 = which(mesi == 04)
        } else if (f_date == '5') {
          mesi_8 = which(mesi == 05)
        } else if (f_date == '6') {
          mesi_8 = which(mesi == 06)
        } else if (f_date == '7') {
          mesi_8 = which(mesi == 07)
        }else if (f_date == '8') {
          mesi_8 = which(mesi == 08)
        } else if (f_date == '9') {
          mesi_8 = which(mesi == 09)
        } else if (f_date == '10') {
          mesi_8 = which(mesi == 10)
        } else if (f_date == '11') {
          mesi_8 = which(mesi == 11)
        } else if (f_date == '12') {
          mesi_8 = which(mesi == 12)
        }
        
        # Carga de pronósticos de SPI6 y limpieza
        nam <- paste("spi6", sep = "")
        load(file.path(dir_drop,paste("SPI", sc, "_AEMET_1981_2017.RData", sep = "")))
        ens = spi6[,,mesi_8]
        ens[is.infinite(ens)]=NA
        ens[is.na(ens)]=NA
        
        # Nombre de la variable para el pronóstico de SPI
        nam <- paste("spi",sc,"pred", sep = "")
        print(nam)
        
        # Carga de datos según el conjunto de datos
        if (dataset == "EOBS") {
          load(file.path(dir_4drop,paste('SPI',sc,'ESP_',start_date ,"M_EOBS.RData", sep = "")))
        } else if (dataset == "CHIRPS") {
          load(file.path(dir_4drop, paste('SPI',sc,'ESP_',start_date ,"M_CHIRPS.RData", sep = "" )))
        } else if (dataset == "ERA5") {
          load(file.path(dir_4drop, paste('SPI',sc,'ESP_',start_date ,"M_ERA5.RData", sep = "" ) ))
        } else {
          print('dataset not known')
        }
        
        # Cálculo de la media para los pronósticos mensuales de SPI6
        data = apply(spi6pred[,,mesi_8,], c(1,2,3), mean, na.rm=TRUE)
        data[is.infinite(data)]=NA
        data[is.na(data)]=NA
        
        ni = dim(data)[1]
        nj = dim(data)[2]
        nt = dim(data)[3]
        
        # Inicialización de matrices para el MAE
        corre <- matrix(data = NA,nrow = ni, ncol = nj)
        data[is.infinite(data)]=NA
        
        # Bucle para calcular el MAE
        for (i in 1:ni) {
          for (j in 1:nj) {
            
            OK <- complete.cases(ens[i, j,], data[i, j,])
            x <- ens[i, j, OK]
            y <- data[i, j, OK]
            n <- length(x)
            #if (n >= nt*0.9) {
      
            corre[i, j]=mae(x,y)
            
          #}
          
          
          rm(OK, n, x, y)
        }
      }
      #almacenamiento de resultados
      save(corre, file = file.path(dir_out2, paste("MAE_ESP_spi",sc,"_",sprintf("%02d", mesi_8[1]),"_", start_date, "M_",dataset,".RData", sep = "") ))
      
      }
    }
  }
}






