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

data(wrld_simpl)          # Carga de datos mundiales simplificados

# Directorios
dir_oss = '~/Chapter3-4SPAIN/Data/'
dir_data = '~/Chapter3-4SPAIN/Data/'
dir_out = '~/Chapter3-4SPAIN/Data/'

# Establecer la ruta del archivo NetCDF que contiene los datos de AEMET
fname <- file.path(dir_oss, 'AEMET_025_spain_1981_2017.nc')
obs.nc <- nc_open(fname) # Abrir el archivo NetCDF

# Extraer los datos de precipitación, latitud y longitud del archivo NetCDF
obs = ncvar_get(obs.nc, "precipitation")
lat = ncvar_get(obs.nc, "latitude")
lat = rev(lat)  # Revertir los valores de latitud
lon = ncvar_get(obs.nc, "longitude")

# Reemplazar valores inválidos por NA
obs[obs == "-32767s"] <- NA
# Revertir las columnas de los datos de precipitación
obs = obs[, dim(obs)[2]:1,]

# Cargar datos del mapa mundial
data(wrld_simpl)

# Crear una matriz binaria 'inout' basada en los valores de precipitación
inout <- obs
inout[obs > 0] <- 1
inout = inout[,, 444]
dim(inout)

# Definir índices para diferentes meses
mesi_start = which(mesi == 1)
mesi_1 = which(mesi == 1)

# Definir conjuntos de datos y fechas de inicio
mesi_start = which(mesi == 1)
mesi_1 = which(mesi == 1)
mesi_2 = which(mesi == 2)
mesi_3 = which(mesi == 3)
mesi_4 = which(mesi == 4)
mesi_5 = which(mesi == 5)
mesi_6 = which(mesi == 6)
mesi_7 = which(mesi == 7)
mesi_8 = which(mesi == 8)
mesi_9 = which(mesi == 9)
mesi_10 = which(mesi == 10)
mesi_11 = which(mesi == 11)
mesi_12 = which(mesi == 12)

datasets = c('ERA5')

start_dates = c( 3, 4, 5,   
                 8, 9, 10
                 )

for (istart_date in 1:length(start_dates)) {
  start_date = start_dates[istart_date]
  
  # dates = seq(start_date, start_date + 3)
  if (start_date == 3) {
    target_season = 'junio'
  } else if (start_date == 4) {
    target_season = 'junio'
  } else if (start_date == 5) {
    target_season = 'junio' 
    
  }  else if (start_date == 8) {
    target_season = 'noviembre'
  } else if (start_date == 9) {
    target_season = 'noviembre'
  } else if (start_date == 10) {
    target_season = 'noviembre'
  
  }
  
  ## load data and spi calculation
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    print(dataset)
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
      
    } else {
      print('dataset not known')
    }

    ## keep only 1981-2017 period
    prec = obs[, , 1:444]
    
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = inout * prec[, , i]
    }
    
    aux = apply(prec, c(1, 2), mean, na.rm = TRUE)
    
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = aux / aux * prec[, , i]
    }
    

    spi6pred = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3], (length(anni) -
                                                                                     1)))
    
    for (ianni in 1:length(anni)) {
      print(paste0('loop ', ianni, ' of ', length(anni)))
      
      
      anno_for = which(anni == anni[ianni])
      
      mese_start = mesi_start[anno_for]
      anni_resampling = anni[-anno_for]
      
      #resampling dati storici
      for (ires in 1:length(anni_resampling)) {
        cat("loop", ires, "\n")
        
        PRE_FOR = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], length(anni) * 12))
        PRE_FOR = prec 
        PRE_FOR[, , (mese_start + start_date - 1):(mese_start + 11)] = NA # cometar con 11

        
        anno_tmp = which(anni == anni_resampling[ires])
        mese_1 = mesi_1[anno_tmp]
        mese_2 = mesi_2[anno_tmp]
        mese_3 = mesi_3[anno_tmp]
        mese_4 = mesi_4[anno_tmp]
        mese_5 = mesi_5[anno_tmp]
        mese_6 = mesi_6[anno_tmp]
        mese_7 = mesi_7[anno_tmp]
        mese_8 = mesi_8[anno_tmp]
        mese_9 = mesi_9[anno_tmp]
        mese_10 = mesi_10[anno_tmp]
        mese_11 = mesi_11[anno_tmp]
        mese_12 = mesi_12[anno_tmp]
        
        # djfmamjjason
        if (target_season == 'junio' && start_date == 5) {
          PRE_FOR[, , mese_start + 4] = prec[, , mese_5]
          PRE_FOR[, , mese_start + 5] = prec[, , mese_6]
        } else if (target_season=='junio' && start_date == 4){
          PRE_FOR[, , mese_start + 3] = prec[, , mese_4]
          PRE_FOR[, , mese_start + 4] = prec[, , mese_5]
          PRE_FOR[, , mese_start + 5] = prec[, , mese_6]
        } else if (target_season=='junio' && start_date == 3){
          PRE_FOR[, , mese_start + 2] = prec[, , mese_3]
          PRE_FOR[, , mese_start + 3] = prec[, , mese_4]
          PRE_FOR[, , mese_start + 4] = prec[, , mese_5]
          PRE_FOR[, , mese_start + 5] = prec[, , mese_6]
          
          
        } else if (target_season == 'noviembre' && start_date == 10) {
          PRE_FOR[, , mese_start + 9] = prec[, , mese_10]
          PRE_FOR[, , mese_start + 10] = prec[, , mese_11]
        } else if (target_season=='noviembre' && start_date == 9){
          PRE_FOR[, , mese_start + 8] = prec[, , mese_9]
          PRE_FOR[, , mese_start + 9] = prec[, , mese_10]            
          PRE_FOR[, , mese_start + 10] = prec[, , mese_11]
        } else if (target_season=='noviembre' && start_date == 8){
          PRE_FOR[, , mese_start + 7] = prec[, , mese_8]
          PRE_FOR[, , mese_start + 8] = prec[, , mese_9]
          PRE_FOR[, , mese_start + 9] = prec[, , mese_10]
          PRE_FOR[, , mese_start + 10] = prec[, , mese_11]
          
        
        } else {
          print('start date not known')
        }
        
        ## calculate SPI
        spitmp = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3]))
        
        
        
        for (i in 1:dim(prec)[1]) {
          print(paste0('grid ', i, ' of ', length(lon)))
          
          for (j in 1:dim(prec)[2]) {
            if (!is.na(aux[i, j] / aux[i, j])) {
              
              
              dum <- spi(PRE_FOR[i, j, ], 6, na.rm = TRUE)
              spitmp[i, j, ] = dum$fitted
              rm(dum)
              
            }
          }
        }
        
        if (target_season == 'junio') {
          spi6pred[, , mese_start + 5, ires] = spitmp[, , mese_start + 5]
        } else if (target_season == 'noviembre') {
          spi6pred[, , mese_start + 10, ires] = spitmp[, , mese_start + 10]

        }
        
        rm(spitmp)
        
      }
    }
    
    
    save(spi6pred, file = file.path(
      dir_out,
      paste(
        "SPI6ESP_",
        sprintf("%02d", start_date),
        "_",
        target_season,
        "_",
        dataset,
        ".RData",
        sep = ""
      )
    ))
    
  }
}
