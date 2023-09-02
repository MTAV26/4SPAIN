# Limpieza del ambiente
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
lat = rev(lat)                            # Revertir los valores de latitud
lon = ncvar_get(obs.nc, "longitude")
obs[obs == "-32767s"] <- NA               # Reemplazar valores inválidos por NA
obs = obs[, dim(obs)[2]:1,]               # Revertir las columnas de los datos de precipitación

data(wrld_simpl)                          # Cargar datos del mapa mundial


# Crear una matriz binaria 'inout' basada en los valores de precipitación
inout <- obs
inout[obs > 0] <- 1
inout = inout[,, 444]
dim(inout)

# Definir índices para diferentes meses
mesi_start = which(mesi == 1)
mesi_1 = which(mesi == 1)

# Definir conjuntos de datos y fechas de inicio
datasets = c('ERA5')
start_dates = c(10, 11, 12)

# Iterar sobre las fechas de inicio
for (istart_date in 1:length(start_dates)) {
  start_date = start_dates[istart_date]
  
  # Definir la estación objetivo según la fecha de inicio
  if (start_date == 10) {
    target_season = 'enero'
  } else if (start_date == 11) {
    target_season = 'enero'
  } else if (start_date == 12) {
    target_season = 'enero'
  }
  
  ## Cargar datos y calcular SPI
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
      print('conjunto de datos desconocido')
    }
    
    prec = obs[, , 1:444]
    
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = inout * prec[, , i]
    }
    
    aux = apply(prec, c(1, 2), mean, na.rm = TRUE)
    image.plot(lon, lat, aux / aux)
    plot(wrld_simpl, add = TRUE)
    
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = aux / aux * prec[, , i]
    }
    
    image.plot(lon, lat, apply(prec, c(1, 2), mean, na.rm = TRUE))
    plot(wrld_simpl, add = TRUE)
    
    ## Predecir SPI6 para agosto, julio y agosto; marzo, abril, mayo, junio, observados
    ## Pronóstico de SPEI
    spi6pred = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3], (length(anni) - 1)))
    
    for (ianni in 1:length(anni)) {
      print(paste0('ciclo ', ianni, ' de ', length(anni)))
      
      ## Omitir si no se toma el año anterior
      if (target_season == 'enero' && ianni==1) {
        next
      } 
      
      anno_for = which(anni == anni[ianni])
      
      mese_start = mesi_start[anno_for]
      anni_resampling = anni[-anno_for]
      
      if (target_season == 'enero') {
        anni_resampling=anni_resampling[-1]
      } 
      
      # Resamplear datos históricos
      for (ires in 1:length(anni_resampling)) {
        cat("ciclo", ires, "\n")
        
        PRE_FOR = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], length(anni) * 12))
        PRE_FOR = prec 
        
        # # 11
        if (target_season == 'enero' && start_date==10) {
          PRE_FOR[, , (mese_start  - 3):(mese_start + 11)] = NA 
          
        } else if (target_season == 'enero' && start_date==11) {
          PRE_FOR[, , (mese_start  - 2):(mese_start + 11)] = NA
          
          
        } else if (target_season == 'enero' && start_date==12) {
          PRE_FOR[, , (mese_start  - 1):(mese_start + 11)] = NA    
          
        } else {
          PRE_FOR[, , (mese_start + start_date - 1):(mese_start + 11)] = NA
        }
        
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
        if (target_season == 'enero' && start_date == 12) {
          mese_12 = mesi_12[anno_tmp-1]
          PRE_FOR[, , mese_start    -1] = prec[, , mese_12]
          PRE_FOR[, , mese_start      ] = prec[, , mese_1 ]
        } else if (target_season == 'enero' && start_date == 11) {
          mese_11 = mesi_11[anno_tmp-1]
          mese_12 = mesi_12[anno_tmp-1]
          PRE_FOR[, , mese_start    -2] = prec[, , mese_11]
          PRE_FOR[, , mese_start    -1] = prec[, , mese_12]
          PRE_FOR[, , mese_start      ] = prec[, , mese_1 ]
        } else if (target_season == 'enero' && start_date == 10) {
          mese_10 = mesi_10[anno_tmp-1]
          mese_11 = mesi_11[anno_tmp-1]
          mese_12 = mesi_12[anno_tmp-1]
          PRE_FOR[, , mese_start    -3] = prec[, , mese_10]
          PRE_FOR[, , mese_start    -2] = prec[, , mese_11]
          PRE_FOR[, , mese_start    -1] = prec[, , mese_12]
          PRE_FOR[, , mese_start      ] = prec[, , mese_1 ]
        } else {
          print('fecha de inicio desconocida')
        }
        
        ## Calcular SPI
        spitmp = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3]))
        
        for (i in 1:dim(prec)[1]) {
          print(paste0('celda ', i, ' de ', length(lon)))
          
          for (j in 1:dim(prec)[2]) {
            if (!is.na(aux[i, j] / aux[i, j])) {
              
              dum <- spi(PRE_FOR[i, j, ], 6, na.rm = TRUE)
              spitmp[i, j, ] = dum$fitted
              rm(dum)
              
            }
          }
        }
        
        if (target_season == 'enero') {
          spi6pred[, , mese_start , ires] = spitmp[, , mese_start ]
          
        }
        
        rm(spitmp)
        
      }
    }
    
    # Guardar resultados de predicción de SPI6
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
