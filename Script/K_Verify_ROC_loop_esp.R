
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
library(scales)

# Carga de scripts externos
source("~/Chapter3-4SPAIN/script/Common/CorrMIO.R")
source("~/Chapter3-4SPAIN/script/Common/ColorBarM.R")
source("~/Chapter3-4SPAIN/script/Common/mioplot_global.R") 
source("~/Chapter3-4SPAIN/script/Common/myroc.R")

dir_drop = '~/Chapter3-4SPAIN/data/'
dir_oss = '~/Chapter3-4SPAIN/data/'
dir_out= '~/Chapter3-4SPAIN/data/'
dir_out2='~/Chapter3-4SPAIN/Figure/'



##################################################
nb = 1000
time_scale = c(6)
thresholds = c(-0.8)

sc=6
anni = 1981:2017
mesi = rep(1:12, length(anni))
##################################################

data(wrld_simpl)

identity.col = "darkgrey"
identity.lty = 1
identity.lwd = 1


pstep = 0.2
prob1 = seq(0, 1 - pstep, pstep)
prob1[1] = -prob1[length(prob1)]
prob2 = seq(0 + pstep, 1, pstep)

load(file.path(dir_drop, "inout.RData"))
load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

#start_dates = c(2)
start_dates = c(6,7,8,9,10,11)

for (istart_date in 1:length(start_dates)) {
  start_date = start_dates[istart_date]
    
  if (start_date == 12) {
    target_season = 'diciembre'
  } else if (start_date == 1) {
    target_season = 'enero'
  } else if (start_date == 2) {
    target_season = 'febrero'
  } else if (start_date == 3) {
    target_season = 'marzo'
  } else if (start_date == 4) {
    target_season = 'abril'
  } else if (start_date == 5) {
    target_season = 'mayo'
  } else if (start_date == 6) {
    target_season = 'junio'
  } else if (start_date == 7) {
    target_season = 'julio'
  } else if (start_date == 8) {
    target_season = 'august'
  } else if (start_date == 9) {
    target_season = 'septiembre'
  } else if (start_date == 10) {
    target_season = 'octubre'
  } else if (start_date == 11) {
    target_season = 'noviembre'
  }
  
  
  if (target_season == 'enero') {
    mesi_8 = which(mesi== 01)
  } else if (target_season == 'febrero') {
    mesi_8 = which(mesi == 02)
  } else if (target_season == 'marzo') {
    mesi_8 = which(mesi == 03)
  } else if (target_season == 'abril') {
    mesi_8 = which(mesi == 04)
  } else if (target_season == 'mayo') {
    mesi_8 = which(mesi == 05)
  } else if (target_season == 'junio') {
    mesi_8 = which(mesi == 06)
  } else if (target_season == 'julio') {
    mesi_8 = which(mesi == 07)
  }else if (target_season == 'august') {
    mesi_8 = which(mesi == 08)
  } else if (target_season == 'septiembre') {
    mesi_8 = which(mesi == 09)
  } else if (target_season == 'octubre') {
    mesi_8 = which(mesi == 10)
  } else if (target_season == 'noviembre') {
    mesi_8 = which(mesi == 11)
  } else if (target_season == 'diciembre') {
    mesi_8 = which(mesi == 12)
  }
  
  
  load(file.path(paste(dir_drop,"SPI6_AEMET_1981_2017.RData", sep = "")  ))
  obs = spi6[,,mesi_8]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  mb=c(1:36) 
  
  pred = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], length(mb)))
 
  if (start_date == 1){
  load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+9),  "_",  target_season, "_ERA5.RData", sep = "")))
  } else if (start_date == 2){
  load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+9),  "_",  target_season, "_ERA5.RData", sep = "")))
  } else if (start_date == 3){
  load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+9),  "_",  target_season, "_ERA5.RData", sep = "")))
  } else {
  load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date - 3),  "_",  target_season, "_ERA5.RData", sep = "")))
  }

  pred = spi6pred[,,mesi_8,]
  pred[is.infinite(pred)] <- NA
  pred[is.na(pred)] <- NA
  
  rm(spi6pred) 
  ## load data
  nam <- paste("spi", sc,"pred", sep = "")
  print(nam)
  
  mb=c(1:36) 
  
  pred2 = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], length(mb)))
  
  if (start_date == 1){
	  load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+10),  "_",  target_season, "_ERA5.RData", sep = "")))
  } else if (start_date == 2){
	  load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+10),  "_",  target_season, "_ERA5.RData", sep = "")))
  } else {
     	  load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date - 2),  "_",  target_season, "_ERA5.RData", sep = "")))
  }

  pred2 = spi6pred[,,mesi_8,]
  pred2[is.infinite(pred2)] <- NA
  pred2[is.na(pred2)] <- NA
  
  
  rm(spi6pred)
  
  pred3 = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], length(mb)))
  
  
  if (start_date == 1){
	load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+11),  "_",  target_season, "_ERA5.RData", sep = "")))
   } else {
        load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date - 1),  "_",  target_season, "_ERA5.RData", sep = "")))
   } 


  pred3 = spi6pred[,,mesi_8,]
  pred3[is.infinite(pred3)] <- NA
  pred3[is.na(pred3)] <- NA
  
  for (ith in 1:length(thresholds)) {
    th = thresholds[ith]
    print(th)

    ni = dim(obs)[1]
    nj = dim(obs)[2]
    
    ###############
    ## roc
    ###############
    
    roc_1 <- myroc(obs, pred, th, pstep, lat, inout)  
    hr_1 = as.numeric(unlist(roc_1[1]))
    far_1 = as.numeric(unlist(roc_1[2]))
    hr_1[length(hr_1) + 1] = 0
    far_1[length(far_1) + 1] = 0
    
    plot(far_1, hr_1, xlim = c(0, 1), ylim = c(0, 1))
    
    ##boot
    
    far_b_1 = matrix(NA, nb, length(hr_1) - 1)
    hr_b_1 = matrix(NA, nb, length(hr_1) - 1)
    auc_b_1 = matrix(NA, nb, 1)
    
    for (ib in 1:nb) {
      cat('Processing ', ib, 'of', nb, 'boostraps', '\n')
      
      ind <-
        sample(1:dim(obs)[3],
               size = dim(obs)[3],
               replace = TRUE)
      
      obs_b = obs * NA
      for (i in 1:ni) {
        for (j in 1:nj) {
          obs_b[i, j, ] = obs[i, j, ind]
        }
      }
      
      pred_b = pred * NA
      for (i in 1:ni) {
        for (j in 1:nj) {
          for (k in 1:dim(pred)[4]) {
            pred_b[i, j, , k] = pred[i, j, ind, k]
          }
        }
      }
      
      roc_b <- myroc(obs_b, pred_b, th, pstep, lat, inout)
      hr_b_1[ib,] = as.numeric(unlist(roc_b[1]))
      far_b_1[ib,] = as.numeric(unlist(roc_b[2]))
      auc_b_1[ib] = as.numeric(unlist(roc_b[3]))
      rm(roc_b)
  
    } 
   
    hr_ci_1 = matrix(0, dim(hr_b_1)[2] + 1, 2)
    far_ci_1 = matrix(0, dim(hr_b_1)[2] + 1, 2)
    for (i in 1:dim(hr_b_1)[2]) {
      hr_ci_1[i,] = as.numeric(quantile(hr_b_1[, i], c(0.025, 0.975), na.rm = TRUE))
      far_ci_1[i,] = as.numeric(quantile(far_b_1[, i], c(0.025, 0.975), na.rm = TRUE))
    }
    
    roc_1 = auc_b_1
    
  if (start_date == 1){
    save(roc_1, file = file.path(dir_out,paste("rocarea_ESP_",(th),
	"_spi6_", sprintf("%02d", start_date+9),  "_",  target_season,"_ERA5.RData",sep = "")))
  } else if (start_date == 2){
    save(roc_1, file = file.path(dir_out,paste("rocarea_ESP_",(th),
	"_spi6_", sprintf("%02d", start_date+9),  "_",  target_season,"_ERA5.RData",sep = "")))
  } else if(start_date == 3) {
    save(roc_1, file = file.path(dir_out,paste("rocarea_ESP_",(th),
        "_spi6_", sprintf("%02d", start_date+9),  "_",  target_season,"_ERA5.RData",sep = "")))
  } else { 
    save(roc_1, file = file.path(dir_out,paste("rocarea_ESP_",(th),
	"_spi6_", sprintf("%02d", start_date-3),  "_",  target_season,"_ERA5.RData",sep = "")))
  }
    ###############
    ## roc
    ###############
    
    roc_2 <- myroc(obs, pred2, th, pstep, lat, inout)
    hr_2 = as.numeric(unlist(roc_2[1]))
    far_2 = as.numeric(unlist(roc_2[2]))
    hr_2[length(hr_2) + 1] = 0
    far_2[length(far_2) + 1] = 0
    
    plot(far_2, hr_2, xlim = c(0, 1), ylim = c(0, 1))
    
    ##boot
    
    far_b_2 = matrix(NA, nb, length(hr_2) - 1)
    hr_b_2 = matrix(NA, nb, length(hr_2) - 1)
    auc_b_2 = matrix(NA, nb, 1)
    
    for (ib in 1:nb) {
      cat('Processing ', ib, 'of', nb, 'boostraps', '\n')
      
      ind <-
        sample(1:dim(obs)[3],
               size = dim(obs)[3],
               replace = TRUE)
      
      obs_b = obs * NA
      for (i in 1:ni) {
        for (j in 1:nj) {
          obs_b[i, j, ] = obs[i, j, ind]
        }
      }
      
      pred_b2 = pred2 * NA
      for (i in 1:ni) {
        for (j in 1:nj) {
          for (k in 1:dim(pred2)[4]) {
            pred_b2[i, j, , k] = pred2[i, j, ind, k]
          }
        }
      }
     
      roc_b2 <- myroc(obs_b, pred_b2, th, pstep, lat, inout)
      hr_b_2[ib,] = as.numeric(unlist(roc_b2[1]))
      far_b_2[ib,] = as.numeric(unlist(roc_b2[2]))
      auc_b_2[ib] = as.numeric(unlist(roc_b2[3]))
      rm(roc_b2)   
    }
   
    hr_ci_2 = matrix(0, dim(hr_b_2)[2] + 1, 2)
    far_ci_2 = matrix(0, dim(hr_b_2)[2] + 1, 2)
    for (i in 1:dim(hr_b_2)[2]) {
      hr_ci_2[i,] = as.numeric(quantile(hr_b_2[, i], c(0.025, 0.975), na.rm = TRUE))
      far_ci_2[i,] = as.numeric(quantile(far_b_2[, i], c(0.025, 0.975), na.rm = TRUE))
    }
    # }
    roc_2 = auc_b_2
    
      if (start_date == 1){
	  save(roc_2, file = file.path(dir_out,paste("rocarea_ESP_",(th),
	  "_spi6_", sprintf("%02d", start_date+10),  "_",  target_season,"_ERA5.RData",sep = "")))
      } else if (start_date == 2){
	  save(roc_2, file = file.path(dir_out,paste("rocarea_ESP_",(th),
	  "_spi6_", sprintf("%02d", start_date+10),  "_",  target_season,"_ERA5.RData",sep = "")))
      } else {
	  save(roc_2, file = file.path(dir_out,paste("rocarea_ESP_",(th),
	  "_spi6_", sprintf("%02d", start_date-2),  "_",  target_season,"_ERA5.RData",sep = "")))
      }
    
    
    ###############
    ## roc
    ###############
    
    roc_3 <- myroc(obs, pred3, th, pstep, lat, inout)
    hr_3 = as.numeric(unlist(roc_3[1]))
    far_3 = as.numeric(unlist(roc_3[2]))
    hr_3[length(hr_3) + 1] = 0
    far_3[length(far_3) + 1] = 0
    
    plot(far_3, hr_3, xlim = c(0, 1), ylim = c(0, 1))
    
    ##boot
    
    far_b_3 = matrix(NA, nb, length(hr_3) - 1)
    hr_b_3 = matrix(NA, nb, length(hr_3) - 1)
    auc_b_3 = matrix(NA, nb, 1)
    
    for (ib in 1:nb) {
      cat('Processing ', ib, 'of', nb, 'boostraps', '\n')
      
      ind <-
        sample(1:dim(obs)[3],
               size = dim(obs)[3],
               replace = TRUE)
      
      obs_b = obs * NA
      for (i in 1:ni) {
        for (j in 1:nj) {
          obs_b[i, j, ] = obs[i, j, ind]
        }
      }
      
      pred_b3 = pred3 * NA
      for (i in 1:ni) {
        for (j in 1:nj) {
          for (k in 1:dim(pred3)[4]) {
            pred_b3[i, j, , k] = pred3[i, j, ind, k]
          }
        }
      }
      
      roc_b3 <- myroc(obs_b, pred_b3, th, pstep, lat, inout)
      hr_b_3[ib,] = as.numeric(unlist(roc_b3[1]))
      far_b_3[ib,] = as.numeric(unlist(roc_b3[2]))
      auc_b_3[ib] = as.numeric(unlist(roc_b3[3]))
      rm(roc_b3)   
    }
    
    hr_ci_3 = matrix(0, dim(hr_b_3)[2] + 1, 2)
    far_ci_3 = matrix(0, dim(hr_b_3)[2] + 1, 2)
    for (i in 1:dim(hr_b_3)[2]) {
      hr_ci_3[i,] = as.numeric(quantile(hr_b_3[, i], c(0.025, 0.975), na.rm = TRUE))
      far_ci_3[i,] = as.numeric(quantile(far_b_3[, i], c(0.025, 0.975), na.rm = TRUE))
    }
   
    roc_3 = auc_b_3
    
    if (start_date == 1){
	save(roc_3, file = file.path(dir_out,paste("rocarea_ESP_",(th),
	"_spi6_", sprintf("%02d", start_date+11),  "_",  target_season,"_ERA5.RData",sep = "")))
    } else {
	save(roc_3, file = file.path(dir_out,paste("rocarea_ESP_",(th),
        "_spi6_", sprintf("%02d", start_date-1),  "_",  target_season,"_ERA5.RData",sep = "")))		        
    }    
    
    
    pdf(file.path(dir_out2, paste("roc_", (th),"_spi_",  target_season, ".pdf",sep = "")),
        width = 5.5, height = 5.5) 
    par(pty = "s")
    plot( far_1, hr_1, xlim = c(0, 1), ylim = c(0, 1), col = "white", xlab = "False Alarm Rate", ylab = "Hit Rate" )
    
    polygon(c(far_1, rev(far_1)), c(hr_ci_1[, 1], rev(hr_ci_1[, 2])), col = rgb(0,0,0,0.6), border = NA)
    polygon(c(far_2, rev(far_2)), c(hr_ci_2[, 1], rev(hr_ci_2[, 2])), col = rgb(0,1,1,0.6), border = NA)
    polygon(c(far_3, rev(far_3)), c(hr_ci_3[, 1], rev(hr_ci_3[, 2])), col =  rgb(0,1,0,0.7), border = NA)
   
    lines(
      x = c(0, 1),
      y = c(0, 1),
      col = "darkgrey",
      lwd = identity.lwd,
      lty = identity.lty
    )
    dev.off()
  }
}

