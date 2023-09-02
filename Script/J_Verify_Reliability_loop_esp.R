
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
 source("~/Chapter3-4SPAIN/script/Common/CorrMIO.R")
 source("~/Chapter3-4SPAIN/script/Common/ColorBarM.R")
 source("~/Chapter3-4SPAIN/script/Common/mioplot_global.R") 
 source("~/Chapter3-4SPAIN/script/Common/ReliabilityDiagram_MIO2.R")
 source("~/Chapter3-4SPAIN/script/Common/myreliability.R")

 dir_drop = '~/Chapter3-4SPAIN/data/'
 dir_oss = '~/Chapter3-4SPAIN/data/'
 dir_out= '~/Chapter3-4SPAIN/data/'
 dir_out2='~/Chapter3-4SPAIN/Figure/'

 load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
 load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
 load(file.path(dir_drop, "inout.RData"))
 ni = length(lon)
 nj = length(lat)

 
 data(wrld_simpl)
 
 sc=6
 anni = 1981:2017
 mesi = rep(1:12, length(anni))
 # Define el número de simulaciones boostrap
 nb = 1000
 # Define los umbrales de sequía para la predicción
 thresholds = c(-0.8)

 # Define pasos de probabilidad
 pstep = 0.2 
 prob1 = seq(0, 1 - pstep, pstep)
 prob1[1] = -prob1[length(prob1)]
 prob2 = seq(0 + pstep, 1, pstep)

# Definición de los meses y sus números correspondientes
start_dates = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

# Iteración a través de los meses
for (istart_date in 1:length(start_dates)) {
  start_date = start_dates[istart_date]
  
  # Asignación del nombre de la estación según el número del mes
  if (start_date == 1) {
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
  } else if (start_date == 12) {
    target_season = 'diciembre'
  }
  
  
  # Asignación de índices de meses en base a la estación objetivo
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
  
  # Carga de datos climáticos observados (spi6) desde un archivo
  load(file.path(paste(dir_drop,"SPI6_AEMET_1981_2017.RData", sep = "")  ))
  obs = spi6[,,mesi_8]
 
  # Asignación del nombre de archivo y carga de predicciones
  nam <- paste("spi", sc, sep = "")
  print(nam)
  mb=c(1:36) 
  pred = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], length(mb)))
  
  # Selección de archivos de predicción basados en la fecha de inicio 4M
   if (start_date == 1){
	    load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+9),  "_",  target_season, "_ERA5.RData", sep = "")))
   } else if (start_date == 2){
	    load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+9),  "_",  target_season, "_ERA5.RData", sep = "")))
   } else if (start_date == 3){
	    load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+9),  "_",  target_season, "_ERA5.RData", sep = "")))
   } else {
	    load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date - 3),  "_",  target_season, "_ERA5.RData", sep = "")))
   } 
  
  
  # Asignación de datos de predicción y manejo de valores NA e infinitos
  pred = spi6pred[,,mesi_8,]
  pred[is.infinite(pred)] <- NA
  pred[is.na(pred)] <- NA
  
  rm(spi6pred)
  
  
  # Carga de datos de predicción adicionales
  nam <- paste("spi", sc,"pred", sep = "")
  print(nam)
  mb=c(1:36) 
  pred2 = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], length(mb)))
  
  # Selección de archivos de predicción adicionales basados en la fecha de inicio 3M
    if (start_date == 1){
	       load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+10),  "_",  target_season, "_ERA5.RData", sep = "")))
      } else if (start_date == 2){
	       load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+10),  "_",  target_season, "_ERA5.RData", sep = "")))
      } else {
	       load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date - 2),  "_",  target_season, "_ERA5.RData", sep = "")))
      }
 
  # Asignación de datos de predicción adicionales y manejo de valores NA e infinitos 2M
  pred2 = spi6pred[,,mesi_8,]
  pred2[is.infinite(pred2)] <- NA
  pred2[is.na(pred2)] <- NA
  rm(spi6pred)
  
  # Más carga y manipulación de datos de predicción
  pred3 = array(data = NA, dim = c(dim(obs)[1], dim(obs)[2], dim(obs)[3], length(mb)))
    if (start_date == 1){
	     load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date+11),  "_",  target_season, "_ERA5.RData", sep = "")))
     } else {
	     load(file.path(paste(dir_oss, 'SPI',sc,'ESP_', sprintf("%02d", start_date - 1),  "_",  target_season, "_ERA5.RData", sep = "")))
     }
  
  # Asignación de datos de predicción adicionales y manejo de valores NA e infinitos
  pred3 = spi6pred[,,mesi_8,]
  pred3[is.infinite(pred3)] <- NA
  pred3[is.na(pred3)] <- NA

  # Iteración a través de umbrales de pronóstico
  for (ith in 1:length(thresholds)) {
    th = thresholds[ith]
    print(th)
    
    # Cálculos y manipulación relacionados con la fiabilidad  
    rel_s4 <- myreliability(obs, pred, th, pstep, lat, inout)
    freq_s4 = as.numeric(unlist(rel_s4[1]))
    h_s4 = as.numeric(unlist(rel_s4[2]))
    g_s4 = as.numeric(unlist(rel_s4[3]))
    slope_s4 = as.numeric(unlist(rel_s4[4]))
    obs.clim = sum(g_s4) / sum(h_s4)
    
    ##boot
    
    
    slope_s4_b = matrix(NA, nb, 1)
    
    
    if (!is.na(slope_s4)) {
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
        
     
        rel_b <-
          myreliability(obs_b, pred_b, th, pstep, lat, inout)
        slope_s4_b[ib] = as.numeric(unlist(rel_b[4]))
        rm(rel_b)
   
      }
    }
    
    # Creación de una lista para almacenar resultados de fiabilidad
    rel <- list()
    rel$slope1 <- slope_s4_b
   
    # Guardar los resultados de fiabilidad en archivos
  if (start_date == 1){
     save(rel, file = file.path(dir_out,paste("reliability_ESP_",(th),
                           "_spi_", sprintf("%02d", start_date+9),"_",target_season,"_ERA5.RData",sep = "")))
    } else if (start_date == 2){
     save(rel, file = file.path(dir_out,paste("reliability_ESP_",(th),
                           "_spi_", sprintf("%02d", start_date+9),  "_",  target_season,"_ERA5.RData",sep = "")))
    } else if(start_date == 3) {
	    save(rel, file = file.path(dir_out,paste("reliability_ESP_",(th),
							             "_spi_", sprintf("%02d", start_date+9),  "_",  target_season,"_ERA5.RData",sep = "")))
    } else {
		  save(rel, file = file.path(dir_out,paste("reliability_ESP_",(th),
							             "_spi_", sprintf("%02d", start_date-3),  "_",  target_season,"_ERA5.RData",sep = "")))
	  }

    ## reliability
    
    rel_s5 <- myreliability(obs, pred2, th, pstep, lat, inout)
    freq_s5 = as.numeric(unlist(rel_s5[1]))
    h_s5 = as.numeric(unlist(rel_s5[2]))
    g_s5 = as.numeric(unlist(rel_s5[3]))
    slope_s5 = as.numeric(unlist(rel_s5[4]))
    obs.clim = sum(g_s5) / sum(h_s5)
    
    ##boot
    
    slope_s5_b = matrix(NA, nb, 1)
    
    
    if (!is.na(slope_s5)) {
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
        
        
        
        rel_b2 <-
          myreliability(obs_b, pred_b2, th, pstep, lat, inout)
        slope_s5_b[ib] = as.numeric(unlist(rel_b2[4]))
        rm(rel_b2)
        
      }
    }
    
    
    rel2 <- list()
    rel2$slope1 <- slope_s5_b
       
    
     if (start_date == 1){
       save(rel2, file = file.path(dir_out,paste("reliability_ESP_",(th),
                              "_spi_", sprintf("%02d", start_date+10),"_",target_season,"_ERA5.RData",sep = "")))
       } else if (start_date == 2){save(rel2, file = file.path(dir_out,paste("reliability_ESP_",(th),
                              "_spi_", sprintf("%02d", start_date+10),"_",target_season,"_ERA5.RData",sep = "")))
       } else {save(rel2, file = file.path(dir_out,paste("reliability_ESP_",(th),
                              "_spi_", sprintf("%02d", start_date-2),  "_",  target_season,"_ERA5.RData",sep = "")))
       }	  

    ## reliability
    rel_3 <- myreliability(obs, pred3, th, pstep, lat, inout)
    freq_3 = as.numeric(unlist(rel_3[1]))
    h_3 = as.numeric(unlist(rel_3[2]))
    g_3 = as.numeric(unlist(rel_3[3]))
    slope_3 = as.numeric(unlist(rel_3[4]))
    obs.clim = sum(g_3) / sum(h_3)
    
    ##boot
    
    slope_3_b = matrix(NA, nb, 1)
    
    
    if (!is.na(slope_3)) {
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
        
        
        
        rel_b3 <-
          myreliability(obs_b, pred_b3, th, pstep, lat, inout)
        slope_3_b[ib] = as.numeric(unlist(rel_b3[4]))
        rm(rel_b3)

        
      }
    }

    rel3 <- list()
    rel3$slope1 <- slope_3_b
    
      if (start_date == 1){ save(rel3, file = file.path(dir_out,paste("reliability_ESP_",(th),
                               "_spi_", sprintf("%02d", start_date+11),"_",target_season,"_ERA5.RData",sep = "")))
      } else {save(rel3, file = file.path(dir_out,paste("reliability_ESP_",(th),
                               "_spi_", sprintf("%02d", start_date-1),  "_",  target_season,"_ERA5.RData",sep = "")))
	    }
    
    
    ##PLOT
    # Comentario: Marca el inicio del bloque de código para generar un gráfico de fiabilidad y guardarlo en un archivo PDF.
    
    pdf(
      file.path(dir_out2, paste("reliability_", (th), "_spi", sc, "_", target_season, ".pdf", sep = "")),
      width = 6.5,
      height = 4.5
    )
    # Comentario: Crea un archivo PDF con un nombre basado en variables y define el ancho y alto del gráfico.
    
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    # Comentario: Guarda los parámetros gráficos actuales y asegura que se restauren al final del bloque de código.
    
    # Se calculan estadísticas de las pendientes y los intervalos de confianza para diferentes series de datos.
    slope1.stat <- quantile(slope_s4_b, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    inter1.stat1 <- obs.clim * (1 - slope1.stat[1])
    inter1.stat2 <- obs.clim * (1 - slope1.stat[2])
    inter1.stat3 <- obs.clim * (1 - slope1.stat[3])
    
    slope2.stat <- quantile(slope_s5_b, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    inter2.stat1 <- obs.clim * (1 - slope2.stat[1])
    inter2.stat2 <- obs.clim * (1 - slope2.stat[2])
    inter2.stat3 <- obs.clim * (1 - slope2.stat[3])
    
    slope3.stat <- quantile(slope_3_b, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
    inter3.stat1 <- obs.clim * (1 - slope3.stat[1])
    inter3.stat2 <- obs.clim * (1 - slope3.stat[2])
    inter3.stat3 <- obs.clim * (1 - slope3.stat[3])
    
    # Comentario: Calcula estadísticas y intervalos de confianza para diferentes variables.
    
    # Se configura el gráfico con ejes, etiquetas y límites.
    plot(
      NULL,
      xlim = c(0, 1),
      ylim = c(0, 1),
      xlab = "Forecast probability",
      ylab = "Observed relative frequency"
    )
    
    # Comentario: Crea un gráfico vacío con ejes y etiquetas.
    
    # Se definen puntos y polígonos para representar regiones de falta de resolución y habilidad.
    a <- (1 - obs.clim) / 2 + obs.clim
    b <- obs.clim / 2
    x.p <- c(obs.clim, obs.clim, 1, 1, 0, 0)
    y.p <- c(0, 1, 1, a, b, 0)
    polygon(x.p, y.p, col = "#e6e6e6")
    abline(h = obs.clim, lty = 2)
    text(0.9, obs.clim, "No resolution", pos = 3)
    text(
      0.9,
      obs.clim + (a - b) * (0.9 - obs.clim),
      "No skill",
      pos = 1,
      srt = atan(a - b) / (2 * pi) * 360
    )
    
    # Comentario: Dibuja polígonos y líneas para representar diferentes regiones en el gráfico.
    
    # Se calculan puntos extremos para representar áreas en el gráfico.
    pointxlow1 = c(max(0, -inter1.stat1 / slope1.stat[1]), min(1, (1 - inter1.stat1) / slope1.stat[1]))
    if (pointxlow1[1] > 1) {
      pointxlow1 = c(0, 1)
    }
    pointxhigh1 = c(max(0, -inter1.stat3 / slope1.stat[3]), min(1, (1 - inter1.stat3) / slope1.stat[3]))
    
    # Comentario: Calcula puntos extremos para el primer conjunto de datos.
    
    # Se repite el proceso para otros dos conjuntos de datos.
    pointxlow2 = c(max(0, -inter2.stat1 / slope2.stat[1]), min(1, (1 - inter2.stat1) / slope2.stat[1]))
    if (pointxlow2[1] > 1) {
      pointxlow2 = c(0, 1)
    }
    pointxhigh2 = c(max(0, -inter2.stat3 / slope2.stat[3]), min(1, (1 - inter2.stat3) / slope2.stat[3]))
    
    pointxlow3 = c(max(0, -inter3.stat1 / slope3.stat[1]), min(1, (1 - inter3.stat1) / slope3.stat[1]))
    if (pointxlow3[1] > 1) {
      pointxlow3 = c(0, 1)
    }
    pointxhigh3 = c(max(0, -inter3.stat3 / slope3.stat[3]), min(1, (1 - inter3.stat3) / slope3.stat[3]))
    
    # Comentario: Calcula puntos extremos para el segundo y tercer conjunto de datos.
    
    # Se definen valores para las coordenadas y para los puntos en el gráfico.
    y1 = 1
    y3 = 1
    
    if (slope1.stat[1] <= 1) {
      y1 = inter1.stat1 + slope1.stat[1]
    }
    if (slope1.stat[3] <= 1) {
      y3 = inter1.stat3 + slope1.stat[3]
    }
    pointylow1 = c(max(0, inter1.stat1), y1)
    pointyhigh1 = c(max(0, inter1.stat3), y3)
    
    # Comentario: Calcula valores para coordenadas y puntos para el primer conjunto de datos.
    
    # Se repite el proceso para los otros dos conjuntos de datos.
    y1 = 1
    y3 = 1
    
    if (slope2.stat[1] <= 1) {
      y1 = inter2.stat1 + slope2.stat[1]
    }
    if (slope2.stat[3] <= 1) {
      y3 = inter2.stat3 + slope2.stat[3]
    }
    
    pointylow2 = c(max(0, inter2.stat1), y1)
    pointyhigh2 = c(max(0, inter2.stat3), y3)
    
    y1 = 1
    y3 = 1
    
    if (slope3.stat[1] <= 1) {
      y1 = inter3.stat1 + slope3.stat[1]
    }
    if (slope2.stat[3] <= 1) {
      y3 = inter3.stat3 + slope3.stat[3]
    }
    
    pointylow3 = c(max(0, inter3.stat1), y1)
    pointyhigh3 = c(max(0, inter3.stat3), y3)
    
    # Comentario: Calcula valores para coordenadas y puntos para el segundo y tercer conjunto de datos.
    
    # Se dibujan polígonos para las áreas calculadas.
    polygon(
      c(pointxhigh1, rev(pointxlow1)),
      c(pointyhigh1, rev(pointylow1)),
      col = rgb(0, 0, 0, 0.6),
      border = NA
    )
    
    polygon(
      c(pointxhigh2, rev(pointxlow2)),
      c(pointyhigh2, rev(pointylow2)),
      col = rgb(0, 1, 1, 0.6),
      border = NA
    )
    
    polygon(
      c(pointxhigh3, rev(pointxlow3)),
      c(pointyhigh3, rev(pointylow3)),
      col = rgb(0, 1, 0, 0.6),
      border = NA
    )
    
    # Comentario: Dibuja polígonos coloreados para representar diferentes áreas en el gráfico.
    
    # Se agrega una leyenda en la esquina superior izquierda.
    legend(
      "topleft",
      inset = 0.04,
      c("4 Months", "3 Months", "2 Months"),
      fill = c(
        rgb(0, 0, 0, 0.6),
        rgb(0, 1, 1, 0.6),
        rgb(0, 1, 0, 0.6)
      ),
      horiz = FALSE,
      cex = 0.9
    )
    
    # Comentario: Agrega una leyenda al gráfico para explicar los colores y los conjuntos de datos.
    
    # Se añaden puntos al gráfico para representar frecuencias observadas.
    points(
      prob2 - (pstep / 2),
      freq_s4,
      col = "black",
      pch = 21,
      bg = rgb(0, 0, 0, 0.6),
      lwd = 2,
      type = "p"
    )
    
    points(
      prob2 - (pstep / 2),
      freq_s5,
      col = "black",
      pch = 21,
      bg = rgb(0, 1, 1, 0.6),
      lwd = 2,
      type = "p"
    )
    
    points(
      prob2 - (pstep / 2),
      freq_3,
      col = "black",
      pch = 21,
      bg = rgb(0, 1, 0, 0.6),
      lwd = 2,
      type = "p"
    )
    
    # Comentario: Agrega puntos al gráfico para representar frecuencias observadas para diferentes conjuntos de datos.
    
    # Se traza una línea diagonal en el gráfico.
    lines(c(0, 1), c(0, 1), lty = 1)
    
    # Comentario: Dibuja una línea diagonal en el gráfico.
    
    # Se ajusta el espacio para el gráfico de barras y se crea un segundo eje y en el lado derecho.
    pp <- par("plt")
    par(plt = c(pp[2] - 0.2, pp[2], pp[3], pp[3] + 0.2))
    par(new = TRUE)
    barplot(
      rbind(h_s4, h_s5, h_3),
      beside = TRUE,
      axes = FALSE,
      axisnames = FALSE,
      col = c(
        rgb(0, 0, 0, 0.6),
        rgb(0, 1, 1, 0.6),
        rgb(0, 1, 0, 0.6)
      )
    )
    axis(4)
    box()
    
    # Comentario: Ajusta el espacio para el gráfico de barras y agrega un segundo eje y en el lado derecho.
    
    # Cierra el archivo PDF.
    dev.off()
    
    # Comentario: Finaliza la creación del gráfico y cierra el archivo PDF.
    
    
  }
}



