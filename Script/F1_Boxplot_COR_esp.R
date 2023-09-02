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
library(StatDA)           # para los gráficos

# Carga de datos mundiales simplificados
data(wrld_simpl)

# Directorios
source("~/Chapter3-4SPAIN/script/Common/CorrMIO.R")
source("~/Chapter3-4SPAIN/script/Common/ColorBarM.R")
source("~/Chapter3-4SPAIN/script/Common/mioplot_global.R")
source("~/Chapter3-4SPAIN/script/Common/my_boxplot_stat.R")
source("~/Chapter3-4SPAIN/script/Common/my_boxplot.R")

time_scale = c(6)

dir_drop = '~/Chapter3-4SPAIN/Data/'
dir_4drop = '~/Chapter3-4SPAIN/Data/'
dir_out = '~/Chapter3-4SPAIN/Figures/'


sc=6
anni = 1981:2017
mesi = rep(1:12, length(anni))
mesi_8 = which(mesi == 08)


load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

ni = length(lon)
nj = length(lat)

corre_box <- matrix(data = NA, nrow = ni * nj, ncol = 36)
dim(corre_box)



for (i in 1:12) {
  load(paste(dir_out, sprintf("COR_ESP_spi6_%02d_4M_ERA5_original.RData", i), sep = ""))
  corre_box[, (i * 3) - 2] = as.vector(corre)
  
  load(paste(dir_out, sprintf("COR_ESP_spi6_%02d_3M_ERA5_original.RData", i), sep = ""))
  corre_box[, (i * 3) - 1] = as.vector(corre)
  
  load(paste(dir_out, sprintf("COR_ESP_spi6_%02d_2M_ERA5_original.RData", i), sep = ""))
  corre_box[, i * 3] = as.vector(corre)
}

# Creación del dataframe plot_data
plot_data <- data.frame(corre_box)

# Definición de colores
colores <- rep(c('#f7fcb9', '#addd8e', '#31a354'), 23)


  
  setEPS()
  postscript(
    file.path(
      dir_out,
      paste("BOX_COR_DIF_spi_original_all.eps", sep = "")
      #paste("boxplot_corre_spi_", sc, "_ENS.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  par(mar = c(9, 4, 3, 1))
  boxplotperc(
    na.omit(plot_data),
    quant = c(0.025, 0.975),
    outline = FALSE,
    las = 2,
    ylim = c(-0.2, 1),
    
    ylab="COR",
    main=paste('Forecast for ERA5 against AEMET;\n three lead times', sep=""),
    
    col=c(rgb(0,0.3,0.1),
          rgb(0,1,1),
          rgb(0,1,0)),
    
    #names = datasets
    names = c('', 'January'  , '',
              '', 'February' , '',
              '', 'March'    , '',
              '', 'April'    , '',
              '', 'May'      , '',
              '', 'June'     , '',
              '', 'July'     , '',
              '', 'August'   , '',
              '', 'September', '',
              '', 'October'  , '',
              '', 'November' , '',
              '', 'December' , ''
    ),
    
    
    at =c(1,2,3,    #4,5,6,
          7,8,9,    #10,11,12,
          13,14,15, #16,17,18,
          19,20,21, #22,23,24,
          25,26,27, #28,29,30,
          31,32,33, #34,35,36,
          37,38,39, #40,41,42,
          43,44,45, #46,47,48,
          49,50,51, #52,53,54,
          55,56,57, #58,59,60,
          61,62,63, #64,65,66,
          67,68,69
    ))  
  
  # Líneas de referencia
  abline(v = c(5,11,17,23,29,35,41,47,53,59,65), lty = 6,col = "gray")
  abline(h = c(-0.2,0, 0.2, 0.4, 0.6, 0.8,1.0), lty = 6,col = "gray")
  
  
  # Leyenda
  legend("topleft",inset=0,
         c("4 Months","3 Months", "2 Months"), fill=c(rgb(0,0.3,0.1),
                                                      rgb(0,1,1),
                                                      rgb(0,1,0)), horiz=F, cex=0.6)
  # Guardar la figura
  #grid (NULL, NULL, lty = 6, col = "cornsilk2")
  dev.off()
  