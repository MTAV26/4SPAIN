rm(list = ls())
graphics.off()
gc()

library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(s2dverification)
library(maps)
library(StatDA)

source("./4DROP/script/Common/my_boxplot_stat.R")
source("./4DROP/script/Common/my_boxplot.R")
source("./4DROP/script/Common/CorrMIO.R")
source("./4DROP/script/Common/ColorBarM.R")
source("./4DROP/script/Common/mioplot_global.R")
source("./4DROP/script/Common/ReliabilityDiagram_MIO2.R")
source("./4DROP/script/Common/myreliability.R")


dir_out = './4SPAIN/results/roc/'
dir_drop = './4SPAIN/data/'


sc=6
anni = 1981:2017
mesi = rep(1:12, length(anni))

th=c(-0.8)
load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

ni = length(lon)
nj = length(lat)
nb=1000
datasets=c("ERA5")

for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  
  corre_box <- matrix(data = NA,  nrow = nb, ncol = 36)
  dim(corre_box)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_10_enero_",dataset,".RData", sep = ""))
  corre_box[,1] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_11_enero_",dataset,".RData", sep = ""))
  corre_box[,2] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_12_enero_",dataset,".RData", sep = ""))
  corre_box[,3] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_11_febrero_",dataset,".RData", sep = ""))
  corre_box[,4] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_12_febrero_",dataset,".RData", sep = ""))
  corre_box[,5] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_01_febrero_",dataset,".RData", sep = ""))
  corre_box[,6] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_12_marzo_",dataset,".RData", sep = ""))
  corre_box[,7] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_01_marzo_",dataset,".RData", sep = ""))
  corre_box[,8] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_02_marzo_",dataset,".RData", sep = ""))
  corre_box[,9] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_01_abril_",dataset,".RData", sep = ""))
  corre_box[,10] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_02_abril_",dataset,".RData", sep = ""))
  corre_box[,11] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_03_abril_",dataset,".RData", sep = ""))
  corre_box[,12] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_02_mayo_",dataset,".RData", sep = ""))
  corre_box[,13] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_03_mayo_",dataset,".RData", sep = ""))
  corre_box[,14] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_04_mayo_",dataset,".RData", sep = ""))
  corre_box[,15] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_03_junio_",dataset,".RData", sep = ""))
  corre_box[,16] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_04_junio_",dataset,".RData", sep = ""))
  corre_box[,17] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_05_junio_",dataset,".RData", sep = ""))
  corre_box[,18] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_04_julio_",dataset,".RData", sep = ""))
  corre_box[,19] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_05_julio_",dataset,".RData", sep = ""))
  corre_box[,20] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_06_julio_",dataset,".RData", sep = ""))
  corre_box[,21] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_05_august_",dataset,".RData", sep = ""))
  corre_box[,22] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_06_august_",dataset,".RData", sep = ""))
  corre_box[,23] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_07_august_",dataset,".RData", sep = ""))
  corre_box[,24] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_06_septiembre_",dataset,".RData", sep = ""))
  corre_box[,25] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_07_septiembre_",dataset,".RData", sep = ""))
  corre_box[,26] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_08_septiembre_",dataset,".RData", sep = ""))
  corre_box[,27] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_07_octubre_",dataset,".RData", sep = ""))
  corre_box[,28] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_08_octubre_",dataset,".RData", sep = ""))
  corre_box[,29] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_09_octubre_",dataset,".RData", sep = ""))
  corre_box[,30] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_08_noviembre_",dataset,".RData", sep = ""))
  corre_box[,31] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_09_noviembre_",dataset,".RData", sep = ""))
  corre_box[,32] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_10_noviembre_",dataset,".RData", sep = ""))
  corre_box[,33] = (roc_2)
  
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_09_diciembre_",dataset,".RData", sep = ""))
  corre_box[,34] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_10_diciembre_",dataset,".RData", sep = ""))
  corre_box[,35] = (roc_2)
  load(paste(dir_out, "rocarea_ESP_-0.8_spi6_11_diciembre_",dataset,".RData", sep = ""))
  corre_box[,36] = (roc_2)
  
  corre_box = corre_box*2-1
  
  plot_data <-
    data.frame(
      corre_box[,1],corre_box [,2], corre_box[,3],corre_box [,4],
      corre_box[,5],corre_box [,6], corre_box[,7],corre_box [,8],
      corre_box[,9],corre_box[,10],corre_box[,11],corre_box[,12],
      
      corre_box[,13],corre_box[,14],corre_box[,15],corre_box[,16],
      corre_box[,17],corre_box[,18],corre_box[,19],corre_box[,20],
      corre_box[,21],corre_box[,22],corre_box[,23],corre_box[,24],
      corre_box[,25],corre_box[,26],corre_box[,27],corre_box[,28],
      
      corre_box[,29],corre_box[,30],corre_box[,31],corre_box[,32],
      corre_box[,33],corre_box[,34],corre_box[,35],corre_box[,36]
    )
  
  setEPS()
  postscript(
    file.path(
      dir_out,
      paste("BOX_ROC_DIF_spi_all.eps", sep = "")
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
    ylab="ROCSS",
    ylim = c(-0.1, 1),
    main=paste('Forecast for ERA5 against AEMET;\n three lead times (SPI6 < ' ,th,")", sep=""),
    
    col=c(rgb(0,0.3,0.1),
          rgb(0,1,1),
          rgb(0,1,0)),
    names = c('', 'January',   '',
              '', 'February',  '',
              '', 'March',     '',
              '', 'April',     '',
              '', 'May',       '',
              '', 'June',      '',
              '', 'July',      '',
              '', 'August',    '',
              '', 'September', '',
              '', 'October',   '',
              '', 'November',  '',
              '', 'Dicember',  ''
              ),
    
    at =c(1,2,3,
          7,8,9,
          13,14,15,
          19,20,21,
          25,26,27,
          31,32,33,
          37,38,39,
          43,44,45,
          49,50,51,
          55,56,57,
          61,62,63,
          67,68,69
    ))  
  
  abline(v = c(5,11,17,23,29,35,41,47,53,59,65), lty = 6,col = "gray")
  abline(h = c(0, 0.2, 0.4, 0.6, 0.8, 1.), lty = 6,col = "gray")

  legend("topleft",inset=0,
         c("4 Months","3 Months", "2 Months"), fill=c(rgb(0,0.3,0.1),
                                                      rgb(0,1,1),
                                                      rgb(0,1,0)), horiz=T, cex=1)

  dev.off()

}

