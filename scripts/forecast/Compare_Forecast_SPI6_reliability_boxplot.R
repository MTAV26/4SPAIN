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


dir_out = './4SPAIN/results/reliability/'
dir_drop = './4SPAIN/data/'


sc=6
anni = 1981:2017
mesi = rep(1:12, length(anni))
# mesi_8 = which(mesi == 08)

load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat
th=c(-0.8)
ni = length(lon)
nj = length(lat)
nb=1000
datasets=c("ERA5")

for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  
  corre_box <- matrix(data = NA,  nrow = nb, ncol = 36)

  load(paste(dir_out, "reliability_ESP_-0.8_spi_10_enero_",dataset,".RData", sep = ""))
  corre_box[,1] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_11_enero_",dataset,".RData", sep = ""))
  corre_box[,2] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_12_enero_",dataset,".RData", sep = ""))
  corre_box[,3] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_11_febrero_",dataset,".RData", sep = ""))
  corre_box[,4] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_12_febrero_",dataset,".RData", sep = ""))
  corre_box[,5] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_01_febrero_",dataset,".RData", sep = ""))
  corre_box[,6] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_12_marzo_",dataset,".RData", sep = ""))
  corre_box[,7] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_01_marzo_",dataset,".RData", sep = ""))
  corre_box[,8] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_02_marzo_",dataset,".RData", sep = ""))
  corre_box[,9] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_01_abril_",dataset,".RData", sep = ""))
  corre_box[,10] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_02_abril_",dataset,".RData", sep = ""))
  corre_box[,11] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_03_abril_",dataset,".RData", sep = ""))
  corre_box[,12] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_02_mayo_",dataset,".RData", sep = ""))
  corre_box[,13] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_03_mayo_",dataset,".RData", sep = ""))
  corre_box[,14] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_04_mayo_",dataset,".RData", sep = ""))
  corre_box[,15] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_03_junio_",dataset,".RData", sep = ""))
  corre_box[,16] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_04_junio_",dataset,".RData", sep = ""))
  corre_box[,17] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_05_junio_",dataset,".RData", sep = ""))
  corre_box[,18] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_04_julio_",dataset,".RData", sep = ""))
  corre_box[,19] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_05_julio_",dataset,".RData", sep = ""))
  corre_box[,20] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_06_julio_",dataset,".RData", sep = ""))
  corre_box[,21] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_05_august_",dataset,".RData", sep = ""))
  corre_box[,22] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_06_august_",dataset,".RData", sep = ""))
  corre_box[,23] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_07_august_",dataset,".RData", sep = ""))
  corre_box[,24] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_06_septiembre_",dataset,".RData", sep = ""))
  corre_box[,25] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_07_septiembre_",dataset,".RData", sep = ""))
  corre_box[,26] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_08_septiembre_",dataset,".RData", sep = ""))
  corre_box[,27] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_07_octubre_",dataset,".RData", sep = ""))
  corre_box[,28] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_08_octubre_",dataset,".RData", sep = ""))
  corre_box[,29] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_09_octubre_",dataset,".RData", sep = ""))
  corre_box[,30] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_08_noviembre_",dataset,".RData", sep = ""))
  corre_box[,31] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_09_noviembre_",dataset,".RData", sep = ""))
  corre_box[,32] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_10_noviembre_",dataset,".RData", sep = ""))
  corre_box[,33] = (rel2$slope1)
  
  load(paste(dir_out, "reliability_ESP_-0.8_spi_09_diciembre_",dataset,".RData", sep = ""))
  corre_box[,34] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_10_diciembre_",dataset,".RData", sep = ""))
  corre_box[,35] = (rel2$slope1)
  load(paste(dir_out, "reliability_ESP_-0.8_spi_11_diciembre_",dataset,".RData", sep = ""))
  corre_box[,36] = (rel2$slope1)
  
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
      paste("BOX_RELY_DIF_spi_all.eps", sep = "")
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
    ylab="Reliability Slope",
    ylim = c(0, 1.6),
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
              '', 'Dicember',  ''),
    
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
  abline(h = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6), lty = 6,col = "gray")
  abline(h = 1, lwd = 1)
  
  legend("topleft",inset=0,
         c("4 Months","3 Months", "2 Months"), fill=c(rgb(0,0.3,0.1),
                                    rgb(0,1,1),
                                    rgb(0,1,0)), horiz=T, cex=1)

  dev.off()
  
  
}
