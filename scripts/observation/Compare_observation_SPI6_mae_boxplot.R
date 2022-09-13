rm(list = ls())
graphics.off()
gc()

library(StatDA)

source("./4DROP/script/Common/CorrMIO.R")
source("./4DROP/script/Common/ColorBarM.R")
source("./4DROP/script/Common/mioplot_global.R")
source("./4DROP/script/Common/my_boxplot_stat.R")
source("./4DROP/script/Common/my_boxplot.R")

## fixed parameters
#time_scale = c(1, 3, 6, 12)
time_scale = c(6)

dir_drop = './4SPAIN/data/'
dir_out = './4SPAIN/results/observation/mae/'

sc=6
anni = 1981:2017
mesi = rep(1:12, length(anni))
mesi_8 = which(mesi == 08)

load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
load(file.path(dir_drop, "inout.RData"))

ni = length(lon)
nj = length(lat)

datasets=c("EOBS", "ERA5", "CHIRPS")

# for (idata in 1:length(datasets)) {
#   dataset = datasets[idata]
#   
{ corre_box <-
    matrix(data = NA,
           nrow = ni * nj,
           ncol = 36)
  dim(corre_box)
  load(paste(dir_out, "MAE_OBS_spi6_01_CHIRPS.RData", sep = ""))
  corre_box[,1] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_01_EOBS.RData", sep = ""))
  corre_box[,2] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_01_ERA5.RData", sep = ""))
  corre_box[,3] = as.vector(corre)
  
  load(paste(dir_out, "MAE_OBS_spi6_02_CHIRPS.RData", sep = ""))
  corre_box[,4] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_02_EOBS.RData", sep = ""))
  corre_box[,5] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_02_ERA5.RData", sep = ""))
  corre_box[,6] = as.vector(corre)

  load(paste(dir_out, "MAE_OBS_spi6_03_CHIRPS.RData", sep = ""))
  corre_box[,7] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_03_EOBS.RData", sep = ""))
  corre_box[,8] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_03_ERA5.RData", sep = ""))
  corre_box[,9] = as.vector(corre)

  load(paste(dir_out, "MAE_OBS_spi6_04_CHIRPS.RData", sep = ""))
  corre_box[,10] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_04_EOBS.RData", sep = ""))
  corre_box[,11] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_04_ERA5.RData", sep = ""))
  corre_box[,12] = as.vector(corre)

  load(paste(dir_out, "MAE_OBS_spi6_05_CHIRPS.RData", sep = ""))
  corre_box[,13] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_05_EOBS.RData", sep = ""))
  corre_box[,14] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_05_ERA5.RData", sep = ""))
  corre_box[,15] = as.vector(corre)

  load(paste(dir_out, "MAE_OBS_spi6_06_CHIRPS.RData", sep = ""))
  corre_box[,16] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_06_EOBS.RData", sep = ""))
  corre_box[,17] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_06_ERA5.RData", sep = ""))
  corre_box[,18] = as.vector(corre)

  load(paste(dir_out, "MAE_OBS_spi6_07_CHIRPS.RData", sep = ""))
  corre_box[,19] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_07_EOBS.RData", sep = ""))
  corre_box[,20] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_07_ERA5.RData", sep = ""))
  corre_box[,21] = as.vector(corre)

  load(paste(dir_out, "MAE_OBS_spi6_08_CHIRPS.RData", sep = ""))
  corre_box[,22] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_08_EOBS.RData", sep = ""))
  corre_box[,23] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_08_ERA5.RData", sep = ""))
  corre_box[,24] = as.vector(corre)
  
  load(paste(dir_out, "MAE_OBS_spi6_09_CHIRPS.RData", sep = ""))
  corre_box[,25] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_09_EOBS.RData", sep = ""))
  corre_box[,26] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_09_ERA5.RData", sep = ""))
  corre_box[,27] = as.vector(corre)

  load(paste(dir_out, "MAE_OBS_spi6_10_CHIRPS.RData", sep = ""))
  corre_box[,28] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_10_EOBS.RData", sep = ""))
  corre_box[,29] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_10_ERA5.RData", sep = ""))
  corre_box[,30] = as.vector(corre)
 
  load(paste(dir_out, "MAE_OBS_spi6_11_CHIRPS.RData", sep = ""))
  corre_box[,31] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_11_EOBS.RData", sep = ""))
  corre_box[,32] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_11_ERA5.RData", sep = ""))
  corre_box[,33] = as.vector(corre)

  load(paste(dir_out, "MAE_OBS_spi6_12_CHIRPS.RData", sep = ""))
  corre_box[,34] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_12_EOBS.RData", sep = ""))
  corre_box[,35] = as.vector(corre)
  load(paste(dir_out, "MAE_OBS_spi6_12_ERA5.RData", sep = ""))
  corre_box[,36] = as.vector(corre)
  
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
      paste("BOX_MAE_DIF_spi_all.eps", sep = "")
      #paste("boxplot_corre_spi_", sc, "_ENS.eps", sep = "")
    ),
    horiz = FALSE,
    onefile = FALSE,
    width = 8.5,
    height = 5.5
  )
  par(mar = c(10, 4, 2, 1))
  boxplotperc(
    na.omit(plot_data),
    quant = c(0.025, 0.975),
    outline = FALSE,
    las = 2,
    ylim = c(0, 1),
    
    ylab="MAE",
    main=paste('Data observed against AEMET;', sep=""),
    
    col = rep(c('#f7fcb9', '#addd8e', '#31a354'),36),
    
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
  abline(h = c(-0.2,0, 0.2, 0.4, 0.6, 0.8,1.0), lty = 6,col = "gray")

  legend("bottomleft",inset=0,
         c("CHIRPS","EOBS", "ERA5"), fill=c('#f7fcb9', '#addd8e', '#31a354'), horiz=T, cex=1, col ="white")
  
  dev.off()

}
