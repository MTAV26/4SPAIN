
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
library(pracma)
library(verification)
library(psych)
library(Metrics)

source("./4DROP/script/Common/CorrMIO.R")
source("./4DROP/script/Common/ColorBarM.R")
source("./4DROP/script/Common/mioplot_global.R")

## fixed parameters
#time_scale = c(1, 3, 6, 12)
time_scale = c(6)

anni = 1981:2017
mesi = rep(1:12, length(anni))
mesi_start = which(mesi == 1)

dir_drop = './4SPAIN/data/'
dir_out2 = './4SPAIN/results/observation/mae/'

brk_cor <- seq(0, 1, 0.1)
col_cor <- (colorRampPalette(rev(brewer.pal(11, "PiYG")))(10))

load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

datasets = c( 'ERA5', 'CHIRPS', 'EOBS')

mesi_8 = which(mesi == 1)
for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi6", sep = "")
  load(file.path(dir_drop,paste("SPI", sc, "_AEMET_1981_2017.RData", sep = "")))

  ens = spi6[,,mesi_8]
  ens[is.infinite(ens)]=NA
  ens[is.na(ens)]=NA

  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]

    nam <- paste("spi",sc,"pred", sep = "")
    print(nam)
    
    if (dataset == "EOBS") {
      load(file.path(dir_drop,paste('SPI',sc,"_EOBS_1981_2017.RData", sep = "")))
    } else if (dataset == "CHIRPS") {
      load(file.path(dir_drop, paste('SPI',sc,"_CHIRPS_1981_2017.RData", sep = "" )))
    } else if (dataset == "ERA5") {
      load(file.path(dir_drop, paste('SPI',sc,"_ERA5_1981_2017.RData", sep = "" ) ))
      
    } else {
      print('dataset not known')
    }
    
    data = spi6[,,mesi_8]
    data[is.infinite(data)]=NA
    data[is.na(data)]=NA
    
    
    ni = dim(data)[1]
    nj = dim(data)[2]
    nt = dim(data)[3]
    
    corre <- matrix(data = NA,nrow = ni, ncol = nj)

    data[is.infinite(data)]=NA
    
    for (i in 1:ni) {
      for (j in 1:nj) {
        
        OK <- complete.cases(ens[i, j,], data[i, j,])
        x <- ens[i, j, OK]
        y <- data[i, j, OK]
        n <- length(x)
        #if (n >= anniok * 12) {
        if (n >= nt*0.9) {

          corre[i, j]=mae(x,y)
          rm(dum)
          
        }
        
        rm(OK, n, x, y#, x1d, y1d, x1, y1, n1
        )
      }
    }
    
    postscript(
      file.path(dir_out2,paste("MAE_OBS_spi",sc,"_",sprintf("%02d", mesi_8[1]),"_",dataset,".eps",sep = "")),
      paper = "special",
      width = 11,
      height = 7,
      horizontal = T
    )
    
    layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
    par(oma = c(1, 1, 4, 1))
    tit <-
      paste(
        'MAE for SPI6 in ', dataset, ' against AEMET', sep=""
      )
    
    mioplot(corre, lon,lat,
            toptitle = '',sizetit = 0.8,
            brks = brk_cor,  cols = col_cor,
            axelab =F, filled.continents = FALSE,
            drawleg = F #,  
    )
    title(tit, line = 0.5, outer = T)
    ce = 1.4
    ColorBar(
      brks = brk_cor,
      cols = col_cor,
      vert = T
    )
    
    save(corre, file = file.path(dir_out2, paste("MAE_OBS_spi",sc,"_",sprintf("%02d", mesi_8[1]),"_",dataset,".RData", sep = "") ))
    dev.off()
  
  }
}

