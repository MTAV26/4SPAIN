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

source("./4DROP/script/Common/CorrMIO.R")
source("./4DROP/script/Common/ColorBarM.R")
source("./4DROP/script/Common/mioplot_global.R")


## fixed parameters
time_scale = c(6)

anni = 1981:2017
mesi = rep(1:12, length(anni))
mesi_start = which(mesi == 1)

dir_drop = './4SPAIN/data/'
dir_4drop = './4SPAIN/forecast/'
dir_out2 = './4SPAIN/results/cor/'


load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))

datasets = c( 'ERA5')
start_dates = c(2)
mesi_8 = which(mesi == 11)

brk <- seq(-1, 1, 0.2)
col <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  
  nam <- paste("spi6", sep = "")
  load(file.path(dir_drop,paste("SPI", sc, "_AEMET_1981_2017.RData", sep = "")))
  ens = spi6[,,mesi_8]

  ens[is.infinite(ens)]=NA
  ens[is.na(ens)]=NA

  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    for (istart_date in 1:length(start_dates)) {
      start_date = start_dates[istart_date]
      
      nam <- paste("spi",sc,"pred", sep = "")
      print(nam)
      
      
      if (dataset == "EOBS") {
        load(file.path(dir_4drop,paste('SPI',sc,'ESP_',start_date ,"M_EOBS.RData", sep = "")))
      } else if (dataset == "CHIRPS") {
        load(file.path(dir_4drop, paste('SPI',sc,'ESP_',start_date ,"M_CHIRPS.RData", sep = "" )))
      } else if (dataset == "ERA5") {
        load(file.path(dir_4drop, paste('SPI',sc,'ESP_',start_date ,"M_ERA5.RData", sep = "" ) ))
        
      } else {
        print('dataset not known')
      }

      data = apply(spi6pred[,,mesi_8,], c(1,2,3), mean, na.rm=TRUE)
      data[is.infinite(data)]=NA
      data[is.na(data)]=NA

      ni = dim(data)[1]
      nj = dim(data)[2]
      nt = dim(data)[3]
      
      corre <- matrix(data = NA,nrow = ni, ncol = nj)
      pvalue <- matrix(data = NA, nrow = ni, ncol = nj)
      
      corre_det <- matrix(data = NA,nrow = ni, ncol = nj)
      pvalue_det <- matrix(data = NA, nrow = ni, ncol = nj)

      data[is.infinite(data)]=NA
      
      for (i in 1:ni) {
        for (j in 1:nj) {
          
          OK <- complete.cases(ens[i, j,], data[i, j,])
          x <- ens[i, j, OK]
          y <- data[i, j, OK]
          n <- length(x)
          #if (n >= anniok * 12) {
          if (n >= nt*0.9) {
            
            dum = CorrMIO((y), (x), method = 'pearson', pval = TRUE)
            corre[i, j] = as.numeric(dum)[1]
            pvalue[i, j] <- as.numeric(dum)[4]
            rm(dum)
            
          }
          
          OK1 <- complete.cases(ens[i, j,], data[i, j,])
          x1 <- ens[i, j, OK1]
          y1 <- data[i, j, OK1]
          n1 <- length(x1)
          #if (n >= anniok * 12) {
          if (n1 >= nt*0.9) {
            
            x1d = as.vector(detrend(x1, tt = 'linear', bp = c()))
            y1d = as.vector(detrend(y1, tt = 'linear', bp = c()))
            
            dum = CorrMIO((y1d), (x1d), method = 'pearson', pval = TRUE)
            corre_det[i, j] = as.numeric(dum)[1]
            pvalue_det[i, j] <- as.numeric(dum)[4]
            rm(dum)
          }
          
          rm(OK, n, x, y, x1d, y1d, x1, y1, n1)
        }
      }
      pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
      pvalue_adj = matrix(pvalue_adj, nrow = ni, ncol = nj)

      pvalue_adj_det = p.adjust(pvalue_det, method = "fdr", n = length(pvalue_det[!is.na(pvalue_det)]))
      pvalue_adj_det = matrix(pvalue_adj_det, nrow = ni, ncol = nj)

      
      if (dataset == "EOBS") {
        dat = "EOBS"
      }else if (dataset == "ERA5") {
        dat = "ERA5"
      } else if (dataset =="CHIRPS") {
        dat = "CHIRPS"
      }

      postscript(
        file.path(dir_out2,paste("COR_ESP_spi",sc,"_",sprintf("%02d", mesi_8[1]),
                                 "_", start_date, "M_",dataset,"_original.eps",sep = "")),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )


      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
          'Correlation for SPI6 in ', dat, ' (ESP) against AEMET \n Start date: ',  
          month.name[mesi_8[1]-1],'; Forecast date: ', month.name[mesi_8[1]], sep=""
        )

      mioplot(corre, lon,lat,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F,
              #dots = pvalue <= 0.05,
              dots2 = pvalue_adj <= 0.05
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBar(
        brks = brk,
        cols = col,
        vert = T
      )

      save(corre, file = file.path(dir_out2, paste("COR_ESP_spi",sc,"_",sprintf("%02d", mesi_8[1]),
                                                   "_",start_date, "M_",dataset,"_original.RData", sep = "") ))
      save(pvalue_adj, file = file.path(dir_out2, paste("PVALUE_ESP_spi",sc, "_",sprintf("%02d", mesi_8[1]),
                                                        "_",start_date, "M_",dataset,"_original.RData", sep = "") ))

      dev.off()
      
      nam1 <- round(mean(corre, na.rm=TRUE),3)
      print(nam1)
      
    }
  }
}
