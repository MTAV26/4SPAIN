
rm(list = ls())
graphics.off()
gc()

#library(ncdf)
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


source("~/4DROP/script/Common/CorrMIO.R")
source("~/4DROP/script/Common/ColorBarM.R")
source("~/4DROP/script/Common/mioplot_global.R")


## fixed parameters
#time_scale = c(1, 3, 6, 12)
time_scale = c(6)

anni = 1981:2017
mesi = rep(1:12, length(anni))

mesi_start = which(mesi == 1)
mesi_8 = which(mesi == 2)
 
dir_drop = '~/SPAIN2017/data/'
dir_4drop = '~/SPAIN2017/forecast/'
dir_out2 = '~/SPAIN2017/results/cor/'
 
 
# mesi_8 = which(mesi == 02)

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))


load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat

#mesi= 1:40
datasets = c( 'ERA5', 'CHIRPS', 'EOBS')
# datasets = c( 'ERA5')
# datasets = c('ENS')

# par(oma=c( 0,0,0,4)) # margin of 4 spaces width at right hand side
# set.panel( 4,3) #


brk <- seq(-1, 1, 0.2)
col <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))
#datasets = c('ENS')
## load gpcp
# start_dates = c(5, 7, 8, 10, 2, 4, 1, 11)
start_dates = c(4,3,2)

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]

  
  nam <- paste("spi6", sep = "")
  load(file.path(dir_drop,paste("SPI", sc, "_AEMET_1981_2017.RData", sep = "")))
  ens = spi6[,,mesi_8]
  #ens = spi6
  ens[is.infinite(ens)]=NA
  ens[is.na(ens)]=NA
  # for (std in 1:length(m)) {
  #   start_date = m[std]
  # }
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    for (istart_date in 1:length(start_dates)) {
      start_date = start_dates[istart_date]
      
      # # dates = seq(start_date, start_date + 3)
      # if (start_date == 5) {
      #   target_season = 'JJA'
      # } else if (start_date == 7) {
      #   target_season = 'JJA'
      # } else if (start_date == 8) {
      #   target_season = 'SON'
      # } else if (start_date == 10) {
      #   target_season = 'SON'
      # } else if (start_date == 2) {
      #   target_season = 'MAM'
      # } else if (start_date == 4) {
      #   target_season = 'MAM'
      # } else if (start_date == 1) {
      #   target_season = 'DJF'
      # } else if (start_date == 11) {
      #   target_season = 'DJF'
      # }
      
      
      # 
      # if (target_season == 'MAM') {
      #   mesi_8 = which(mesi== 05)
      # } else if (target_season == 'JJA') {
      #   mesi_8 = which(mesi == 08)
      # } else if (target_season == 'SON') {
      #   mesi_8 = which(mesi == 11)
      # } else if (target_season == 'DJF') {
      #   mesi_8 = which(mesi == 02)
      # }
      
  
      #nam <- paste("spi", sc, "pred", sep = "")
      #print(nam)
      #load(file.path(dir_drop,paste("FORDROP", sc, "_ENS_1981_2020.RData", sep = "")))
      #ens = get(nam)
      #ens = ens[,,mesi]
      #dim(ens)
      
      nam <- paste("spi",sc,"pred", sep = "")
      print(nam)
      
    
    # for (me in 1:length(mes)) {
    #   ms = mes[me]
    #   
    #   if (ms == "11") {
    #     start_date = 11
    #   } else if (ms == "01") {
    #     start_date = 1
    #   } else {
    #     print('dataset not known')
    #   }
    #   
      
      if (dataset == "EOBS") {
        load(file.path(dir_4drop,paste('SPI',sc,'ESP_',start_date ,"M_EOBS.RData", sep = "")))
      } else if (dataset == "CHIRPS") {
        load(file.path(dir_4drop, paste('SPI',sc,'ESP_',start_date ,"M_CHIRPS.RData", sep = "" )))
      } else if (dataset == "ERA5") {
        load(file.path(dir_4drop, paste('SPI',sc,'ESP_',start_date ,"M_ERA5.RData", sep = "" ) ))

      } else {
        print('dataset not known')
      }
      
      
      
      data = spi6pred
      data[is.infinite(data)]=NA
      
      
      data = apply(spi6pred[,,mesi_8,], c(1,2,3), mean, na.rm=TRUE)
      data[is.infinite(data)]=NA
      data[is.na(data)]=NA

      
       # if (start_date == "2") {
       #   data = apply(spi6pred[,,,1:35, c(1,2,3), mean, na.rm=TRUE)
       # } else if (dataset == "ENS") {
       #   data = spi6pred
       # } else {
       #   print('dataset not known')
       # }
      
      #nam <- paste("spi", sc, sep = "")
      #print(nam)
      #data = spi6[,,mesi_8]
      #data[is.infinite(data)]=NA
      #data = get(nam)
      #data = data[,,mesi_8]
      
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
            
            dum = CorrMIO((x), (y), method = 'pearson', pval = TRUE)
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
            
            dum = CorrMIO((x1d), (y1d), method = 'pearson', pval = TRUE)
            corre_det[i, j] = as.numeric(dum)[1]
            pvalue_det[i, j] <- as.numeric(dum)[4]
            rm(dum)
          }
          
          rm(OK, n, x, y, x1d, y1d, x1, y1, n1)
        }
      }
      pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
      pvalue_adj = matrix(pvalue_adj, nrow = ni, ncol = nj)
      # 
      # lat2 = lat[which(lat > -60 & lat < 85)]
      # corre2 = corre[, which(lat > -60 & lat < 85)]
      # pvalue2 = pvalue[, which(lat > -60 & lat < 85)]
      # pvalue_adj2 = pvalue_adj[, which(lat > -60 & lat < 85)]
      # 
      
      
      pvalue_adj_det = p.adjust(pvalue_det, method = "fdr", n = length(pvalue_det[!is.na(pvalue_det)]))
      pvalue_adj_det = matrix(pvalue_adj_det, nrow = ni, ncol = nj)
      # 
      # lat2 = lat[which(lat > -60 & lat < 85)]
      # corre2_det = corre_det[, which(lat > -60 & lat < 85)]
      # pvalue2_det = pvalue_det[, which(lat > -60 & lat < 85)]
      # pvalue_adj2_det = pvalue_adj_det[, which(lat > -60 & lat < 85)]
      
      
      ## plot 1
      
      postscript(
        file.path(dir_out2,paste("COR_ESP_spi",sc,"_",sprintf("%02d", mesi_8[1]),"_", start_date, "M_",dataset,"_original.eps",sep = "")),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      
      
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
          'Correlation for SPI6 in ', dataset, ' (ESP)  against AEMET', sep=""
        )
      
          #\n Start date: ',
          #month.name[ start_date],
          ##' - Period: 1981/2020 \n (Original data; points: p<0.05 )', sep=""
      #  )
      
      
      mioplot(corre, lon,lat,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F,  
              dots = pvalue <= 0.05,
              dots2 = pvalue_adj <= 0.05 
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBar(
        brks = brk,
        cols = col,
        vert = T
      )
      
      save(corre, file = file.path(dir_out2, paste("COR_ESP_spi",sc,"_",sprintf("%02d", mesi_8[1]),"_",start_date, "M_",dataset,"_original.RData", sep = "") ))
      save(pvalue_adj, file = file.path(dir_out2, paste("PVALUE_ESP_spi",sc, "_",sprintf("%02d", mesi_8[1]),"_",start_date, "M_",dataset,"_original.RData", sep = "") ))
      
      dev.off()
      
      
      ## plot 2
      
      # brk <- seq(-1, 1, 0.2)
      # col <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))
      # brk_cor <- seq(-1, 1, 0.2)
      # col_cor <- (colorRampPalette(brewer.pal(11, "PiYG"))(10))
      
      postscript(
        file.path(dir_out2,paste("COR_ESP_spi",sc,"_",sprintf("%02d", mesi_8[1]),"_",start_date, "M_",dataset,"_detrended.eps",sep = "")),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      
      
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <-
        paste(
          'Correlation for SPI6 in ',dataset, ' (ESP) against AEMET', sep="")
          
          #\n Start date: ',
          #month.name[ start_date],
          #' - Period: 1981/2020\n (Detrended data; points: p<0.05 )', sep=""
        #)
      
      #dev.off()
      mioplot(corre_det, lon,lat,
              toptitle = '',sizetit = 0.8,
              brks = brk_cor,  cols = col_cor,
              axelab =F, filled.continents = FALSE,
              drawleg = F,  
              dots = pvalue_det <= 0.05,
              dots2 = pvalue_adj_det <= 0.05 
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      ColorBar(
        brks = brk,
        cols = col,
        vert = T
      )
      
      # 
      # segments(x0 = 36,
      #          x1 = 20,
      #          y0 = -0.2,
      #          y1 = 1.05,
      #          lwd = 2,
      #          col = "red",
      #          lty = 2:3) 
      # 
      
      
      
      
      
      save(corre_det, file = file.path(dir_out2, paste("COR_ESP_spi",sc,"_",sprintf("%02d", mesi_8[1]),"_",start_date, "M_",dataset,"_detrended.RData", sep = "") ))
      save(pvalue_adj_det, file = file.path(dir_out2, paste("PVALUE_ESP_spi",sc,"_",sprintf("%02d", mesi_8[1]),"_",start_date, "M_",dataset,"_detrended.RData", sep = "") ))
      dev.off()
    }
  }
}

#sprintf("%02d", mesi_8[1]) #selecciona el primero numero del mes 8 para la serie temporal.




dim(data)
image(data[,,6])
