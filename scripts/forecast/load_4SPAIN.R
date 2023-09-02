#script 4

rm(list = ls())
graphics.off()
gc()

library(ncdf4)
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
data(wrld_simpl)

## fixed parameters
time_scale = c(6)

anni = 1979:2022
mesi = rep(1:12, length(anni))
mesi_start = which(mesi == 1)


dir_drop = './4SPAIN/ERA5_2022/forecast/'
dir_out2 = './4SPAIN/ERA5_2022/forecast'


load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))
load(file.path(dir_drop, "inout.RData"))


som <- function(x) {
  as.Date(format(x, "%Y-%m-01"))
}
previous_month=as.numeric(format(som(Sys.Date()) - 1,"%m"))+3
previous_year=as.numeric(format(som(Sys.Date()) - 1,"%Y"))

last_month = ((previous_year - 1979 + 1) * 12) - (12 - previous_month)

## fix parameters

datasets= c("ERA5")
## load data

timelead=c(2,3,4)

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    for (tlead in 1:length(timelead)) {
      tmld = timelead[tlead]
      
      load(file.path(paste(dir_drop, "SPI", sc, "ESP_",tmld,"M_",dataset,".RData", sep = "") ))
####################################################################################################
#SPI
      #####################################################
      # PALETA SPI MEAN
      #####################################################
       brk_new = c(-2,-1.6,-0.8,-0.5, 0, 0.5, 0.8, 1.3, 2)
       brk2 = union(-1e+05, brk_new)
       brk2 = union(brk2, 1e+05)
       col <- (colorRampPalette(brewer.pal(11, "BrBG"))(10))

      # brk_col <- seq(-2, 2, length.out = 12)
      # cols <-(colorRampPalette(brewer.pal(length(brk_col), "BrBG"))(length(brk_col) - 1))
      ######################################################
       spi_mean=apply(spi6pred, c(1,2,3), mean, na.rm = TRUE)

       spi = apply(spi6pred, c(1,2,3), mean, na.rm=TRUE)
       spi[spi < -2] = -2
       spi[spi > 2] = 2
       spi_mean=spi

       image.plot(lon, lat,  spi_mean[,,524], col = col, breaks = brk_new)
      #image.plot(lon, lat,  spi_mean[,,524])
      save(spi_mean, file = paste0(dir_drop, "/SPI_MEAN_", sc, "_ESP_",tmld,"M_",dataset,".RData"))


      pred = spi_mean
####################################################################################################
# SPREAD
      ####################################################
      #PALETA DE COLOR DE SD
      ####################################################
      brk_spread <- seq(0, 1, length.out = 6)
      cols_spread <- brewer.pal(5, "BuPu")
      ###################################################


      spi_sd = apply(spi6pred, c(1, 2,3), sd, na.rm = TRUE)
      image.plot(lon, lat, spi_sd[,,524], col = cols_spread, breaks = brk_spread)
      save(spi_sd, file = paste0(dir_drop, "/SPI_SPREAD_", sc, "_ESP_",tmld,"M_",dataset,".RData"))
###################################################################################################
# PROBABILITY

      ####################################################
      #PALETA DE COLOR DE PROBABILITY
      ####################################################
      brk_prob <- seq(0, 1, length.out = 6)
      col_prob <-
        (colorRampPalette(brewer.pal(length(brk_prob), "YlOrBr"))(length(brk_prob) -
                                                                    1))
      col_prob[1] = "#C0C0C0"
      ####################################################

      spei6obs = spi6pred
      obs_drought = array(data = NA, dim = c(dim(spei6obs)[1], dim(spei6obs)[2], dim(spei6obs)[3]))
      dim(pred)
      dim(spei6obs)
      for (im in 1:dim(pred)[3]) {
        for (i in 1:dim(pred)[1]) {
          for (j in 1:dim(pred)[2]) {

            obs_drought[i,j,im]=sum(spei6obs[i,j,im,]<=-0.8,na.rm=TRUE)/dim(spei6obs)[4];
          }
        }

        obs_drought[, , im]=obs_drought[, , im]*inout
      }

      spi_prob=obs_drought

      
      if(tmld == 2) {
        spi_prob[,,(last_month-1)][spi_prob[,,(last_month-1)] == 0] = NA
        spi_prob[,,(last_month)][spi_prob[,,(last_month)] == 0] = NA
      }else if (tmld == 3){
        spi_prob[,,(last_month)][spi_prob[,,(last_month)] == 0] = NA
      }

      
      image.plot(lon, lat,  spi_prob[,,524], col = col_prob, breaks = brk_prob)

      save(spi_prob, file = paste0(dir_drop, "/SPI_PROB_", sc, "_ESP_",tmld,"M_",dataset,".RData"))
    
      
###########################################
#WARNING LEVELS
      ###################################################
      #PALETA DE COLOR DE WARNING LEVELS
      ###################################################
      cols_tl=c('#FFFF00','#FFA500','#FF0000')
      brk_tl <- seq(1, 4, length.out = 4)
      ###################################################

      spi_tl = array(data = NA, dim = c(length(lon), length(lat), dim(spi6pred)[3]))
      for (im in 1:dim(spi6pred)[3]) {
        for (i in 1:dim(spi6pred)[1]) {
          for (j in 1:dim(spi6pred)[2]) {
            aux = spi6pred[i, j, im,]
            if (sum(!is.na(aux))!=0) {
              if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0 &
                  sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) <= 0.25)  {
                spi_tl[i, j, im] = 2 #yellow code
              } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0.25 &
                         sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) <= 0.75)  {
                spi_tl[i, j, im] = 3 #orange code
              } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux)) > 0.75)  {
                spi_tl[i, j, im] = 4 #red code
              } else  if (sum(aux[!is.na(aux)] > -1.30 &
                              aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) > 0 &
                          sum(aux[!is.na(aux)] > -1.30 &
                              aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) <= 0.5)  {
                spi_tl[i, j, im] = 2 #yellow code
              } else if (sum(aux[!is.na(aux)] > -1.30 &
                             aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)) > 0.5)  {
                spi_tl[i, j, im] = 3 #orange code
              } else if (sum(aux[!is.na(aux)] > -0.8 &
                             aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) > 0.5)  {
                spi_tl[i, j, im] = 2 #yellow code
              } else  if (sum(aux[!is.na(aux)] > -0.8 &
                              aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) > 0 &
                          sum(aux[!is.na(aux)] > -0.8 &
                              aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux)) <= 0.5)  {
                spi_tl[i, j, im] = 1 #green code
              } else  {
                spi_tl[i, j, im] = 1 #green code
              }
            }
          }
        }
        spi_tl[, , im] = spi_tl[, , im] * inout
      }
      # spi_tl[spi_tl== 0] = NA
      image.plot(lon, lat,  spi_tl[,,525], col = cols_tl, breaks = brk_tl, label= c("Hight", "Medium", "Low"))
      save(spi_tl, file = paste0(dir_drop, "/SPI_TRAF_LIG_", sc,"_ESP_",tmld,"M_",dataset,".RData"))
    }
  }
}

