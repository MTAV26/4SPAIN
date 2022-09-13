
rm(list = ls())
graphics.off()
gc()


library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(maps)
library(s2dverification)
library(ggplot2)
library(reshape)

source("./4DROP/script/Common/CorrMIO.R")
source("./4DROP/script/Common/ColorBarM.R")
source("./4DROP/script/Common/mioplot_global.R")
data(wrld_simpl)

## fixed parameters

time_scale = c(6)

anni = 1981:2017
mesi = rep(1:12, length(anni))
mesi_start = which(mesi == 1)


dir_drop = './4SPAIN/forecast/'
dir_data = './4SPAIN/data/'
dir_out2 = './4SPAIN/results/caso_estudio/'

load(file.path(dir_data, "lon_ESP_1981_2017.RData"))
load(file.path(dir_data, "lat_ESP_1981_2017.RData"))

datasets = c( 'ERA5')
timelead =c(4)
mesi_8 = which(mesi == 9)
anno_for = which(anni == 2017)
anno_case = mesi_8[anno_for]

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]
    
    for (tlead in 1:length(timelead)) {
      tmld = timelead[tlead]
      
## load dati
      load(file.path( dir_drop, paste("SPI6ESP_",tmld,"M_",dataset,".RData", sep = "") ))
      
      dim(spi6pred)
      prob = spi6pred

      
      ni = length(lon)
      nj = length(lat)
      
      anno_for = anno_case
      
      brk_prob <- seq(-0.1,1,length.out=7)
      pal.1=colorRampPalette(c("white","yellow","red","black"), space="rgb")
      col_prob=pal.1(length(brk_prob)-1)
      col_prob[1]="white"
      
      spei6obs = prob[,, anno_for,]
      obs_drought = array(data = NA, dim = c(dim(spei6obs)[1], dim(spei6obs)[2]))
  
      for (i in 1:ni) {
        for (j in 1:nj) {
          obs_drought[i,j]=sum(spei6obs[i,j,]<=-0.8,na.rm=TRUE)/dim(spei6obs)[3];
        }
      }

      ## plot obs level
      postscript(
        file.path(
          dir_out2,
          paste("SPI6_obs_", anno_case, "_level_",tmld,"M_",dataset,".eps", sep = "")
        ),
        paper = "special",
        width = 11,
        height = 7,
        horizontal = T
      )
      layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
      par(oma = c(1, 1, 4, 1))
      tit <- paste("Probability drought\n Forecast for ", dataset, "; lead time: ",tmld, " months", sep = "")
      PlotEquiMap(
        obs_drought,
        lon,
        lat,
        toptitle = '',
        sizetit = 0.8,
        brks = brk_prob,
        cols = col_prob,
        axelab =
          F,
        filled.continents = FALSE,
        drawleg = F,
        colNA = "white"
      )
      title(tit, line = 0.5, outer = T)
      ce = 1.4
      
      
      ColorBar(
        brks = brk_prob,
        cols = col_prob,
        vert = T,
        cex = 1
      )
      dev.off()


## plot obs spi
brk_new = c(-2,-1.6,-0.8,-0.5, 0, 0.5, 0.8, 1.3, 2)
brk2 = union(-1e+05, brk_new)
brk2 = union(brk2, 1e+05)
col <- (colorRampPalette(brewer.pal(11, "BrBG"))(10))

obs = apply(spi6pred, c(1,2,3), mean, na.rm=TRUE)
dim(obs)
spei6obs = obs[,, anno_for]


postscript(
  file.path(
    dir_out2,
    paste("SPI6_obs_", anno_case, "_",tmld,"M_",dataset,".eps", sep = "")
  ),
  paper = "special",
  width = 11,
  height = 7,
  horizontal = T
)
layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
par(oma = c(1, 1, 4, 1))
tit <- paste("Standardized index\n Forecast for ", dataset, "; lead time: ",tmld, "months", sep = "")
PlotEquiMap(
  spei6obs,
  lon,
  lat,
  toptitle = '',
  sizetit = 0.8,
  brks = brk2,
  cols = col,
  axelab =
    F,
  filled.continents = FALSE,
  drawleg = F,
  colNA = "white"
)
title(tit, line = 0.5, outer = T)
ce = 1.4

ColorBarM(
  brks = brk2,
  cols = col,
  vert = T,
  cex = 1,
  labs = seq(2, length(brk2) - 1, 1)
)
dev.off()

    }
  }
}

