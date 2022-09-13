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

dir_drop = './4SPAIN/data/'
dir_out2 = './4SPAIN/results/caso_estudio/'
load(file.path("./4SPAIN/forecast/SPI6ESP_4M_ERA5.RData" ))
pred=apply(spi6pred, c(1,2), mean, na.rm = TRUE)
image.plot(lon, lat, pred)

load(file.path(dir_drop, "lon_ESP_1981_2017.RData"))
load(file.path(dir_drop, "lat_ESP_1981_2017.RData"))

# spi_sd = apply(pred, c(1, 2), sd, na.rm = TRUE)
# save(spi_sd, file = paste0(dir_drop, "/SPI", sc, "_",dataset,"_SPREAD_1981_2017.RData"))

points <- expand.grid(lon, lat)
pts = SpatialPoints(points, proj4string = CRS(proj4string(wrld_simpl)))
ii <- !is.na(over(pts, wrld_simpl))
inout = ii[, 1]
dim(inout) <- c(nrow(pred), ncol(pred))
inout[inout == 0] = NA

image.plot(lon, lat, inout)

datasets = c( 'AEMET')

mesi_8 = which(mesi == 09)
anno_for = which(anni == 2017)
anno_case = mesi_8[anno_for]


for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  for (idata in 1:length(datasets)) {
    dataset = datasets[idata]

  ## load dati
  load(file.path( dir_drop, paste("SPI6_AEMET_1981_2017.RData", sep = "") ))
  obs = spi6

  ni = length(lon)
  nj = length(lat)

  ## select case study
  ilon = lon
  ilat = lat
  anno_for = anno_case

  spei6obs = obs[,, anno_for]
  obs_drought = spei6obs*NA

   nam1 <- round(((sum (spei6obs <= -1.3, na.rm = TRUE)/sum(spei6obs != "NA", na.rm = TRUE))*100),2)
   print(nam1)

  obs_drought[(spei6obs > -0.5)] = 0
  obs_drought[(spei6obs <= -0.5) & (spei6obs > -0.8)] = 1
  obs_drought[(spei6obs <= -0.8) & (spei6obs > -1.3)] = 2
  obs_drought[(spei6obs <= -1.3) & (spei6obs > -1.6)] = 3
  obs_drought[(spei6obs <= -1.6) & (spei6obs > -2)] = 4
  obs_drought[(spei6obs <= -2)] = 5
  
  brk <- seq(-1, 5, length.out = 7)
  pal.1 = colorRampPalette(c("yellow", "red", "black"), space = "rgb")
  col = pal.1(length(brk) - 1)
  col[1] = "#C0C0C0"

  ## plot obs level
  postscript(
    file.path(
      dir_out2,
      paste("SPI6_obs_", anno_case, "_level.eps", sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <-  ("Drought condition;\n Observed AEMET")
  PlotEquiMap(
    obs_drought,
    lon,
    lat,
    toptitle = '',
    sizetit = 0.8,
    brks = brk,
    cols = col,
    axelab =
      F,
    filled.continents = FALSE,
    drawleg = F,
    colNA = "white"
  )
  title(tit, line = 0.5, outer = T)
  ce = 1.4
  
  
  ColorBar(
    brks = brk,
    cols = col,
    vert = T,
    cex = 1
  )
  dev.off()
  
  ## plot obs spi
  brk_new = c(-2,-1.6,-0.8,-0.5, 0, 0.5, 0.8, 1.3, 2)
  brk2 = union(-1e+05, brk_new)
  brk2 = union(brk2, 1e+05)
  col <- (colorRampPalette(brewer.pal(11, "BrBG"))(10))
  

  
  postscript(
    file.path(
      dir_out2,
      paste("SPI6_obs_", anno_case, ".eps", sep = "")
    ),
    paper = "special",
    width = 11,
    height = 7,
    horizontal = T
  )
  layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  par(oma = c(1, 1, 4, 1))
  tit <-  ("Standardized index;\n Observed AEMET")
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
  
  