
rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(sp)
library(maptools) # loads sp library too
library(fields)
library(maps)
library(SPEI)
library(RColorBrewer)
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
library(sf)
library(oce)
library(raster)
library(ggplot2)
library(terra)
library(raster)
library(graticule)
library(rgdal)
library(rworldmap)
library(rnaturalearth)

data(wrld_simpl)

anni = 1981:2017
mesi = rep(1:12, length(anni))


mesi_1= which(mesi == 1)
mesi_2= which(mesi == 2)
mesi_3 = which(mesi == 3)
mesi_4 = which(mesi == 4)
mesi_5 = which(mesi == 5)
mesi_6 = which(mesi == 6)
mesi_7 = which(mesi == 7)
mesi_8 = which(mesi == 8)
mesi_9 = which(mesi == 9)
mesi_10 = which(mesi == 10)
mesi_11 = which(mesi == 11)
mesi_12= which(mesi == 12)

spring= cbind(mesi_3, mesi_4, mesi_5)
summer= cbind(mesi_6, mesi_7, mesi_8)
autumn= cbind(mesi_9, mesi_10, mesi_11)
winter= cbind(mesi_12, mesi_1, mesi)# Utilizar script A0_winter

dir_oss = '~/Chapter3-4SPAIN/data/'
dir_drop = '~/Chapter3-4SPAIN/data/'
dir_4drop = '~/Chapter3-4SPAIN/data/'
dir_out2 = '~/Chapter3-4SPAIN/Figure/'

fname <- file.path(dir_oss, 'AEMET_025_spain_1981_2017.nc')
obs.nc <- nc_open(fname)
obs = ncvar_get(obs.nc, "precipitation")
lat = ncvar_get(obs.nc, "latitude")
lat=rev(lat)
lon = ncvar_get(obs.nc, "longitude")
obs[obs == "-32767s"] <- NA
obs=obs[,dim(obs)[2]:1,]

pr_aemet=obs

dic= apply(pr_aemet[,,mesi_12], c(1, 2), mean)
ene= apply(pr_aemet[,,mesi_1], c(1, 2), mean)
feb= apply(pr_aemet[,,mesi_2], c(1, 2), mean)

mar= apply(pr_aemet[,,mesi_3], c(1, 2), mean)
apr= apply(pr_aemet[,,mesi_4], c(1, 2), mean)
may= apply(pr_aemet[,,mesi_5], c(1, 2), mean)

jun= apply(pr_aemet[,,mesi_6], c(1, 2), mean)
jul= apply(pr_aemet[,,mesi_7], c(1, 2), mean)
aug= apply(pr_aemet[,,mesi_8], c(1, 2), mean)

sep= apply(pr_aemet[,,mesi_9], c(1, 2), mean)
oct= apply(pr_aemet[,,mesi_10], c(1, 2), mean)
nov= apply(pr_aemet[,,mesi_11], c(1, 2), mean)

aemet_winter <-dic+ene+feb
aemet_spring <-mar+apr+may
aemet_summer <-jun+jul+aug
aemet_autumn <-sep+oct+nov

data(coastlineWorld)
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
worldmap <- rworldmap::getMap(resolution = "coarse") # Load countries 
raster::crs(worldmap)
worldmap <- sp::spTransform(worldmap,crs) # Project to Robinson

lati <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135,-90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lati, xline = -180, yline = 90, proj = crs) # labels for the graticules
lines <- graticule::graticule(lons = long, lats = lati, proj = crs) # graticules
####################################################
# Mascara de Oceanos
####################################################

shapename <- read_sf('C:/Users/Usuario/Desktop/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha
rm(shapename)

####################################################
# Mascara de Fronteras
####################################################
paises <- read_sf('C:/Users/Usuario/Desktop/ne_10m_admin_0_countries.shp')
Morroco<- paises[27,1]
morroco <- sf::st_transform(Morroco , crs = crs) # cha

Argelia<- paises[122,1]
argelia <- sf::st_transform(Argelia , crs = crs) # cha

shapename <- read_sf('C:/Users/Usuario/Desktop/Europa.shp')
Francia <- shapename[11,1]
francia <- sf::st_transform(Francia , crs = crs) # cha

Portugal <- shapename[32, 1]
portugal<-sf::st_transform(Portugal , crs = crs)

rm(Argelia, Francia, Morroco, Portugal, paises, shapename)

 
pdf(paste(dir_out2,"PRECIP.pdf",sep = ""),
    width=11, height=9)


set.panel() 
split.screen( rbind(c(0, .9,0,1), c(.9,1,0,1)))
split.screen(c(2,2), screen=1)-> ind
zr<- range(0,800)
screen( ind[1])
par(oma=c( 1.5,1,0.5,1),mar=c(1.5,1,0.5,1)) 
mapPlot(coastlineWorld, col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-9,3),
        latitudelim=c(35, 44.5),
        drawBox = TRUE, main = "", cex.main = 1.3,
        line = 0.6, adj = 0.5)

mapImage(lon, lat, aemet_winter, 
         col=brewer.pal(11, "YlGnBu"), zlim=zr)
plot(ocean, col = "lightblue", add =TRUE)
plot(argelia, col = "gray", add =TRUE)
plot(morroco, col = "gray", add =TRUE)
plot(francia, add =TRUE, col= "gray")
plot(portugal, add =TRUE, col="gray")
mapGrid(dlongitude = 5, dlatitude = 5, lty = 1,lwd = 1, col="black")



screen( ind[2])
par(oma=c( 1.5,1,0.5,1),mar=c(1.5,1,0.5,1)) 
mapPlot(coastlineWorld, col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-9,3),
        latitudelim=c(35, 44.5),
        drawBox = TRUE, main = "", cex.main = 1.3,
        line = 0.6, adj = 0.5)

mapImage(lon, lat, aemet_spring, 
         col=brewer.pal(11, "YlGnBu"), zlim=zr)
plot(ocean, col = "lightblue", add =TRUE)
plot(argelia, col = "gray", add =TRUE)
plot(morroco, col = "gray", add =TRUE)
plot(francia, add =TRUE, col= "gray")
plot(portugal, add =TRUE, col="gray")
mapGrid(dlongitude = 5, dlatitude = 5, lty = 1,lwd = 1, col="black")


screen( ind[3])
par(oma=c( 1,1,1,1),mar=c(1,1,1,1)) 
mapPlot(coastlineWorld, col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-9,3),
        latitudelim=c(35, 44.5),
        drawBox = TRUE, main = "", cex.main = 1.3,
        line = 0.6, adj = 0.5)

mapImage(lon, lat, aemet_summer, 
         col=brewer.pal(11, "YlGnBu"), zlim=zr)
plot(ocean, col = "lightblue", add =TRUE)
plot(argelia, col = "gray", add =TRUE)
plot(morroco, col = "gray", add =TRUE)
plot(francia, add =TRUE, col= "gray")
plot(portugal, add =TRUE, col="gray")
mapGrid(dlongitude = 5, dlatitude = 5, lty = 1,lwd = 1, col="black")


screen( ind[4])
par(oma=c( 1,1,1,1),mar=c(1,1,1,1)) 
mapPlot(coastlineWorld, col="lightgray",
        projection="+proj=robin",
        longitudelim=c(-9,3),
        latitudelim=c(35, 44.5),
        drawBox = TRUE, main = "", cex.main = 1.3,
        line = 0.6, adj = 0.5)

mapImage(lon, lat, aemet_autumn, 
         col=brewer.pal(11, "YlGnBu"), zlim=zr)
plot(ocean, col = "lightblue", add =TRUE)
plot(argelia, col = "gray", add =TRUE)
plot(morroco, col = "gray", add =TRUE)
plot(francia, add =TRUE, col= "gray")
plot(portugal, add =TRUE, col="gray")
mapGrid(dlongitude = 5, dlatitude = 5, lty = 1,lwd = 1, col="black")


# move to skinny region on right and draw the legend strip 
screen( 2)
image.plot( zlim=zr,legend.only=TRUE, smallplot=c(.1,.2, .2,.8),
            col = brewer.pal(11, "YlGnBu"))

close.screen( all=TRUE)
dev.off()


round(mean(aemet_winter, na.rm=TRUE),1)
round(mean(aemet_spring, na.rm=TRUE),1)
round(mean(aemet_summer, na.rm=TRUE),1)
round(mean(aemet_autumn, na.rm=TRUE),1)
