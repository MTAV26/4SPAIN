PlotAllMapsMostLikeTercileM2 <- function(lon,lat,dataObs=NULL,data1,data2,data3,
                   Odata1=NULL,Odata2=NULL,Odata3=NULL,
                   dataOverPlot=NULL, 
                   maintit='',sizeTitle=1.4,titleON=F,sublegend='',
                   breaks,n,colCateg,
                   titleg=c("Below normal (%)","Normal (%)","Above normal (%)"),
                   titlegObs=NULL,
                   Otitleg=NULL,transparency=NULL,
                   colorMask=NULL,
                   sizeColBarLegend=1.5,
                   colorContMap=c(0,0,0),showAxis='TRUE') {
  
  source('/Users/marco/Dropbox/estcena/scripts/BSC/R/common/ColourScalesM.R')
 
  # if there is not overplotting data, layaout single map.
  if(is.null(Odata1)){
    # generate subwindows with specified positions to plot different figures 
    if (titleON == T){layout(matrix(c(1,2,1,3,1,4),ncol=3,nrow=2),heights=c(4.6,1.4))
                      par(mar=c(1,1,4,1),las=1,cex=1.4)
    }else{layout(matrix(c(1,2,1,3,1,4),ncol=3,nrow=2),heights=c(5,0.8))
          par(mar=c(1.2,1,1.2,1),las=1,cex=1.4)
    }
  }
  # if there is overplotting data, layaout overplotting map.
  else{
    # generate subwindows with specified positions to plot different figures 
    if (titleON == T){layout(matrix(c(1,2,5,1,3,6,1,4,7),ncol=3,nrow=3),heights=c(4.6,1.4,1.4))
                      par(mar=c(1,1,4,1),las=1,cex=1.4)
    }else{layout(matrix(c(1,1,2,5,1,1,3,6,1,1,4,7),ncol=3,nrow=4),heights=c(2,2,0.8,0.8))
          par(mar=c(1.2,1,1.2,1),las=1,cex=1.4)
    }
  }
  
  if(is.null(dataObs)){
    # Create probabilistic map.
    image(lon,lat,data1,axes=F,col=ColourScalesM(colCateg,1)$cbelow,xlab='',ylab='',breaks=breaks)
    image(lon,lat,data2,axes=F,col=ColourScalesM(colCateg,1)$cnormal,xlab='',ylab='',breaks=breaks,add=T)
    image(lon,lat,data3,axes=F,col=ColourScalesM(colCateg,1)$cabove,xlab='',ylab='',breaks=breaks,add=T)
    # if there is overplotting data, we add more probabilistic map on top.
    if(is.null(Odata1)!= T){
      image(lon,lat,Odata1,axes=F,col=ColourScalesM(colCateg,0.2)$cbelow,xlab='',ylab='',breaks=breaks,add=T)
      image(lon,lat,Odata2,axes=F,col=ColourScalesM(colCateg,0.2)$cnormal,xlab='',ylab='',breaks=breaks,add=T)
      image(lon,lat,Odata3,axes=F,col=ColourScalesM(colCateg,0.2)$cabove,xlab='',ylab='',breaks=breaks,add=T)
    }
    if (is.null(dataOverPlot)!= T){
      image(lon,lat,dataOverPlot,axes=F,col=colorMask,xlab='',ylab='',add=T)
    }
  }
  else{
    # Create probabilistic map.
    image(lon,lat,data1,axes=F,col=ColourScalesM(colCateg,1)$cbelow_obs,xlab='',ylab='',breaks=breaks)
    image(lon,lat,data2,axes=F,col=ColourScalesM(colCateg,1)$cnormal_obs,xlab='',ylab='',breaks=breaks,add=T)
    image(lon,lat,data3,axes=F,col=ColourScalesM(colCateg,1)$cabove_obs,xlab='',ylab='',breaks=breaks,add=T)
  }
  # add world map
  # Check if lon is from 0 to 360 or -180 to 180 to use appropriate world map.
  if (min(lon)<0) {
    map('world',interior = F,add = T, lwd=2, col=rgb(colorContMap[1],colorContMap[2],colorContMap[3])) # Low resolution world map (lon -180 to 180).
  } else {
    map('world2',interior = F,add = T, lwd=2, col=rgb(colorContMap[1],colorContMap[2],colorContMap[3])) # Low resolution world map (lon 0 to 360).
  }
  box()
  if (showAxis=='TRUE'){
    map.axes()
  }
  # plot title and sublegend
  par(font.main=1)
  title(maintit,cex.main=sizeTitle)
  mtext(sublegend,side=1,cex=1.3, outer=T)
  
  # if we are not plotting observed data
  if(is.null(dataObs)){
    # Adding colorbar.
    colours <- c(ColourScalesM(colCateg,1)$cbelow, 
                 ColourScalesM(colCateg,1)$cnormal, 
                 ColourScalesM(colCateg,1)$cabove)
    nameLab <- list (titleg[1], titleg[2], titleg[3])
    for (k in 1:length(nameLab)){
      if (titleON == T){par(mar=c(5.8,1,1,1),mgp=c(1.5,0.3,0),las=1)
      }else{par(mar=c(2.3,1,1.1,1),mgp=c(1.5,0.3,0),las=1)
      }   
      pp <- switch(k,(1:4),(5:8),(9:12))
      image(c(1:n),1,t(t(c(1:n))),axes=F,col=colours[pp],xlab='',ylab='') 
      box()
      axis(1,at=seq(0.5,4.5),tick=T,cex.axis=sizeColBarLegend,
           labels = c("40","55","70","85","100"))
      axis(3,at=2.5,tick=F,cex.axis=sizeColBarLegend,labels=as.character(nameLab[k]))
    }    
    
    # if there is overplotting data, we add a second colorbar
    if(is.null(Odata1)!= T){
      Ocolours <- c(ColourScalesM(colCateg,transparency)$cbelow,
                    ColourScalesM(colCateg,transparency)$cnormal,
                    ColourScalesM(colCateg,transparency)$cabove)
      OnameLab <- list (Otitleg[1], Otitleg[2], Otitleg[3])
      for (k in 1:length(OnameLab)){
        pp <- switch(k,(1:4),(5:8),(9:12))
        
        if (titleON == T){par(mar=c(5.8,1,1,1),mgp=c(1.5,0.3,0),las=1)
        }else{par(mar=c(2.3,1,1.1,1),mgp=c(1.5,0.3,0),las=1)
        } 
        image(c(1:n),1,t(t(c(1:n))),axes=F,col=Ocolours[pp],xlab='',ylab='') 
        box()
        axis(1,at=seq(0.5,4.5),tick=T,cex.axis=sizeColBarLegend,
             labels = c("40","55","70","85","100"))
        axis(3,at=2.5,tick=F,cex.axis=sizeColBarLegend,labels=as.character(OnameLab[k]))
      }          
    }  
  }
  # if we are plotting observed data
  else {
    # Adding colorbar.
    colours <- c(ColourScalesM(colCateg,1)$cbelow_obs, 
                 ColourScalesM(colCateg,1)$cnormal_obs, 
                 ColourScalesM(colCateg,1)$cabove_obs)
    nameLab <- list (titleg[1], titleg[2], titleg[3])
    
    for (k in 1:length(nameLab)){
      if (titleON == T){par(mar=c(5.8,1,1,1),mgp=c(1.5,0.3,0),las=1)
      }else{par(mar=c(2.3,1,1.1,1),mgp=c(1.5,0.3,0),las=1)
      } 
      image(c(1:n),1,t(t(c(1:n))),axes=F,col=colours[k],xlab='',ylab='') 
      box()
      axis(3,at=1,tick=F,cex.axis=sizeColBarLegend,labels=as.character(nameLab[k]))
    }
  }
  
}
