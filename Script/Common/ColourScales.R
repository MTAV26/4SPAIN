ColourScales <- function(colCateg,vtransp) {
  
  #  different colour scales
  if (colCateg=='Blue_Yellow_Red'){    
    cabove0<-alpha(brewer.pal(6,"Reds"),vtransp)
    cabove <- cabove0[3:6]
    cabove_obs <- cabove0[6]
    cbelow0<-alpha(brewer.pal(6,"Blues"),vtransp)
    cbelow <- cbelow0[3:6]
    cbelow_obs <- cbelow0[6]
    cnormal<-alpha(c(rgb(1,1,0.6),rgb(1,1,0),rgb(1,0.87,0),rgb(0.85,0.65,0.3)),vtransp)
    cnormal_obs <- cnormal[4]
  }
  else if(colCateg=='BlueL_Grey_OrangeL'){
    cabove0<-alpha(brewer.pal(6,"Oranges"),vtransp)
    cabove <- cabove0[2:5]
    cabove_obs <- cabove0[5]
    cbelow0<-alpha(brewer.pal(6,"Blues"),vtransp)
    cbelow <- cbelow0[2:5]
    cbelow_obs <- cbelow0[5]
    cnormal0<-alpha(brewer.pal(6,"Greys"),vtransp)
    cnormal <- cnormal0[2:5]
    cnormal_obs <- cnormal0[5]
  }
  else if (colCateg=='BlueD_Grey_OrangeD'){
    cabove0<-alpha(brewer.pal(6,"Oranges"),vtransp)
    cabove <- cabove0[3:6]
    cabove_obs <- cabove0[6]
    cbelow0<-alpha(brewer.pal(6,"Blues"),vtransp)
    cbelow <- cbelow0[3:6]
    cbelow_obs <- cbelow0[6]
    cnormal0<-alpha(brewer.pal(6,"Greys"),vtransp)
    cnormal <- cnormal0[2:5]
    cnormal_obs <- cnormal0[5]
  }
  else if (colCateg=='BlueD_Grey_RedD'){
    cabove0<-alpha(brewer.pal(9,"YlOrRd"),vtransp)
    cabove <- cabove0[6:9]
    cabove_obs <- cabove0[9]
    cbelow0<-alpha(brewer.pal(9,"Blues"),vtransp)
    cbelow <- cbelow0[5:8]
    cbelow_obs <- cbelow0[8]
    cnormal0<-alpha(brewer.pal(6,"Greys"),vtransp)
    cnormal <- cnormal0[2:5]
    cnormal_obs <- cnormal0[5]
  }    
  else if (colCateg=='BlueD_Orange_RedD') {
    cabove0<-alpha(brewer.pal(9,"YlOrRd"),vtransp)
    cabove <- cabove0[6:9]
    cabove_obs <- cabove0[9]
    cbelow0<-alpha(brewer.pal(9,"Blues"),vtransp)
    cbelow <- cbelow0[5:8]
    cbelow_obs <- cbelow0[8]
    oranges0<-alpha(brewer.pal(9,"YlOrRd"),vtransp)
    cnormal <- oranges0[2:5]
    cnormal_obs <- oranges0[5]
  }
  
  return(list(cbelow=cbelow,cnormal=cnormal,cabove=cabove,
              cbelow_obs=cbelow_obs,cnormal_obs=cnormal_obs,cabove_obs=cabove_obs))
  
 
}