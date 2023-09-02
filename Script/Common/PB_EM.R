PB_EM<- function(lon,lat,ptc) {
  # Calculate the probability of most likely tercile of the ensemble mean of the forecast  
  
  nlon <- length(lon); nlat <- length(lat)
  nfc <- dim(ptc)[1]
  # Probability of most likely tercile of the ensemble mean of the forecast.
  PMLT <- (ptc[1,1,,]*NA)
  ter_up <- (ptc[1,1,,]*NA)
  ter_mid <- (ptc[1,1,,]*NA)
  ter_lo <- (ptc[1,1,,]*NA)
  for (i in 1:nlon) {
    for (j in 1:nlat) {
      lo <- sum(ptc[,1,j,i],na.rm=T)/nfc
      mid <- sum(ptc[,2,j,i],na.rm=T)/nfc
      up <- sum(ptc[,3,j,i],na.rm=T)/nfc
      if (all(up > c(mid,lo))) {
        PMLT[j,i] <- up
        ter_up[j,i] <- up
      } else if (all(mid > c(up,lo))) {
        PMLT[j,i] <- mid
        ter_mid[j,i] <- mid
      } else if (all(lo > c(mid,up))) {
        PMLT[j,i] <- lo
        ter_lo[j,i] <- lo
      }
    }
  }
  
#   return(list(tall=t(PMLT[nlat:1,c((nlonr+1):nlon,1:nlonr)]*100),
#               tlow=t(ter_lo[nlat:1,c((nlonr+1):nlon,1:nlonr)]*100),
#               tmid=t(ter_mid[nlat:1,c((nlonr+1):nlon,1:nlonr)]*100),
#               tup=t(ter_up[nlat:1,c((nlonr+1):nlon,1:nlonr)]*100))) 
  return(list(tall=PMLT,tlow=ter_lo,tmid=ter_mid,tup=ter_up))
  
}
