STI <- function(tmean,sc) {
  
  # Compute STI with monthly temperature data (tmean).
  #prec=obs[1,1,]
  #sc=3  Specify the time scale (e.g., 3-month STI)
  nm = length(tmean);
  
  #Compute the empirical (or parametric)  SPI
  
  # Get the accumulated data for the time scale sc
  A1 = matrix(NA,(length(tmean) - sc + 1),sc)
  for (i in 1:sc) {
    A1[,i] = tmean[i:(length(tmean) - sc + i)]
  }
  Y = apply(A1,1,mean,na.rm=TRUE)
  
  SPI=c(matrix(NA,sc - 1,1),Y)
}