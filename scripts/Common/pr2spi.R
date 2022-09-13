SPI <- function(prec,sc) {
  # This code computes the Standardized Precipitation Index (SPI)
  # Ref Reference Publications:
  # Hao Z., AghaKouchak A., Nakhjiri N., Farahmand A., 2014, Global Integrated Drought Monitoring and Prediction System, Scientific Data.
  # Hao Z., AghaKouchak A., 2013a, Multivariate Standardized Drought Index: A Parametric Multi-Index Model, Advances in Water Resources, 57, 12-18, doi: 10.1016/j.advwatres.2013.03.009.
  # Hao Z., AghaKouchak A., 2013b, A Nonparametric Multivariate Multi-Index Drought Monitoring Framework, Journal of Hydrometeorology.
  # Developed for Matlab by: Zengchao Hao, PhD, University of Califnornia, Irvine
  # Web:  http://climate.eng.uci.edu/msdi.html
  # Last update:  8/24/2013
  # Modified for R by: Marco Turco, PhD, BSC, Barcelona, Spain
  # Last update:  2/18/2016

  # Compute SPI with monthly precipitation data (prec).
  #prec=obs[1,1,]
  #sc=3  Specify the time scale (e.g., 3-month SPI)
  nm = length(prec);
  
  #Compute the empirical (or parametric)  SPI
  # (1) Get the accumulated soil moisture data for the specific time scale
  # (2) Compute the empirical (or parametric) drought index (SSI) from the data
  
  # Get the accumulated data for the time scale sc
  A1 = matrix(NA,(length(prec) - sc + 1),sc)
  for (i in 1:sc) {
    A1[,i] = prec[i:(length(prec) - sc + i)]
  }
  Y = apply(A1,1,sum,na.rm=TRUE)
  
  # Compute the SPI
  n = length(Y)
  SI = matrix(0,n,1)
  for (k in 1:12) {
    d = Y[seq(k, n, by = 12)]
    #  Compute the empirical probability
    bp = matrix(0,length(d),1)
    for (i in 1:length(d)) {
      bp[i,1]=sum(d[]<=d[i]);
    }
    # Gringorten plotting position
    SI[seq(k, n, by = 12)]=(bp-0.44)/(length(d)+0.12)
  }
  SI=qnorm(SI)
  SPI=c(matrix(NA,sc - 1,1),SI)
}