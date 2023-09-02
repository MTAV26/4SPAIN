Reliability <- function(ano_exp, ano_obs,quantile = TRUE,nbins=5,thr = c(2/3),nboot=1000) {
  
  # Checking ano_exp
  if (!is.numeric(ano_exp) || !is.array(ano_exp)) {
    stop("Parameter 'ano_exp' must be a numeric array.")
  }

  # Checking ano_obs
  if (!is.numeric(ano_obs) || !is.array(ano_obs)) {
    stop("Parameter 'ano_obs' must be a numeric array.")
  }

  # Checking consistency in ano_exp and ano_obs
  if (!(length(dim(ano_exp)) == length(dim(ano_obs)))) {
    stop("'ano_obs' and 'ano_exp' must have the same number of dimensions.")
  }
  
  # Checking quantile
  if (!is.logical(quantile)) {
    stop("Parameter 'quantile' must be either TRUE or FALSE.")
  }

  # Checking thr
  if (!is.numeric(thr)) {
    stop("Parameter 'thr' must be a numerical vector.")
  }

  if (quantile) {
    if (!all(thr <= 1 & thr >= 0)) {
      stop("All quantiles specified in parameter 'thr' must fall in the range [0, 1].")
    }
  }
  
  if (quantile) { 
    tau_m <- quantile(ano_exp,probs=thr,na.rm=TRUE)
    tau_o <- quantile(ano_obs,probs=thr,na.rm=TRUE)
  } else {
    tau_m <- thr; tau_o <- thr
  } 

  ensc <- rowSums(t(ano_exp) > tau_m,na.rm=TRUE)
  obsc <- 1*(t(ano_obs) > tau_o)

  exp_probs <- ensc/nmemb
  obs_probs <- obsc

  ws_slope <- array(NA,dim=nboot)

  for (i in 1:nboot) {
    n <- sample(length(exp_probs),length(exp_probs),replace=TRUE)
    probst <- exp_probs[n]
    vert <- obs_probs[n]

    # This part of the code is from SpecsVerification
    # should we re-formulate this?
    # --------------------------------------------------
    # breaks of bins
    bn <- seq(0, 1, length.out=nbins+1) +
          c(-.Machine$double.eps, rep(0, nbins-1), .Machine$double.eps)
    
    counts.bins <- hist(probst, breaks=bn, plot=FALSE)$counts
    g <- hist(probst[vert==1], breaks=bn, plot=FALSE)$counts
    cond.probs <- g / counts.bins
    
    # In bin average
    p.bins <- as.numeric(cut(probst, breaks=bn))
    p.avgs <- sapply(seq(nbins),function(ii) mean(probst[p.bins == ii], na.rm=TRUE))
    # --------------------------------------------------
    
    rel.data=data.frame(p.avgs,cond.probs,counts.bins)
 
    # Fit regression such that it goes through the climatology of obs and ens
    rel.data$cond.probs <- rel.data$cond.probs - cprob
    rel.data$p.avgs <- rel.data$p.avgs - cprob
    
    # Reliability using weighted linear regression (Weissheimer and Palmer (2014))
    weiss.rel<-glm(cond.probs ~ 0 + p.avgs ,data=rel.data,weights=counts.bins,na.action="na.exclude")
    ws_slope[i] <- summary(weiss.rel)$coefficients[1]
  }
        
  slope.stat<-quantile(ws_slope,probs=c(.25,0.5,.75),na.rm=TRUE)
     
  # Define reliability classes
    
  if (slope.stat[1]>0.5 & slope.stat[3]>1){
    ws_cat<-5
  } else if (slope.stat[1]>=0.5 & slope.stat[3]<1){
    ws_cat<-4
  } else if (slope.stat[1]>0){
    ws_cat<-3
  } else if (slope.stat[1]<0 & slope.stat[3]>0){
    ws_cat<-2
  } else if (slope.stat[3]<0){
    ws_cat<-1
  } else {
    ws_cat<-6
  }
        
  ws_rel<-list()
  ws_rel$cat<-ws_cat
  ws_rel$slope<-slope.stat[2]
  return(ws_rel)
}
