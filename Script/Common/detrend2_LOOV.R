detrend2_LOOV <- function(x) {
  
  
  
  n_folds <- length(x)
  folds_i <- seq(1:n_folds)
  xd <- x*NA
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    train_i <- folds_i[-test_i]
    
    x1=x[train_i]
    fit=lm(x1 ~ train_i + train_i^2)
    #fit=lm(x1 ~ train_i)
    coeffs = coefficients(fit)
    sid = coeffs[1] + coeffs[2]*train_i 
    xd[test_i]=x[test_i]-(coeffs[1] + coeffs[2]*test_i)
    
    
    
    
    
  }
  
  #fit=lm(x1 ~ poly(train_i,1))
  #sid = coeffs[1] + coeffs[2]*train_i[1] 
  #sid=as.numeric(fit$coefficients)[1]+(as.numeric(fit$coefficients)[2]*train_i[1])
  
  #lines(sid, col="blue")
  #plot(x1)
  #abline(fit, col="red")
  #lines(fitted(fit))
  
  # dev.off()
  # plot(x)
  # lines(sid, col="blue")
  
  #  Output
  # ~~~~~~~~
  #
  #results <- c(CORR,conf.int, p.val)  
  #names(results) <- c("Corr", names(conf.int), names(p.val))
  results <- xd
  return(results) 
}