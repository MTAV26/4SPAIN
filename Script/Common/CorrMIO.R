CorrMIO <- function(ens, obs, siglev = 0.95, method = 'pearson', 
                 conf = TRUE, pval = TRUE) {
  
  if (method != "kendall" && method != "spearman" && method != "pearson") {
    stop("Wrong correlation method")
    
    # Check the siglev arguments:
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (siglev > 1 || siglev < 0) {
      stop("siglev need to be higher than O and lower than 1")
    }
  }
  conf_low <- (1 - siglev) / 2
  conf_high <- 1 - conf_low
  #ens.mean <- rowMeans(ens)
  #CORR <- cor(obs, ens.mean, use = "pairwise.complete.obs", method = method)
  CORR <- cor(obs, ens, use = "pairwise.complete.obs", method = method)

  if (pval || conf) {
    if (method == "kendall" | method == "spearman") {
      eno <- Eno(rank(obs), 1)                          
    } else if (method == "pearson") {
      eno <- Eno(obs, 1)   
      #cat(eno)
    }
  }
  if (pval) {
    
    t <- CORR*sqrt((eno-2)/(1-(CORR^2)))
    p <- 1 - pt(t, eno-2)
    p.val <-  p
    names(p.val) <- "p.val"
  } else {
    p.val <- c()
    names(p.val) <- c()
  }
  if (conf) {
    conf.int <- c(tanh(atanh(CORR) + qnorm(conf_low) / sqrt(
      eno - 3)), tanh(atanh(CORR) + qnorm(conf_high) / sqrt(
        eno - 3)))
    conf.int <- conf.int[!is.na(CORR)]
    names(conf.int) <- c("conf_low","conf_high")          
  } else {
    conf.int <- c() 
    names(conf.int) <- c() 
  }
  
  #  Output
  # ~~~~~~~~
  #
  results <- c(CORR,conf.int, p.val)  
  names(results) <- c("Corr", names(conf.int), names(p.val))
  return(results) 
}
