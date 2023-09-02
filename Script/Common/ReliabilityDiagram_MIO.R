rel_wp <- function (probs, obs, bins = 10, nboot = 500, plot = FALSE, plot.refin = TRUE, 
          cons.probs = c(0.025, 0.975), attributes = FALSE) 
{
  if (class(probs) == "data.frame") {
    probs <- c(as.matrix(probs))
  }
  if (class(obs) == "data.frame") {
    obs <- c(as.matrix(obs))
  }
  stopifnot(length(probs) == length(obs))
  stopifnot(nboot >= 0)
  stopifnot(all(probs >= 0), all(probs <= 1), all(obs %in% 
                                                    c(0, 1)))
  stopifnot(length(cons.probs) == 2, all(cons.probs >= 0), 
            all(cons.probs <= 1))
  n <- length(obs)
  nboot <- floor(nboot)
  cons.probs <- sort(cons.probs)
  
  ws_slope<-array(NA,dim=nboot)
  
  if (length(bins) == 1) {
    nbins <- floor(bins)
    brx <- seq(0, 1, length.out = nbins + 1) + c(-0.1, rep(0, 
                                                           nbins - 1), 0.1)
  }
  else {
    nbins <- length(bins) - 1
    bins <- sort(bins)
    stopifnot(min(bins) <= 0 & max(bins) >= 1)
    brx <- bins
  }
  cons.bars <- matrix(NA, ncol = nbins, nrow = 2)
  h <- hist(probs, breaks = brx, plot = FALSE)$counts
  g <- hist(probs[obs == 1], breaks = brx, plot = FALSE)$counts
  obs.clim <- sum(g)/sum(h)
  
  for (i in 1:nboot) {
    n<-sample(length(probs),length(probs),replace=TRUE)
    probst<-probs[n]
    obst<-obs[n]
    ht <- hist(probst, breaks = brx, plot = FALSE)$counts
    gt <- hist(probst[obst == 1], breaks = brx, plot = FALSE)$counts
    obar.i <- gt/ht
    obar.i[is.nan(obar.i)] <- NA
    p.bins <- as.numeric(cut(probst, breaks = brx, include.lowest = TRUE))
    p.avgs <- sapply(seq(nbins), function(ii) mean(probst[p.bins == 
                                                           ii], na.rm = TRUE))
    p.avgs[is.nan(p.avgs)] <- NA
    
    # Fit regression such that it goes through the climatology of obs and ens
    rel.data=data.frame(p.avgs,obar.i,h)
    rel.data$obar.i <- rel.data$obar.i - obs.clim
    rel.data$p.avgs  <- rel.data$p.avgs  - obs.clim
    
    # Reliability using weighted linear regression (Weissheimer and Palmer (2014))
    weiss.rel<-glm(obar.i ~ 0 + p.avgs ,data=rel.data,weights=h,na.action="na.exclude")
    ws_slope[i] <- summary(weiss.rel)$coefficients[1]
    
    
  }
  
  #slope.stat<-quantile(ws_slope,probs=c(.25,0.5,.75))
  slope.stat<-quantile(ws_slope,probs=c(.025,0.5,.975))
  inter.stat1<-obs.clim*(1-slope.stat[1])
  inter.stat2<-obs.clim*(1-slope.stat[2])
  inter.stat3<-obs.clim*(1-slope.stat[3])
  #fin.stat<-1*(slope.stat[2])
  #in.stat<-(1-slope.stat[2])
  
  if (plot) {
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "Forecast probability", 
         ylab = "Observed relative frequency")
    if (attributes) {
      a <- (1 - obs.clim)/2 + obs.clim
      b <- obs.clim/2
      x.p <- c(obs.clim, obs.clim, 1, 1, 0, 0)
      y.p <- c(0, 1, 1, a, b, 0)
      polygon(x.p, y.p, col = "#e6e6e6")
      abline(h = obs.clim, lty = 2)
      text(0.9, obs.clim, "No resolution", pos = 3)
      text(0.9, obs.clim + (a - b) * (0.9 - obs.clim), 
           "No skill", pos = 1, srt = atan(a - b)/(2 * pi) * 
             360)
    }
    #for (i in 1:length(p.avgs)) {
    #  lines(rep(p.avgs[i], 2), cons.bars[, i], col = "#CCCCCC", 
    #        lwd = 6)
    #}
    
    
    #abline(inter.stat, slope.stat[1], lty = 1, col = "green")
    #abline(inter.stat, slope.stat[2], lty = 1, col = "green")
    #abline(inter.stat, slope.stat[3], lty = 1, col = "green")
    
    #segments(0,inter.stat1, 1,(inter.stat1+slope.stat[1]), lty = 1, col = "green")
    #segments(0,inter.stat2, 1,(inter.stat2+slope.stat[2]), lty = 1, col = "red")
    #segments(0,inter.stat3, 1,(inter.stat3+slope.stat[3]), lty = 1, col = "green")
    pointx=c(0, 1)
    pointylow=c(inter.stat1,(inter.stat1+slope.stat[1]))
    pointyhigh=c(inter.stat3,(inter.stat3+slope.stat[3]))
    
    polygon(c(pointx, rev(pointx)), c(pointyhigh, rev(pointylow)),
            col = "green", border = NA)
    
    lines(c(0, 1), c(0, 1), lty = 1)
    points(p.avgs, obar.i, col = "black", pch = 1, lwd = 2, 
           type = "p") #type = "b")
    
    if (plot.refin) {
      pp <- par("plt")
      par(plt = c(pp[2] - 0.2, pp[2], pp[3], pp[3] + 0.2))
      par(new = TRUE)
      barplot(h, axes = FALSE, axisnames = FALSE)
      axis(4)
      box()
    }
  }
  #ws_rel<-list()
  #ws_rel$cat<-ws_cat
  #ws_rel$slope<-slope.stat[2]
  #return(ws_rel)
  
  #ret.df <- data.frame(p.avgs = p.avgs, cond.probs = obar.i, 
  #                     cbar.lo = cons.bars[1, ], cbar.hi = cons.bars[2, ], ws_slope, inter.stat)
  #ret.df <- data.frame(p.avgs = p.avgs, cond.probs = obar.i, 
  #                     cbar.lo = cons.bars[1, ], cbar.hi = cons.bars[2, ], ws_slope, inter.stat)
  ret.df<-list()
  ret.df$slope<-ws_slope
  ret.df$slope1<-slope.stat[1]
  ret.df$slope2<-slope.stat[2]
  ret.df$slope3<-slope.stat[3]
  
  return(ret.df)
}
#<environment: namespace:SpecsVerification>