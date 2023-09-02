rel_wp <- function (probs1, probs2, obs, nomeout, bins = 10, nboot = 500, plot = FALSE, plot.refin = TRUE, 
          cons.probs = c(0.025, 0.975), attributes = FALSE) 
{
  if (class(probs1) == "data.frame") {
    probs1 <- c(as.matrix(probs1))
  }
  if (class(probs2) == "data.frame") {
    probs2 <- c(as.matrix(probs2))
  }
  if (class(obs) == "data.frame") {
    obs <- c(as.matrix(obs))
  }
  stopifnot((length(probs1) == length(obs)) & (length(probs2) == length(obs)) )
  stopifnot(nboot >= 0)
  stopifnot(all(probs1 >= 0), all(probs1 <= 1), all(probs2 >= 0), all(probs2 <= 1), all(obs %in%
                                                    c(0, 1)))
  stopifnot(length(cons.probs) == 2, all(cons.probs >= 0),
            all(cons.probs <= 1))
  n <- length(obs)
  nboot <- floor(nboot)
  cons.probs <- sort(cons.probs)
  
  ws_slope1<-array(NA,dim=nboot)
  ws_slope2<-array(NA,dim=nboot)
  
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
  
  h1 <- hist(probs1, breaks = brx, plot = FALSE)$counts
  g1 <- hist(probs1[obs == 1], breaks = brx, plot = FALSE)$counts
  
  h2 <- hist(probs2, breaks = brx, plot = FALSE)$counts
  g2 <- hist(probs2[obs == 1], breaks = brx, plot = FALSE)$counts
  
  obs.clim <- sum(g1)/sum(h1)

  for (i in 1:nboot) {
    
    n<-sample(length(probs1),length(probs1),replace=TRUE)
    
    probst1<-probs1[n]
    probst2<-probs2[n]
    
    obst<-obs[n]
    
    ht1 <- hist(probst1, breaks = brx, plot = FALSE)$counts
    gt1 <- hist(probst1[obst == 1], breaks = brx, plot = FALSE)$counts
    
    ht2 <- hist(probst2, breaks = brx, plot = FALSE)$counts
    gt2 <- hist(probst2[obst == 1], breaks = brx, plot = FALSE)$counts
    
    obar1.i <- gt1/ht1
    obar1.i[is.nan(obar1.i)] <- NA
    
    obar2.i <- gt2/ht2
    obar2.i[is.nan(obar2.i)] <- NA
    
    p1.bins <- as.numeric(cut(probst1, breaks = brx, include.lowest = TRUE))
    p1.avgs <- sapply(seq(nbins), function(ii) mean(probst1[p1.bins == 
                                                           ii], na.rm = TRUE))
    p1.avgs[is.nan(p1.avgs)] <- NA
    
    p2.bins <- as.numeric(cut(probst2, breaks = brx, include.lowest = TRUE))
    p2.avgs <- sapply(seq(nbins), function(ii) mean(probst2[p2.bins == 
                                                            ii], na.rm = TRUE))
    p2.avgs[is.nan(p2.avgs)] <- NA
    
    # Fit regression such that it goes through the climatology of obs and ens
    rel1.data=data.frame(p1.avgs,obar1.i,h1)
    rel1.data$obar1.i <- rel1.data$obar1.i - obs.clim
    rel1.data$p1.avgs  <- rel1.data$p1.avgs  - obs.clim
    
    rel2.data=data.frame(p2.avgs,obar2.i,h2)
    rel2.data$obar2.i <- rel2.data$obar2.i - obs.clim
    rel2.data$p2.avgs  <- rel2.data$p2.avgs  - obs.clim
    
    # Reliability using weighted linear regression (Weissheimer and Palmer (2014))
    weiss1.rel<-glm(obar1.i ~ 0 + p1.avgs ,data=rel1.data,weights=h1,na.action="na.exclude")
    ws_slope1[i] <- summary(weiss1.rel)$coefficients[1]
    
    weiss2.rel<-glm(obar2.i ~ 0 + p2.avgs ,data=rel2.data,weights=h2,na.action="na.exclude")
    ws_slope2[i] <- summary(weiss2.rel)$coefficients[1]
    
  }
  
  slope1.stat<-quantile(ws_slope1,probs=c(.025,0.5,.975))
  inter1.stat1<-obs.clim*(1-slope1.stat[1])
  #inter1.stat2<-obs.clim*(1-slope1.stat[2])
  inter1.stat3<-obs.clim*(1-slope1.stat[3])
  
  slope2.stat<-quantile(ws_slope2,probs=c(.025,0.5,.975))
  inter2.stat1<-obs.clim*(1-slope2.stat[1])
  #inter2.stat2<-obs.clim*(1-slope2.stat[2])
  inter2.stat3<-obs.clim*(1-slope2.stat[3])
  
  if (plot) {
    setEPS()
    postscript(nomeout,horiz=FALSE,onefile=FALSE,width=8.5,height=5.5)
    
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
    
    #abline(inter1.stat1, slope1.stat[1], lty = 1, col = "black")
    #abline(inter1.stat2, slope1.stat[2], lty = 1, col = "black")
    #abline(inter1.stat3, slope1.stat[3], lty = 1, col = "black")
    
    #abline(inter2.stat1, slope2.stat[1], lty = 1, col = "red")
    #abline(inter2.stat2, slope2.stat[2], lty = 1, col = "red")
    #abline(inter2.stat3, slope2.stat[3], lty = 1, col = "red")
    
    #segments(0,inter.stat1, 1,(inter.stat1+slope.stat[1]), lty = 1, col = "green")
    #segments(0,inter.stat2, 1,(inter.stat2+slope.stat[2]), lty = 1, col = "red")
    #segments(0,inter.stat3, 1,(inter.stat3+slope.stat[3]), lty = 1, col = "green")
    
    pointxlow1=c(max(0,-inter1.stat1/slope1.stat[1]), min(1,(1-inter1.stat1)/slope1.stat[1]))
    pointxhigh1=c(max(0,-inter1.stat3/slope1.stat[3]), min(1,(1-inter1.stat3)/slope1.stat[3]))
    y1=1
    y3 = 1
    if (slope1.stat[1] <=1) {
      y1=inter1.stat1+slope1.stat[1]
    }
    if (slope1.stat[3] <=1) {
      y3 = inter1.stat3 + slope1.stat[3]
    }
    
    pointylow1=c(max(0,inter1.stat1),y1)
    pointyhigh1=c(max(0,inter1.stat3),y3)
    
    polygon(c(pointxhigh1, rev(pointxlow1)), c(pointyhigh1, rev(pointylow1)),
            col = "green", border = NA)
    
    pointxlow2=c(max(0,-inter2.stat1/slope2.stat[1]), min(1,(1-inter2.stat1)/slope2.stat[1]))
    pointxhigh2=c(max(0,-inter2.stat3/slope2.stat[3]), min(1,(1-inter2.stat3)/slope2.stat[3]))
    
    y1=1
    y3 = 1
    if (slope2.stat[1] <=1) {
      y1=inter2.stat1+slope2.stat[1]
    }
    if (slope2.stat[3] <=1) {
      y3 = inter2.stat3 + slope2.stat[3]
    }
    
    pointylow2=c(max(0,inter2.stat1),y1)
    pointyhigh2=c(max(0,inter2.stat3),y3)
    
    #pointylow2=c(max(0,inter2.stat1),min(1,inter2.stat1+slope2.stat[1]))
    #pointyhigh2=c(max(0,inter2.stat3),min(1,inter2.stat3+slope2.stat[3]))
    
    polygon(c(pointxhigh2, rev(pointxlow2)), c(pointyhigh2, rev(pointylow2)),
            col = "blue", border = NA)
    
    points(p1.avgs, obar1.i, col = "black", pch = 21, bg = "green",lwd = 2, 
           type = "p") #type = "b")
    
    points(p2.avgs, obar2.i, col = "black", pch = 21, bg = "blue", lwd = 2, 
           type = "p") #type = "b")
    
    lines(c(0, 1), c(0, 1), lty = 1)
    
    if (plot.refin) {
      pp <- par("plt")
      par(plt = c(pp[2] - 0.2, pp[2], pp[3], pp[3] + 0.2))
      par(new = TRUE)
      barplot(rbind(h1,h2), beside = TRUE,axes = FALSE, axisnames = FALSE,col=c("green","blue"))
      axis(4)
      box()
    }
  }
  
  
  dev.off()
  
  ret.df<-list()
  ret.df$slope1<-ws_slope1  
  ret.df$slope2<-ws_slope2 
  ret.df$clim<-obs.clim
  
  return(ret.df)
}
#<environment: namespace:SpecsVerification>