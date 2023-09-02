rel_wp <- function(probs,ver,bn,nboot,cprob) {
    ws_slope<-array(NA,dim=nboot)
    # probs <- model predictions, ver <- observations (0,1), bn <- number of bins in reliability diagram
    # nboot <- number of resamples to compute confidence interval, cprob <- 1 - p , where p is the exceedance treshold
    for (i in 1:nboot) {
      n<-sample(length(probs),length(probs),replace=TRUE)
      probst<-probs[n]
      vert<-ver[n]
      counts.bins <- hist(probst, breaks=bn, plot=FALSE)$counts
      g <- hist(probst[vert==1], breaks=bn, plot=FALSE)$counts
      cond.probs <- g / counts.bins

      # In bin averages
      p.bins <- as.numeric(cut(probst, breaks=bn))
      p.avgs <- sapply(seq(binnum),function(ii) mean(probst[p.bins == ii], na.rm=TRUE))
      rel.data=data.frame(p.avgs,cond.probs,counts.bins)

      # Fit regression such that it goes through the climatology of obs and ens
      rel.data$cond.probs <- rel.data$cond.probs - cprob
      rel.data$p.avgs <- rel.data$p.avgs - cprob

      # Reliability using weighted linear regression (Weissheimer and Palmer (2014))
      weiss.rel<-glm(cond.probs ~ 0 + p.avgs ,data=rel.data,weights=counts.bins,na.action="na.exclude")
      ws_slope[i] <- summary(weiss.rel)$coefficients[1]
    }
    
   slope.stat<-quantile(ws_slope,probs=c(.25,0.5,.75))
   
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
    
    if (rplot) {    
      postscript(paste(psave,'_',pn[1],'_',pn[2],'.ps',sep=''),width=5,height=5)
      inter.stat<-cprob*(1-slope.stat)
      t<-cbind(0,cprob,1,1,cprob,0)
      s<-cbind(inter.stat[3],cprob,slope.stat[1]+inter.stat[1],slope.stat[3]+inter.stat[3],cprob,inter.stat[1])
      rel.diag.wp(probs=probs,ver=ver,nbins=binnum,nboot=FALSE,
            plot=TRUE, plot.refin=TRUE,s=s,t=t,col.unc="#7BCCC4",inter.stat=inter.stat,slope.stat=slope.stat)
      title(main=paste(ptitle," grid-point nx=",pn[1],' ny=',pn[2],"",sep=""),font.main=1)
      dev.off()
    }
    ws_rel<-list()
    ws_rel$cat<-ws_cat
    ws_rel$slope<-slope.stat[2]
    return(ws_rel)
}
