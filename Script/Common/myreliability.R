myreliability <- function(obs,pred,th,pstep,lat,inout) {
  
  
  #pstep = 0.2
  prob1 = seq(0, 1 - pstep, pstep)
  prob1[1] = -prob1[length(prob1)]
  prob2 = seq(0 + pstep, 1, pstep)
  
  ni=dim(obs)[1]
  nj=dim(obs)[2]
  
  #print(dim(obs));
  
  k = 0
  o10 = prob1 * 0
  no10 = prob1 * 0
  
  for (ii in 1:ni) {
    for (jj in 1:nj) {
      if (!is.na(inout[ii, jj])) {
        
        k = k + 1
        
        #print(inout[i,j])
        
        #i = 4
        #j = 1
        x = obs[ii, jj, ]
        x_prob = x * NA
        x_prob[(x > th)] = 0
        x_prob[(x <= th)] = 1
        
        y1 = pred[ii, jj, , ]
        y1_prob = x * NA
        
        for (i in 1:dim(y1)[1]) {
          y1_prob[i]=sum(y1[i, !is.na(y1[i, ])]<= th)/length(y1[i, !is.na(y1[i, ])])
          #y1_prob[i] = sum(y1[i, ] <= th,na.rm=TRUE) / dim(y1)[2]
          #y1_prob[i] = sum(y1[i, ] <= th) / dim(y1)[2]
        }
        
        # aux=y1_prob[y1_prob > prob1[length(prob1)]]
        # if (length(aux)>0) {
        #   message(paste('ok made it this far with x=',aux)) 
        # }
        
        
        
        for (ip in 1:length(prob1)) {
          dum = x_prob[y1_prob > prob1[ip] & y1_prob <= prob2[ip]]
          o10[ip] = o10[ip] + sum(dum,na.rm=TRUE) * cos(lat[jj]*pi/180)
          no10[ip] = no10[ip] + length(which(dum == 0)) * cos(lat[jj]*pi/180)
          rm(dum)
        }
      }
    }
  }
  
  #myroc <-
  #  matrix(data = NA,nrow = length(o10),2)
  
  hr = o10 * NA
  fr = o10 * NA
  gr = o10 * NA
  #far = o10 * NA
  for (ip in 1:length(prob1)) {
    hr[ip] = o10[ip] / (o10[ip]+no10[ip])
    fr[ip] = o10[ip]+no10[ip]
    gr[ip] = o10[ip]
    }
  
  
  ## slope
  
  obs.clim=sum(gr)/sum(fr)
  px=prob2-(pstep/2)
  py=hr
  
  rel1.data=data.frame(px,py,fr)
  #rel1.data$py <- rel1.data$py - obs.clim
  #rel1.data$px  <- rel1.data$px  - obs.clim
  #rel1.data$py <- rel1.data$py
  #rel1.data$px  <- rel1.data$px  
  
  #weiss1.rel<-glm(px ~ 0 + py ,data=rel1.data,weights=fr,na.action="na.exclude")
  nmod <- (lm(I(py-obs.clim)~I(px-obs.clim) +0, rel1.data, weights=fr,na.action="na.exclude"))
  #slope <- summary(nmod)$coefficients[1]
  #intercept <- predict(nmod, newdata = list(px=0))+obs.clim
  slope <- coef(nmod)
  
  
  
  
  #  Output
  # ~~~~~~~~
  #
  #results <- list(hr,far,auc)  
  results <- list(hr,fr,gr,slope)  
  #names(results) <- c("HR","FAR")
  
  #return(new("roc",
  #           hr=hr,
  #           far=far))
  
  return(results) 
}