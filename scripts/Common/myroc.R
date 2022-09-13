myroc <- function(obs,pred,th,pstep,lat,inout) {
  
  # inout=inout1
  #pstep = 0.1
  prob1 = seq(0, 1 - pstep, pstep)
  prob1[1] = -prob1[length(prob1)]
  prob2 = seq(0 + pstep, 1, pstep)
  
  ni=dim(obs)[1]
  nj=dim(obs)[2]
  
  #print(dim(obs))
  
  k = 0
  o10 = prob1 * 0
  no10 = prob1 * 0
  
  for (ii in 1:ni) {
    for (jj in 1:nj) {
      if (!is.na(inout[ii, jj])) {
        
        
        
        k = k + 1
        
        #i = 4
        #j = 1
        #print(i); print(j)
        
        x = obs[ii, jj, ]
        x_prob = x * NA
        x_prob[(x > th)] = 0
        x_prob[(x <= th)] = 1
        
        y1 = pred[ii, jj, , ]
        y1_prob = x * NA
        
        for (i in 1:dim(y1)[1]) {
          
          y1_prob[i]=sum(y1[i, !is.na(y1[i, ])]<= th)/length(y1[i, !is.na(y1[i, ])])
          #y1_prob[i] = sum(y1[i, ] <= th,na.rm=TRUE) / dim(y1)[2]
        }
        
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
  far = o10 * NA
  for (ip in 1:length(prob1)) {
    hr[ip] = sum(o10[ip:length(o10)]) / sum(o10)
    far[ip] = sum(no10[ip:length(o10)]) / sum(no10)
    }
  
  
  # inputs already sorted, best scores first 
  #dhr <- c(diff(hr), 0)
  #dfar <- c(diff(far), 0)
  #auc=sum(hr * dfar) + sum(dhr * dfar)/2
  
  height = (hr[-1]+hr[-length(hr)])/2
  width = -diff(far) # = diff(rev(omspec))
  auc=sum(height*width)
  
  
  #  Output
  # ~~~~~~~~
  #
  results <- list(hr,far,auc)  
  #names(results) <- c("HR","FAR")
  
  #return(new("roc",
  #           hr=hr,
  #           far=far))
  
  return(results) 
}