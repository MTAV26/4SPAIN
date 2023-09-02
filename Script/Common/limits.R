####################################

#Plot-map bar limits and colorbar retrieval

####################################

limits <-
  function(varr,
           round.factor,
           up.percentile = 97.5,
           low.percentile = 2.5,
           lim_sup = NULL,
           lim_inf = NULL,
           num.colors = NULL
  ) {
    
    if (is.null(lim_sup)) {
      lim_sup = 0
    }
    
    if (is.null(lim_inf)) {
      lim_inf = 0
    }
    
    dummy.round.factor <- round.factor
    xif.signif <- round.factor
    
    while (lim_sup == lim_inf) {
      lim_sup <-
        max(quantile((varr), up.percentile / 100, na.rm = TRUE), na.rm = TRUE)
      lim_inf <-
        min(quantile((varr), low.percentile / 100, na.rm = TRUE), na.rm = TRUE)
      
      dummy.round.factor <- dummy.round.factor + 1
      
    }
    
    round.factor <- 10 * round.factor
    
    if (lim_inf >= 0) {
      col1 <- c()
      lim_inf <- round(lim_inf * round.factor) / round.factor
      lim_sup <- round(lim_sup * round.factor) / round.factor
      if (lim_inf == lim_sup) {
        lim_inf = lim_inf - round(1.5 * round.factor) / round.factor
        lim_sup = lim_sup + round(1.5 * round.factor) / round.factor
      }
      
      if (is.null(num.colors)) {
        num.colors <- 4
      }
      
      colfunc <- colorRampPalette(c('#ffca21','#ff9c72','#ff6b4f','#ff0000'))
      col1 <- colfunc(num.colors)
      
      colmin <- '#FFFFFF'
      colmax <- '#8b0000'
      
      
    } else {
      col1 <- c()
      if (lim_sup > 0) {
        lim_sup <- round(lim_sup * round.factor) / round.factor
        lim_inf <- round(lim_inf * round.factor) / round.factor
        
        if (lim_inf == lim_sup) {
          lim_inf = lim_inf - round(1.5 * round.factor) / round.factor
          lim_sup = lim_sup + round(1.5 * round.factor) / round.factor
        }
        if (abs(lim_sup) > abs(lim_inf)) {
          lim_inf = -lim_sup
        } else {
          lim_sup = -lim_inf
        }
        
        if (is.null(num.colors)) {
          num.colors <- 8
        }
        
        colfunc <- colorRampPalette(c('#0000ff','#4c59ff', '#668fff','#7ec2fa',
                                      '#FFFFFF','#FFFFFF','#FFFFFF',
                                      '#ffca21','#ff9c72','#ff6b4f','#ff0000'))
        col1 <- colfunc(num.colors)
        
        colmin <- '#00008b'
        colmax <- '#8b0000'
      }
      if (lim_sup <= 0) {
        col1 <- c()
        lim_sup <- round(lim_sup * round.factor) / round.factor
        lim_inf <- round(lim_inf * round.factor) / round.factor
        if (lim_inf == lim_sup) {
          lim_inf = lim_inf - round(1.5 * round.factor) / round.factor
          lim_sup = lim_sup + round(1.5 * round.factor) / round.factor
        }
        
        if (is.null(num.colors)) {
          num.colors <- 4
        }
        
        colfunc <- colorRampPalette(c('#0000ff', '#4c59ff', '#668fff','#7ec2fa'))
        col1 <- colfunc(num.colors)
        
        #col1 <- c('#0000ff', '#4c59ff', '#668fff', )
        colmin <- '#00008b'
        colmax <- '#FFFFFF'
      }
    } 
    
    lim_sup <- signif(lim_sup, xif.signif)
    lim_inf <- signif(lim_inf, xif.signif)
    
    limits <- list(col1, colmin, colmax, lim_inf, lim_sup)
    names(limits)
    
    print(limits)
    
    return(limits)
    
  } 