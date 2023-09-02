library(s2dverification)
library(multiApply)

####
# Functions in this file
#  corr.seas - point.corr - fairRPSS.seas - ReliabilityDiagramHist - CFRPSS - FRPSS
#
####

config.verif <-
  function(varr = NULL,
           y1.ref = NULL,
           y2.ref = NULL,
           y1.frc = NULL,
           y2.frc = NULL,
           dataset.ref = NULL,
           status.ref = NULL,
           dataset.frc = NULL,
           status.frc = NULL,
           biascr = F,
           ensmean = F,
           method = NULL,
           chunk = F,
           ncores = 1,
           seas = F,
           latitudes = NULL,
           chunksize = 8,
           category = NULL,
           probcat = NULL,
           ensemb.size = NULL,
           latmax = NULL,
           latmin = NULL,
           lonmax = NULL,
           lonmin = NULL,
           region = NULL,
           category.names = NULL,
           consistbars = NULL,
           chunk.unif = NULL) {
    varrr              <- varr
    
    y1.reff            <- y1.ref
    y2.reff           <- y2.ref
    
    y1.frcc           <- y1.frc
    y2.frcc           <- y2.frc
    
    dataset.reff        <- dataset.ref#'jra55'
    status.reff             <- status.ref # ''
    dataset.frcc        <- dataset.frc #'system5_m1'
    status.frcc         <- status.frc #
    #'bias_correction/'#'raw/stations/raimat'
    #'raw/stations/raimat'
    biascrr           <- biascr #F
    ensmeann <-  ensmean#T
    
    methodd        <- method #'correlation'
    chunkk        <- chunk #F
    ncoress      <- ncores#1
    categoryy    <- category
    probcatt     <- probcat
    ensembb     <- ensemb.size
    latmaxx     <- latmax
    latminn     <- latmin
    lonmaxx     <- lonmax
    lonminn     <- lonmin
    regionn     <- region
    category.namess <- category.names
    consistbarss    <- consistbars
    chunk.uniff     <- chunk.unif
    
    seass          <- seas
    #T       # False if we  want to have monthly data
    if (isTRUE(chunkk)) {
      latitudess     <- latitudes#256
      chunksizee <- chunksize #8
    } else {
      latitudess <- 1
      chunksizee <- 1
    }
    if (!isTRUE(seass)) {
      ltmaxx          <- 7
      movv           <- F
      disgg          <- F
      targg <-
        list('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
      
      storefreqq     <- 'monthly'
      disg.namee <- 'agg'
      mov.namee <-  'monthly'
      
    } else{
      ltmaxx         <- 5
      
      movv           <- T
      disgg          <- F
      targg <-
        list('JFM',
             'FMA',
             'MAM',
             'AMJ',
             'MJJ',
             'JJA',
             'JAS',
             'ASO',
             'SON',
             'OND',
             'NDJ',
             'DJF')
      
      storefreqq     <- 'seasonal'
      if (isTRUE(disgg)) {
        disg.namee <- 'disg'
        
        if (isTRUE(movv)) {
          mov.namee <- 'mov_seas'
          
        } else {
          mov.namee <- 'seas'
          
        }
        
      } else {
        disg.namee <- 'agg'
        
        if (isTRUE(movv)) {
          mov.namee <- 'mov_seas'
          
        } else {
          mov.namee <- 'seas'
          
        }
        
      }
    }
  
    config <-
      list(
        varrr,
        # 1
        y1.reff,
        # 2
        y2.reff,
        # 3
        y1.frcc,
        # 4
        y2.frcc,
        # 5
        dataset.reff,
        # 6
        status.reff,
        # 7
        dataset.frcc,
        # 8
        status.frcc,
        # 9
        biascrr,
        # 10
        ensmeann,
        # 11
        methodd,
        # 12
        chunkk,
        # 13
        ncoress,
        # 14
        seass,
        # 15
        latitudess,
        # 16
        chunksizee,
        # 17
        ltmaxx,
        # 18
        movv,
        # 19
        disgg,
        # 20
        targg,
        # 21
        storefreqq,
        # 22
        disg.namee,
        # 23
        mov.namee,
        # 24
        categoryy,
        # 25
        probcatt,
        # 26
        ensembb,
        # 27
        latmaxx,
        # 28
        latminn,
        # 29
        lonmaxx,
        # 30
        lonminn,
        # 31
        regionn,
        # 32
        category.namess,
        # 33
        consistbarss,
        # 34
        chunk.uniff                     # 35
      )
    return(config)
  }


## FairRPSS. It can be used for seasons or months, if they are formatted according to the funciont retrieve_seasons.R.

#########################
FairRPSS <-
  function (ens,
            ens.ref,
            obs,
            format = c("category", "members"))
    
  {
    SkillScore(
      EnsRps(ens, obs, R.new = Inf, format = format),
      EnsRps(ens.ref, obs, R.new = Inf, format = format),
      handle.na = "na.fail"
    )
  }



FRPSS <-
  function(obs, frc, ltmax, targ, y1, y2, ncor = 1) {
    dummy.obs <- list()
    dummy.frc <- list()
    fairRPSS <- list()
    metric <- list()
    
    for (rs in 1:length(targ)) {
      print(rs)
      
      dummy.frc[[rs]] <- drop(frc[[rs]])
      
      dummy.obs[[rs]] <- drop(obs[[rs]])
      
      dims.obs <- dim(dummy.obs[[rs]])
      dims.frc <- dim(dummy.frc[[rs]])
      
      if (is.null(dims.obs)) {
        dims.obs <- length(dummy.obs[[rs]])
      }
      
      if (dims.obs[1] != dims.frc[1]) {
        frc_ini <- 1
        frc_end <-
          ifelse(dims.obs[1] < (y2 - y1 + 1), (dims.frc[1] - 1), dims.frc[1])
        obs_ini <- ifelse(dims.frc[1] < (y2 - y1 + 1), 2, 1)
        obs_end <- dims.obs[1]
        
        if (length(dims.obs) <= 2) {
          dummy.obs[[rs]] <- dummy.obs[[rs]][c(obs_ini:obs_end), ]
          dummy.frc[[rs]] <- dummy.frc[[rs]][c(frc_ini:frc_end), , ]
          
        } else {
          dummy.obs[[rs]] <- dummy.obs[[rs]][c(obs_ini:obs_end), , , ]
          dummy.frc[[rs]] <-
            dummy.frc[[rs]][c(frc_ini:frc_end), , , , ]
          
        }
        
      }
      
      print(dim(dummy.frc[[rs]]))
      
      if (length(dims.obs) <= 2) {
        print(dim(dummy.obs[[rs]]))
        
        obs.ind.na <- which(!is.na(dummy.obs[[rs]][, 1]))
        
        dummy.obs[[rs]] <- dummy.obs[[rs]][obs.ind.na, ]
        
        dummy.array <-
          array(NA, c(length(obs.ind.na), dims.frc[2], dims.frc[3]))
        
        for (i in seq(1:length(obs.ind.na))) {
          dummy.array[i, , ] <- dummy.frc[[rs]][obs.ind.na[i], , ]
        }
        
        clim.margins <- c(2, 3)
        
        dummy.frc[[rs]] <- dummy.array
      } else {
        clim.margins <- c(2, 3, 4)
        
      }
     print('na')
print(dummy.array)
      dummy.obs2 <-
        InsertDim(InsertDim(InsertDim(dummy.obs[[rs]], 1, 1), 2, 1), 5, ltmax) # retrieve the same dimensions as Load output (Corr s2d expects that)
      
      dummy.frc2 <-
        InsertDim(InsertDim(dummy.frc[[rs]], 1, 1), 2, 1) # return the same dimensions as Load output (corr s2d expects that)
      
      obs.clim <-
        array(apply(drop(dummy.obs2), clim.margins, sum), dim = dim(dummy.obs2)[-c(3)])
      
      dims.obs2 <- dim(dummy.obs2)
      
      obs.clim <- InsertDim(obs.clim, 3, dims.obs2[3])
      
      print(dim(dummy.frc2))
      print(dim(dummy.obs2))
      print(dim(obs.clim))
      
      targ.dim <- list(c(1, 2), c(1, 2), c(1, 2))
      
      metric <-
        Apply(
          list(drop(dummy.frc2), drop(obs.clim), drop(dummy.obs2)),
          AtomicFun = "FairRPSS",
          format = "members",
          target_dims = targ.dim,
          ncores = ncor
        )
      
      fairRPSS[[rs]] <- metric$output1
      
      names(fairRPSS)[rs] <- paste(targ[[rs]], sep = '')
      
    }
    return(fairRPSS)
  }

#########################

## ContinousFairRPSS (CFRPSS). It can be used for seasons or months, if they are formatted according to the funciont retrieve_seasons.R.

#########################

# We have to change the category by the ensemble tal cual

CFRPSS <-
  function(obs, frc, ltmax, targ, y1, y2, ncor = 1) {
    dummy.obs <- list()
    dummy.frc <- list()
    metric    <- list()
    
    for (rs in 1:length(targ)) {
      print(rs)
      
      dummy.frc[[rs]] <- drop(frc[[rs]])
      
      dummy.obs[[rs]] <- drop(obs[[rs]])
      
      dims.obs <- dim(dummy.obs[[rs]])
      dims.frc <- dim(dummy.frc[[rs]])
      
      if (is.null(dims.obs)) {
        dims.obs <- length(dummy.obs[[rs]])
      }
      
      if (dims.obs[1] != dims.frc[1]) {
        frc_ini <- 1
        frc_end <-
          ifelse(dims.obs[1] < (y2 - y1 + 1), (dims.frc[1] - 1), dims.frc[1])
        obs_ini <- ifelse(dims.frc[1] < (y2 - y1 + 1), 2, 1)
        obs_end <- dims.obs[1]
        
        if (length(dims.obs) == 1) {
          dummy.obs[[rs]] <- dummy.obs[[rs]][c(obs_ini:obs_end)]
          dummy.frc[[rs]] <- dummy.frc[[rs]][c(frc_ini:frc_end), , ]
          
          score.margins <- c(2)
          
          
        } else {
          dummy.obs[[rs]] <- dummy.obs[[rs]][c(obs_ini:obs_end), , ]
          dummy.frc[[rs]] <-
            dummy.frc[[rs]][c(frc_ini:frc_end), , , , ]
          
          score.margins <- c(2, 3, 4)
          
        }
        
      } else {
        if (length(dims.obs) == 1) {
          score.margins <- c(2)
        } else {
          score.margins <- c(2, 3, 4)
        }
        
      }
      
      inv.margins.frc <- list(c(1, 2), c(1))
      
      if (length(dims.obs) == 1) {
        obs.ind.na <- which(!is.na(dummy.obs[[rs]]))
        dummy.obs[[rs]] <- dummy.obs[[rs]][obs.ind.na]
        
        dummy.array <-
          array(NA, c(length(obs.ind.na), dims.frc[2], dims.frc[3]))
        
        for (i in seq(1:length(obs.ind.na))) {
          dummy.array[i, , ] <- dummy.frc[[rs]][obs.ind.na[i], , ]
        }
        
        dummy.frc[[rs]] <- dummy.array
      }
      
      dummy.obs2 <-
        InsertDim(InsertDim(InsertDim(InsertDim(dummy.obs[[rs]], 1, 1), 2, 1), 4, dims.frc[2]), 5, ltmax) # retrieve the same dimensions as Load output (Corr s2d expects that)
      
      dummy.frc2 <-
        InsertDim(InsertDim(dummy.frc[[rs]], 1, 1), 2, 1) # return the same dimensions as Load output (corr s2d expects that)
      
      clims <- Clim(dummy.frc2, dummy.obs2)
      
      ano.obs <- Ano(dummy.obs2, clims$clim_obs)
      ano.frc <- Ano(dummy.frc2, clims$clim_exp)
      
      if (length(dims.obs) == 1) {
        ano.obs <- ano.obs[, , , 1,]
      } else {
        ano.obs <- ano.obs[, , , 1, , ,]
      }
      
      ano.obs.ens <-         Apply(list(ano.obs),
                                   AtomicFun = "ClimEns",
                                   target_dims = list(c(1),
                                                      ncores = ncor))
      
      ano.obs.ens <- ano.obs.ens$output1
      
      metric.frc <-
        Apply(
          list(drop(ano.frc), ano.obs),
          AtomicFun = "FairCrps",
          target_dims = inv.margins.frc,
          ncores = ncor
        )
      
      metric.frc <- metric.frc$output1
      
      metric.clim <-
        Apply(
          list(ano.obs.ens, ano.obs),
          AtomicFun = "FairCrps",
          target_dims = inv.margins.frc,
          ncores = ncor
        )
      
      metric.clim <- metric.clim$output1
      
      metric.clim <- drop(metric.clim)
      metric.frc <- drop(metric.frc)
      
      print(dim(metric.clim))
      print(dim(metric.frc))
      
      metric.score <-
        1 - (
          apply(metric.frc, score.margins, "mean") / apply(metric.clim, score.margins, "mean")
        )
      
      metric [[rs]] <- metric.score
      
      names(metric)[rs] <- paste(targ[[rs]], sep = '')
      
    }
    
    return(metric)
  }

######################################################################
# #
# RELIABILITY DIAGRAM FOR A COLLECTION OF PROBABILITY FORECASTS #
### Veronica: Modified to include in the outputs the hist.counts
# #
######################################################################

ReliabilityDiagramHist <-
  function(probss,
           obss,
           bins = 10,
           nboot = 500,
           plot = FALSE,
           plot.refin = TRUE,
           mc.cores = 1,
           cons.probs = c(0.025, 0.975),
           bins.plot = 10,
           consbars = NULL,
           colLine = NULL,
           colBar = NULL,
           marHist = T,
           hist_ylim = NULL,
           category.names = 1,
           varr.name,
           targ = 1,
           outputdir,
           method.name,
           leadmax = 1,
           y1,
           y2,
           ref.dataset = NULL,
           frc.dataset = NULL,
           reg.name = NULL)

  {
    dummy.obs <- list()
    dummy.frc <- list()
    
    for (numb in 1:length(targ)) {
      dummy.year <- targ[[numb]]
      
      # if (numb < leadmax) {
      #   y1 <- y1 + 1
      # }
      # 
      # if (dummy.year == 'DJF' | dummy.year == 'NDJ') {
      #   y2 <- y2 - 1
      # }
      for (ij in 1:leadmax) {
        rel.diag <- c()
        for (sr in 1:length(category.names)) {
          print("RD")
          
          dims.obs <- dim(obss[[numb]])
          dims.probs <- dim(probss[[numb]])
  

          if (is.null(dims.obs)) {
            dims.obs <- length(obss[[numb]])
          }
          
          print(dim(probss[[numb]]))
          if (dims.obs[1] != dims.probs[1]) {

            frc_ini <- 1
            frc_end <-
              ifelse(dims.obs[1] < (y2 - y1 + 1), (dims.probs[1] - 1), dims.probs[1])
            obs_ini <- ifelse(dims.probs[1] < (y2 - y1 + 1), 2, 1)
            obs_end <- dims.obs[1]
 
            if (length(dims.obs) <= 2) {
            
              obss[[numb]] <- obss[[numb]][c(obs_ini:obs_end), ]

              probss[[numb]] <-
                probss[[numb]][c(frc_ini:frc_end), , ]
              
            } else {

              obss[[numb]] <- obss[[numb]][c(obs_ini:obs_end), , ]
              probss[[numb]] <-
                probss[[numb]][c(frc_ini:frc_end), , ,]
              
            }
            
          }
        
           print(dim(probss[[numb]]))
          if (length(dims.obs) <= 2) {
            obs.ind.na <- which(!is.na(obss[[numb]][, 1]))
            
            obss[[numb]] <- obss[[numb]][obs.ind.na, ]
            
            dummy.array <-
              array(NA, c(length(obs.ind.na), dims.probs[2], dims.probs[3]))

            for (i in seq(1:length(obs.ind.na))) {
              dummy.array[i, , ] <- probss[[numb]][obs.ind.na[i], , ]
            }
            
            probss[[numb]] <- dummy.array
            
            obs   <- obss[[numb]][, sr]
            probs <- probss[[numb]][, sr, ij]
            
          } else {
            obs   <- dummy.obs[[numb]][, sr,]
            probs <- dummy.frc[[numb]][, sr, ij,]
          }

          #
          # Plot reliability diagram for a probability forecast
          #
          # Usage: ReliabilityDiagram(probs, obs, nbins, nboot)
          #
          # Arguments:
          #
          # probs ... vector of length N, probs[k] has the predicted probability for
          # the event obs[k]
          # obs ... obs[k] = 1 if the event happened at instance k, obs[k] = 0
          # otherwise
          # bins ... either scalar: number of equidistant bins to discretize the
          # forecast probabilities,
          # or a vector: user-defined breakpoints of the bins; the `hist`
          # function will produce errors if these are not valid
          # nboot ... number of bootstrap resamples for estimating consistency bars
          # if nboot==0, no resampling is done and NAs are returned as
          # consistency bars
          # plot ... boolean; whether to plot the reliability diagram
          # plot.refin ... boolean; whether to plot the small refinement histogram
          # in lower right corner
          # cons.probs ... a 2-vector, lower and upper confidence limit
          # mc.cores ... number of cores for resampling (if > 1, library `multicore`
          # is required)
          #
          # Return value:
          #
          # a data frame of K+1 rows with the following columns:
          #
          # * p.avgs ... in-bin averages of the forecast probabilities
          # * cond.probs ... observed conditional frequency of event, given i
          # * cbar.lo ... lower limit consistency of consistency bar[i], as specified by user
          # * cbar.hi ... upper limit consistency of consistency bar[i], as specified by user
          #
          # Author:
          #
          # Stefan Siegert
          # s.siegert@exeter.ac.uk
          # December 2013
          #
          # Example:
          #
          # N <- 1000
          # p <- rbeta(N, 1, 3)
          # y <- rbinom(N, 1, p)
          # rd <- rel.diag(p, y, plot=TRUE)
          # print(rd)
          #
          #
          # change log:
          #
          # 2013/12/02
          # * manual definition of bin-breaks
          # * manual definition of consistency intervals
          # * sanity checks
          # * multicore option for resampling
          #
          # 2013/10/31:
          # * return summary data as data frame
          # * added options `plot` and `plot.refin`
          #
          # 2013/08/20:
          # * points are plotted at in-bin-averages, not at bin centres
          # * legend has been removed
          # * consistency bars have been added, calculated by a resampling technique
          # * see Broecker (2007) http://dx.doi.org/10.1175/WAF993.1 for details
          # * the bars are pointwise 2.5% ... 97.5% intervals around the hypothesis of reliability
          # * dependency on package "verification" was removed
          #
          # Author: Stefan Siegert <s.siegert@exeter.ac.uk>
          #
          # based on previous version by Caio Coelho and the routine
          # reliability.plot.default of the R-package `verification`
          #
          
          # sanity checks
          if (class(probs) == "data.frame") {
            probs <- c(as.matrix(probs))
          }
          if (class(obs) == "data.frame") {
            obs <- c(as.matrix(obs))
          }
          stopifnot(length(probs) == length(obs))
          stopifnot(nboot >= 0, mc.cores >= 0)
          stopifnot(all(probs >= 0), all(probs <= 1), all(obs %in% c(0, 1)))
          stopifnot(length(cons.probs) == 2,
                    all(cons.probs >= 0),
                    all(cons.probs <= 1))
          # optional use of multicore without warning message
          warn <- getOption("warn")
          options(warn = -1)
          if (require(multicore, quietly = TRUE)) {
            mclapply <- multicore::mclapply
          } else {
            mclapply <- function(..., mc.cores)
              lapply(...)
          }
          options(warn = warn)
          
          # some definitions and corrections
          n <- length(obs)
          mc.cores <- floor(mc.cores)
          nboot <- floor(nboot)
          cons.probs <- sort(cons.probs)
          
          #############################################
          # reliability analysis
          #############################################
          # estimate refinement function
          if (length(bins) == 1) {
            nbins <- floor(bins)
            brx <- seq(0, 1, length.out = nbins + 1) +
              c(-.1, rep(0, nbins - 1), .1)
          } else {
            nbins <- length(bins) - 1
            bins <- sort(bins)
            stopifnot(min(bins) <= 0 & max(bins) >= 1)
            brx <- bins
          }
          h <- hist(probs, breaks = brx, plot = FALSE)$counts
          #print(h)
          #print(sum(h))
          #print(probs)
          #print(sum(probs))
          p <- sum(probs)
          print(sum(probs) / sum(h))
          
          # estimate calibration function
          g <-
            hist(probs[obs == 1], breaks = brx, plot = FALSE)$counts
          #print(g)
          #print(sum(g))
          obar.i <- g / h
          #print(obar.i)
          no_res <- sum(g) / sum(h)
          print(no_res)
          obar.i[is.nan(obar.i)] <- NA
          
          # calculate in-bin averages
          p.bins <-
            as.numeric(cut(probs, breaks = brx, include.lowest = TRUE))
          p.avgs <- sapply(seq(nbins),
                           function(ii)
                             mean(probs[p.bins == ii], na.rm = TRUE))
          p.avgs[is.nan(p.avgs)] <- NA
          
          #
          #print(p.avgs)
          #    vertline <- sum(p.avgs,na.rm = TRUE)/bins
          #print(vertline)
          
          #############################################
          # consistency resampling (broecker and smith 2007)
          #############################################
          if (nboot) {
            resamp.mat <- matrix(nrow = 0, ncol = nbins)
            # the resampling function
            sample.rel.diag <- function(dummy = 0) {
              p.hat <- sample(x = probs,
                              size = n,
                              replace = TRUE)
              x.hat <- rbinom(n = n,
                              size = 1,
                              prob = p.hat)
              hh <- hist(p.hat, breaks = brx, plot = FALSE)$counts
              gg <-
                hist(p.hat[x.hat == 1], breaks = brx, plot = FALSE)$counts
              return(gg / hh)
            }
            # multicore?
            if (mc.cores > 1) {
              l <- mclapply(1:nboot, sample.rel.diag, mc.cores = mc.cores)
              resamp.mat <- do.call(rbind, l)
            } else {
              l <- replicate(nboot, sample.rel.diag())
              resamp.mat <- t(l)
            }
            cons.bars <- apply(resamp.mat, 2,
                               function(z)
                                 quantile(z, cons.probs, na.rm = TRUE))
          } else {
            cons.bars <- matrix(NA, ncol = nbins, nrow = 2)
          }
          
          #############################################
          # return data
          #############################################
          ret.df <- data.frame(
            p.avgs = p.avgs,
            cond.probs = obar.i,
            cbar.lo = cons.bars[1, ],
            cbar.hi = cons.bars[2, ],
            hist.counts = h,
            obs.counts = g,
            for.prob = p
          )
          
          rel.diag[[sr]] <- ret.df
          
        }
        
        print(length(rel.diag))
        
        output.plot <-
          plot.output.names(
            varr.name,
            targ[[numb]],
            ref.dataset = ref.dataset,
            frc.dataset = frc.dataset,
            outputdir,
            method.name,
            leadmax = leadmax,
            lead = ij,
            y1,
            y2,
            reg.name
          )
        
        out.file <- output.plot[[1]]
        tit <- output.plot [[2]]
        
        # Aqui s'ha d'acabar un for que vagi per totes les categories. En total hi ha d'haver dos for: un per cada ??poca de l'any i
        # l'altre per cada categoria (el de les categories ha de ser local, mentre que el de l'??poca ha de ser particular)
        # cal incloure el t??tol i el guardat.
        
        #   rel.diag<-rd
        #   nbins=10
        #   consbars=T
        #   colLine=col_line
        #   colBar=col_bar
        #   tit=tit1
        #   marHist=T
        #   hist_ylim=c(0,100)
        #   x11(width=12,height=10)
        #   x11()
        #  PLOT OF THE RELIABILITY DIAGRAM
        #
        ######################################################################################
        # rd: a list with the reliability diagrams that will be represented in the same plot
        # cons.bars : if the consistency bar must be represented or not.
        # nbins : number of equidistant points used to compute the reliability diagram (optional)
        # tit: the title of the plot (optional)
        # brierScores: The brier score linked to the reliability diagram (optional)
        # marHist: Whether to plot the small refinement histogram is showed
        #####################################################################################
        
        # Some parameters are defined
        nrd <- length(rel.diag)
        rg <- list()
        # Check the dimensions of the rank histogram
        for (i in 1:nrd) {
          if (dim(rel.diag[[i]])[1] != nbins) {
            stop ('The bins of the reliability diagram must be the same that nbins')
          }
          rg[[i]] <-
            range(rel.diag[[i]]$hist.counts)# check the range of the histograms
        }
        if (is.null(hist_ylim)) {
          rgH <- range(rg)
        } else{
          rgH <- hist_ylim
        }
        
        
        ##########################################
        # reliability plot
        ##########################################
        
        cairo_ps(out.file, width = 22, height = 19.5) # we have to use this one instead of postscript because it supports transparency
        
        layout(matrix(c(rep(1, nrd), seq(2, (
          nrd + 1
        ))), nrd, 2, byrow = F), width = c(9, 2))
        #par(oma = c(2.5, 4, 5, 2))
        par(oma = c(10, 10, 10, 10))
        #layout.show(a)
        
        # The axis are defined
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #x11(width=12,height=10)
        old.par <- par(no.readonly = TRUE)
        on.exit(par(old.par))
        par(mar = c(9, 10, 8, 8))
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = T,
          xlab = "Forecast probability",
          ylab = "Observed probability",
          at = seq(0, 1, by = 0.1),
          labels = seq(0, 1, by = 0.1),
          mgp = c(7, 2, 0),
          cex.lab = 4.5,
          cex.axis = 3.5
        )
        #         axis(
        #           1,
        #           at = seq(0, 1, by = 0.1),
        #           labels = seq(0, 1, by = 0.1),
        #           cex.axis = 3
        #         )
        
        #         title(xlab = "Forecast probability",
        #               #line = 1,
        #               cex.lab = 3.5,
        #               outer = T
        #               )
        #         axis(
        #           2,
        #           at = seq(0, 1, by = 0.1),
        #           labels = seq(0, 1, by = 0.1),
        #           las = 1,
        #           cex.axis = 3
        #        )
        box()
        
        #         title(
        #           ylab = "Observed relative frequency",
        #           #line = 3,
        #           cex.lab = 3.5
        #         )
        if (is.null(tit) == F) {
          title(tit,
                cex.main = 3,
                outer = T,
                line = 0.5)
        }
        
        # Consistency bars
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        HI <- matrix(NA,
                     nrow = nrd,
                     ncol = length(rel.diag[[1]]$hist.counts))
        print(nrd)
        for (j in 1:nrd) {
          HI[j, ] <- rel.diag[[j]]$hist.counts
          if (consbars == T) {
            # The lower limit consestince of consistency bar i and the upper limit are combined in one list
            consBars <- list()
            consBars[[j]] <-
              abind(
                InsertDim(rel.diag[[j]]$cbar.lo, 1, 1),
                InsertDim(rel.diag[[j]]$cbar.hi, 1, 1),
                along = 1
              )
            # plot consistency bars
            for (i in 1:nbins) {
              lines(rep(rel.diag[[j]]$p.avgs[i], 2),
                    consBars[[j]][, i],
                    col = colBar[j],
                    lwd = 3)
            }
          }
          
          points(
            rel.diag[[j]]$p.avgs,
            rel.diag[[j]]$cond.probs,
            type = "b",
            pch = 1 ,
            col = colLine[[j]],
            cex = 2.5 ,
            lwd = 4
          )
          
        }
        
        lines(c(0, 1), c(0, 1), lty = 1)
        
        if (marHist == TRUE) {
          for (i in 1:nrd) {
            par(mar = c(9, 0, 8, 2))
            barplot(
              HI[i, ],
              beside = T,
              space = c(0, 1.2),
              axes = FALSE,
              axisnames = FALSE,
              col = colLine[[i]],
              main = category.names[[i]],
              ylim = rgH,
              cex.main = 2.5
            )
            grid(1, 5, col = '#525252')
            axis(4, cex.axis = 2)
            box(bg = 'grey')
          }
          #pp<- par("plt")
          #par("plt" = c(pp[2] - 0.14 , pp[2], pp[3], pp[3]+ 0.15) )
          #par(new = TRUE)
        }
        
        dev.off()
        
      }
    }
    
  }
