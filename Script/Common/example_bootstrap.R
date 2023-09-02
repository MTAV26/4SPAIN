performBootstrap<-function(series, bootstrap.method=c("meboot", "tsbootstrap", "tsboot", "simple"), mc.replications=100, tsbootstrap.type=c("stationary", "block"), tsbootstrap.block.length=5, sample.replace=TRUE) {
	bootstrap.method <- bootstrap.method[1]
	
	if (!(bootstrap.method %in% c("meboot", "tsbootstrap", "tsboot", "simple"))) {
		stop(paste0("[ERROR] Unsupported bootstrap.method=", bootstrap.method, " . Currently supported are bootstrap.method=c(\"meboot\", \"tsbootstrap\", \"tsboot\", \"simple\")"))
	}
	
	if (bootstrap.method == "meboot") {
		# From package meboot
		meboot.result <- meboot(x=as.ts(series[, 1]), reps=mc.replications)
		
		meboot.ensemble <- meboot.result$ensemble
		colnames(meboot.ensemble) <- make.names(colnames(meboot.ensemble))
		
		bootstrapped.returns <- reclass(meboot.ensemble, series)
	} else if (bootstrap.method == "tsbootstrap") {
		# From package tseries
		block.length <- b.star(series[, 1], round=TRUE)[1]
		tsbootstrap.result <- tsbootstrap(x=series[, 1], nb=mc.replications, type=tsbootstrap.type, b=block.length)#tsbootstrap.block.length)
		
		bootstrapped.returns <- reclass(tsbootstrap.result, series)
	} else if (bootstrap.method == "tsboot") {
		# From package nb
		# b.star is a function which computes optimal block lengths for the 
		# stationary and circular bootstraps. This allows the use of tsboot 
		# from the boot package to be fully automatic by using the output 
		# from b.star as an input to the argument l = in tsboot.
		block.length <- b.star(series[, 1], round=TRUE)[1] # for "geom", mean of blocks dist.	
		
		# tsboot needs a statstics function but I actually want all the paths (no statistcs applied)
		id.fun <- function(x) {
			return(x)
		}
		
		# From package boot
		tsboot.result <- tsboot(tseries=series[, 1],
								statistic=id.fun,
								R=mc.replications,
								sim="geom",  #"geom"
								l=block.length)
				
		bootstrapped.returns <- reclass(t(tsboot.result$t), series)
	} else if (bootstrap.method == "simple") {
		coredata.of.xts <- coredata(series[, 1])
		bootstrapped.returns <- reclass(do.call(cbind, replicate(mc.replications, coredata.of.xts[sample(1:NROW(coredata.of.xts), replace=sample.replace), ], simplify=FALSE)), series)
	} else {
		stop(paste0("[ERROR] Unsupported bootstrap.method=", bootstrap.method, " . Currently supported are bootstrap.method=c(\"meboot\", \"tsbootstrap\", \"tsboot\", \"simple\")"))
	}
	
	return(bootstrapped.returns)
}