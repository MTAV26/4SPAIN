## An example of netcdf creation in R
# important steps: ncdim_def, ncvar_def, nc_create, ncvar_put

  ## write netcdf file
  x = ncdim_def(name = "lon", units = "Degrees", vals = longitude, longname = "Longitude")
  y = ncdim_def(name = "lat", units = "Degrees", vals = latitude, longname = "Latitude")
  
  RE_ = ncvar_def("RE", "", list(x,y), -1, prec="float")
  CE_ = ncvar_def("CE", "", list(x,y), -1, prec="float")
  EXPL_ = ncvar_def("EXPL", "", list(x,y), -1, prec="float")
  
  nc_val= nc_create(filename=paste("SUM_STAT_7_16_", nest, ".nc", sep=""), list(RE_, CE_, EXPL_))
  
  load(paste("Models_EOF1_7_16_", nest, ".RData", sep=""))
  load(paste("FULL_model_", nest, ".RData", sep=""))
  
  for(i in 1:length(models))
  {
    k = which(longitude == CDD_7_16$X[i])
    l = which(latitude == CDD_7_16$Y[i])
    ncvar_put(nc = nc_val, varid = "RE", vals = mean(models[[i]]$RE, na.rm=TRUE), start=c(k,l), count=c(1,1))  
    ncvar_put(nc = nc_val, varid = "CE", vals = mean(models[[i]]$CE, na.rm=TRUE), start=c(k,l), count=c(1,1))  
    ncvar_put(nc = nc_val, varid = "RMSE", vals = mean(models[[i]]$RMSE, na.rm=TRUE), start=c(k,l), count=c(1,1))  
    ncvar_put(nc = nc_val, varid = "R2", vals = mean(models[[i]]$R2, na.rm=TRUE), start=c(k,l), count=c(1,1))  
    ncvar_put(nc = nc_val, varid = "EXPL", vals = mean(models[[i]]$EXPL, na.rm=TRUE), start=c(k,l), count=c(1,1))  
    ncvar_put(nc = nc_val, varid = "COR", vals = mean(models[[i]]$COR, na.rm=TRUE), start=c(k,l), count=c(1,1))  
  }
  nc_close(nc_val)

