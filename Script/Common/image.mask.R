image.mask <- function(lon,lat,z,  zlim, col, crop.color='gray', axis.args,...)
{
  zstep <- (zlim[2] - zlim[1]) / length(col); # step in the color palette
  newz.na <- zlim[2] + 1 * zstep # new z for crop with no model
  z[which(z==-999)] <- newz.na # same for newz.na
  zlim[2] <- zlim[2] + 1 * zstep # extend top limit to include the two new values above and na
  col <- c(col, crop.color) #correct by including col[1] at bottom of range
  image.plot(lon,lat,z=z,  zlim=zlim, col=col, axis.args = axis.args,...) # we finally call image(...)
  plot(wrld_simpl, add = TRUE)
}


