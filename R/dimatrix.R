### Build matrix n pixels x n pixels with euclidean distances between pixels centroids ###

#input: 
  # size of landscape and resolution in m
  # or
  # raster map (maybe, for landscapes of different shapes??)

xp <- rep(c(1:60), 60) # x coordinates
yp <- rep(c(1:60), each=60) # y coordinates
xv = cbind(xp,yp)
di <- as.matrix(dist(xv,upper=T,diag=T))#matrix of distances between pixels;in m/10; maybe change for m or km to make it easier to interpretate
#di <- di/100 #matrix of distances between pixels; in km