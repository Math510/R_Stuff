library(rgl)
library(misc3d)
library(MASS)

# standard Gaussian data
xi <- matrix(rnorm(3000),1000,3)

# three-dimensional kernel density estimate, default bandwidth (Gaussian reference rule).
fit <- kde3d(xi[,1],xi[,2],xi[,3],
             h=0.3, # bandwidth
             n=50, # number of estimation points per dimension
             lims=c(-4,4, # x-dim limits
                    -4,4, # y-dim limits
                    -4,4)) # z-dim limits

# find probability shells
dd <- seq(0.001,floor(max(fit$d)*1000)/1000,0.00025) # density levels
prb <- rep(NA,length(dd))
delta <- diff(fit$x)[1]*diff(fit$y)[1]*diff(fit$z)[1] # width of grid box (assumes equally spaced)

for(i in 1:length(dd)){
  prb[i] <- sum(fit$d[fit$d > dd[i]])/sum(fit$d) # probability "inside" contour
}

# 50, 90 percent levels
lvls <- c(min(dd[prb <= 0.5]),
          min(dd[prb <= 0.75]),
          min(dd[prb <= 0.9]))

contour3d(f=fit$d,
          x=fit$x,y=fit$y,z=fit$z,
          level=lvls,
          alpha=c(0.5, 0.25,0.1), # contour transparency level, inside contours should be less transparant
          color=c('red','blue', 'white')) # contour color
box3d() # add bounding box
title3d(xlab='x',ylab='y',zlab='z') # add axis labels, see axis3d for additional box/text options
axes3d()



