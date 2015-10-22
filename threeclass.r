library(rgl)
library(misc3d)

data('iris')

lim <- apply(iris[,1:4],2,range)
nms <- c('Sepal Length','Sepal Width','Petal Length','Petal Width')

ind <- c(1,2,4)
dat1 <- iris[iris[,5]=='setosa',ind]
dat2 <- iris[iris[,5]=='versicolor',ind]
dat3 <- iris[iris[,5]=='virginica',ind]

fit1 <- kde3d(dat1[,1],dat1[,2],dat1[,3],
              h=0.25,
              n=50,
              lims=c(lim[,ind[1]],
                     lim[,ind[2]],
                     lim[,ind[3]]))

fit2 <- kde3d(dat2[,1],dat2[,2],dat2[,3],
              h=0.25,
              n=50,
              lims=c(lim[,ind[1]],
                     lim[,ind[2]],
                     lim[,ind[3]]))

fit3 <- kde3d(dat3[,1],dat3[,2],dat3[,3],
              h=0.25,
              n=50,
              lims=c(lim[,ind[1]],
                     lim[,ind[2]],
                     lim[,ind[3]]))

thresh <- 0.75

dd <- seq(0.001,floor(max(fit1$d)*1000)/1000,0.0005) # density levels
prb <- rep(NA,length(dd))
delta <- diff(fit1$x)[1]*diff(fit1$y)[1]*diff(fit1$z)[1] # width of grid box (assumes equally spaced)
for(i in 1:length(dd)){
  prb[i] <- sum(fit1$d[fit1$d > dd[i]])/sum(fit1$d) # probability "inside" contour
}
contour3d(f=fit1$d,
          x=fit1$x,y=fit1$y,z=fit1$z,
          level=min(dd[prb <= thresh]),
          alpha=0.5,
          color='red')

dd <- seq(0.001,floor(max(fit2$d)*1000)/1000,0.0005) # density levels
prb <- rep(NA,length(dd))
delta <- diff(fit2$x)[1]*diff(fit2$y)[1]*diff(fit2$z)[1] # width of grid box (assumes equally spaced)
for(i in 1:length(dd)){
  prb[i] <- sum(fit2$d[fit2$d > dd[i]])/sum(fit2$d) # probability "inside" contour
}
contour3d(f=fit2$d,
          x=fit2$x,y=fit2$y,z=fit2$z,
          level=min(dd[prb <= thresh]),
          alpha=0.5,
          color='yellow',
          add=TRUE)

dd <- seq(0.001,floor(max(fit3$d)*1000)/1000,0.0005) # density levels
prb <- rep(NA,length(dd))
delta <- diff(fit3$x)[1]*diff(fit3$y)[1]*diff(fit3$z)[1] # width of grid box (assumes equally spaced)
for(i in 1:length(dd)){
  prb[i] <- sum(fit3$d[fit3$d > dd[i]])/sum(fit3$d) # probability "inside" contour
}
contour3d(f=fit3$d,
          x=fit3$x,y=fit3$y,z=fit3$z,
          level=min(dd[prb <= thresh]),
          alpha=0.5,
          color='blue',
          add=TRUE)

text3d(x=mean(dat1[,1]),y=mean(dat1[,2]),z=mean(dat1[,3]),'setosa')
text3d(x=mean(dat2[,1]),y=mean(dat2[,2]),z=mean(dat2[,3]),'versicolor')
text3d(x=mean(dat3[,1]),y=mean(dat3[,2]),z=mean(dat3[,3]),'virginica')

box3d()
title3d(xlab=nms[ind[1]],ylab=nms[ind[2]],zlab=nms[ind[3]])


