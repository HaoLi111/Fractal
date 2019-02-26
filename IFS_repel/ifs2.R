# IFS from
browseURL('https://www.paulbourke.net/fractals/ifs/')
# From the gallery by Paul Bourke

# The 2d transformation rule follows
# x_n+1= a*x_n+b*y_n+e
# y_n+1= c*y_n+d*n+f
# which is equivalent to
# [         [         [    [
#   x         a   b    x    e 
#   y    <-   c   d   .y +  f
#     ]             ]   ]    ]

#a2 + d2 < 1
#b2 + e2 < 1
#a2 + b2 + d2 + e2 < 1 + (ae - db)2

is.feasible<-function(m1,m2){
  m[1,1]^2+m[2,1]^2<1 & m[2,2]^2+ m[2,1]^2<1 & sum(m^2)<1+(m[1,1]*m[2,2]-m[2,1]*m[1,2])^2
}
iterate2d_lin<-function(init,m1,m2,n=20){
  for(i in 1:n){
  init<-m1%*%init+m2
  }
  init
}



m1=rbind(c(.14,.01),c(.0,.51))
m2=c(-.08,-1.31)
nrand<-10000
n=20
m<-cbind(runif(nrand),runif(nrand))

IFS_randxy<-function(m1,m2,n,nrand=20){
  m<-cbind(runif(nrand),runif(nrand))
  v=matrix(NA,nrand,2)
  for(i in 1:nrand){
    v[i,]<-iterate2d_lin(init=m[i,],m1,m2,n)
  }
  v
}

v=IFS_randxy(m1,m2,n,nrand)

plot(v,type='p')

library(grDevices)
jpeg('1.jpeg')
tiff('i.tif')
