library('parallel')
(core<-detectCores()-1)
cluster<-makeCluster(core)

xbase =seq(from=-5,to=5,by=.01)
system.time(density<-sapply(xbase,function(x) x^1.3252114))
system.time(density<-parSapply(cluster,xbase,function(x) x^1.3252114))

#install.packages('doParallel')
library('doParallel')

registerDoParallel(cores = 7)
getDoParWorkers()
a=31;b=15;c=12

system.time(foreach(x=1:7) %do% {
  x^35-12*x/32/(x^2)-12*c+25*a^(b/10)/log(c)
})
system.time(foreach(x=1:7) %dopar% {
  x^35-12*x/32/(x^2)-12*c+25*a^(b/10)/log(c)
})
install.packages('gputools')
install.packages('devtools')
library('devtools')

install_github('nullsatz/gputools')
install_github('HaoLi111/SimpleAerospace',force=T)
library(MFVN)
