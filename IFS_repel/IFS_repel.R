#library(parallel)
#n=detectCores()
#cl=makeCluster(n)


library(doParallel)
registerDoParallel(7)
getDoParWorkers()


ParSetup<-function(core=7){
  library(doParallel)
  registerDoParallel(core)
  getDoParWorkers()
}

ParSetup(6)
#IFS

Julia<-function(z,C) z=z^2+C
modlim=2


rbase=seq(from=-2.5,to=1,by=.001)
ibase=seq(from=-1.4,to=1.4,by=.001)





escape<-function(m,modlim,itfunc,takein=NULL,n=100){
  i<-0
  while(Mod(m)<modlim&i<n){
    i<-i+1
    if(is.null(takein)){
      m<-itfunc(m)
    }else{
      m<-itfunc(m,takein)
    }
  }
  i
}

#system.time({
#m=matrix(NA,length(rbase),length(ibase))
#for(y in seq_along(ibase)){
 # for(x in seq_along(rbase)){
  #  a=complex(real=rbase[x],imaginary = ibase[y])
   # m[x,y]<-escape(a,modlim = 2,itfunc = Julia,takein = a)
  #}
#}
#})

MapIFS_repel_complex<-function(modlim,itfunc,takein=NULL,n,rbase=seq(from=-2.5,to=1,by=.001),
                    ibase=seq(from=-1,to=1,by=.001),type='j'){
    m=matrix(NA,length(rbase),length(ibase))
    if(type=='m'){
      for(y in seq_along(ibase)){
        for(x in seq_along(rbase)){
          a=complex(real=rbase[x],imaginary = ibase[y])
          m[x,y]<-escape(a,modlim=modlim,itfunc=itfunc,takein = a)
        }
      }
      return(m)
    }
    for(y in seq_along(ibase)){
      for(x in seq_along(rbase)){
        a=complex(real=rbase[x],imaginary = ibase[y])
        m[x,y]<-escape(a,modlim=modlim,itfunc=itfunc,takein = takein)
      }
    }
  return(m)
}


#m<-MapIFS_repel_complex(modlim,itfunc=Julia,takein = complex(real=.2,imaginary = .5),
#                     n=50, rbase=(1:10)/10,ibase=(1:10)/10)


#contour((1:10)/10,(1:10)/10,m)


parMapIFS_repel_complex<-function(modlim,itfunc,takein=NULL,n=1000,rbase=seq(from=-2.5,to=1,by=.001),
                    ibase=seq(from=-1,to=1,by=.001),type='j'){
  escape<-function(m,modlim,itfunc,takein=NULL,n=100){
    i<-0
    while(Mod(m)<modlim&i<n){
      i<-i+1
      if(is.null(takein)){
        m<-itfunc(m)
      }else{
        m<-itfunc(m,takein)
      }
    }
    i
  }
  message('Require dopar')
  message(getDoParWorkers())
  if(type=='m'){
    m<-foreach(y=ibase,.combine = cbind) %dopar% {
      v=NULL
      for(x in seq_along(rbase)){
        a=complex(real=rbase[x],imaginary = y)
        v[x]=escape(a,modlim=modlim,n=n,itfunc =itfunc,takein=a)
      }
      v
    }
    return(m)
  }
  m<-foreach(y=ibase,.combine = cbind) %dopar% {
    v=NULL
    for(x in seq_along(rbase)){
      a=complex(real=rbase[x],imaginary = y)
      v[x]=escape(a,modlim=modlim,n=n,itfunc =itfunc,takein = takein)
    }
    v
  }
  m
}

#pm=parMapIFS_repel_complex(modlim,itfunc=Julia,takein = complex(real=.2,imaginary = .5),
#             n=50, rbase=(1:10)/10,ibase=(1:10)/10)

#contour((1:10)/10,(1:10)/10,pm)
#
#system.time({
#m<-foreach(y=ibase,.combine = cbind) %dopar% {
#v=NULL
#for(x in seq_along(rbase)){
#  a=complex(real=rbase[x],imaginary = y)
#  v[x]=escape(a,valfunc = Mandelbrot_val,itfunc = Mandelbrot,takein = a)
#}
#v
#}
#})

#a=complex(real=rbase[x],imaginary = ibase[y])
options("max.contour.segments"= 300000)

rbase=seq(from=-2.5,to=1,by=.001)
ibase=seq(from=-1.4,to=1.4,by=.001)
system.time(mandelbrot<-parMapIFS_repel_complex(modlim = 2,rbase=rbase,ibase=ibase,n=100,itfunc=Julia,type='m'))



write.csv(mandelbrot,'m.csv')
library(tiff)
#normalize


library(pixmap)
m100=pixmapGrey(mandelbrot/100,cellres=c(length(rbase),length(ibase)))
write.pnm(m100,'m100.pgm',type='pgm')

setwd("E:/")
m1002<-read.pnm('m100.pgm')
m1002@grey
plot(m1002)

julia<-parMapIFS_repel_complex(modlim = 1,rbase=rbase,ibase=ibase,n=100,itfunc = Julia,takein=complex(real=.9,imaginary = -.7),type='j')

tiff('Mandelbrot100it.tif',width=length(rbase),height=length(ibase))
contour(rbase,ibase,mandelbrot,asp=1,method = 'simple')
dev.off()
contour(rbase,ibase,julia,asp=1)
filled.contour(rbase,ibase,julia,asp=1)


# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
#chart_link = api_create(p, filename="contour-coordinates")
