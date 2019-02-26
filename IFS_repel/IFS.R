


iterate<-function(m,modlim,itfunc,takein=NULL,n=20){
  if(n==0){
    return(m)
  }else{
    if(is.null(takein)){
      m<-itfunc(m)
    }else{
        m<-itfunc(m,takein)
      }
    
    if(m>modlim){ 
      return(NULL)
    }
    iterate(m,itfunc,takein = takein,n-1)
  }
}

parMapIFS_attract_complex<-function(modlim,
                       itfunc,takein=NULL,
                       nrand=100,n=20,rlb,rub,ilb,iub,
                       type=NULL){
  re<-foreach(i=seq(nrand),.combine = rbind) %dopar% {
  m<-c(runif(min = rlb,max=rub),runif(min=ilb,max=iub))
  as.num(iterate(as.complex(m),n=n,modlim=modlim,itfunc=itfunc,takein = takein))
  }
  return(re)
}
parMapIFS_attract_complex(modlim=1,itfunc=julia,nrand=1000,n=20,rlb=-2,rub=2,ilb=-1,iub=1)


Rbase<-function(rlb,rub,ilb,iub,n) cbind(runif(n,rlb,rub),runif(n,ilb,iub))

#
as.complex<-function(m) complex(real=m[1],imaginary = m[2])
as.num<-function(x) UseMethod('as.num')
as.num.complex<-function(m){
  r=Re(m);i=Im(m)
  c(r,i)
}
as.num.NULL<-function(m) NULL
#




(m<-Rbase(-2,2,-2,2,n=1000))

iterate(as.complex(m[2,]),valfunc = Mandelbrot_val,itfunc=Mandelbrot,takein = as.complex(m[1,]),n=1000)

create_Mandelbrot<-function(m) as.num(iterate(as.complex(m),valfunc=get('Mandelbrot_val',envir=.GlobalEnv),itfunc=get('Mandelbrot',envir=.GlobalEnv),takein = as.complex(m)))
create_Mandelbrot(m[3,])


m = read.csv('m.csv')
m = m[,2:ncol(m)]
m=m/6
m[m>1] = 1
writeTIFF(m,'m6.tif')