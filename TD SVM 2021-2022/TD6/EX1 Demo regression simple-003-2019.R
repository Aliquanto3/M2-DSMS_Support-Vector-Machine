
library(kernlab)
########################################################################################################
## regression
# create data
x <- seq(-20,20,0.1)
y <- ifelse(x==0,1,sin(x)/x) + rnorm(401,sd=0.1)

plot(x,y,type="l",col="blue")


# train support vector machine
# define the parameters of the eps-regression

###################################################################################
# eps-svr epsilon regression 

par(mfrow=c(2,1))

h=1/16
eps=0.03
reg.eps <- ksvm(x,y,epsilon=eps,kpar=list(sigma=1/h),cross=3,C=0.5)

reg.eps
summary(reg.eps)
plot(x,y,type="b",col="blue")
lines(x,predict(reg.eps,x),col="blue",lwd=2)
lines(x,sin(x)/x,col="red",lwd=2)



reg.eps <- ksvm(x,y)

reg.eps
summary(reg.eps)
plot(x,y,type="b",col="blue")
lines(x,predict(reg.eps,x),col="blue",lwd=2)
lines(x,sin(x)/x,col="red",lwd=2)



###################################################################################
# nu-svr nu regression 
h=0.03
nu=0.5
reg.nu <- ksvm(x,y,nu=nu,kpar=list(sigma=1/h),cross=0,type="nu-svr")
reg.nu


plot(x,y,type="l")
#lines(x,sin(x)/x,col="blue",lwd=2)
lines(x,predict(reg.nu,x),col="blue",lwd=2)
#lines(x,predict(reg.eps,x),col="magenta",lwd=2)

yest=predict(reg.nu,x)
resid=predict(reg.nu,x)- y
#plot(resid)
plot(resid,ylim=c(-1,1))

sum(resid^2)/length(y)

###########################################################

grille.geom = function(a,b,size){
m=size
q=exp(   (log(b)-log(a))/(m-1)  )
q^(0:(m-1))*a
}


HH=grille.geom(1,0.00001,100)

par(mfrow=c(2,1))

EQMcross=NULL
for(i in 1:length(HH)){
#cat("h=",i,"\n")
h=HH[i]
cat("h=",h,"\n")
REZ=ksvm(x,y,nu=nu,kpar=list(sigma=1/h),cross=3)
resid.cross = predict(REZ,newdata=x)-y
EQMcross[i]=sum(resid.cross^2,na.rm=T)
}

plot(HH,EQMcross)
plot(log(HH),EQMcross)


#############################################################

n=length(x)

appr=sample(1:n,  n*2/3  )
length(appr)
length(y)

xtr=x[appr]
ytr=y[appr]

plot(xtr,ytr)

xte=x[-appr]
yte=y[-appr]

nu=0.2

grille.geom = function(a,b,size){
m=size
q=exp(   (log(b)-log(a))/(m-1)  )
q^(0:(m-1))*a
}


HH=grille.geom(1,0.00001,100)

par(mfrow=c(2,1))

EQMappr=NULL
EQMverif=NULL
for(i in 1:length(HH)){
h=HH[i]
#print("h=",h,"\n")
cat("h=",h,"\n")
REZ=ksvm(xtr,ytr,nu=nu,kpar=list(sigma=1/h),cross=0)

resid.appr = predict(REZ,newdata=xtr)-ytr

resid.verif = predict(REZ,newdata=xte)-yte

EQMappr[i]=sum(resid.appr^2,na.rm=T)
EQMverif[i]=sum(resid.verif^2,na.rm=T)
}

EQMappr
EQMverif

par(mfrow=c(2,1))

plot(HH,EQMverif,col="red")
points(HH,EQMappr)



plot(log(HH), EQMverif,col="red" )
points(log(HH),EQMappr)


imin = which.min(EQMverif)
HH[imin]



###################################################################################
# nu-svr nu regression 
par(mfrow=c(1,1))
h=HH[imin]
nu=0.3
reg.nu <- ksvm(x,y,nu=nu,kpar=list(sigma=1/h),cross=3)

plot(x,y,type="l")
lines(x,sin(x)/x,col="blue",lwd=2)
lines(x,predict(reg.nu,x),col="violet",lwd=2)
lines(x,predict(reg.eps,x),col="magenta",lwd=2)
reg.nu

###################################################################################
# nu-svr nu regression 
h=0.1
reg.nu <- ksvm(x,y,nu=nu,kpar=list(sigma=1/h),cross=3)

plot(x,y,type="l")
lines(x,sin(x)/x,col="blue",lwd=2)
lines(x,predict(reg.nu,x),col="violet",lwd=2)
lines(x,predict(reg.eps,x),col="magenta",lwd=2)
reg.nu






plot(xtr[order(xtr)],ytr[order(xtr)],type="l")
#lines(x,sin(x)/x,col="blue")
points(xtr[order(xtr)],predict(REZ,xtr)[order(xtr)],col="red",lwd=2,type='l')


plot(xte[order(xte)],yte[order(xte)],type="l")
#lines(x,sin(x)/x,col="blue")
points(xte[order(xte)],predict(REZ,xte)[order(xte)],col="blue",lwd=2,type='l')




HH=grille.geom(1,0.001,100)

EQMtr=rep(NA,len=length(HH))
EQMte=rep(NA,len=length(HH))


for(i in 1:length(HH)){
h=HH[i]
#print("h=",h,"\n")
REZ=ksvm(xtr,ytr,nu=nu,kpar=list(sigma=1/h),cross=0)
resid.tr = predict(REZ,newdata=xtr)-ytr
resid.te = predict(REZ,newdata=xte)-yte
EQMtr[i]=mean(resid.tr^2,na.rm=T)
EQMte[i]=mean(resid.te^2,na.rm=T)

#plot(xtr[order(xtr)],ytr[order(xtr)],type="l")
#lines(x,sin(x)/x,col="blue")
#points(xtr[order(xtr)],predict(REZ,xtr)[order(xtr)],col="red",lwd=2,type='l')
}


EQMtr

EQMte
par(mfrow=c(2,1))
plot(HH,EQMtr,xlim=c(0,0.5),col="red",type="l",main="training error")
#points(HH,EQMte,col="blue",type="l")
#par(mfrow=c(2,2))
plot(HH,EQMte,xlim=c(0,0.5),col="blue",type="l",main="testing error")


par(mfrow=c(2,1))
plot(log(HH),EQMtr,col="red",type="l",main="training error")
#points(HH,EQMte,col="blue",type="l")
#par(mfrow=c(2,2))
plot(log(HH),EQMte,col="blue",type="l",main="testing error")


imin = which.min(EQMte)
h=HH[imin]
#h=HH[75]
abline(v=log(h))

#print("h=",h,"\n")
REZ=ksvm(xtr,ytr,nu=nu,kpar=list(sigma=1/h),cross=0)
resid.tr = predict(REZ,newdata=xtr)-ytr
resid.te = predict(REZ,newdata=xte)-yte
EQMtr[i]=sum(resid.tr^2,na.rm=T)
EQMte[i]=sum(resid.te^2,na.rm=T)

plot(xtr[order(xtr)],ytr[order(xtr)],type="l")
#lines(x,sin(x)/x,col="blue")
points(xtr[order(xtr)],predict(REZ,xtr)[order(xtr)],col="red",lwd=2,type='l')


plot(xte[order(xte)],yte[order(xte)],type="l")
#lines(x,sin(x)/x,col="blue")
points(xte[order(xte)],predict(REZ,xte)[order(xte)],col="blue",lwd=2,type='l')






library(kernlab)
########################################################################################################
## regression
# create data
x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.1)

# train support vector machine
plot(x,y,type="l")

# define the parameters of the eps-regression

h=1/16  # la largeur de la fenetre du noyau Gaussien
eps=0.03

regm <- ksvm(x,y,epsilon=eps,kpar=list(sigma=1/h),cross=3)

predict(regm,x)

lines(x,sin(x)/x,col="blue")
lines(x,predict(regm,x),col="red",lwd=2)

resid = predict(regm,x)-y
plot(x,resid)


EQM=sum(resid^2,na.rm=T)
EQM

