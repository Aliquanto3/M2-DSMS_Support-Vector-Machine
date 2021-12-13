##########################################################
# The package "kernlab"
##########################################################
##########################################################
# To install the package "kernlab" execute:
# En R cliquer: "Packages" ->  Installer le(s) package(s) ->  ... -> kernlab
# 
library("kernlab")  # connect library



#################################################################################################################################

grille.geom = function(a,b,size){
m=size
q=exp(   (log(b)-log(a))/(m-1)  )
q^(0:(m-1))*a
}

grille.geom(a=0.001,b=20,size=100)

#################################################################################################################################
# donn?es:  "diabetes"

library(mclust)
data(diabetes)
diabetes

n=dim(diabetes)[1];n
m=floor(2*n/3);m
set.seed(87654)
appr=sample(1:n,m)

ERRappr=NULL
ERRcross=NULL
ERRverif=NULL
h=grille.geom(a=0.001,b=20,size=20)

for(i in 1:length(h)){
  CLASS=ksvm(class ~ ., data = diabetes[appr,], kpar=list(sigma=1/h[i]))
  ERRappr[i]=error(CLASS)
  ERRverif[i]=sum(predict(CLASS,newdata=diabetes[-appr,])!=diabetes[-appr,1])/(n-m)
  set.seed(453)
  CLASScross=ksvm(class ~ ., data = diabetes[appr,],cross=3, kpar=list(sigma=1/h[i]))
  ERRcross[i]=cross(CLASScross)
}

plot(log(h),ERRappr,col="red",ylim=c(0,1),type="l",xlab="log(h)",ylab="Error",
     main="KSVM error")
lines(log(h),ERRverif,col="blue")
lines(log(h),ERRcross,col="green")
legend(-4, 0.95, legend=c("ERRappr","ERRverif","ERRcross"),
       col=c("red", "blue","green"),lty=1)


imin=which(ERRverif==min(ERRverif))[1]

h0=h[imin]

gamma=grille.geom(a=0.01,b=30,size=20)
ERRappr0=NULL
ERRcross0=NULL
ERRverif0=NULL

for (i in 1:length(gamma)){
  CLASS0=ksvm(class ~ ., data = diabetes[appr,], kpar=list(sigma=1/h0),C=gamma[i])
  ERRappr0[i]=error(CLASS0)
  ERRverif0[i]=sum(predict(CLASS0,newdata=diabetes[-appr,])!=diabetes[-appr,1])/(n-m)
  set.seed(453)
  CLASScross0=ksvm(class ~ ., data = diabetes[appr,],cross=3, kpar=list(sigma=1/h0),C=gamma[i])
  ERRcross0[i]=cross(CLASScross0)
}

plot(gamma,ERRappr0,col="red",ylim=c(0,1),type="l",xlab="gamma",ylab="Error",
     main="KSVM error")
lines(gamma,ERRverif0,col="blue")
lines(gamma,ERRcross0,col="green")
legend(10, 0.95, legend=c("ERRappr0","ERRverif0","ERRcross0"),
       col=c("red", "blue","green"),lty=1)

#################################################################################################################################
# donn?es:  "spam"
data(spam)
spam
names(spam)


########################################
# donn?es simul?es
n=5000
x1=runif(n,min=0,max=1)
x2=runif(n,min=0,max=1)
y=as.factor(ifelse(x1<1/2,ifelse(x2 > 2*x1,1,0), ifelse(x2 > 2- 2*x1,1,0) ))

plot(x1,x2,col=c("red","blue")[y])

d.train=data.frame(y,x1,x2)


library(randomForest)

RF=randomForest(y ~ .,data=d.train,nreee=200)







