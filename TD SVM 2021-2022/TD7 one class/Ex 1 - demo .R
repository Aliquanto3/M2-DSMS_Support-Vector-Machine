##########################################################
# The package "kernlab"
##########################################################
##########################################################
# To install the package "kernlab" execute:
# En R cliquer: "Packages" ->  Installer le(s) package(s) -> [...France (Lyon2)...] -> kernlab
# 
library("kernlab")  # connect library

##############################################################################################
##############################################################################################
##############################################################################################

######################################################################################
# Un exemple pour la detection des valeurs abberantes (novelty detection):
# On utilise ksvm avec les param?tres suivants 
# type="one-svc"
# kernel= "rbfdot" (noyau Gaussien)  et  kpar=list(sigma=0.2)  (il est possible  kpar="automatic")
#
set.seed(123)  

# Generate two dimensional data
n1=1000
mu1=c(0,0)
XX1 = mu1 + cbind(rnorm(n1),rnorm(n1))

n2=1000
mu2=c(3,4)
XX2 = mu2 + cbind(rnorm(n2),rnorm(n2))

Xtrain=rbind(XX1,XX2)
nn=n1+n2
Ytrain=rep(1,nn)

# visualise
plot(Xtrain)

#plot(Xtrain,xlim=c(-4,8),ylim=c(-20,20))


trainsvm = ksvm(Xtrain,Ytrain, kernel= "rbfdot", kpar=list(sigma=0.2), type="one-svc", cross=5 ,nu=0.01)

trainsvm


#trainsvm = ksvm(Xtrain,Ytrain, kernel= "rbfdot", type="one-svc", cross=5 )
#trainsvm

#alpha(trainsvm)
#alphaindex(trainsvm)
#nSV(trainsvm)
#coef(trainsvm)
#b(trainsvm)
error(trainsvm)
cross(trainsvm)
#prob.model(trainsvm)


Ytrain = predict(trainsvm,newdata=Xtrain)

#predict(trainsvm)

plot(Xtrain,col=c("red", "blue")[1+Ytrain])

Xtest=rbind(Xtrain,c(10,10))
plot(Xtest)

Ytest=predict(trainsvm,Xtest,type="response")
plot(Xtest,col=c("red", "black")[1+Ytest])


# predire une grille des valeurs

rezolution=200
GRID=expand.grid(X=seq(-4,10,length=rezolution), Y=seq(-4,10,length=rezolution));
#plot(GRID,type="p")

par(mfrow=c(1,1))
YGRID=predict(trainsvm,newdata=GRID,type="response")

plot(GRID,col=c("cyan", "bisque1")[1+YGRID])
points(Xtest,col=c("red", "black")[1+Ytest])



##############################################################################
# Exercice: Determiner le support pour les observations suivantes ################################################################################

n=500
a=b=3
sd=0.999
X=NULL
Y=NULL
M=1


X1=rbind(X,cbind(rnorm(n,sd=sd)+a, rnorm(n,sd=sd)+0))
X2=rbind(X,cbind(rnorm(n,sd=sd)-a, rnorm(n,sd=sd)+0))

X3=rbind(X,cbind(rnorm(n,sd=sd)+0, rnorm(n,sd=sd)+b))
X4=rbind(X,cbind(rnorm(n,sd=sd)+0, rnorm(n,sd=sd)-b))

X=rbind(X1,X2,X3,X4)
Y=as.factor(c(rep(1, 2*n),rep(-1, 2*n)  ))
Y=as.factor(rep(1, 4*n))

#plot(X,col=c("red","blue")[as.numeric(Y)])
plot(X)

Xtrain=rbind(X1,X2,X3,X4)

trainsvm = ksvm(Xtrain,Y, kernel= "rbfdot", kpar=list(sigma=1/10), type="one-svc", 
                cross=5 ,nu=0.2);trainsvm
#jour sur kpar et nu
error(trainsvm)
cross(trainsvm)

Ytrain = predict(trainsvm,newdata=Xtrain)
plot(Xtrain,col=c("red", "blue")[1+Ytrain])

Xtest=rbind(Xtrain,c(10,10))
plot(Xtest)

Ytest=predict(trainsvm,Xtest,type="response")
plot(Xtest,col=c("red", "blue")[1+Ytest])

# predire une grille des valeurs

rezolution=200
GRID=expand.grid(X=seq(-10,10,length=rezolution),Y=seq(-10,10,length=rezolution))

par(mfrow=c(1,1))
YGRID=predict(trainsvm,newdata=GRID,type="response")

plot(GRID,col=c("cyan", "bisque1")[1+YGRID])
points(Xtest,col=c("red", "blue")[1+Ytest],pch = 4)

