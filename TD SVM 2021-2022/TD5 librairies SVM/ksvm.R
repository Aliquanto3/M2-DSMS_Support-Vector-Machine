##########################################################
# The package "kernlab"
##########################################################
##########################################################
# To install the package "kernlab" execute:
# En R cliquer: "Packages" ->  Installer le(s) package(s) -> [...France (Lyon2)...] -> kernlab
library("kernlab")  # connect library


#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

set.seed(123)  

# Generate two dimensional data
nn=500

mu1=c(0,0)
X = mu1 + cbind(rnorm(nn),rnorm(nn))

d = apply(X^2,MARGIN=1,sum)
Y = ifelse(d<1,1,2)

# visualise

plot(X,col=c("red","yellow","blue")[Y])

#En utilisant ksvm with kernel = "vanilladot", on obtient
?ksvm

REZsvm = ksvm(X,Y,kernel="vanilladot",type="C-svc",cross=0,C=100/nn)

plot(REZsvm,data=X)
REZsvm

#vanilladot indique qu'il n'y a pas de noyau, ou plutôt le noyau trivial
#avec "rbfdot"
REZsvm = ksvm(X,Y,kernel="rbfdot", kpar=list(sigma=2),type="C-svc",cross=0,C=50)

plot(REZsvm,data=X) #ne marche qu'en bidimensionnel
REZsvm

Ypred=predict(REZsvm)

sum(Ypred==Y)/length(Y)
1-sum(Ypred==Y)/length(Y) #correspond à la training error de REZsvm
REZsvm

#############################################################################
#Test avec d'autres valeurs
m=1230
mu2=c(0,0)
X2 = mu2 + cbind(rnorm(m),rnorm(m))

d2 = apply(X2^2,MARGIN=1,sum)
Y2 = ifelse(d2<1,1,2)
plot(X2,col=c("red","blue")[Y2])

newPred=predict(REZsvm,newdata=X2)
sum(newPred==Y2)/length(Y2)
1-sum(newPred==Y2)/length(Y2) #Erreur de vérification sur un autre ensemble

#Si on teste un nouveau modèle sur les données de vérification
# REZsvm2 = ksvm(X2,Y2,kernel="rbfdot", kpar=list(sigma=2),type="C-svc",cross=0,C=50)
# plot(REZsvm2,data=X2)
# Ypred2=predict(REZsvm2)
# sum(Ypred2==Y2)/length(Y2)
# 1-sum(Ypred2==Y2)/length(Y2)
# REZsvm2

############################################################################
#Test en variant sigma
trainErr=c()
verifError=c()

A=0.01
B=100
M=20
sigmaValues=exp(seq(log(A),log(B),length=M))

for (i in 1:length(sigmaValues)){
  REZsvmi = ksvm(X,Y,kernel="rbfdot", kpar=list(sigma=sigmaValues[i]),
                 type="C-svc",cross=0,C=50)
  Ypredi=predict(REZsvmi)
  trainErr[i]=1-sum(Ypredi==Y)/length(Y)
  
  newPredi=predict(REZsvmi,newdata=X2)
  verifError[i]=1-sum(newPredi==Y2)/length(Y2)
}

plot(log(sigmaValues),trainErr,main="Training error depending on sigma",
     col="red",type="l",
     xlab="Log(sigma)",ylab="Error")
lines(log(sigmaValues),verifError,col="blue")
legend("top",legend=c("Verification error", "Training error"), 
       col=c("blue","red"), lty=1)

###########
#Cross validation
cross(REZsvm)
REZcross = ksvm(X,Y,kernel="rbfdot", kpar=list(sigma=2),type="C-svc",cross=12,C=50)
cross(REZcross)
