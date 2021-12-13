##########################################################
# The package "kernlab"
##########################################################
##########################################################
# To install the package "kernlab" execute:
# En R cliquer: "Packages" ->  Installer le(s) package(s) -> [...France (Lyon2)...] -> kernlab
# 
library("kernlab")  # connect library

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################



#####################################
par(mfrow=c(2,2))

# test example from web
# Training data

x1 <- rnorm(1000)
x2 <- rnorm(1000)
xxTrain <- matrix(c(x1,x2), nrow=1000, ncol=2, byrow=F)

par(mfrow=c(2,2))
plot(xxTrain[,1], col="cyan", ylim=c(-5,5),type="l")
plot(xxTrain[,2], col="grey", ylim=c(-5,5),type="l")

y=rep(1, length(x1))

classifier <- ksvm(xxTrain, y,  type="one-svc", kernel="rbfdot", 
                   kpar=list(sigma=1/50),cross=5,nu=0.2)
classifier




# Test data
x1 <- rnorm(1000)
scale <- c(rep(1,500), rep(4,50), rep(1,450))
x2 <- rnorm(1000)*scale
xxTest <- matrix(c(x1,x2), nrow=1000, ncol=2, byrow=F)

# Visualisation
#plot(xxTest)
plot(xxTest[,1], col="cyan", ylim=c(-5,5),type="l")
plot(xxTest[,2], col="grey", ylim=c(-5,5),type="l")


# Prediction
pTest <- predict(classifier,newdata = xxTest, type="response")



par(mfrow=c(2,2))
# Visualization

plot(x1, type='l', col="cyan", ylim=c(-5,5))
plot(x2, type='l',col="grey", ylim=c(-5,5))
#points(5*as.integer(pTest), type='l', col="blue")
plot(as.integer(pTest), type='p', col="blue")


##############################################
# Prediction sur pTrain

par(mfrow=c(2,2))
pTrain <- predict(classifier, xxTrain, type="response")
plot(xxTrain,xlim=c(-5,5),ylim=c(-5,5),col=c("cyan","grey")[1+pTrain], 
     main="Prediction for training data")


##############################################
# Prediction sur pTest

plot(xxTest,xlim=c(-5,5),ylim=c(-5,5),col=c("cyan","gray")[1+pTest],
     main="Prediction for test data")


grille.geom=function(a,b,size){
  m=size
  q=exp((log(b)-log(a))/(m-1))
  q^(0:(m-1))*a
}

HH=grille.geom(1,0.00001,100)

M=length(HH)
CROSS=NULL

for (i in 1:M){
  set.seed(2345678)
  classifier=ksvm(xxTrain,y,type="one-svc",kernel="rbfdot", 
                  kpar=list(sigma=1/HH[i]),cross=5,nu=0.2)
  CROSS[i]=cross(classifier)
}

dev.off()
par(mfrow=c(1,1))

plot(log(HH),CROSS)






