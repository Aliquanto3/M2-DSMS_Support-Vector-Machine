

#################################################################################################################################
# Ex 1a (Donnees non-separables)
# Toy binary classification for non-separable data
# 2 groupes des observations non-separables en R2.


# .Random.seed is an integer vector, containing the random number generator (RNG) state for random number generation in R. 
# It can be saved and restored, but should not be altered by the user. 
# set.seed is the recommended way to specify seeds. See ?set.seed

library(kernlab)

set.seed(123)  # pour avoir le meme echantillon

## Rappel: pour calculer un produit de 2 matrices A et B: A %*% B


##########################################################
# Generer un echantillon d'apprentissage
# Generate two dimensional non separable data
######################################################################
######################################################################
######################################################################
# donnees non-separables
# Generate two dimensional non separable data
n1=60
n2=60

mu1=c(0,0)
X1= mu1 + cbind(rnorm(n1),rnorm(n1))
mu2=c(2,2)
X2= mu2 + cbind(rnorm(n2),rnorm(n2))


X <- rbind( X1,X2 ) # put 2 groups in X   
Y= c(rep(1,n1),rep(-1,n2))
nn=n1+n2


# visualisation

pairs(X,labels=c("X1","X2"), main="visualise data", pch=22, bg=c("red", "yellow", "blue")[2+Y])

plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="red",pch=24)



######################################################################
# on genere une fonction qui calcule la solution du probl?me dual

probl.dual = function(X,Y,gamma){
c <- matrix(rep(-1,nn))
# calcul de H
t(X[1,])%*%X[2,]
P <- X%*%t(X)     # soit    kernelMatrix(vanilladot(),X)
H <- Y%*%t(Y)*P
A <- t(Y)
b <- 0
l <- matrix(rep(0,nn))
u <- matrix(rep(gamma,nn))
r <- 0
sv <- ipop(c,H,A,b,l,u,r)
round(primal(sv),6)
}

##########################################
 
C=2  # fixer la valeur de la constante de regularisation

# determiner les alpha

alpha=probl.dual(X,Y,gamma=C)
plot(alpha)
alpha


##########################################################
# Question 1: determiner les indices des vecteurs support:

suppk = which(alpha > 0 & alpha < C);supp

####################################################################
# Question 2: determiner les alpha positives et les vecteurs support

supp = which(alpha > 0);supp

alpha[supp]
X[suppk,]
#############################################
# ON DETERMINE LE HYPERPLAN OPTIMAL
#################################################
# Question 3: calculer la valeur de w0
k=suppk[1]
w0=1/Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% X[k,])
w0

#################################################
# Question 4: calculer la valeur de w0 a l'aide d'un autre vecteur support
k=suppk[2]
w0=1/Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% X[k,])
w0

#################################################
# Question 5: calculer la valeur de w0 a l'aide d'un troisieme vecteur support
k=suppk[3]
w0=1/Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% X[k,])
w0

##########################################
#  CONSTRUCTION DU CLASSIFIEUR
#################################################
#  Question 6:   creation du hyperplan optimal pour classifier plusieurs points: 
Hyper=function(X,Y,alpha,newdata,indice=1){
  k=suppk[indice]
  w0=Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% as.matrix(X[k,],ncol=1))
  colSums(alpha[supp]*Y[supp] * X[supp,]%*% t(newdata)  ) +w0
}

########################################################
# Question 7: classer les points X 
Hyper(X,Y,alpha,newdata=X)

########################################################
# Question 8: Identifier les vecteurs support 

plot(X, type='n')
points(X[Y==1,], col='blue')
points(X[Y==-1,], col='red', pch=24)

#On identifie les 3 vecteurs supports 
points(X[suppk,], col=c('blue','blue','red'), 
       pch=c(1,1,24), lwd=3)
########################################################
# Question 9:  Afficher le hyperplan separateur et la bande de marge maximale

wstar = colSums(alpha[supp]*Y[supp]*X[supp,])
k=suppk[1]
Y[k]
P <- X%*%t(X)     # soit    kernelMatrix(vanilladot(),X)
dim(P)
w0 = Y[k]-sum(P[supp,k]*Y[supp]*alpha[supp])

abline(a= -w0/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
abline(a= (1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
abline(a= (-1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )

##########################################################
# Question 10: Classifier une grille de points

# creation d'une grille des points
NGRID=100
GRID = expand.grid(X=seq(-2.5,5.5,length=NGRID), Y=seq(-2.5,5.5,length=NGRID)); 
x=cbind(GRID$X,GRID$Y)
x[1:10,]

Hyper(X,Y,alpha,newdata=x)

Ypredx = Hyper(X,Y,alpha,newdata=x)
Dpredx = ifelse(Ypredx>0, 1,2)
plot(x[,1],x[,2], col=c("Salmon","SkyBlue")[Dpredx])
points(X)

# Question 11:  on ajoute le hyperplan separateur et la bande de marge maximale
abline(a= -w0/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
abline(a= (1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
abline(a= (-1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )

points(X[suppk,], col=c('blue','blue','red'), 
       pch=c(1,1,24), lwd=3)


