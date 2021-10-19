

#################################################################################################################################
# Ex 1a (Donnees separables)
# Toy binary classification for separable data
# 2 groupes des observations normales  en R2.


# .Random.seed is an integer vector, containing the random number generator (RNG) state for random number generation in R. 
# It can be saved and restored, but should not be altered by the user. 
# set.seed is the recommended way to specify seeds. See ?set.seed

library(kernlab)

set.seed(123)  # pour avoir le m?me echantillon

## Rappel: pour calculer un produit de 2 matrices A et B: A %*% B


##########################################################
# Generer un echantillon d'apprentissage
# Generate two dimensional non separable data
n1=60
n2=60

mu1=c(0,0)
X1= mu1 + cbind(rnorm(n1),rnorm(n1))
mu2=c(5,5)
X2= mu2 + cbind(rnorm(n2),rnorm(n2))

X <- rbind( X1,X2 ) # put 2 groups in X   
Y= c(rep(1,n1),rep(-1,n2))
nn=n1+n2

##########################################################
# la visualisation
plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="red",pch=24)

# visualiser par des moyens standart
pairs(X,labels=c("X1","X2"), main="visualise data", pch=22, bg=c("red", "yellow", "blue")[2+Y])


##########################################################
# utiliser la fonction ipop du package kernlab pour determiner la solution du probl?me dual.
#################
# 1. Exemple comment calculer l'element 1,2 de la matrice ( <X_i,X_j> ) 
X[1,]
X[2,]
PP.12=t( X[1,] )  %*% X[2,]
PP.12


#calculer toute la matrice ( <X_i,X_j> ) 
PP <- X%*%t(X)     
#verifier:
PP[2,1]
PP[1,2]
PP.12

# alternative way:  P=kernelMatrix(vanilladot(),X)
# P[2,1]
# P[1,2]



#############################################################################
# resoudre le probleme dual
#??ipop
# gamma = C = constante de regularisarion

C=gamma=500

c <- matrix(rep(-1,nn))
# calcul de H
P <- X%*%t(X) # soit P <-kernelMatrix(vanilladot(),X) ## soit 
H <- Y%*%t(Y)*P
A <- t(Y)
b <- 0
l <- matrix(rep(0,nn))
u <- matrix(rep(gamma,nn))
r <- 0
sv <- ipop(c,H,A,b,l,u,r)

attributes(sv)
#la solution du probleme dual se trouve dans le primal(sv): elle correspond correspond ? la sol du prob dual.

alpha = round(primal(sv),6)
alpha

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

C=500  # C=Inf=infinie
alpha=probl.dual(X,Y,gamma=C)
alpha

# il n'y a que 3 alpha differents de zero
plot(alpha)

########################################
#les indices des vecteurs support:

supp = which(alpha>0)
supp

##############################
# les alpha positives
alpha[supp]

##############################
# les vecteurs support
X[supp,]

#############################################
#############################################
# IMPORTRANT: ON DETERMINE LE HYPERPLAN OPTIMAL


#################################################
# calcul de w0 par 3 methodes
k=supp[1]
w0=Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% X[k,])
w0
#################################################
# calcul de w0
k=supp[2]
w0=Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% X[k,])
w0
#################################################
# calcul de w0
k=supp[3]
w0=Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% X[k,])
w0

##########################################
#sum(alpha[supp]*Y[supp] * X[supp,]%*% x)
##########################################
# construction du classifieur

# on choit un point a classifier, par exemple
x=as.matrix(c(1,2),ncol=1)

k=supp[1]
w0=Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% as.matrix(X[k,],ncol=1))
H=sum(alpha[supp]*Y[supp] * X[supp,]%*% x ) + w0
H

#################################################
# comme exemple on construit le hyperplan pour classifier un point x
Hyper1=function(x){ 
sum(alpha[supp]*Y[supp] * X[supp,]%*% x ) + w0
}

Hyper1(x)

x=as.matrix(c(-2,2),col=1)
Hyper1(x)
x=as.matrix(c(-2,-2),col=1)
Hyper1(x)
x=as.matrix(c(-10,-2),col=1)
Hyper1(x)
x=as.matrix(c(30,10),col=1)
Hyper1(x)
#Classe selon que la valeur soit positive ou négative

###################################################
# Exemple: on applique les calcul pour plusieurs points: ici pour X
#colSums(alpha[supp]*Y[supp] * X[supp,]%*% t(X)  ) +w0

################################################################################
#  creation du hyperplan optimal pour classifier plusieurs points: 
Hyper=function(X,Y,alpha,newdata,indice=1){
k=supp[indice]
w0=Y[k] - sum(alpha[supp]*Y[supp] * X[supp,]%*% as.matrix(X[k,],ncol=1))
colSums(alpha[supp]*Y[supp] * X[supp,]%*% t(newdata)  ) +w0
}

#on classe les points X 
Hyper(X,Y,alpha,newdata=X)


###############################
# Affichage

plot(X, type='n')
points(X[Y==1,], col='blue')
points(X[Y==-1,], col='red', pch=24)
#On identifie les 3 vecteurs supports 
points(X[supp,], col=c('blue','blue','red'), 
pch=c(1,1,24), lwd=3)

# abline( a=4, b=-1  ) # un hyperplan separateur quelquonque

# on affiche le hyperplan separateur et la bande de marge maximale
wstar = colSums(alpha[supp]*Y[supp]*X[supp,])
k=supp[1]
Y[k]
dim(P)
w0 = Y[k]-sum(P[supp,k]*Y[supp]*alpha[supp])

abline(a= -w0/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
abline(a= (1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
abline(a= (-1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )


##########################################################
##########################################################
# On classifie une grille de points

# creation d'une grille des points
NGRID=100
GRID = expand.grid(X=seq(-4,9,length=NGRID), Y=seq(-4,8,length=NGRID)); 
# x=cbind(GRID$X,GRID$Y)
# x[1:10,]
x=GRID

Hyper(X,Y,alpha,newdata=x)

Ypredx = Hyper(X,Y,alpha,newdata=x)
Dpredx = ifelse(Ypredx>0, 1,2)
plot(x[,1],x[,2], col=c("Salmon","SkyBlue")[Dpredx])
points(X)


abline(a= -w0/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
abline(a= (1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
abline(a= (-1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )

points(X[supp,], col=c('blue','blue','red'), 
pch=c(1,1,24), lwd=3)

