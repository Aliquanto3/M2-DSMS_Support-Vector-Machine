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
Y = ifelse(d<1,1,-1)


# visualisation

pairs(X,labels=c("X1","X2"), main="visualise data", pch=22, bg=c("red", "yellow", "blue")[2+Y])

plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="red",pch=24)

######################################################################
#Calcul de la matrice k(X_i,X_j)
#K=???

A=X[1:5,1]
B=X[1:10,1]

#On veut toutes les différences possibles entre tous les éléments de A par 
#rapport à tous les élements de B

sigma=1

M=matrix(rep(A,length(B)),ncol=length(B))
N=matrix(rep(B,length(A)),ncol=length(A))
D1=M-t(N);D1

A2=X[1:5,2]
B2=X[1:10,2]
M2=matrix(rep(A2,length(B2)),ncol=length(B2))
N2=matrix(rep(B2,length(A2)),ncol=length(A2))
D2=M2-t(N2);D2

norm=D1^2+D2^2

exp(-sigma*norm)

gausskern=function(X1,X2,sigma=1){
  A=X1[,1]
  B=X2[,1]
  M=matrix(rep(A,length(B)),ncol=length(B))
  N=matrix(rep(B,length(A)),ncol=length(A))
  D1=M-t(N)
  
  A=X1[,2]
  B=X2[,2]
  M=matrix(rep(A,length(B)),ncol=length(B))
  N=matrix(rep(B,length(A)),ncol=length(A))
  D2=M-t(N)
  
  norm=D1^2+D2^2
  exp(-sigma*norm)
}

gausskern(X[1:5,],X[10:30,])

matrK=gausskern(X,X,sigma=1)
matrK

######################################################################
# on genere une fonction qui calcule la solution du probl?me dual

probl.dualK = function(X,Y,gamma,sigma=1){
  c <- matrix(rep(-1,nn))
  # calcul de H
  #t(X[1,])%*%X[2,]
  #P <- X%*%t(X)     # soit    kernelMatrix(vanilladot(),X)
  P=gausskern(X,X,sigma)
  
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

#K=?

#Méthode pour faire toutes les différences possibles
#en soustrayant par la transposée
# A=matrix(rep(X[,1],nn),ncol=nn)
# dim(A)
# A[,1]==X[,1]
# 
# DA=A-t(A)
# DA[3,2]==X[3,1]-X[2,1]
# 
# B=matrix(rep(X[,2],nn),ncol=nn)
# DB=B-t(B)
# DB[3,2]==X[3,2]-X[2,2]
# 
# Norm=DA^2+DB^2
# 
# sigma=1
# matrK=exp(-sigma*Norm)
# 
# 
# gausskernel = function(X,sigma=1){
#   A=matrix(rep(X[,1],nn),ncol=nn)
#   DA=A-t(A)
#   B=matrix(rep(X[,2],nn),ncol=nn)
#   DB=B-t(B)
#   Norm=DA^2+DB^2
#   exp(-sigma*Norm)
# }
# 
# matrK=gausskernel(X,sigma = 1/2)


# determiner les alpha

alpha=probl.dualK(X,Y,gamma=C)
plot(alpha)
alpha


##########################################################
# Question 1: determiner les indices des vecteurs support:

supp = which(alpha > 0);supp

####################################################################
# Question 2: determiner les alpha positives et les vecteurs support
alpha[supp]

#############################################
# ON DETERMINE LE HYPERPLAN OPTIMAL
#################################################
# Question 3: calculer la valeur de w0
suppk = which(alpha > 0 & alpha < C);suppk

k=suppk[1]
w0=1/Y[k] - sum(alpha[supp]*Y[supp] * matrK[supp,k])
w0

#################################################
# Question 4: calculer la valeur de w0 a l'aide d'un autre vecteur support
k=suppk[2]
w0=1/Y[k] - sum(alpha[supp]*Y[supp] * matrK[supp,k])
w0

#################################################
# Question 5: calculer la valeur de w0 a l'aide d'un troisieme vecteur support
k=suppk[3]
w0=1/Y[k] - sum(alpha[supp]*Y[supp] * matrK[supp,k])
w0

##########################################
#  CONSTRUCTION DU CLASSIFIEUR
#################################################
#  Question 6:   creation du hyperplan optimal pour classifier plusieurs points: 
Hyper=function(X,Y,alpha,newdata,indice=1){
  k=suppk[indice]
  w0=Y[k] - sum(alpha[supp]*Y[supp] * matrK[supp,k])
  colSums(alpha[supp]*Y[supp] * gausskern(X[supp,],newdata) ) +w0
}

########################################################
# Question 7: classer les points X 
Hyper(X,Y,alpha,newdata=X)

########################################################
# Question 8: Identifier les vecteurs support 

plot(X, type='n')
points(X[Y==1,], col='blue')
points(X[Y==-1,], col='red', pch=24)

# #On identifie les 3 vecteurs supports 
# points(X[suppk,], col=c('blue','blue','red'), 
#        pch=c(1,1,24), lwd=3)


########################################################
# # Question 9:  Afficher le hyperplan separateur et la bande de marge maximale
# 
# wstar = colSums(alpha[supp]*Y[supp]*X[supp,])
# k=suppk[1]
# Y[k]
# P <- X%*%t(X)     # soit    kernelMatrix(vanilladot(),X)
# dim(P)
# w0 = Y[k]-sum(P[supp,k]*Y[supp]*alpha[supp])
# 
# abline(a= -w0/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
# abline(a= (1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
# abline(a= (-1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )

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
points(X[Y==1,],col="red")
points(X[Y==-1,],col="blue",pch=2)
#points(X)

# # Question 11:  on ajoute le hyperplan separateur et la bande de marge maximale
# abline(a= -w0/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
# abline(a= (1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )
# abline(a= (-1-w0)/wstar[2] , b= - wstar[1]/wstar[2]  , col="red" )

points(X[suppk,], col=c('blue','blue','red'), 
       pch=c(1,1,24), lwd=3)


