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
n3=60

set.seed(123)  # pour avoir le meme echantillon
X1= cbind(rnorm(n1),rnorm(n1))
X2= cbind(4+rnorm(n2),rnorm(n2))
X3= cbind(rnorm(n3),4+rnorm(n3))

X <- rbind( X1,X2,X3 ) # put 3 groups in X   
Y= c(rep(1,n1),rep(-1,n2),rep(-1,n3))
nn=n1+n2+n3


# visualisation

pairs(X,labels=c("X1","X2"), main="visualise data", pch=22, bg=c("red", "yellow", "blue")[2+Y])

plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="red",pch=24)



######################################################################
# on genere une fonction qui calcule la solution du probl?me dual

probl.dualK = function(X,Y,gamma,K){
  c <- matrix(rep(-1,nn))
  # calcul de H
  #t(X[1,])%*%X[2,]
  #P <- X%*%t(X)     # soit    kernelMatrix(vanilladot(),X)
  
  H <- Y%*%t(Y)*K #on a remplacé P par K
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
A=matrix(rep(X[,1],nn),ncol=nn)
dim(A)
A[,1]==X[,1]

DA=A-t(A)
DA[3,2]==X[3,1]-X[2,1]

B=matrix(rep(X[,2],nn),ncol=nn)
DB=B-t(B)
DB[3,2]==X[3,2]-X[2,2]

Norm=DA^2+DB^2

sigma=1
matrK=exp(-sigma*Norm)

# determiner les alpha

alpha=probl.dualK(X,Y,gamma=C,K=matrK)
plot(alpha)
alpha