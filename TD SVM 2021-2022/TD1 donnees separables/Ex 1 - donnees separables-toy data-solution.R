

#################################################################################################################################
# Ex 1a (Donnees separables)
# Toy binary classification for separable data
# 2 groupes des observations normales  en R2.


# .Random.seed is an integer vector, containing the random number generator (RNG) state for random number generation in R. 
# It can be saved and restored, but should not be altered by the user. 
# set.seed is the recommended way to specify seeds. See ?set.seed

library(kernlab)

set.seed(123)  

## produit scalaire : A %*% B

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


pairs(X,labels=c("X1","X2"), main="visualise data", pch=22, bg=c("red", "yellow", "blue")[2+Y])

plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="red",pch=24)

# utiliser la fonction ipop de kernlab pour determiner la solution du problème dual.

# 1. Calculer la matrice ( <X_i,X_j> ) 

#X
#X[1,]
#X[2,]
#t( X[1,] )  %*% X[2,]


??ipop
# gamma = cc = contrainte

# utiliser la fonction ipop de kernlab pour determiner la solution du problème dual.

# 1. Calculer la matrice ( <X_i,X_j> ) 

#X
#X[1,]
#X[2,]
#t( X[1,] )  %*% X[2,]


help(ipop)
# gamma = cc = contrainte

gamma=500

c <- matrix(rep(-1,nn))

# calcul de H
P <- X%*%t(X)     # soit    kernelMatrix(vanilladot(),X)

t(X[3,])%*%X[4,]
P[3,4]
P[4,3]

H <- Y%*%t(Y)*P

A <- t(Y)
b <- 0
l <- matrix(rep(0,nn))
u <- matrix(rep(gamma,nn))
r <- 0

sv <- ipop(c,H,A,b,l,u,r)
sv

primal(sv)

attributes(sv)
# On a utilisé IPOP pour maximiser la fonction duale,
# donc la solution primal(sv) correspond à la sol du prob dual.

alpha = round(primal(sv),6)

# il n'y a que 3 alphas differents de zero
plot(primal(sv))




