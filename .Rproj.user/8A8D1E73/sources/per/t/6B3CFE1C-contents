#install.packages("kernlab")

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

X1
X2

X <- rbind( X1,X2 ) # put 2 groups in X   
Y= c(rep(1,n1),rep(-1,n2))
nn=n1+n2


pairs(X,labels=c("X1","X2"), main="visualise data", pch=22, bg=c("red", "yellow", "blue")[2+Y])

plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="red",pch=24)

# utiliser la fonction ipop de kernlab pour determiner la solution du problème dual.
class(X)
mode(X)
is(X)

# 1. Calculer la matrice ( <X_i,X_j> ) 

X
X[1,]
X[2,]
t( X[1,] )  %*% X[2,]

#Matrice de taille n*n des produits scalaires
P=X%*%t(X)
P[1,2] #égal à t( X[1,] )  %*% X[2,]
#On calcule ceci car on veut maximiser g(alpha), qui contient une double somme 
#des produits scalaires des Xi

?ipop

#On cherche Hi=Yi*Yj*P[i,j] pour correspondre à l'entrée de ipop
H=(Y%*%t(Y))*P
H

c=matrix(rep(-1,nn),ncol=1)
c
# gamma = cc = contrainte
l=matrix(rep(0,nn),ncol=1)
l

cc=10000 #Tester avec cc=Inf => Ne fonctionne pas pour ipop
u=matrix(rep(cc,nn),ncol=1)
u

b=0
b

r=0
r

A=matrix(Y,nrow=1)
A

alpha=ipop(c, H, A, b, l, u, r)
str(alpha)
primal(alpha)
#Les deux en dessous ne fonctionnent pas
# alpha$primal
# alpha["primal"]

alpha=round(primal(ipop(c, H, A, b, l, u, r)),3)
alpha
