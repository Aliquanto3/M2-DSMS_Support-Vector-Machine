
########################################################################################################################
# Ex 1a (Donnees separables)
# Toy binary classification for separable data
# 2 groupes des observations normales  en R2.


# .Random.seed is an integer vector, containing the random number generator (RNG) state for random number generation in R. 
# It can be saved and restored, but should not be altered by the user. 
# set.seed is the recommended way to specify seeds. See ?set.seed

library(kernlab)
# pour installer executer:    install.packages("kernlab")

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

X


X[1,]
X[2,]

t( X[1,])  %*% X[2,]

P = X %*% t(X)

P[1,2]


H = (Y%*%t(Y))* P

c=matrix(rep(-1,nn),ncol=1)

CC=10000

l=matrix(rep(0,nn),ncol=1)
u=matrix(rep(CC,nn),ncol=1)

b=0
r=0

A = matrix(Y,nrow=1) 

?ipop
# gamma = cc = contrainte

primal(ipop(c, H, A, b, l, u, r))

alpha = round(primal(ipop(c, H, A, b, l, u, r)),6)

# on determine le hyperplan optimal

beta=alpha*Y
B=P %*% beta

# determination de l'indice k d'un des vecteur support
supp=which(alpha>0)
k=supp[3]
w0 = 1/Y[k] - B[k] 
B-w0



# matrix(P[17,],nrow=1) %*% matrix(beta,ncol=1)
# w0

H= function(x){ k=supp[3]
beta=alpha*Y
B=P %*% beta
w0 = 1/Y[k] - B[k] 
B+w0
}

H(1)

w= colSums(beta*X)

abline(a=-w0/w[2] , b = -w[1]/w[2] , col="magenta")
abline(a=-(w0+1)/w[2] , b = -w[1]/w[2] , col="red")
abline(a=-(w0-1)/w[2] , b = -w[1]/w[2] , col="blue")

# regle de decision

# Xnew = grille des points sur [-2,9]x[-2,8]
M=20
xx=seq(-2,9,length=M)
yy=seq(-2,8,length=M)
Xnew = expand.grid(xx,yy)

# a definir la regle de decision

#X1=
#rbind(X,Xnew)
#Pnew=


   





