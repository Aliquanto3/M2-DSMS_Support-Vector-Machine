
#################################################################################################################################
# Ex 2a (Donnees non-separables avec noyau)
# Toy binary classification for non-separable data with kernel
# 2 groupes des observations non-separables en R2.


# .Random.seed is an integer vector, containing the random number generator (RNG) state for random number generation in R. 
# It can be saved and restored, but should not be altered by the user. 
# set.seed is the recommended way to specify seeds. See ?set.seed

library(kernlab)



## Rappel: pour calculer un produit de 2 matrices A et B: A %*% B


##########################################################
# Generer un echantillon d'apprentissage
# Generate two dimensional non separable data
######################################################################
######################################################################
######################################################################
# donn?es non-separables
# Generate two dimensional non separable data
n1=60
n2=60
n3=60
set.seed(123)  # pour avoir le m?me echantillon
X1= cbind(rnorm(n1),rnorm(n1))
X2= cbind(4+rnorm(n2),rnorm(n2))
X3= cbind(rnorm(n3),4+ rnorm(n3))




X <- rbind( X1,X2,X3) # put 2 groups in X   
Y= c(rep(1,n1),rep(-1,n2),rep(-1,n3))
nn=n1+n2+n3


# visualisation
#pairs(X,labels=c("X1","X2"), main="visualise data", pch=22, bg=c("red", "yellow", "blue")[2+Y])
plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="red",pch=24)



######################################################################
# on genere une fonction qui calcule la solution du probl?me dual


probl.dualK = function(X,Y,gamma, K){
c <- matrix(rep(-1,nn))
# calcul de H
#P <- X%*%t(X)     # soit    kernelMatrix(vanilladot(),X)
H <- Y%*%t(Y)*K
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

# K=???

A=matrix(rep(X[,1],nn),ncol=nn)
DA=A-t(A)
#DA[3,2]== X[3,1]-X[2,1]
B=matrix(rep(X[,2],nn),ncol=nn)
DB=B-t(B)
#DB[3,2]== X[3,2]-X[2,2]
Norm = DA^2 + DB^2
sigma=1
matrK=exp(-sigma*Norm)


gausskernel = function(X,sigma=1){
nn=length(X[,1])
A=matrix(rep(X[,1],nn),ncol=nn)
DA=A-t(A)
#DA[3,2]== X[3,1]-X[2,1]
B=matrix(rep(X[,2],nn),ncol=nn)
DB=B-t(B)
#DB[3,2]== X[3,2]-X[2,2]
Norm = DA^2 + DB^2
exp(-sigma*Norm)
}

matrK = gausskernel(X[1:10,],sigma=1/2)

alpha=probl.dualK(X,Y,gamma=C,K=matrK)
plot(alpha)
alpha


#########################################################
X1=X[1:10,1]
X2=X[1:5,1]
nn1=length(X1)
nn2=length(X2)
A=matrix(rep(X1,length(X2)),ncol=length(X1))
B=matrix(rep(X2,length(X1)),ncol=length(X2))
D1=A-t(B)

X1=X[,2]
X2=X[,2]
A=matrix(rep(X1,length(X2)),ncol=length(X1))
B=matrix(rep(X2,length(X1)),ncol=length(X2))
D2=A-t(B)

Norm = D1^2 + D2^2
sigma=1
matrK2=exp(-sigma*Norm)

matrK2==matrK

##########################################################
# Question 1: determiner les indices des vecteurs support:
supp=which(alpha>0)


####################################################################
# Question 2: determiner les alpha positives et les vecteurs support
alpha[supp]

#############################################
# ON DETERMINE LE HYPERPLAN OPTIMAL
#################################################
# Question 3: calculer la valeur de w0

suppk=which( (alpha>0) & (alpha< C))

k=suppk[7]

w0=1/Y[k] - sum(alpha[supp]*Y[supp]* matrK[supp,k])
w0

#################################################
# Question 4: calculer la valeur de w0 a l'aide d'un autre vecteur support


#################################################
# Question 5: calculer la valeur de w0 a l'aide d'un troisieme vecteur support


Hyper=function(X,Y,alpha,newdata,indice=1,w0){
k=supp[indice]
#w0=Y[k] - sum(alpha[supp]*Y[supp] * matrK[supp,k])

colSums(alpha[supp]*Y[supp] * X[supp,]%*% t(newdata)  ) +w0
}

#on classe les points X 
Hyper(X,Y,alpha,newdata=X)


##########################################
#  CONSTRUCTION DU CLASSIFIEUR
#################################################
#  Question 6:   creation du hyperplan optimal pour classifier plusieurs points: 


########################################################
# Question 7: classer les points X 


########################################################
# Question 8: Identifier les vecteurs support 

plot(X, type='n')
points(X[Y==1,], col='blue')
points(X[Y==-1,], col='red', pch=24)


########################################################
# Question 9:  Afficher le hyperplan separateur et la bande de marge maximale



##########################################################
# Question 10: Clasifier une grille de points

# creation d'une grille des points
NGRID=100
GRID = expand.grid(X=seq(-2.5,5.5,length=NGRID), Y=seq(-2.5,5.5,length=NGRID)); 
x=cbind(GRID$X,GRID$Y)
x[1:10,]


# Question 11:  on ajoute le hyperplan separateur et la bande de marge maximale



