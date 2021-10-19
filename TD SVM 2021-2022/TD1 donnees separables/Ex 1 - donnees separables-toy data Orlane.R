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
n1 = 60
n2 = 60

mu1 = c(0,0)
X1 = mu1 + cbind(rnorm(n1),rnorm(n1))
mu2 = c(5,5)
X2 = mu2 + cbind(rnorm(n2),rnorm(n2))

X <- rbind( X1,X2 ) # put 2 groups in X   
Y = c(rep(1,n1),rep(-1,n2))
nn = n1+n2


pairs(X,labels=c("X1","X2"), main="visualise data", pch=22, bg=c("red", "yellow", "blue")[2+Y])

plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="red",pch=24)

# 1. Calculer la matrice ( <X_i,X_j> ) 
gramMat = X  %*% t(X)

X
X[1,]
X[2,]
t( X[1,] )  %*% X[2,]

# 2. Calculer la matrice H telle que H i,j = Yi Yj gramMat i,j
H = (Y  %*% t(Y)) * gramMat

# 3. On définit le vecteur c
c = matrix(-1, ncol = 1, nrow = nn)

# 4.On définit les contraites du modèle
# 4.1 alpha i > 0

# lower bound
l = matrix(0, ncol = 1, nrow = nn)

# upper bound
u = matrix(10^6, ncol = 1, nrow = nn)

# 4.2 sum Yi alpha i = 0
# A * alpha = b + r
# b + r = 0 (ici on pose b = 0 et r = 0)
b = 0
r = 0

# A = t(Y)
A = matrix(Y, nrow = 1)

# 5. utiliser la fonction ipop de kernlab pour determiner la solution du problème dual.
sv <- ipop(c,H,A,b,l,u,r,sigf = 5)
sv

# 6. la solution du problème dual est : 
Alpha = round(primal(sv),6)

# 7. On affiche les vecteurs supports
plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="black",pch=24)
points(X[which(Alpha>0),],col="red",pch=18)
vSupp = X[which(Alpha>0),]
supp = which(Alpha>0)

# 8. On calcule l'hyperplan optimal 
# On calcule <w,x>
B = Alpha*Y
S = gramMat%*%B

# On calcule W 
W = colSums(B*X)

# Pour vérifier les résulats obtenues dans mat 
gramMat[,120]%*%B


# On calcule w0
# peut importe le support, on obtient tj 2,44 pour w0 
k = supp[1]
w0 = 1/Y[k] - S[k]

# On trouve l'hyperplan optimal 
H = function (x)
{
  B = Alpha*Y
  S = gramMat%*%B
  
  k = supp[1]
  w0 = 1/Y[k] - S[k]
  
  S+w0
}

# 9. On trace l'hyperplan sur le graphique : ici on le fait en dim = 2, sinon pas possible
# On a w1*x1+w2*x2*w0=0 
# -> x2 = -w0/w2 - w1/w2*x1
intercept = -w0/W[2]
slope = -W[1]/W[2]

plot(X,typ="n")
points(X[Y==1,],col="blue")
points(X[Y==-1,],col="black",pch=24)
points(X[which(Alpha>0),],col="red",pch=18)
abline(a = intercept, b=slope, col='red')
abline(a =  -(w0+1)/W[2], b=slope, col='black')
abline(a =  -(w0-1)/W[2], b=slope, col='blue')

# On voit que les limites passent bien par les 3 vecteurs supports 

# 10. Comment faire de la classification ?
# On commence par créer un nouveau jeu de données 
X1New = seq(-2,8, 0.1)
X2New = seq(-2,7, 0.1)

xnew = expand.grid(X1New, X2New)

