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

# visualise

plot(X,col=c("red","yellow","blue")[Y+2])

####################################################
# gausskern

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
norm=D1^2 +D2^2
exp(-sigma*norm)
}

######################################################################
# on genere une fonction qui calcule la solution du probl?me dual


probl.dual = function(X,Y,gamma,sigma=1){
nn=length(Y)
c <- matrix(rep(-1,nn))
# calcul de H
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



################################################

Hyper=function(X,Y,alpha,newdata,indice=1,sigma=1){
matrK = gausskern(X,X,sigma)
suppk=which( (alpha>0) & (alpha< C))
k=suppk[indice]
w0=Y[k] - sum(alpha[supp]*Y[supp] * matrK[supp,k])
colSums(alpha[supp]*Y[supp] * gausskern(X[supp,], newdata)  ) +  w0
}

C=2
alpha=probl.dual(X,Y,gamma=C,sigma=1)

Hyper(X,Y,alpha,newdata=X,indice=1,sigma=1)

##############################################
# creation d'une grille des points
NGRID=100
GRID = expand.grid(X=seq(-3,3,length=NGRID), Y=seq(-3,3,length=NGRID)); 
x=cbind(GRID$X,GRID$Y)

Ypred=ifelse(Hyper(X,Y,alpha,newdata=x)>0,1,2)

plot(x[,1],x[,2],col=c("Salmon","SkyBlue")[Ypred])
points(X[Ypred==1,],col="red")
points(X[Ypred==2,],col="blue",pch=24)













