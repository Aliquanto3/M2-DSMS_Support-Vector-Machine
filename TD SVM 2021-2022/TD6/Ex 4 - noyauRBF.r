##########################################################
# The package "kernlab"
##########################################################
##########################################################
# Pour installer le package "kernlab":
# En R cliquer: "Packages" ->  Installer le(s) package(s) -> [...France (Lyon2)...] -> kernlab

library("kernlab")  # connect library


#################################################################################################################################
#################################################################################################################################
#################################################################################################################################

set.seed(123)  

# Generer des données

nn=500

mu1=c(0,0)
X = mu1 + cbind(rnorm(nn),rnorm(nn))

d = apply(X^2,MARGIN=1,sum)
Y = ifelse(d<1,1,2)

# visualiser les observations

plot(X,col=c("red", "blue")[Y])



######################################################################################
# En utilisant kvsm  with  kernel= "vanilladot" on obtient
# ?ksvm

REZsvm = ksvm(X, Y, kernel= "vanilladot", type="C-svc", cross=0 ,C=100/nn)
plot(REZsvm,data=X)
REZsvm


# Exercise 1.

# Question 1: Déterminer l'erreur d'apprentissage (kernel= "vanilladot") 
# Question 2: Déterminer l'erreur de validation-croisée (en posant le paramètre cross=5) (kernel= "vanilladot")

error(REZsvm)

REZsvm = ksvm(X, Y, kernel= "vanilladot", type="C-svc", cross=5 ,C=100/nn)

error(REZsvm)
cross(REZsvm)


######################################################################################
# Question 3: Utiliser maintenant ksvm avec les paramètres suivants kernel= "rbfdot" (noyau Gaussien)  et  kpar=list(sigma=2)
#


REZsvm = ksvm(X, Y, kernel= "rbfdot", kpar=list(sigma=0.008), type="C-svc", cross=0 ,C=50)
#plot(REZsvm,data=X)
REZsvm
error(REZsvm)


##########################################
# comment generer une grille des points: exemple
# G00=expand.grid(X=1:5, Y=1:5); G00[15,]; plot(G00)
# G00=expand.grid(X=seq(0,1,length=50), Y=seq(0,1,length=50)); plot(G00,type="p")

G00=expand.grid(X=seq(-3,3,length=500), Y=seq(-3,3,length=500));
plot(G00,type="p")

PG00=predict(REZsvm,newdata=G00)

plot(G00,col=c(2,4)[PG00],type="p")




# Question 4: Determiner l'erreur d'apprentissage.

error(REZsvm)

# Question 5: Generer un nouveau echantillon et détrerminer l'erreur de verification.

# Question 6: Determiner l'erreur de validation-croisée (en posant le paramètre cross=5) 

set.seed(67890)
REZsvm = ksvm(X, Y, kernel= "rbfdot", kpar=list(sigma=1), type="C-svc", cross=5 ,C=50)
error(REZsvm)
cross(REZsvm)

# Question 7: Tracer toutes les erreurs en fonction du paramètre $sigma.$




#########################################################################################

# Exercice 2. Repeter l'Exercice 1 avec type="nu-svc" pour les mêmes données 
# (ici nu est la proportion des vecteurs support).

# Repondre aux questions:

# Question 1: Déterminer l'erreur d'apprentissage.

set.seed(67890)
REZsvm = ksvm(X, Y, kernel= "rbfdot", kpar=list(sigma=1), type="nu-svc", cross=0 ,C=50)
error(REZsvm)
#cross(REZsvm)



# Question 3: Generer un nouveau echantillon et détrerminer l'erreur de verification.
# Question 2: Déterminer l'erreur de validation-croisée (en posant le paramètre cross=5).

# Question 4: Faites variér le nu de O.1 a 0.5 avec le pas 0.1 et tracer la courbe des erreurs d'apperntissage est de verification.
# Question 5: Faites variér le nu de O.1 a 0.5 avec le pas 0.1 et tracer la courbe des erreurs validation croisée (cross=5).

# Question 6: Faites variér sigma et tracer les courbes des 3 erreurs.







