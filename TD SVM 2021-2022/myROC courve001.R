
###########################################################################

#Rappel: comment recuperer les elements d'une liste.
 
alist <- list(A=c("a", "b", "c"), B=c(1,2,3,4), C=c(8e6, 5.2e9, -9.3e7))

is(alist[[1]])

alist[[3]]

(alist[[1]])[1]

alist[[3]][1]
#is(alist[[1]])
#alist[[1]][3]

#mylist=list( list(1:100, "c"), list(200:500)) 
#mylist[1][1]

#mylist[[1]]
#mylist[[1]][[1]][29]

#legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2),pch=c(1,2,1,2),text.col=c(1,1,2,2))

###########################################################################
library(kernlab)
library(ROCR)


set.seed(123)


n=2000
X=rnorm(n)
Y=rbinom(n,size=1,prob= exp(2*X-1)/(1+exp(2*X-1)))
plot(X,Y)

B=data.frame(Y,X)
reg=glm(Y~X,family=binomial,data=B)
score.glm=predict(reg,type="response")
Ypred=ifelse(score.glm>=0.5,1,0)
points(X,score.glm,col="red")
abline(h=0.5)


# FP faux positifs = ce qui sont clasés posititifs parmis les vrais negativs
# TP true positifs = ce qui sont clasés posititifs parmis les vrais positifs

# calcul de FP et TP 
s=0.5
score.glm

ClasPositifs = score.glm>=s;ClasPositifs
VraisPositifs= Y==1;VraisPositifs
VraisNegatifs= Y==0;VraisNegatifs

FPs=sum(ClasPositifs*VraisNegatifs)/sum(VraisNegatifs)

sum(ClasPositifs*VraisPositifs)  # TP
sum(ClasPositifs*VraisNegatifs)  # FP

FP=sum(ClasPositifs*VraisNegatifs)/sum(VraisNegatifs) # taux de FP
TP=sum(ClasPositifs*VraisPositifs)/sum(VraisPositifs) # taux de TP

FP
TP


#################################################################

m=1000
ss=seq(0,1,length=m)

FP=TP=rep(NA,m)
plot(0:1,0:1,type="n",xlab="False Positive Rate",ylab="True Positive Rate")
for(i in 1:m){
s=ss[i]
FP[i]=sum((score.glm>s)*(Y==0))/sum(Y==0)
TP[i]=sum((score.glm>s)*(Y==1))/sum(Y==1)
}
oldrocglm=cbind(c(FP),c(TP))
lines(oldrocglm,type="s",col="red")
#lines(c(FP),c(TP),type="s",col="magenta")



##################################################################################
# Le meme plot avec le package ROCR
library(ROCR)

ppp = prediction(score.glm, Y)
ROCglm = performance(ppp, measure = "tpr", x.measure = "fpr")
plot(ROCglm)


#########################################################################
# pour comparer avec le graphique precedent
plot(ROCglm)

# et

lines(oldrocglm,type="s",col="red")  # comparer


#############################
# creer une fonction pour caluler ROC courbe

ROCfun = function(score,Y){
ppp = prediction(score,Y)
performance(ppp, measure = "tpr", x.measure = "fpr")
}

# exemple d'utilisation
ROCfun(score.glm,Y)
plot(ROCfun(score.glm,Y))

ROCglm = ROCfun(score.glm,Y)
plot(ROCglm)

##########################################################################
# recuperer les valeurs manuelement

ROCglm@x.values[[1]] #or write slot(rocglm,"x.values")[[1]] to have the same
ROCglm@y.values[[1]]

points(ROCglm@x.values[[1]],ROCglm@y.values[[1]],type="l",col="red")


slotNames(ROCglm)
# use  slot(rocglm,"x.values") #instead of rocglm@x.values[[1]]


newrocglm=cbind(ROCglm@x.values[[1]],ROCglm@y.values[[1]])

plot(newrocglm,type="l",col="blue")
lines(oldrocglm,type="s",col="red")  # comparer

rocglm=newrocglm  # pour la comparaison

sum(Y!=Ypred)/length(Y)



##########################################################################
#Application au ksvm
#Utile car le ksvm ne prend pas de modèle
###########################################################################
# 
svm=ksvm(Y~X,data=B,type="C-svc")
svm
#predict(svm,type="decision")
predict(svm,newdata=data.frame(X),type="decision")

svmscore=predict(svm,newdata=data.frame(X),type="decision")

#svmscore=predict(svm,newdata=data.frame(X),type="d")
Ypredsvm=predict(svm,newdata=data.frame(X),type="response")

table(svmscore>0,Ypredsvm) 
par(mfrow=c(1,2))
plot(X,svmscore)
plot(X,score.glm)

# Plot ROC curve
rocsvm <- performance(prediction(svmscore,Y), measure = "tpr", x.measure = "fpr") 
par(mfrow=c(1,1))
plot(rocsvm,col="magenta")
points(rocglm,type="l",col="red")  #  ROC pour glm


###########################################################################
# the same with probability model

svm=ksvm(Y~X,data=B,type="C-svc",prob.model=T)

SCR=predict(svm,newdata=data.frame(X),type="p")

head(SCR)


svmprobscore=SCR[,2]


#Ypred=predict(svm,newdata=data.frame(X),type="r")
#SCR[,1]+SCR[,2]
slotNames(svm)

plot(X,svmprobscore)
plot(X,svmscore)

# Plot ROC curve
rocsvmprob <- ROCfun(svmprobscore,Y) 
#=performance(prediction(svmprobscore,Y), measure = "tpr", x.measure = "fpr") 
plot(rocsvmprob,col="blue")
points(rocsvm@x.values[[1]],rocsvm@y.values[[1]],type="l",col="magenta")
points(rocglm,type="l",col="red")  #  ROC pour glm



###########################################################################
# other types of performances

#?performance

# Here is the list of available performance measures. 
# Let Y and Ypred be random variables representing the class and the prediction for a randomly drawn sample, respectively. 
# We denote by + and - the positive and negative class, respectively. 
# Further, we use the following abbreviations for empirical quantities: 
# P (\# positive samples), 
# N (\# negative samples), 
# TP (\# true positives), 
# TN (\# true negatives), 
# FP (\# false positives), 
# FN (\# false negatives).


#acc:
#Accuracy. P(Ypred = Y). Estimated as: (TP+TN)/(P+N).
#err:
#Error rate. P(Ypred != Y). Estimated as: (FP+FN)/(P+N).
#fpr:
#False positive rate. P(Ypred = + | Y = -). Estimated as: FP/N.
#tpr:
#True positive rate. P(Ypred = + | Y = +). Estimated as: TP/P.
#rec:
#Recall. Same as tpr.
#sens:
#Sensitivity. Same as tpr.
#fnr:
#False negative rate. P(Ypred = - | Y = +). Estimated as: FN/P.
#miss:
#Miss. Same as fnr.
#tnr:
#True negative rate. P(Yhat = - | Y = -).
#spec:
#Specificity. Same as tnr.
#prec:
#Precision. Same as ppv.
#phi or mat :
#Matthews correlation coefficient or Phi correlation coefficient. (TP*TN - FP*FN)/(sqrt((TP+FN)*(TN+FP)*(TP+FP)*(TN+FN))). 
#Yields a number between -1 and 1, with 1 indicating a perfect prediction, 0 indicating a random prediction. 
#Values below 0 indicate a worse than random prediction.
#odds:
#Odds ratio. (TP*TN)/(FN*FP). Note that odds ratio produces Inf or NA values for all cutoffs corresponding to FN=0 or FP=0. 
#This can substantially decrease the plotted cutoff region.
#lift:
#Lift value. P(Yhat = + | Y = +)/P(Yhat = +).
#auc:
#Area under the ROC curve. This is equal to the value of the Wilcoxon-Mann-Whitney test statistic 
#and also the probability that the classifier will score are randomly drawn 
#positive sample higher than a randomly drawn negative sample. 
#Since the output of auc is cutoff-independent, 
#this measure cannot be combined with other measures into a parametric curve. 
#The partial area under the ROC curve up to a given 
#false positive rate can be calculated by passing the optional parameter fpr.stop=0.5 
#(or any other value between 0 and 1) to performance.


# La courbe ROC:  performance(pred, measure = "tpr", x.measure = "fpr")
# Precision/recall plot:  measure="prec", x.measure="rec".
# Sensitivity/specificity plot:  measure="sens", x.measure="spec".
# Lift charts: measure="lift", x.measure="rpp".
# Plot accuracy as function of threshold: measure = "acc"


Ytrue=Y
svm=ksvm(Y~X,data=B,type="C-svc")
svmscore=predict(svm,newdata=data.frame(X),type="decision")
pred <- prediction(svmscore,Ytrue)

# ROC courve
perform <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perform)
#plot(x.values(perf),perf$y)

# Area under the ROC curve
perform <- performance(pred, measure = "auc") 
slotNames(perform)
Area=slot(perform,"y.values")[[1]]
Area


# precision/recall courve
perform <- performance(pred, measure = "prec", x.measure = "rec") 
plot(perform)
#plot(x.values(perf),perf$y)

# Plot accuracy as function of threshold
perform <- performance(pred, measure = "acc") 
plot(perform)



perform <- performance(ppp, measure = "acc") 
plot(perform)




# sensitivity/specificity curve (x-axis: specificity, y-axis: sensitivity)
perform <- performance(pred,measure = "sens", x.measure = "spec")
plot(perform)

# precision as a function of threshold
perform <- performance(pred, measure = "prec") 
plot(perform)


# Plot odds ratio as function of threshold
perform <- performance(pred, measure = "odds") 
plot(perform)

# Plot odds ratio as function of spec
perform <- performance(pred, measure = "odds",x.measure = "spec") 
plot(perform)

# Lift chart
perform <- performance(pred, measure = "lift",x.measure = "rpp") 
plot(perform)





##############################################################################
# Create new data for testing ksvm and randomForest ################################################################################

n=500
a=b=3
sd=0.999
X=NULL
Y=NULL
M=1


X1=rbind(X,cbind(rnorm(n,sd=sd)+a, rnorm(n,sd=sd)+0))
X2=rbind(X,cbind(rnorm(n,sd=sd)-a, rnorm(n,sd=sd)+0))

X3=rbind(X,cbind(rnorm(n,sd=sd)+0, rnorm(n,sd=sd)+b))
X4=rbind(X,cbind(rnorm(n,sd=sd)+0, rnorm(n,sd=sd)-b))

X=rbind(X1,X2,X3,X4)
Y=as.factor(c(rep(1, 2*n),rep(-1, 2*n)  ))

plot(X,col=c("red","blue")[as.numeric(Y)])


library(kernlab)
svmfit=ksvm(X,Y,kernel="rbf",kpar=list(sigma=5),type="C-svc",C=1)
plot(svmfit)
print(svmfit)


install.packages("randomForest")

library(randomForest)

fit=randomForest(X,Y)
print(fit)
plot(fit)


predict(fit,newdata=X,type="prob")


#dim(X)
#Yone=rep(1,dim(X)[1])
#svmfit=ksvm(X,Yone,kernel="rbf",kpar=list(sigma=5),type="one-svc",nu=0.05)
#plot(svmfit)
#print(svmfit)


##################################################

n=500
a=b=3
sd=0.8
X=NULL
Y=NULL
M=1
for(i in -M:M){
for(j in -M:M){	
X=rbind(X,cbind(rnorm(n,sd=sd)+i*a, rnorm(n,sd=sd)+j*b))
Y=c(Y,rep( (((-1)^(i+j)) + 1 )/2, n)  )
}}

Y=as.factor(Y)
plot(X,col=c("red","blue")[as.numeric(Y)])

#library(randomForest)
#fit <- randomForest(Secteur.Public.PU.Privé.PR ~ ., data = lycee2, na.action = na.roughfix)

library(randomForest)
fit=randomForest(X,Y)
print(fit)

library(kernlab)
svmfit=ksvm(X,Y,kernel="rbf",kpar=list(sigma=5),C=0.01)
plot(svmfit)
print(svmfit)









