##########################################################
# The package "kernlab"
##########################################################
##########################################################
# To install the package "kernlab" execute:
# En R cliquer: "Packages" ->  Installer le(s) package(s) -> [...France (Lyon2)...] -> kernlab
# 
library("kernlab")  # connect library


####################################################################
# données:  "diabetes"

library(mclust)
data(diabetes)
str(diabetes)

TrainD = as.matrix(diabetes[diabetes$class == "Normal",-1],bycol=T)
str(TrainD)

TestD  = as.matrix(diabetes[diabetes$class != "Normal",-1], bycol=T)


# 
# Determiner le comportement de 3 erreurs en fonction de h, 
# 


TrainY = rep(1,nrow(TrainD))

#TrainD=as.matrix(TrainD)
#str()

nrow(TrainD)
length(TrainY)

trainsvm  =  ksvm(TrainD , TrainY, kernel= "rbfdot", kpar=list(sigma=0.01), type="one-svc", cross=10 ,nu=0.001)
trainsvm 














