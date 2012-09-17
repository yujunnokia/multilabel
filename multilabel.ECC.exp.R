# multilabel experiments
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################

source("multilabel/multilabel.buildinfo.R")
source("multilabel/multilabel.utility.R")

######################
# experiment settings
######################
dataset <- "medical"
alphas <- c(0)
lambdas <- seq(0,1,0.2)

# load data
data <- MultiLabel.Load(dataset, nFeatures[[dataset]])
trainDataX <- data$trainDataX
trainDataY <- data$trainDataY
testDataX <- data$testDataX
testDataY <- data$testDataY

# train multi-label learner
predictions <- MultiLable.ECC.GLM(trainDataX, trainDataY, testDataX, nChains=10, alphas=alphas, lambdas=lambdas)

# save predictions
outputFile <- paste("./result/multilabel/",dataset,"/",dataset,"_ECC.RData",sep="")
save(dataset, alphas, lambdas, data, predictions, file=outputFile)

#for (alpha in alphas) {
#	for (lambda in lambdas) {
#		# save predictions		
#		outputFile <- paste("./result/multilabel/",dataset,"/",dataset,"_ECC_",alpha,"_",lambda,".csv",sep="")
#		write.table(predictions, outputFile, sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)
#	}
#}
#
## evaluate
#results <- MultiLabel.GLM.Evaluate(predictions, testDataY, 
#		metrics=c("HammingLoss", "ExactMatch", "SpeciesAUC", "SiteAUC", "MacroF1", "MicroF1"))
#print(results)
