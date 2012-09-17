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
models <- MultiLable.BR.GLM.Train(trainDataX, trainDataY, alphas)

# predict
predictions <- MultiLable.BR.GLM.Predict(models, testDataX, lambdas)
	
# save predictions
outputFile <- paste("./result/multilabel/",dataset,"/",dataset,"_BR.RData",sep="")
save(dataset, alphas, lambdas, data, models, predictions, file=outputFile)



#for (alpha in alphas) {
#	for (lambda in lambdas) {
#		# save predictions		
#		outputFile <- paste("./result/multilabel/",dataset,"/",dataset,"_BR_",alpha,"_",lambda,".csv",sep="")
#		write.table(predictions, outputFile, sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)
#	}
#}

# evaluate
#results <- MultiLabel.GLM.Evaluate(predictions, testDataY, 
#		metrics=c("HammingLoss", "ExactMatch", "SpeciesAUC", "SiteAUC", "MacroF1", "MicroF1"))
#print(results)
