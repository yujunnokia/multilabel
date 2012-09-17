# multilable dropone experiment using ECC as multi-label learner
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################

source("multilabel/multilabel.buildinfo.R")
source("multilabel/multilabel.utility.R")

######################
# experiment settings
######################
dataset <- "emotions"
alphas <- c(0)
lambdas <- seq(0,1,0.2)

# load data
data <- MultiLabel.Load(dataset, nFeatures[[dataset]])
trainDataX <- data$trainDataX
trainDataY <- data$trainDataY
testDataX <- data$testDataX
testDataY <- data$testDataY

labels <- colnames(trainDataY)

for (label in labels) {
	
	# remove one label
	
	
	# train multi-label learner
	predictions <- MultiLable.ECC.GLM(trainDataX, trainDataY, testDataX, nChains=5, alphas=alphas, lambdas=lambdas)
	
}

# save predictions
outputFile <- paste("./result/multilabel/",dataset,"/",dataset,"_ECC.RData",sep="")
save(dataset, alphas, lambdas, data, predictions, file=outputFile)
