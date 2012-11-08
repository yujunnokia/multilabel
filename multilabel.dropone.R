# multilable dropone experiment using ECC as multi-label learner
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################

source("./multilabel.buildinfo.R")
source("./multilabel.utility.R")

######################
# experiment settings
######################
dataset <- "emotions"
alphas <- c(0)	
lambdas <- seq(0,1,0.2)
nChains <- 10

# load data
data <- MultiLabel.Load(dataset, nFeatures[[dataset]])
trainDataX <- data$trainDataX
trainDataY <- data$trainDataY
testDataX <- data$testDataX
testDataY <- data$testDataY

labels <- colnames(trainDataY)
nLabels <- length(labels)

# generate random chain orderings
orderings <- list()
for (c in 1:nChains) {
	orderings[[c]] <- labels[sample(nLabels, replace = FALSE)]
}

# run ECC with all labels
{
	# train multi-label learner
	predictions <- MultiLable.ECC.GLM(trainDataX, trainDataY, testDataX, nChains=nChains, alphas=alphas, lambdas=lambdas, orderings)
	
	# save predictions
	outputFile <- paste("./result/multilabel/",dataset,"/",dataset,"_ECC_all.RData",sep="")
	save(dataset, alphas, lambdas, labels, data, predictions, file=outputFile)
}

for (label in labels) {
	cat("label",label,"\n")
	
	# remove removeLabel from orderings
	droponeOrderings <- list()
	for (c in 1:nChains) {
		droponeOrderings[[c]] <- orderings[[c]][-which(orderings[[c]] == label)] 
	}	
	leftLabels <- labels[-which(labels==label)]
	
	# train multi-label learner
	predictions <- MultiLable.ECC.GLM(trainDataX, trainDataY[,leftLabels], testDataX, nChains=nChains, alphas=alphas, lambdas=lambdas, droponeOrderings)

	# save predictions
	outputFile <- paste("./result/multilabel/",dataset,"/",dataset,"_ECC_",label,".RData",sep="")
	save(dataset, alphas, lambdas, labels, label, leftLabels, data, predictions, file=outputFile)
}

