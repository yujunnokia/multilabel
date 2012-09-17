# multilabel evaluation 
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################


source("multilabel/multilabel.buildinfo.R")
source("multilabel/multilabel.utility.R")

######################
# experiment settings
######################
dataset <- "yeast"
learner <- "ECC"  # BR ECC

# BR result
{
	# load predictions
	resultFile <- paste("./result/multilabel/",dataset,"/",dataset,"_BR.RData",sep="")
	load(resultFile)
	
	# evaluate
	results <- MultiLabel.GLM.Evaluate(predictions, data$testDataY, 
			metrics=c("HammingLoss", "ExactMatch", "SpeciesAUC", "SiteAUC", "MacroF1", "MicroF1"))
	print(results[["HammingLossLabel"]])	
	print(results[["SpeciesAUCLabel"]])
	
	resultsBR <- results
}

# ECC result
{
	# load predictions
	resultFile <- paste("./result/multilabel/",dataset,"/",dataset,"_ECC.RData",sep="")
	load(resultFile)
	
	# evaluate
	results <- MultiLabel.GLM.Evaluate(predictions, data$testDataY, 
			metrics=c("HammingLoss", "ExactMatch", "SpeciesAUC", "SiteAUC", "MacroF1", "MicroF1"))
	print(results[["HammingLossLabel"]])	
	print(results[["SpeciesAUCLabel"]])
	
	resultsECC <- results
}

# compute label frequence
dataY <- rbind(data$trainDataY, data$testDataY)
dataY <- as.matrix(dataY)
class(dataY) <- "numeric"
labelFreq <- colSums(dataY) / nrow(dataY)
print (labelFreq)
