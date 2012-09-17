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
dataset <- "scene"
learner <- "ECC"  # BR ECC

# load predictions
resultFile <- paste("./result/multilabel/",dataset,"/",dataset,"_",learner,".RData",sep="")
load(resultFile)

# evaluate
results <- MultiLabel.GLM.Evaluate(predictions, data$testDataY, 
		metrics=c("HammingLoss", "ExactMatch", "SpeciesAUC", "SiteAUC", "MacroF1", "MicroF1"))
print(results)