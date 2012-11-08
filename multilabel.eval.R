# multilabel evaluation 
#  Usage:
#	Rscript multilabel.eval.R -args HJAbirds BR_CV
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################


# set working directory	
setwd("/Users/yujunnokia/workspace/multilabel")

source("./multilabel.buildinfo.R")
source("./multilabel.utility.R")

######################
# experiment settings
######################
dataset <- "ebirdNY" # yeast HJAbirds HBRbirds ebirdNY ebirdCO ebirdMA ebirdMD
learner <- "ECC_Beta"  # BR ECC ECC_Beta ECC_Cheat

args <- commandArgs(trailingOnly = TRUE)
#dataset <- as.character(args[2])
#learner <- as.character(args[3])

# load predictions
predictionFile <- paste("../result/multilabel/",dataset,"/",dataset,"_",learner,".RData",sep="")
load(predictionFile)

# evaluate
results <- MultiLabel.Evaluate(data$testDataY, predictions, 
		metrics=c("HammingLoss", "ExactMatch", "SpeciesAUC", "SiteAUC", "MacroF1", "MicroF1"))
print(results)	


