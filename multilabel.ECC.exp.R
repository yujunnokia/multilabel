# multilabel experiments
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
model <- "ECC_Beta" # BR ECC ECC_Beta ECC_Cheat
alpha <- 0
nChains <- 10

args <- commandArgs(trailingOnly = TRUE)
#dataset <- as.character(args[2]) 
#model <- as.character(args[3]) 

# load data
data <- MultiLabel.Load.csv(dataset, nFeatures[[dataset]])

# run model
if (model == "BR") {
	predictions <- MultiLable.BR.GLM(data$trainDataX, data$trainDataY, data$testDataX, alpha=alpha)
} else if (model == "ECC") {
	predictions <- MultiLable.ECC.GLM(data$trainDataX, data$trainDataY, data$testDataX, nChains=nChains, alpha=alpha)
} else if (model == "ECC_Beta") {
	predictions <- MultiLable.ECC.Beta.GLM(data$trainDataX, data$trainDataY, data$testDataX, nChains=nChains, alpha=alpha)
} else if (model == "ECC_Cheat") {
	predictions <- MultiLable.ECC.Cheat.GLM(data$trainDataX, data$trainDataY, data$testDataX, data$testDataY, nChains=nChains, alpha=alpha)
} else {
	stop("model is invalid...\n")
}

# save predictions
outputFile <- paste("../result/multilabel/",dataset,"/",dataset,"_",model,".RData",sep="")
save(dataset, alpha, data, predictions, file=outputFile)
