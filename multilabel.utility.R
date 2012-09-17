# multilable utility functions
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################


library("glmnet")
library("gbm")
#library("foreign")
library("RWeka")

source("multilabel/multilabel.metric.R")

#
# load dataset
#
MultiLabel.Load <- function(dataset, nFeatures)
{
	# load train data
	trainFile <- paste("./data/multilabel/",dataset,"/",dataset,"-train.arff",sep="")
	trainData <- read.arff(trainFile)
	
	# load train data
	testFile  <- paste("./data/multilabel/",dataset,"/",dataset,"-test.arff",sep="")
	testData  <- read.arff(testFile)
	
	return(list(trainDataX=trainData[,1:nFeatures], trainDataY=trainData[,-(1:nFeatures)],
				testDataX=testData[,1:nFeatures], testDataY=testData[,-(1:nFeatures)]))
}


#
# evaluate methods with GLM as the base learner
#
MultiLabel.GLM.Evaluate <- function(predictions, trueLabels, metrics=c("HammingLoss", "ExactMatch"))
{
	alphas <- names(predictions)
	
	print(metrics)
	
	bestMetrics <- list()
	for (metric in metrics) {
		if (metric == "HammingLoss" || metric == "RankingLoss" ||
			metric == "OneError" || metric == "Coverage") {
			bestMetrics[[metric]] <- 1
			next
		}
		bestMetrics[[metric]] <- -1
	}
	for (alpha in alphas) {
		prediction <- predictions[[alpha]]
		lambdas <- names(prediction)
		
		for (lambda in lambdas) {
			cat("lambda",lambda,"alpha",alpha,"\n")
			
			# get predicted labels
			predLabels <- prediction[[lambda]]
			
			# evaluate
			metric <- MultiLabel.Evaluate(trueLabels, predLabels, metrics)
			
			# store the best value for each metric
			if (!is.null(bestMetrics[["HammingLoss"]]) && (bestMetrics[["HammingLoss"]] > metric[["HammingLoss"]])) { bestMetrics[["HammingLoss"]] <- metric[["HammingLoss"]] }
			if (!is.null(bestMetrics[["ExactMatch"]])  && (bestMetrics[["ExactMatch"]]  < metric[["ExactMatch"]]))  { bestMetrics[["ExactMatch"]]  <- metric[["ExactMatch"]] }
			if (!is.null(bestMetrics[["SiteAUC"]])     && (bestMetrics[["SiteAUC"]]     < metric[["SiteAUC"]]))     { bestMetrics[["SiteAUC"]]     <- metric[["SiteAUC"]] }
			if (!is.null(bestMetrics[["SpeciesAUC"]])  && (bestMetrics[["SpeciesAUC"]]  < metric[["SpeciesAUC"]]))  { bestMetrics[["SpeciesAUC"]]  <- metric[["SpeciesAUC"]] }
			if (!is.null(bestMetrics[["MicroF1"]])     && (bestMetrics[["MicroF1"]]     < metric[["MicroF1"]]))     { bestMetrics[["MicroF1"]]     <- metric[["MicroF1"]] }
			if (!is.null(bestMetrics[["MacroF1"]])     && (bestMetrics[["MacroF1"]]     < metric[["MacroF1"]]))     { bestMetrics[["MacroF1"]]     <- metric[["MacroF1"]] }
		} # lambdas
	} # alphas
	
	return(bestMetrics)
}


#
# binary relevance training with GLMNET as the base learner
#
MultiLable.BR.GLM.Train <- function(trainDataX, trainDataY, alphas = 0)
{
	labelNames <- colnames(trainDataY)
	
	trainDataX <- as.matrix(trainDataX)
	class(trainDataX) <- "numeric"
	
	models <- list()
	for (alpha in alphas) {	
		cat(paste("alpha is",alpha,"\n"))
		
		# train model
		fits <- list()
		for (label in labelNames) {  
			y <- trainDataY[,label]
			
			if (length(unique(y)) != 1) {
				fits[[label]] <- eval(glmnet(as.matrix(trainDataX), y, family="binomial", alpha=alpha))
				#fits[[label]] <- eval(cv.glmnet(as.matrix(trainDataX), y, family="binomial", alpha=alpha))
			} else {
				fits[[label]] <- NULL 
			}
		}
		
		models[[as.character(alpha)]] <- fits
	} # alphas
	
	return(models)
}

#
# binary relevance predicting with GLMNET as the base learner
#
MultiLable.BR.GLM.Predict <- function(models, testDataX, lambdas)
{
	alphas <- names(models)
	labelNames <- names(models[[alphas[1]]])

	testDataX <- as.matrix(testDataX)
	class(testDataX) <- "numeric"
	
	results <- list()
	for (alpha in alphas) {		
		model <- models[[alpha]]
		
		result <- list()
		for (lambda in lambdas) {
			cat("lambda",lambda,"alpha",alpha,"\n")
			
			predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
			predictions <- data.frame(predictions)
			colnames(predictions) <- labelNames
			for (label in labelNames) { 
				if (!is.null(model[[label]])) {
					predictions[,label] <- predict(model[[label]],type="response",newx=as.matrix(testDataX),s=lambda)
				} 
			} # label
			
			result[[as.character(lambda)]] <- predictions
		} # lambdas
		
		results[[alpha]] <- result
	} # alphas
	
	return(results)
}


#
# Ensemble of Classifier Chains (ECC) approach with GLMNET as the base learner
#
MultiLable.ECC.GLM <- function(trainDataX, trainDataY, testDataX, nChains=10, alphas = 0, lambdas = 0)
{
	alphas <- as.character(alphas)
	lambdas <- as.character(lambdas)	
	labelNames <- colnames(trainDataY)
	nLabels <- ncol(trainDataY)
	
	trainDataX <- as.matrix(trainDataX)
	class(trainDataX) <- "numeric"
	testDataX <- as.matrix(testDataX)
	class(testDataX) <- "numeric"
	
	# for each chain
	avgPredictions <- list()
	for (alpha in alphas) {
		avgPredictions[[alpha]] <- list()
		for (lambda in lambdas) {
			avgPredictions[[alpha]][[lambda]] <- matrix(0,nrow=nrow(testDataX),ncol=nLabels)
			colnames(avgPredictions[[alpha]][[lambda]]) <- labelNames
		}
	}
	
	for (c in 1:nChains) {
		cat("chain",c,"\n")
		
		# generate random ordering
		ordering <- sample(nLabels, replace = FALSE)
		orderingLabels <- labelNames[ordering]
				
		# train
		models <- list()
		for (alpha in alphas) {
			cat("alpha is",alpha,"\n")
			
			# pred data for each n.tree
			chainTrainDataX <- trainDataX
			
			# train model
			fits <- list()
			for (label in orderingLabels) {
				y <- trainDataY[,label]

				if (length(unique(y)) != 1) {
					fits[[label]] <- eval(glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha))
				} else {
					fits[[label]] <- NULL 
				}
				
				chainTrainDataX <- cbind(chainTrainDataX,trainDataY[,label])
				names(chainTrainDataX)[ncol(chainTrainDataX)] <- label
			} # labels
			
			models[[alpha]] <- fits
		} # alphas
		
		# predict
		for (alpha in alphas) {
			model <- models[[alpha]]
			
			for (lambda in lambdas) {
				cat("lambda",lambda,"alpha",alpha,"\n")
				
				chainTestDataX <- testDataX
				
				predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
				predictions <- data.frame(predictions)
				colnames(predictions) <- labelNames
				for (label in orderingLabels) { 
					if (!is.null(model[[label]])) {
						predictions[,label] <- predict(model[[label]],type="response",newx=as.matrix(chainTestDataX),s=as.numeric(lambda))
					} 
					
					prediction <- predictions[,label]
					prediction[prediction >= 0.5] <- 1
					prediction[prediction < 0.5] <- 0
					chainTestDataX <- cbind(chainTestDataX,prediction)
					names(chainTestDataX)[ncol(chainTestDataX)] <- label
				}
				
				# update the aggreaged predictions
				avgPredictions[[alpha]][[as.character(lambda)]] <- (avgPredictions[[alpha]][[as.character(lambda)]] * (c-1) + predictions) / c;
			} # lambdas
		} # alphas
	} # chain
	
	return(avgPredictions)
}


