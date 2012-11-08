# multilable utility functions
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################


library("glmnet")
library("gbm")
#library("foreign")
library("RWeka")

source("./multilabel.metric.R")

# GLM cv lambda value
GLM.s <- "lambda.min"  # lambda.min  lambda.1se  

#
# load dataset
#
MultiLabel.Load.arff <- function(dataset, nFeatures)
{
	# load train data
	trainFile <- paste("../data/multilabel/",dataset,"/",dataset,"-train.arff",sep="")
	trainData <- read.arff(trainFile)
	
	# load train data
	testFile  <- paste("../data/multilabel/",dataset,"/",dataset,"-test.arff",sep="")
	testData  <- read.arff(testFile)
	
	return(list(trainDataX=trainData[,1:nFeatures], trainDataY=trainData[,-(1:nFeatures)],
				testDataX=testData[,1:nFeatures], testDataY=testData[,-(1:nFeatures)]))
}

#
# load dataset
#
MultiLabel.Load.csv <- function(dataset, nFeatures)
{
	# load train data
	trainFile <- paste("../data/multilabel/",dataset,"/",dataset,"-train.csv",sep="")
	trainData <- read.csv(trainFile)
	
	# load train data
	testFile  <- paste("../data/multilabel/",dataset,"/",dataset,"-test.csv",sep="")
	testData  <- read.csv(testFile)
	
	return(list(trainDataX=trainData[,1:nFeatures], trainDataY=trainData[,-(1:nFeatures)],
					testDataX=testData[,1:nFeatures], testDataY=testData[,-(1:nFeatures)]))
}

#
# evaluate methods with GLM as the base learner
#
MultiLabel.GLM.Evaluate.old <- function(predictions, trueLabels, metrics=c("HammingLoss", "SpeciesAUC"))
{

	# evaluate
	metric <- MultiLabel.Evaluate(trueLabels, predictions, metrics)
	
	return(bestMetrics)
}

##
## evaluate methods with GLM as the base learner
##
#MultiLabel.GLM.Evaluate.old <- function(predictions, trueLabels, metrics=c("HammingLoss", "SpeciesAUC"))
#{
#	alphas <- names(predictions)
#	
#	bestMetrics <- list()
#	for (metric in metrics) {
#		if (metric == "HammingLoss" || metric == "RankingLoss" ||
#			metric == "OneError" || metric == "Coverage") {
#			bestMetrics[[metric]] <- 1
#			next
#		}
#		bestMetrics[[metric]] <- -1
#	}
#	for (alpha in alphas) {
#		cat("alpha",alpha,"\n")
#		
#		prediction <- predictions[[alpha]]
#		lambdas <- names(prediction)
#		
#		for (lambda in lambdas) {
#			#cat("lambda",lambda,"alpha",alpha,"\n")
#			
#			# get predicted labels
#			predLabels <- prediction[[lambda]]
#			
#			# evaluate
#			metric <- MultiLabel.Evaluate(trueLabels, predLabels, metrics)
#			
#			# store the best value for each metric
#			if (!is.null(bestMetrics[["HammingLoss"]]) && (bestMetrics[["HammingLoss"]] > metric[["HammingLoss"]])) { bestMetrics[["HammingLoss"]] <- metric[["HammingLoss"]]; bestMetrics[["HammingLossLabel"]] <- metric[["HammingLossLabel"]] }
#			if (!is.null(bestMetrics[["ExactMatch"]])  && (bestMetrics[["ExactMatch"]]  < metric[["ExactMatch"]]))  { bestMetrics[["ExactMatch"]]  <- metric[["ExactMatch"]] }
#			if (!is.null(bestMetrics[["SiteAUC"]])     && (bestMetrics[["SiteAUC"]]     < metric[["SiteAUC"]]))     { bestMetrics[["SiteAUC"]]     <- metric[["SiteAUC"]] }
#			if (!is.null(bestMetrics[["SpeciesAUC"]])  && (bestMetrics[["SpeciesAUC"]]  < metric[["SpeciesAUC"]]))  { bestMetrics[["SpeciesAUC"]]  <- metric[["SpeciesAUC"]]; bestMetrics[["SpeciesAUCLabel"]] <- metric[["SpeciesAUCLabel"]] }
#			if (!is.null(bestMetrics[["MicroF1"]])     && (bestMetrics[["MicroF1"]]     < metric[["MicroF1"]]))     { bestMetrics[["MicroF1"]]     <- metric[["MicroF1"]] }
#			if (!is.null(bestMetrics[["MacroF1"]])     && (bestMetrics[["MacroF1"]]     < metric[["MacroF1"]]))     { bestMetrics[["MacroF1"]]     <- metric[["MacroF1"]] }
#		} # lambdas
#	} # alphas
#	
#	return(bestMetrics)
#}


##
## binary relevance training with GLMNET as the base learner
##
#MultiLable.BR.GLM.Train <- function(trainDataX, trainDataY, alphas = 0)
#{
#	labelNames <- colnames(trainDataY)
#	
#	trainDataX <- as.matrix(trainDataX)
#	class(trainDataX) <- "numeric"
#	
#	models <- list()
#	for (alpha in alphas) {	
#		cat(paste("alpha is",alpha,"\n"))
#		
#		# train model
#		fits <- list()
#		for (label in labelNames) {  
#			y <- trainDataY[,label]
#			cat(label,"\n")
#			
#			if (length(unique(y)) != 1) {
#				cat(mean(y),"\n")
#				
#				fits[[label]] <- eval(glmnet(as.matrix(trainDataX), y, family="binomial", alpha=alpha))
#			} else {
#				fits[[label]] <- NA 
#			}
#		}
#		
#		models[[as.character(alpha)]] <- fits
#	} # alphas
#	
#	return(models)
#}
#
#
##
## binary relevance predicting with GLMNET as the base learner
##
#MultiLable.BR.GLM.Predict <- function(models, testDataX, lambdas)
#{
#	alphas <- names(models)
#	labelNames <- names(models[[alphas[1]]])
#	
#	testDataX <- as.matrix(testDataX)
#	class(testDataX) <- "numeric"
#	
#	results <- list()
#	for (alpha in alphas) {		
#		model <- models[[alpha]]
#		
#		result <- list()
#		for (lambda in lambdas) {
#			cat("lambda",lambda,"alpha",alpha,"\n")
#			
#			predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
#			predictions <- data.frame(predictions)
#			colnames(predictions) <- labelNames
#			for (label in labelNames) { 
#				if (length(model[[label]]) > 1) {
#					predictions[,label] <- predict(model[[label]],type="response",newx=as.matrix(testDataX),s=lambda)
#				} 
#			} # label
#			
#			result[[as.character(lambda)]] <- predictions
#		} # lambdas
#		
#		results[[alpha]] <- result
#	} # alphas
#	
#	return(results)
#}
#
#
##
## Ensemble of Classifier Chains (ECC) approach with GLMNET as the base learner
##
#MultiLable.ECC.GLM <- function(trainDataX, trainDataY, testDataX, nChains=10, alphas = 0, lambdas = 0, orderings = NULL)
#{
#	alphas <- as.character(alphas)
#	lambdas <- as.character(lambdas)	
#	labelNames <- colnames(trainDataY)
#	nLabels <- ncol(trainDataY)
#	
#	trainDataX <- as.matrix(trainDataX)
#	class(trainDataX) <- "numeric"
#	testDataX <- as.matrix(testDataX)
#	class(testDataX) <- "numeric"
#	
#	
#	avgPredictions <- list()
#	for (alpha in alphas) {
#		avgPredictions[[alpha]] <- list()
#		for (lambda in lambdas) {
#			avgPredictions[[alpha]][[lambda]] <- matrix(0,nrow=nrow(testDataX),ncol=nLabels)
#			colnames(avgPredictions[[alpha]][[lambda]]) <- labelNames
#		}
#	}
#	
#	# generate random chain orderings
#	if (is.null(orderings)) {
#		orderings <- list()
#		for (c in 1:nChains) {
#			orderings[[c]] <- labelNames[sample(nLabels, replace = FALSE)]
#		}
#	}
#	
#	# for each chain
#	for (c in 1:nChains) {
#		cat("chain",c,"\n")
#		
#		# get random chain ordering
#		orderingLabels <- orderings[[c]]
#		
#		# train
#		models <- list()
#		for (alpha in alphas) {
#			cat("alpha is",alpha,"\n")
#			
#			# pred data for each n.tree
#			chainTrainDataX <- trainDataX
#			
#			# train model
#			fits <- list()
#			for (label in orderingLabels) {
#				y <- trainDataY[,label]
#				
#				if (length(unique(y)) != 1) {
#					fits[[label]] <- eval(glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha))
#				} else {
#					fits[[label]] <- NULL 
#				}
#				
#				chainTrainDataX <- cbind(chainTrainDataX,trainDataY[,label])
#				colnames(chainTrainDataX)[ncol(chainTrainDataX)] <- label
#			} # labels
#			
#			models[[alpha]] <- fits
#		} # alphas
#		
#		# predict
#		for (alpha in alphas) {
#			model <- models[[alpha]]
#			
#			for (lambda in lambdas) {
#				#cat("lambda",lambda,"alpha",alpha,"\n")
#				
#				chainTestDataX <- testDataX
#				
#				predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
#				predictions <- data.frame(predictions)
#				colnames(predictions) <- labelNames
#				for (label in orderingLabels) { 
#					if (!is.null(model[[label]])) {
#						predictions[,label] <- predict(model[[label]],type="response",newx=as.matrix(chainTestDataX),s=as.numeric(lambda))
#					} 
#					
#					prediction <- predictions[,label]
#					prediction[prediction >= 0.5] <- 1
#					prediction[prediction < 0.5] <- 0
#					chainTestDataX <- cbind(chainTestDataX,prediction)
#					colnames(chainTestDataX)[ncol(chainTestDataX)] <- label
#				}
#				
#				# update the aggreaged predictions
#				avgPredictions[[alpha]][[as.character(lambda)]] <- (avgPredictions[[alpha]][[as.character(lambda)]] * (c-1) + predictions) / c;
#			} # lambdas
#		} # alphas
#	} # chain
#	
#	return(avgPredictions)
#}
#
#
#
#
##
## Ensemble of Classifier Chains (ECC) approach with GLMNET as the base learner
## The beta version is different from original ECC in that we use predicted label as features to train classifier chains
## instead of using the true label in the training phase.
##
#MultiLable.ECC.Beta.GLM <- function(trainDataX, trainDataY, testDataX, nChains=10, alphas = 0, lambdas = 0)
#{
#	alphas <- as.character(alphas)
#	lambdas <- as.character(lambdas)	
#	labelNames <- colnames(trainDataY)
#	nLabels <- ncol(trainDataY)
#	
#	trainDataX <- as.matrix(trainDataX)
#	class(trainDataX) <- "numeric"
#	testDataX <- as.matrix(testDataX)
#	class(testDataX) <- "numeric"
#	
#	# for each chain
#	avgPredictions <- list()
#	for (alpha in alphas) {
#		avgPredictions[[alpha]] <- list()
#		for (lambda in lambdas) {
#			avgPredictions[[alpha]][[lambda]] <- matrix(0,nrow=nrow(testDataX),ncol=nLabels)
#			colnames(avgPredictions[[alpha]][[lambda]]) <- labelNames
#		}
#	}
#	
#	for (c in 1:nChains) {
#		cat("chain",c,"\n")
#		
#		# generate random ordering
#		ordering <- sample(nLabels, replace = FALSE)
#		orderingLabels <- labelNames[ordering]
#		
#		# train
#		models <- list()
#		for (alpha in alphas) {
#			cat("alpha is",alpha,"\n")
#			
#			
#			
#			for (lambda in lambdas) {
#				# pred data for each n.tree
#				chainTrainDataX <- trainDataX
#				
#				fits <- list()
#				
#				# train model
#				for (label in orderingLabels) {
#					y <- trainDataY[,label]
#					
#					prediction <- matrix(0, nrow=nrow(trainDataX), ncol=1)
#					if (length(unique(y)) != 1) {
#						fits[[label]] <- eval(glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha))
#						prediction <- predict(fits[[label]],type="response",newx=as.matrix(chainTrainDataX),s=as.numeric(lambda))
#					} else {
#						fits[[label]] <- NULL 
#					}
#					
#					#chainTrainDataX <- cbind(chainTrainDataX,trainDataY[,label])
#					prediction[prediction >= 0.5] <- 1
#					prediction[prediction < 0.5] <- 0
#					chainTrainDataX <- cbind(chainTrainDataX,prediction)
#					colnames(chainTrainDataX)[ncol(chainTrainDataX)] <- label
#				} # labels
#				
#				models[[alpha]][[lambda]] <- fits
#			} # lambdas
#		} # alphas
#		
#		# predict
#		for (alpha in alphas) {
#			for (lambda in lambdas) {
#				#cat("lambda",lambda,"alpha",alpha,"\n")
#				
#				model <- models[[alpha]][[lambda]]
#				chainTestDataX <- testDataX
#				
#				predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
#				predictions <- data.frame(predictions)
#				colnames(predictions) <- labelNames
#				for (label in orderingLabels) { 
#					if (!is.null(model[[label]])) {
#						predictions[,label] <- predict(model[[label]],type="response",newx=as.matrix(chainTestDataX),s=as.numeric(lambda))
#					} 
#					
#					prediction <- predictions[,label]
#					prediction[prediction >= 0.5] <- 1
#					prediction[prediction < 0.5] <- 0
#					chainTestDataX <- cbind(chainTestDataX,prediction)
#					colnames(chainTestDataX)[ncol(chainTestDataX)] <- label
#				}
#				
#				# update the aggreaged predictions
#				avgPredictions[[alpha]][[as.character(lambda)]] <- (avgPredictions[[alpha]][[as.character(lambda)]] * (c-1) + predictions) / c;
#			} # lambdas
#		} # alphas
#	} # chain
#	
#	return(avgPredictions)
#}
#
#
#
##
## Ensemble of Classifier Chains (ECC) approach with GLMNET as the base learner
## This is a cheating experiment by using the true labels during prediction
##
#MultiLable.ECC.Cheat.GLM <- function(trainDataX, trainDataY, testDataX, testDataY, nChains=10, alphas = 0, lambdas = 0, orderings = NULL)
#{
#	alphas <- as.character(alphas)
#	lambdas <- as.character(lambdas)	
#	labelNames <- colnames(trainDataY)
#	nLabels <- ncol(trainDataY)
#	
#	trainDataX <- as.matrix(trainDataX)
#	class(trainDataX) <- "numeric"
#	testDataX <- as.matrix(testDataX)
#	class(testDataX) <- "numeric"
#	
#	
#	avgPredictions <- list()
#	for (alpha in alphas) {
#		avgPredictions[[alpha]] <- list()
#		for (lambda in lambdas) {
#			avgPredictions[[alpha]][[lambda]] <- matrix(0,nrow=nrow(testDataX),ncol=nLabels)
#			colnames(avgPredictions[[alpha]][[lambda]]) <- labelNames
#		}
#	}
#	
#	# generate random chain orderings
#	if (is.null(orderings)) {
#		orderings <- list()
#		for (c in 1:nChains) {
#			orderings[[c]] <- labelNames[sample(nLabels, replace = FALSE)]
#		}
#	}
#	
#	# for each chain
#	for (c in 1:nChains) {
#		cat("chain",c,"\n")
#		
#		# get random chain ordering
#		orderingLabels <- orderings[[c]]
#		
#		# train
#		models <- list()
#		for (alpha in alphas) {
#			cat("alpha is",alpha,"\n")
#			
#			# pred data for each n.tree
#			chainTrainDataX <- trainDataX
#			
#			# train model
#			fits <- list()
#			for (label in orderingLabels) {
#				y <- trainDataY[,label]
#				
#				if (length(unique(y)) != 1) {
#					fits[[label]] <- eval(glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha))
#				} else {
#					fits[[label]] <- NULL 
#				}
#				
#				chainTrainDataX <- cbind(chainTrainDataX,trainDataY[,label])
#				colnames(chainTrainDataX)[ncol(chainTrainDataX)] <- label
#			} # labels
#			
#			models[[alpha]] <- fits
#		} # alphas
#		
#		# predict
#		for (alpha in alphas) {
#			model <- models[[alpha]]
#			
#			for (lambda in lambdas) {
#				#cat("lambda",lambda,"alpha",alpha,"\n")
#				
#				chainTestDataX <- testDataX
#				
#				predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
#				predictions <- data.frame(predictions)
#				colnames(predictions) <- labelNames
#				for (label in orderingLabels) { 
#					if (!is.null(model[[label]])) {
#						predictions[,label] <- predict(model[[label]],type="response",newx=as.matrix(chainTestDataX),s=as.numeric(lambda))
#					} 
#					
#					chainTestDataX <- cbind(chainTestDataX,testDataY[,label])
#					colnames(chainTestDataX)[ncol(chainTestDataX)] <- label
#				}
#				
#				# update the aggreaged predictions
#				avgPredictions[[alpha]][[as.character(lambda)]] <- (avgPredictions[[alpha]][[as.character(lambda)]] * (c-1) + predictions) / c;
#			} # lambdas
#		} # alphas
#	} # chain
#	
#	return(avgPredictions)
#}


#
# binary relevance training with GLMNET as the base learner
#
MultiLable.BR.GLM <- function(trainDataX, trainDataY, testDataX, alpha = 0)
{
	labelNames <- colnames(trainDataY)
	
	trainDataX <- as.matrix(trainDataX)
	class(trainDataX) <- "numeric"
	testDataX <- as.matrix(testDataX)
	class(testDataX) <- "numeric"
	
	cat(paste("alpha is",alpha,"\n"))
	
	predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
	predictions <- data.frame(predictions)
	colnames(predictions) <- labelNames
	for (label in labelNames) {  
		y <- trainDataY[,label]
		cat(label,"\n")
		
		if (length(unique(y)) != 1) {
			# train
			model <- try(eval(cv.glmnet(as.matrix(trainDataX), y, family="binomial", alpha=alpha, nfolds = 5)))
			while (class(model) == "try-error") {
				model <- try(eval(cv.glmnet(as.matrix(trainDataX), y, family="binomial", alpha=alpha, nfolds = 5)))
			}
			
			# predict			
			predictions[,label] <- predict(model,type="response",newx=as.matrix(testDataX), s=GLM.s)
		}
	} # label
	
	return(predictions)
}


#
# Ensemble of Classifier Chains (ECC) approach with GLMNET as the base learner
#
MultiLable.ECC.GLM <- function(trainDataX, trainDataY, testDataX, nChains=10, alpha = 0, orderings = NULL)
{
	labelNames <- colnames(trainDataY)
	nLabels <- ncol(trainDataY)
	nTestData <- nrow(testDataX)
	
	trainDataX <- as.matrix(trainDataX)
	class(trainDataX) <- "numeric"
	testDataX <- as.matrix(testDataX)
	class(testDataX) <- "numeric"
	
	avgPredictions <- matrix(0,nrow=nTestData,ncol=nLabels)
	colnames(avgPredictions) <- labelNames
	
	# generate random chain orderings
	if (is.null(orderings)) {
		orderings <- list()
		for (c in 1:nChains) {
			orderings[[c]] <- labelNames[sample(nLabels, replace = FALSE)]
		}
	}
	
	# for each chain
	for (c in 1:nChains) {
		cat("chain",c,"\n")
		
		# get random chain ordering
		orderingLabels <- orderings[[c]]
		chainTrainDataX <- trainDataX
		
		# train
		fits <- list()
		for (label in orderingLabels) {
			y <- trainDataY[,label]
			
			if (length(unique(y)) != 1) {
				fits[[label]] <- try (eval(cv.glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha, nfolds=5)))
				while (class(fits[[label]]) == "try-error") {
					fits[[label]] <- try (eval(cv.glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha, nfolds=5)))
				}
			} else {
				fits[[label]] <- NULL 
			}
			
			chainTrainDataX <- cbind(chainTrainDataX,trainDataY[,label])
			colnames(chainTrainDataX)[ncol(chainTrainDataX)] <- label
		} # labels
		
		# predict
		chainTestDataX <- testDataX
		predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
		predictions <- data.frame(predictions)
		colnames(predictions) <- labelNames
		for (label in orderingLabels) { 
			if (!is.null(fits[[label]])) {
				predictions[,label] <- predict(fits[[label]],type="response",newx=as.matrix(chainTestDataX),s=GLM.s)
			} 
			
			prediction <- predictions[,label]
			prediction[prediction >= 0.5] <- 1
			prediction[prediction < 0.5] <- 0
			chainTestDataX <- cbind(chainTestDataX,prediction)
			colnames(chainTestDataX)[ncol(chainTestDataX)] <- label
		}
		
		# update the aggreaged predictions
		avgPredictions <- (avgPredictions * (c-1) + predictions) / c;
	} # chain
	
	return(avgPredictions)
}



#
# Ensemble of Classifier Chains (ECC) approach with GLMNET as the base learner
# The beta version is different from original ECC in that we use predicted label as features to train classifier chains
# instead of using the true label in the training phase.
#
MultiLable.ECC.Beta.GLM <- function(trainDataX, trainDataY, testDataX, nChains=10, alpha = 0, orderings = NULL)
{
	labelNames <- colnames(trainDataY)
	nLabels <- ncol(trainDataY)
	nTestData <- nrow(testDataX)
	
	trainDataX <- as.matrix(trainDataX)
	class(trainDataX) <- "numeric"
	testDataX <- as.matrix(testDataX)
	class(testDataX) <- "numeric"
	
	avgPredictions <- matrix(0,nrow=nTestData,ncol=nLabels)
	colnames(avgPredictions) <- labelNames
	
	# generate random chain orderings
	if (is.null(orderings)) {
		orderings <- list()
		for (c in 1:nChains) {
			orderings[[c]] <- labelNames[sample(nLabels, replace = FALSE)]
		}
	}
	
	for (c in 1:nChains) {
		cat("chain",c,"\n")
		
		# get random chain ordering
		orderingLabels <- orderings[[c]]
		chainTrainDataX <- trainDataX
		
		# train
		fits <- list()
		for (label in orderingLabels) {
			y <- trainDataY[,label]
			
			prediction <- matrix(0, nrow=nrow(trainDataX), ncol=1)
			if (length(unique(y)) != 1) {
				fits[[label]] <- try (eval(cv.glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha, nfolds=5)))
				while (class(fits[[label]]) == "try-error") {
					fits[[label]] <- try (eval(cv.glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha, nfolds=5)))
				}
				
				prediction <- predict(fits[[label]],type="response",newx=as.matrix(chainTrainDataX),s=GLM.s)
			} else {
				fits[[label]] <- NULL 
			}
			
			chainTrainDataX <- cbind(chainTrainDataX,prediction)
			colnames(chainTrainDataX)[ncol(chainTrainDataX)] <- label
		} # labels
		
		# predict
		chainTestDataX <- testDataX
		predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
		predictions <- data.frame(predictions)
		colnames(predictions) <- labelNames
		for (label in orderingLabels) { 
			if (!is.null(fits[[label]])) {
				predictions[,label] <- predict(fits[[label]],type="response",newx=as.matrix(chainTestDataX),s=GLM.s)
			} 
			
			prediction <- predictions[,label]
			chainTestDataX <- cbind(chainTestDataX,prediction)
			colnames(chainTestDataX)[ncol(chainTestDataX)] <- label
		}
		
		# update the aggreaged predictions
		avgPredictions <- (avgPredictions * (c-1) + predictions) / c;
	} # chain
	
	return(avgPredictions)
}


#
# Ensemble of Classifier Chains (ECC) approach with GLMNET as the base learner
# This is a cheating experiment by using the true labels during prediction
#
MultiLable.ECC.Cheat.GLM <- function(trainDataX, trainDataY, testDataX, testDataY, nChains=10, alpha = 0, orderings = NULL)
{
	labelNames <- colnames(trainDataY)
	nLabels <- ncol(trainDataY)
	nTestData <- nrow(testDataX)
	
	trainDataX <- as.matrix(trainDataX)
	class(trainDataX) <- "numeric"
	testDataX <- as.matrix(testDataX)
	class(testDataX) <- "numeric"
	
	avgPredictions <- matrix(0,nrow=nTestData,ncol=nLabels)
	colnames(avgPredictions) <- labelNames
	
	# generate random chain orderings
	if (is.null(orderings)) {
		orderings <- list()
		for (c in 1:nChains) {
			orderings[[c]] <- labelNames[sample(nLabels, replace = FALSE)]
		}
	}
	
	# for each chain
	for (c in 1:nChains) {
		cat("chain",c,"\n")
		
		# get random chain ordering
		orderingLabels <- orderings[[c]]
		chainTrainDataX <- trainDataX
		
		# train
		fits <- list()
		for (label in orderingLabels) {
			y <- trainDataY[,label]
			
			if (length(unique(y)) != 1) {
				fits[[label]] <- try (eval(cv.glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha, nfolds=5)))
				while (class(fits[[label]]) == "try-error") {
					fits[[label]] <- try (eval(cv.glmnet(as.matrix(chainTrainDataX), y, family="binomial", alpha=alpha, nfolds=5)))
				}
			} else {
				fits[[label]] <- NULL 
			}
			
			chainTrainDataX <- cbind(chainTrainDataX,trainDataY[,label])
			colnames(chainTrainDataX)[ncol(chainTrainDataX)] <- label
		} # labels
		
		# predict 
		chainTestDataX <- testDataX
		
		predictions <- matrix(0,nrow=nrow(testDataX),ncol=length(labelNames))
		predictions <- data.frame(predictions)
		colnames(predictions) <- labelNames
		for (label in orderingLabels) { 
			if (!is.null(fits[[label]])) {
				predictions[,label] <- predict(fits[[label]],type="response",newx=as.matrix(chainTestDataX),s=GLM.s)
			} 
			
			chainTestDataX <- cbind(chainTestDataX,testDataY[,label])
			colnames(chainTestDataX)[ncol(chainTestDataX)] <- label
		} # labels
		
		# update the aggreaged predictions
		avgPredictions <- (avgPredictions * (c-1) + predictions) / c;
	} # chain
	
	return(avgPredictions)
}


