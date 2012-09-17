# multilable evaluation metric functions 
#	- Hamming Loss
#	- Ranking Loss ( the same as 1 - Site AUC)
#	- Species AUC
#	- Exact Match
# 	- one-error
# 	- average precision
# 	- coverage
# 	- Micro F1
# 	- Macro F1
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################

library(pROC)

THRESHOLD <- 0.5

#
# compute Hamming Loss. Note the predictions must be thresholde into 0/1
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
HammingLoss <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nRow <- nrow(Y)
	nCol <- ncol(Y)
	
	Z[Z < THRESHOLD] <- 0
	Z[Z >= THRESHOLD] <- 1
	
	return(sum(Y != Z)/(nRow*nCol))
}

HammingLossLabel <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nRow <- nrow(Y)
	nCol <- ncol(Y)
	
	Z[Z < THRESHOLD] <- 0
	Z[Z >= THRESHOLD] <- 1
	
	return(colSums(Y != Z)/nRow)
}

#
# compute Ranking Loss.
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
RankingLoss <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nRow <- nrow(Y)
	siteAUC <- req(0,nRow)
	for (i in 1:nRow) {
		siteAUC[i] <- auc(Y[i,],Z[i,])
	}
	
	return(1 - mean(siteAUC))
}


#
# compute Exact match. Note the predictions must be thresholde into 0/1
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
ExactMatch <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	Z[Z < THRESHOLD] <- 0
	Z[Z >= THRESHOLD] <- 1
	
	nRow <- nrow(Y)
	nCol <- ncol(Y)
	match <- apply(Y==Z,1,sum)
	
	return(sum(match==nCol) / nRow)
}


#
# compute species-based AUC
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
SpeciesAUC <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nCol <- ncol(Y)
	speciesAUC <- rep(0,nCol)
	for (j in 1:nCol) {
		speciesAUC[j] <- auc(Y[,j],Z[,j])
	}
	
	return(mean(speciesAUC))
}

SpeciesAUCLabel <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nCol <- ncol(Y)
	speciesAUC <- rep(0,nCol)
	for (j in 1:nCol) {
		speciesAUC[j] <- auc(Y[,j],Z[,j])
	}
	
	return(speciesAUC)
}

#
# compute site-based AUC
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
SiteAUC <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nRow <- nrow(Y)
	siteAUC <- rep(0,nRow)
	for (i in 1:nRow) {
		siteAUC[i] <- auc(Y[i,],Z[i,])
	}
	
	return(mean(siteAUC))
}

#
# compute one-error
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
OneError <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nRow <- nrow(Y)
	nCol <- ncol(Y)
	
	error <- 0
	for (i in 1:nRow) {
		max.idx <- order(Z[i,])[nCol]
		if (Y[i,max.idx] != 1) {
			error <- error + 1	
		}
	}
	
	return(error / nRow)
}

#
# compute average precision
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
AveragePrecision <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nRow <- nrow(Y)
	nCol <- ncol(Y)
	
	avePre <- NULL
	for (i in 1:nRow) {
		label.set <- which(Z[i,]==1)
		ordering <- order(Y[i,],decreasing=TRUE)
		n.label.set <- length(label.set)
		
		if (n.label.set == 0) {
			next
		}
		
		ap = 0;
		for (j in 1:n.label.set) {
			rankedAbove <- 0
			for (jj in 1:n.label.set) {
				if (ordering[label.set[jj]] <= ordering[label.set[j]]) {
					rankedAbove <- rankedAbove + 1;
				}
			}
			ap <- ap + (rankedAbove / ordering[label.set[j]])
		}
		avePre <- c(avePre,ap/n.label.set)
	}
	
	return(mean(avePre))
}

#
# compute coverage
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
Coverage <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	nRow <- nrow(Y)
	nCol <- ncol(Y)
	
	cover <- 0
	for (i in 1:nRow) {
		label.set <- which(Z[i,]==1)
		ordering <- order(Y[i,],decreasing=TRUE)
		cover <- cover + ( max(ordering[label.set]) - 1)	
	}
	
	return(cover / nRow)
}

#
# compute Micro F1. Note the predictions must be thresholde into 0/1
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
MicroMeasure <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	Z[Z < THRESHOLD] <- 0
	Z[Z >= THRESHOLD] <- 1
	
	# compute precision, recall and F1
	ZandY <- Y & Z
	precision <- sum(ZandY)/sum(Z)
	recall <- sum(ZandY)/sum(Y)
	F1 <- 2 * precision * recall / (precision + recall)
	
	return(list(precision=precision,recall=recall,F1=F1))
}

#
# compute Macro F1. Note the predictions must be thresholde into 0/1
#
# Y is true label matrix N * L where N is # of test instances and L is # of labels
# Z is prediction matrix N * L where N is # of test instances and L is # of labels
#
MacroMeasure <- function(Y,Z) 
{
	if (nrow(Y) != nrow(Z) || ncol(Y) != ncol(Z)) {
		stop("Dim of Y and Z does not match...")
	}
	
	Z[Z < THRESHOLD] <- 0
	Z[Z >= THRESHOLD] <- 1
	
	# for each label, compute precision, recall and F1
	ZandY <- Y & Z
	precision <- apply(ZandY,2,sum) / apply(Z,2,sum)
	recall <- apply(ZandY,2,sum) / apply(Y,2,sum)
	F1 <- 2*precision*recall / (precision+recall)
	
	precision <- precision[!is.nan(precision)]
	recall <- recall[!is.nan(recall)]
	F1 <- F1[!is.nan(F1)]
	
	return(list(precision=mean(precision),recall=mean(recall),F1=mean(F1)))
}


#
# evaluate different metrics specified by e
#
MultiLabel.Evaluate <- function(trueLabels, predLabels, metrics=c("HammingLoss")) 
{
	trueLabels <- as.matrix(trueLabels)
	class(trueLabels) <- "numeric"
	predLabels <- as.matrix(predLabels)
	class(trueLabels) <- "numeric"

	results <- list()
	
	# HammingLoss
	if (sum(metrics == "HammingLoss") == 1) 
	{
		results[["HammingLoss"]] <- HammingLoss(trueLabels,predLabels)
		results[["HammingLossLabel"]] <- HammingLossLabel(trueLabels,predLabels)
		#cat("Hamming Loss is",results[["HammingLoss"]],"\n")
	}
	
	# ExactMatch
	if (sum(metrics == "ExactMatch") == 1) 
	{
		results[["ExactMatch"]] <- ExactMatch(trueLabels,predLabels)
		#cat("Exact Match is",results[["ExactMatch"]] ,"\n")
	}
	
	# MicroF1
	if (sum(metrics == "MicroF1") == 1) 
	{
		results[["MicroF1"]] <- MicroMeasure(trueLabels,predLabels)$F1
		#cat("Micro F1 is",results[["MicroF1"]],"\n")
	}
	
	# MacroF1
	if (sum(metrics == "MacroF1") == 1) 
	{
		results[["MacroF1"]] <- MacroMeasure(trueLabels,predLabels)$F1
		#cat("Macro F1 is",results[["MacroF1"]],"\n")
	}
	
	# RankingLoss
	if (sum(metrics == "RankingLoss") == 1) 
	{
		results[["RankingLoss"]] <- RankingLoss(trueLabels,predLabels)
		#cat("Ranking Loss is",results[["RankingLoss"]],"\n")	
	}
	
	# SiteAUC
	if (sum(metrics == "SpeciesAUC") == 1) 
	{
		results[["SiteAUC"]] <- SiteAUC(trueLabels,predLabels)
		#cat("Site AUC is",results[["SiteAUC"]],"\n")
	}
	
	# SpeciesAUC
	if (sum(metrics == "SpeciesAUC") == 1) 
	{
		results[["SpeciesAUC"]] <- SpeciesAUC(trueLabels,predLabels)
		results[["SpeciesAUCLabel"]] <- SpeciesAUCLabel(trueLabels,predLabels)
		#cat("Species AUC is",results[["SpeciesAUC"]],"\n")
	}
	
	# OneError
	if (sum(metrics == "OneError") == 1) 
	{
		results[["OneError"]] <- OneError(trueLabels,predLabels)
		#cat("One Error is",results[["OneError"]],"\n")
	}
	
	# Coverage
	if (sum(metrics == "Coverage") == 1) 
	{
		results[["Coverage"]] <- Coverage(trueLabels,predLabels)
		#cat("Coverage is",results[["Coverage"]],"\n")
	}
	
	# AveragePrecision
	if (sum(metrics == "AveragePrecision") == 1) 
	{
		results[["AveragePrecision"]] <- AveragePrecision(trueLabels,predLabels)
		#cat("Average Precision is",results[["AveragePrecision"]],"\n")
	}
	
	return(results)
}
