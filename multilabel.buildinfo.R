# Specify buildinfo
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################

rm(list=ls())

# set working directory
setwd("/Users/yujunnokia/workspace/multilabel")

# include library
library("lattice")
library("Matrix")
library("glmnet")
library("gbm")
library("ggplot2")

# specify number of features
nFeatures <- list()
nFeatures[["yeast"]] <- 103
nFeatures[["emotions"]] <- 72
nFeatures[["scene"]] <- 294
nFeatures[["medical"]] <- 1449

nFeatures[["birds2009"]] <- 21
nFeatures[["birds2010"]] <- 21
nFeatures[["HJAbirds"]] <- 21
nFeatures[["hubbard1999"]] <- 36
nFeatures[["hubbard2008"]] <- 36
nFeatures[["HBRbirds"]] <- 36
nFeatures[["ebird"]] <- 22
nFeatures[["ebirdNY"]] <- 22
nFeatures[["ebirdCO"]] <- 22
nFeatures[["ebirdMA"]] <- 22
nFeatures[["ebirdMD"]] <- 22

