# Specify buildinfo
# 
# Author: Jun Yu
# Version: Sep, 2012
###############################################################################

rm(list=ls())

# set working directory
setwd("/Users/yujunnokia/workspace")

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