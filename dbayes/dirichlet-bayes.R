
###
# bayesian naive bayes classifier for categorical inputs
# loosely based on some matlab code from the BRML toolbox 
# http://web4.cs.ucl.ac.uk/staff/D.Barber/pmwiki/pmwiki.php?n=Brml.Software
# (c) Peter Werner July 2014
#
# as input it expects a set of features (as R factors) and targets
# prediction returns the probabilities and class labels
#
# the main differences are:
# 1) supports an arbitrary number of states per feature
# 2) "vectorized" prediction function which can take a table of observations
# 3) R-ification, expects features/targets to be factors
# see dbayes-test.R for examples.

condp <- function(pin) {
	p <- pin/max(pin)
	pnew <- p / sum(p)
	return(pnew)
}

logZdirichlet <- function(u) {
	lz <- sum(lgamma(u)) - lgamma(sum(u))
	return(lz)
}

nbd_train <- function(ytrain, xtrain) {

	classlabs <- unique(ytrain)
	nclass <- length(classlabs)
	nfeat <- ncol(xtrain)
	
	classcnt <- vector("numeric")
	upost <- list()
	
	#count occurances for each class
	for (cl in classlabs) {
			classcnt[cl] <- sum(ytrain == cl)
	}
	
	#for each feature
	for (feat in colnames(xtrain)) {
		#get how many states it has
		nstate <- length(levels(xtrain[,feat]))
		#our flat prior
		prior <- matrix(1, nrow=nclass, ncol=nstate, dimnames=list(levels(ytrain), levels(xtrain[,feat])))
		#the posterior we wish to learn from the data
		post <-  matrix(0, nrow=nclass, ncol=nstate, dimnames=list(levels(ytrain), levels(xtrain[,feat])))
		#for each class
		for (cl in classlabs) {
			#for each state in the feature
			for (st in levels(xtrain[,feat])) {
				#count how often this (class, feature) is in state st
				post[cl,st] <- prior[cl, st] + sum(xtrain[which(ytrain==cl),feat] == st)
			}
		}
		upost[[feat]] <- post
	}
	#prob. of each class
	cml <- condp(unlist(classcnt))
	return(list(upost=upost, classML=cml, labs=classlabs))
}

nbd_single <- function(xtest, nbc) {

	nbcp <- nbc$upost
	cml <- nbc$classML
	nclass <- length(cml)
	logclasspost <- vector("numeric")

	for (cl in nbc$labs) {
		logclasspost[cl] <- log(cml[cl])
	}	

	for (feat in names(nbcp)) {
		nstate <- ncol(nbcp[[feat]])
		utest <- matrix(0, nrow=nclass, ncol=nstate, 
						dimnames=list(rownames(nbcp[[feat]]), colnames(nbcp[[feat]])))
		for (cl in nbc$labs) {	
			for (st in colnames(nbcp[[feat]])) {
				utest[cl, st] <- nbcp[[feat]][cl, st] + sum(xtest[feat] == st)
			}
			lztest <- logZdirichlet(utest[cl,])
			lzpost <- logZdirichlet(nbcp[[feat]][cl,])
			logclasspost[cl] <- logclasspost[cl] + lztest - lzpost
		}
	}
	return(condp(exp(logclasspost)))
}


nbd_predict <- function(xtest, nbc) {

	nobs <- nrow(xtest)
	if (is.null(dim(xtest)) || nobs == 1) {
		return(nbd_single(xtest, nbc))
	}

	nbcp <- nbc$upost
	cml <- nbc$classML
	nclass <- length(cml)
	logclasspost <- matrix(log(cml), nrow=nobs, nc=nclass, byrow=T)
	colnames(logclasspost) <- names(cml)
	
	#for each feature
	for (feat in names(nbcp)) {
		#count how many states it has
		nstate <- ncol(nbcp[[feat]])
		#create an array to store class, state count matrix for each input observation
		utmat <- array(0, dim=c(nobs, nclass, nstate))
		dimnames(utmat) <- list(c(), rownames(nbcp[[feat]]), colnames(nbcp[[feat]]))
		#for each class
		for (cl in names(cml)) {	
			#for each state in this feature
			for (st in colnames(nbcp[[feat]])) {
				#record the posterior + if this obs is in state st for feature feat
				utmat[,cl, st] <- nbcp[[feat]][cl, st] + ifelse(xtest[,feat] == st, 1, 0)
			}
			#work out the dirichlet numerator
			lztest <- apply(utmat[,cl,], 1, logZdirichlet)
			#and denominator
			lzpost <- logZdirichlet(nbcp[[feat]][cl,])
			#update the class posterior
			logclasspost[,cl] <- logclasspost[,cl] + lztest - lzpost
		}
	}
	#the class probabilities
	#we run through exp to get out of log space
	#then condp just makes sure they are legitimate probabilities
	classProbs <- t(apply(exp(logclasspost), 1, condp))
	#also return the class labels
	mlc <- apply(classProbs, 1, which.max)
	return(list(prob=classProbs, class=nbc$labs[mlc]))
}




