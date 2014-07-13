###
# this is a rather literal port of matlab code from the BRML examples by Barber
# http://web4.cs.ucl.ac.uk/staff/D.Barber/pmwiki/pmwiki.php?n=Brml.Software
# example comes from his book, chapter 10
#
# (c) Peter Werner 2014
##

condp <- function(pin) {
	p <- pin/max(pin)
	pnew <- p / sum(p)
	return(pnew)
}

logZdirichlet <- function(u) {
	lz <- sum(lgamma(u)) - lgamma(sum(u))
	return(lz)
}

nbdir_train <- function(xtrain, uprior)  {

	dims <- dim(uprior)
	nstate <- dims[1]
	nfeat <- dims[2]
	nclass <- dims[3]
	
	upost <- array(0, dims)
	n <- matrix(0, nr=1, nc=nclass)
	for (cl in 1:nclass) {
		#how many times this class appeared
		n[1, cl] <- ncol(xtrain[[cl]])
		#for each state
		for (st in 1:nstate) {
			#for each feat
			for (i in 1:nfeat) {	
				#count how many times this state
				upost[st, i, cl] <- uprior[st, i, cl] + sum(xtrain[[cl]][i,] == st)
			}
		}
	}
	cml <- condp(n)
	return(list(upost=upost, classML=cml))
}

nbdir_test <- function(xtest, classML, upost) {
	dims <- dim(upost)
	nstate <- dims[1]
	nfeat <- dims[2]
	nclass <- dims[3]

	logclasspost <- matrix(NA, nr=1, nc=nclass)
	utest <- array(0, dims)
	for (cl in 1:nclass) {
		#inital class probability
		logclasspost[1, cl] <- log(classML[cl])
		#for each feature we want to update the class probability
		for (i in 1:nfeat) {
			#for each state for this feature count if the new sample matches the state
			for (st in 1:nstate) {
				utest[st, i, cl] <- upost[st, i, cl] + (xtest[i] == st) 
			}
			
			lztest <- logZdirichlet(utest[,i,cl])
			lzpost <- logZdirichlet(upost[,i,cl])
			#update class posterior prob
			#using logs so the multiplicative relationship becomes additive
			logclasspost[1, cl] <- logclasspost[1, cl] + lztest - lzpost
		}
	}
	return(condp(exp(logclasspost)))
}

#class 'english', 5 features, 6 observations
xe <- matrix(c(0, 1, 1, 1, 0, 0,
0, 0, 1, 1, 1, 0,
1, 1, 0, 0, 0, 0,
1, 1, 0, 0, 0, 1,
1, 0, 1, 0, 1, 0), nr=5, nc=6, byrow=T)

#class 'scottish', 5 features, 7 observations
xs <- matrix(c(1, 1, 1, 1, 1, 1, 1,
0, 1, 1, 1, 1, 0, 0,
0, 0, 1, 0, 0, 1, 1,
1, 0, 1, 1, 1, 1, 0,
1, 1, 0, 0, 1, 0, 0), nr=5, nc=7, byrow=T)

uprior <- array(1,dim=c(2,5,2))
x <- list(xe+1, xs+1)


#train 
nbd <- nbdir_train(x, uprior)
#test
xt <- matrix(c(1, 0, 1, 1, 0), nc=1)
nbdir_test(xt+1, nbd$classML, nbd$upost)