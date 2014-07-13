#usage examples of "bayesian naive bayes" for categorical data
# (c) Peter Werner 2014
# ref Bayesian Reasoning and Machine Learning by Barber

source('dirichlet-bayes.R')

#barber example

##
#this is just a bit of dancing around to setup the data
##
#class 'english', 5 features, 6 observations
xe <- matrix(c(0, 1, 1, 1, 0, 0,
0, 0, 1, 1, 1, 0,
1, 1, 0, 0, 0, 0,
1, 1, 0, 0, 0, 1,
1, 0, 1, 0, 1, 0), nr=5, nc=6, byrow=T)

#class 'scottish',  7 observations
xs <- matrix(c(1, 1, 1, 1, 1, 1, 1,
0, 1, 1, 1, 1, 0, 0,
0, 0, 1, 0, 0, 1, 1,
1, 0, 1, 1, 1, 1, 0,
1, 1, 0, 0, 1, 0, 0), nr=5, nc=7, byrow=T)

#shortbread, lager, whiskey, porridge, football
xe1 <- ifelse(xe == 1, 'y', 'n')
xs1 <- ifelse(xs == 1, 'y', 'n')
xe2 <- rbind(rep('english', ncol(xe1)), xe1)
xs2 <- rbind(rep('scottish', ncol(xs1)), xs1)

dm <- data.frame(t(cbind(xe2, xs2)))
colnames(dm) <- c('class', 'shortbread', 'lager', 'whiskey', 'porridge', 'football')
##
#end dancing
###

#train the model
nbcl <- nbd_train(dm[,1], dm[,-1])

#more dancing
xtmp <- c(2, 1, 2, 2, 1)
xt1 <- ifelse(xtmp == 2, 'y', 'n')
names(xt1) <- colnames(dm)[2:6]

#predict a single datapoint
nbd_single(xt1, nbcl)
nbd_predict(xt1, nbcl)

#see how we go classifiying the training set
ts <- nbd_predict(dm[,-1], nbcl)
table(dm[,1], ts$class)

####
#second example, on the car evaluation dataset
#data from http://archive.ics.uci.edu/ml/datasets/Car+Evaluation
####

cd <- read.csv('car.data', header=F, stringsAsFactors=T)
colnames(cd) <- c('buying', 'main', 'doors', 'persons', 'lugboot', 'safety', 'eval')

#try and balance the training/test observations by each class size
tgts <- unique(cd[,7])
samp <- vector("numeric")
set.seed(12345)
for (cl in tgts) {
	obs <- which(cd$eval == cl)
	sz <- round(length(obs) * 0.7)
	samp <- c(samp, sample(obs, sz))
	cat(sprintf("% 6s: %d/%d\n", cl, sz, length(obs)))
}
#or you can just take your chances
#samp <- sample.int(nrow(cd), nrow(cd) * 0.7)

trainX <- cd[samp, 1:6]
trainY <- cd[samp, 7]
testX <- cd[-samp, 1:6]
testY <- cd[-samp, 7]

cars_nbcl <- nbd_train(trainY, trainX)
cars_nbcl$classML 
tmp <- nbd_predict(testX, cars_nbcl)
table(testY, tmp$class)


