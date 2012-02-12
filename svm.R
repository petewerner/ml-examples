library(e1071)

data <- read.csv('ex2data1.txt', header=FALSE)
names(data) <- c("X1", "X2", "Y")
data[,3] <- as.factor(data[,3])
x0 <- data[data$Y == 0, c(1,2)]
x1 <- data[data$Y == 1, c(1, 2)]

#model <- svm(Y~., data=data, cost=0.3, gamma=0.1)
#model <- svm(Y~., data=data, cost=1, gamma=0.03, coef0=0.001, kernel="sigmoid")
model <- svm(Y~., data=data, cost=0.03, gamma=1, coef0=10, kernel="polynomial")

npoints <- 100
x1vals <- seq(min(data[,1]), max(data[,1]), length=npoints)
x2vals <- seq(min(data[,2]), max(data[,2]), length=npoints)

df <- data.frame(x1vals, x2vals)
names(df) <- labels((terms(model)))

preds <- predict(model,expand.grid(df))
zz <- matrix(as.numeric(preds), nrow=nrow(df), byrow=T)
#dim(zz)

symbols(x0, circles=rep(0.5, nrow(x0)), inches=FALSE, bg="yellow")
symbols(x1, squares=rep(1, nrow(x1)), inches=FALSE, add=TRUE, bg="red")
contour(x1vals, x2vals, zz, add=T, levels=1:3)

#tunevals <- c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30)

#obj <- tune.svm(Y~., data=data, kernel="polynomial", gamma=tunevals, cost=tunevals, coef0=tunevals)

#- best parameters (sigmoid):
# gamma coef0 cost
#  0.03 0.001   10

#- best parameters:
# gamma coef0 cost
#     1    10 0.03