
data <- read.csv('ex2data1.txt', header=FALSE)
names(data) <- c("X1", "X2", "Y")
 
npoints <- 100
x1vals <- seq(min(data[,1]), max(data[,1]), length=npoints)
x2vals <- seq(min(data[,2]), max(data[,2]), length=npoints)

p <- glm(Y~., data=data)
df <- data.frame(x1vals, x2vals)
names(df) <- labels((terms(p)))

preds <- predict(p,expand.grid(df))
zz <- matrix(as.numeric(preds), nrow=nrow(df), byrow=T)
#dim(zz)

x0 <- data[data$Y == 0, c(1,2)]
x1 <- data[data$Y == 1, c(1,2)]
symbols(x0, circles=rep(0.5, nrow(x0)), inches=FALSE, bg="yellow")
symbols(x1, squares=rep(1, nrow(x1)), inches=FALSE, add=TRUE, bg="red")
contour(x1vals, x2vals, zz, add=T, levels=(0.5))