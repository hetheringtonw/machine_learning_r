library(dplyr)
library(data.table)
library(ggplot2)

setwd('/Users/williamhetherington/Desktop/Machine Learning R')

### read in the challenger launch file ####
launch <- data.frame(fread('challenger.csv'))

### regression function ####
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}


########################################
### gradient descent method ##########
########################################

x <- as.matrix(launch$temperature)
y <- as.matrix(launch$distress_ct)

# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*nrow(y))
}

# learning rate and iteration limit
alpha <- 0.00001
num_iters <- 10000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)
initial_theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))

# gradient descent
gradientDescent <- function(theta,num_iters,alpha){
  m <- nrow(y)
  cost_iter <<- matrix(rep(0,num_iters))  
  for (i in 1:num_iters){
    theta <- theta - ((alpha/m)*(t(X)%*%((X%*%theta)-y)))
    cost_iter[i] <<- cost(X,y,theta)
  }
  print(theta)
}


for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3