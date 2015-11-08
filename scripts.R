
## logistic function
g <- function(z) 1 / (1 + exp(-z))

## plot of logistic function over range [-5. 5] by 0.1
plot(x = seq(-5, 5, 0.1), y = g(seq(-5, 5, 0.1)),
     main = 'Logistic Function, g(z) = 1 / (1 + exp(z))',
     xlab = 'z', ylab='g(z)', 
     col = ifelse(g(seq(-5, 5, 0.1)) < 0.5, 2, 4))

## cost function
cost <- function(yhat, y){
  if(y == 1){
    -log(yhat)
  }else{
    -log(1 - yhat)
  }
}

## plot of cost function for y in {0,1} and yhat in (0,1)
yhat <- seq(0.01, 0.99, 0.01)
cost_y0 <- cost(yhat, 0)
cost_y1 <- cost(yhat, 1)
matplot (yhat, cbind (cost_y0, cost_y1), pch = 1,
         main = 'Cost Function',
         xlab = 'yhat', ylab = 'Cost',
         col=c(2,4))
legend("center", inset=0, legend=c("y==0", "y==1"), pch=1, col=c(2,4), horiz=TRUE)

## redefine cost function without if-statement
cost <- function(yhat, y) -y*log(yhat) - (1-y)*log(1 - yhat)



## logistic function
g <- function(z) 1 / (1 + exp(-z))

## gradient function
gradient <- function(x, y, theta) {
  gradient <- (1/m)* (t(x) %*% (g(x %*% t(theta)) - y))
  t(gradient)
}

## gradient decent
grad.descent <- function(x, y, maxiter=1000, alpha = .05, TOL = 0.01){
  theta <- matrix(c(0, 0), nrow=1)
  for(i in 1:maxiter){
    delta <- gradient(x, y, theta)
    theta <- theta - alpha*delta 
    if(sum(delta) < TOL){break}
  }
  list(
    theta = theta,
    cost = sum(delta)
  )
}


data <- iris
head(data)
data$Class <- ifelse(data$Species == 'setosa', 1, 0)
head(data)
x <- data[, 1:4]
y <- data[, 6]



# From calculation, we expect that the local minimum occurs at x=9/4

x_old = 0
x_new = 6 # The algorithm starts at x=6
alpha = 0.01 # step size
precision = 0.00001

## logistic function
g <- function(z) 1 / (1 + exp(-z))

## gradient function
gradient <- function(x, y) {
  x <- x
  y <- y
  m <- length(x)
  fxn <- function(theta){
    t((1/m)* (t(x) %*% (g(x %*% t(theta)) - y))) 
  }
  return(list(
    fxn = fxn
  ))
}

p <- gradient(x,y)

grad.descent <- function(dx, maxiter = 1000, alpha = 0.1, TOL = 0.00001){
  m <- length(x)
  theta_n = matrix(0, nrow = m, ncol = 1)
  for(i in 1:maxiter){
    theta_o = theta_n
    theta_n = theta_o - alpha * dx(theta_o)
    if(abs(theta_n - theta_o) < TOL){break}
  }
  theta_n
}

grad.descent(p$fxn)
