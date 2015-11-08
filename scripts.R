
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

## logistic function
g <- function(z) 1 / (1 + exp(-z))

## gradient function
grad.obj <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  m <- ncol(x)
  
  fxn <- function(theta){
    (1/m)* (t(x) %*%(g(x %*% theta) - y))
  }
  
  return(
    list(m = m, labels = y, features = x, fxn = fxn)
  )
}

## gradient descent method
grad.descent <- function(grad.obj, maxiter = 1000, alpha = 0.05){
  theta_n = matrix(0, nrow = grad.obj$m, ncol = 1)
  for(i in 1:maxiter){
    theta_o = theta_n
    theta_n = theta_o - alpha * grad.obj$fxn(theta_o)
  }
  theta_n
}

fit_theta <- grad.descent(grad.obj(x,y))

length(x)
class(fit_theta)

p <- g(as.matrix(x) %*% fit_theta)
px <- ifelse(p > 0.5, 1, 0)

sum(px == y) / nrow(data)

View(data.frame(p, y))
warnings()
ncol(as.matrix(x))
