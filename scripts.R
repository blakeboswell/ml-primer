
## logistic function
g <- function(z) 1 / (1 + exp(-z))

## plot of logistic function over range [-5. 5] by 0.1
z <- seq(-5, 5, 0.1)
plot(x = z, y = g(z),
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

## gradient object
## contains labels, features, and gradient function
grad.obj <- function(x, y) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  m <- ncol(x)
  
  fxn <- function(theta){
    t(x) %*% (g(x %*% theta) - y)
  }
  
  return(
    list(m = m, labels = y, features = x, fxn = fxn)
  )
}

## gradient descent method
grad.descent <- function(grad.obj, maxiter = 10000, alpha = 0.05){
  theta_n = matrix(0, nrow = grad.obj$m, ncol = 1)
  for(i in 1:maxiter){
    theta_o = theta_n
    theta_n = theta_o - alpha * grad.obj$fxn(theta_o)
  }
  theta_n
}



require(dplyr)
## make 0,1 class for setosa and add intercept
dat <- iris %>% mutate(Intercept = 1, Setosa = (Species == 'setosa'))

## split features and labels
x <- dat %>% select(Intercept, Sepal.Length, Sepal.Width)
y <- dat %>% select(Setosa)

## use gradient descent to fit parameter
fit_theta <- grad.descent(grad.obj(x,y))
## generate predictions using the best fit theta
p_yhat <- g(as.matrix(x) %*% fit_theta)
## convert probabilities to 0,1 label
yhat <- ifelse(p_yhat < 0.5, 0, 1)

## test to see how well the data fits
sum(yhat == y) / nrow(y)
fit_theta

## plot the decision boundary
plot(x$Sepal.Length, x$Sepal.Width, main="Logistic Regression Decision Boundary",
     xlab = 'Sepal Length', ylab = 'Sepal Width', col = ifelse(y == 1, 4, 1))
abline(slope, intercept)
