
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
grad.descent <- function(grad.obj, maxiter = 1000, alpha = 0.05){
  theta_n = matrix(0, nrow = grad.obj$m, ncol = 1)
  for(i in 1:maxiter){
    theta_o = theta_n
    theta_n = theta_o - alpha * grad.obj$fxn(theta_o)
  }
  theta_n
}

levels(iris$Species)

require(dplyr)
## make 0,1 class for species
data <- iris %>% mutate(class = as.integer(Species == 'setosa')) %>% 
  select(-Species)

## split features and labels
x <- data %>% select(-class)
y <- data %>% select(class)

## use gradient descent to fit parameters
fit_theta <- grad.descent(grad.obj(x,y))
p_yhat <- g(as.matrix(x) %*% fit_theta)
yhat <- ifelse(p_yhat < 0.5, 0, 1)

## test to see how well the data fits
sum(yhat == y) / nrow(data)

View(data.frame(p, y))
warnings()
ncol(as.matrix(x))
