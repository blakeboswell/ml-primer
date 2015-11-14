
## logistic function
g <- function(z) 1 / (1 + exp(-z))

## output logistic plot
library(jsonlite)
z <- seq(-5, 5, 0.1)
toJSON(g(z))
toJSON(z)


## vectorized logistic gradient function
logistic.gradient <- function(x, y, theta){
  t(x) %*% (1 / (1 + exp(-(x %*% theta))) - y)
}

## gradient descent closure
gradient.descent <- function(gradient){
  
  grad <- gradient
  
  fit <- function(x, y, theta = NULL, maxiter = 10000, alpha = 0.05, tol = .00001){
    x <- as.matrix(x)
    y <- as.matrix(y)
    m <- ncol(x)
    
    if(is.null(theta)) theta <- matrix(0, nrow = m, 1)
    
    for(i in 1:maxiter){
      theta.tmp <- theta
      theta <- theta.tmp - alpha * grad(x, y, theta.tmp)
    }
    
    return(
      list(theta = theta)
    )
  }
  
  return(fit)
}

## initialize a gradient descent closure with the logistic gradient
mdl <- gradient.descent(logistic.gradient)

library(dplyr)
x <- iris %>% mutate(Intercept = 1) %>% 
  select(Intercept, Sepal.Length, Sepal.Width) %>% as.matrix
y <- iris %>% mutate(Setosa = as.integer(Species == 'setosa')) %>%
  select(Setosa) %>% as.matrix

## fit theta to the training set
theta_star <- mdl(x,y)$theta

## generate predictions using the best fit theta
py <- g(x %*% theta_star)

## apply decision rule to assign to 0,1 label
yhat <- ifelse(py < 0.5, 0, 1)

## test to see how well the data fits
sum(yhat == y) / length(y)

## check out the coefficients
theta_star

## generate data for plotly
toJSON(x[yhat == 1, 2])
toJSON(x[yhat != 1, 2])
toJSON(x[yhat == 1, 3])
toJSON(x[yhat != 1, 3])

## calculate linear coefficients
slope <- -theta_star[2] / theta_star[3]
intercept <- -theta_star[1] / theta_star[3]




