
logistic.gradient <- function(x, y, theta){
  t(x) %*% (1 / (1 + exp(-(x %*% theta))) - y)
}

gradient.descent <- function(gradient, x, y){
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  m <- ncol(x)
  grad <- gradient
  
  theta.var <- matrix(0, nrow = m, 1)

  fit <- function(maxiter = 10000, alpha = 0.05, tol = .00001){
    for(i in 1:maxiter){
      theta.tmp <- theta.var
      theta.var <<- theta.tmp - alpha * grad(x, y, theta.tmp)
    }
  }
  
  fit.theta <- function(){
    theta.var
  }

  return(
    list(
      fit = fit,
      theta = fit.theta
  ))
}


library(dplyr)
x <- iris %>% select(Sepal.Length, Sepal.Width) %>% 
              mutate(Intercept = 1) %>% select(Intercept, Sepal.Length, Sepal.Width)
y <- iris %>% mutate(Setosa = as.integer(Species == 'setosa')) %>%
              select(Setosa)

mdl <- gradient.descent(logistic.gradient, x, y)

mdl$fit()
mdl$theta()  
mdl$error()

theta <- mdl$theta()

theta

## calculate coefficients
slope = -theta[1] / theta[3]
intercept = -theta[2] / theta[3]

slope 
intercept

## plot the decision boundary
plot(x$Sepal.Length, x$Sepal.Width, main="Logistic Regression Decision Boundary",
     xlab = 'Sepal Length', ylab = 'Sepal Width', col = ifelse(y == 1, 4, 1))
abline(slope, intercept)
