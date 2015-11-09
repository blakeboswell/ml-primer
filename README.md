## Logistic Regression
This post is for me to reinforce the material from the Logistic regression section of Andrew Ng's Machine learning Coursera [course](https://www.coursera.org/learn/machine-learning/home/welcome).  I am familiar with Logistic regression and have applied it succesfully to classification problems in the past.  My goal with this post is to strengthen my understanding of the connection between the hypothesis, decision boundary, loss function, and parameter optimization involved.

### Logistic Regression for Classification 
To set up the problem:  given an observed set of labels `Y` having corresponding features `X` we want to predict the label, `y`, that should be assigned to a new set of features `x`.  I will first consider the binary class problem, where the labels `Y` only take on values in {0,1}.

### Hypothesis Representation
For binary classification we formulate a hypothesis that returns a value in (0,1) representing the probability that a set of observed features, `x`, corresponds to a label, `y`.

#### Logistic Function
The Logistic function, `g(z)`, satisfies the requirement that it returns a value in (0,1) for all real numbers `z`.

Plotting the Logistic function in `R`:
```r
## logistic function
g <- function(z) 1 / (1 + exp(-z))

## plot of logistic function over range [-5. 5] by 0.1
z <- seq(-5, 5, 0.1)
plot(x = z, y = g(z),
     main = 'Logistic Function, g(z) = 1 / (1 + exp(z))',
     xlab = 'z', ylab='g(z)', 
     col = ifelse(g(seq(-5, 5, 0.1)) < 0.5, 2, 4))
```
![alt text](https://cloud.githubusercontent.com/assets/12782539/11022001/5845831e-8621-11e5-88b0-b1466105921c.png "Figure I")

We can see the additional property of the Logistic function that `g(z) >= 0.5` when `z >= 0`.

Using the Logistic function, we can formulate our hypothesis as the probability that a given a set of features, `x`, will correspond to a label, `y`.  Formally, if `z = theta^T * x`, the linear combination of the parameters theta with the feature vector `x`, then `g(z) = P(y = 1| x; theta)`, or `g(z)` returns the probability that `y = 1` given the features `x` parameterized by theta.

Summary Points:

- For binary classification, the Logistic function, `g(z)` where `z = theta^T * x`, returns the probability that a set of features, `x`, corresponds to a label, `y`.
- The Logistic function, `g(z)`, is greater than or equal to 0.5 when `z = theta^T * x` is greater than or equal to 0.

### Cost Function for Logistic Regression
To fit the parameters theta of the Logistic regression equation we need a convex cost function:

`J(theta) = -(1/m) * Sum(Cost(g(z), y)), where z = theta^T*x`

The following is the Logistic regression cost function in `R`.  The argument `yhat` represents the prediction produced by the Logistic regression equation and the argument `y` represents the true label.  Note `yhat` in (0,1) and `y` in {0,1}.

```r
cost <- function(yhat, y){
  if(y == 1){
    -log(yhat)
  }else{
    -log(1 - yhat)
  }
}
```
By examining a plot of performance of the cost function for different values of yhat we can intuit that this function is indeed convex, and we will leave it at that. 
```r
yhat <- seq(0.01, 0.99, 0.01)
cost_y0 <- cost(yhat, 0)
cost_y1 <- cost(yhat, 1)
matplot (yhat, cbind (cost_y0, cost_y1), pch = 1,
         main = 'Cost Function',
         xlab = 'yhat', ylab = 'Cost',
         col=c(2,4))
legend("center", inset=0, legend=c("y==0", "y==1"), pch=1, col=c(2,4), horiz=TRUE)
```
![alt text](https://cloud.githubusercontent.com/assets/12782539/11022303/50653736-8629-11e5-9df1-49d38eb06d27.png "Figure II")

We can express the cost function without an if-statement as follows:
```r
cost <- function(yhat, y) -y*log(yhat) - (1-y)*log(1 - yhat)
```

### Minimizing the Logistic Regression Function
I'm going to use gradient descent for the excercise of implementing it in `R`. 

The first step will be to define the gradient of the `J(theta)`.  I'll leave the explanation of that derivation to other sources.  Here is the result in `R` code:
```r
## logistic function
g <- function(z) 1 / (1 + exp(-z))

## gradient of J(theta)
t(x) %*% (g(x %*% theta) - y)
```	

One way to set up the necessary structures and functions for performing gradient descent for Logistic regression in `R` is as follows:
```r
## logistic function
g <- function(z) 1 / (1 + exp(-z))

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
```
To test this implementation of Logistic regression we can use the Iris data set in `R`.  The Iris data set contains observations of three species of Iris and their corresponding features.  To simplify this to a binary classification problem we will create a new variable `setosa` which will be 1 if the observation is a setosa and 0 if it is not.  Additionally, we will predict based on only two features, `Sepal.Length` and `Sepal.Width`.

```r
require(dplyr)
## make 0,1 class for setosa and add intercept
dat <- iris %>% mutate(Intercept = 1, Setosa = (Species == 'setosa'))

## split features and labels
x <- dat %>% select(Intercept, Sepal.Length, Sepal.Width)
y <- dat %>% select(Setosa)
```

Next we will run the `grad.descent` method to find the best fit `theta`.  Then we can plug `theta` back into the Logistic function to produce a probability that an observation is a setosa.  We will then apply the decision rule if probability of an observation being a setosa is greater than 0.5 then label it as a setosa, otherwise do not.

```r
## use gradient descent to fit parameter
fit_theta <- grad.descent(grad.obj(x,y))

## generate predictions using the best fit theta
p_yhat <- g(as.matrix(x) %*% fit_theta)

## convert probabilities to 0,1 label
yhat <- ifelse(p_yhat < 0.5, 0, 1)
```

The last thing to do for this simple test is to check the accuracy of the predictions.
```r
## test to see how well the data fits
sum(yhat == y) / nrow(y)
```

In this instance we find that the predictions are 100% accurate. We did not test the performance of the model on unseen data, so overfitting is a potential issue.  However, the accuracy of the result indicates that the gradient descent method is doing its job correctly.  

### Decision Boundary

The best fit values for theta determined by the gradient descent method are as follows: 

Intercept	  | Sepal Length | Sepal Width
------------- | ------------ | ------------
45.85848      | -19.48671    | 18.89480 

In order to characterize the boundary we rely on the fact that `g(z) < 0.5` when `z < 0` which means that the decision boundary is where `theta^T * x < 0`.  Therefore we have the boundary, `theta_0 + theta_1*x_1 + theta_2*x_2 >=0`.  This relationship can be rearranged to form a linear equation.  Once the slope and intercept of the boundary are obtained we can plot them as follows:

```r
## calculate coefficients
slope = -fit_theta[1] / fit_theta[3]
intercept = -fit_theta[2] / fit_theta[3]

## plot the decision boundary
plot(x$Sepal.Length, x$Sepal.Width, main="Logistic Regression Decision Boundary",
     xlab = 'Sepal Length', ylab = 'Sepal Width', col = ifelse(y == 1, 4, 1))
abline(slope, intercept)
```
![alt text](https://cloud.githubusercontent.com/assets/12782539/11026704/82806a86-867b-11e5-9293-1d82792b92cf.png "Figure III")






>__Point of Contention__:  Given the above definition in the section __Logistic Function__ it seems to me that Logistic regression is an example of *linear* classification method because z is linear in terms of the parameters theta.  However, I've read elsewhere that Logistic regression is linear because the decision boundary is linear - Andrew Ng gives examples of non-linear decision boundaries produced by logistic regression. So .. I'm not sure ..