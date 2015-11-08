## Logistic Regression 101
This post is my project to reinforce the material from the Logistic regression section of Andrew Ng's Machine learning Coursera [course](https://www.coursera.org/learn/machine-learning/home/welcome).  I am familiar with Logistic regression and have applied it succesfully to clasification problems in the past.  My goal with this post is to strengthen my understanding of the connection between the hypothesis, decision boundary, loss function, and parameter optimization involved.

## Logistic Regression for Classification 
To set up the problem:  given an observed set of labels Y and corresponding features X we want to predict what label, y, should be assigned to a new set of features x.  I will first consider the binary class problem, where the labels Y only take on values in {0,1}.

## Hypothesis Function
For the problem of binary classification we formulate a hypothesis that returns a value in (0,1) representing the probability that a set of observed features, x, corresponds to a label, y.

#### Logistic Function
The Logistic function, g(z), satisfies the requirement that it returns a value in (0,1) for all real numbers z.

Plotting the Logistic function in R:
```r
g <- function(z) 1 / (1 + exp(-z))

plot(x = seq(-5, 5, 0.1), y = g(seq(-5, 5, 0.1)),
     main = 'Logistic Function, g(z) = 1 / (1 + exp(z))',
     xlab = 'z', ylab='g(z)', 
     col = ifelse(g(seq(-5, 5, 0.1)) < 0.5, "red", "blue"))
```
![Alt text](plots/logistic-ex.svg?raw=true "Logistic Function")

We can see the additional property of the Logistic function that g(z) >= 0.5 when z >= 0.

Using the Logistic function, we can formulate our hypothesis as outputting the probability that given a set of features, x, will correspond to a label, y.  Formally, if z = [theta]^T * x, the linear combination of the parameters theta with the feature vector x, then g(z) = P(y = 1| x; theta), or g(z) returns the probability that y = 1 given the features x parameterized by theta.

Summary Points:

- For binary classification, the Logistic function, g(z) where z = [theta]^T * x, returns the probability that a set of features, x, corresponds to a label, y.
- The Logistic function, g(z), is greater than or equal to 0.5 when z = [theta]^T * x is greater than or equal to 0.

## Loss Function for Logistic Regression


## Decision Boundary



>__Point of Contention__:  Given the above definition in the section __Logistic Function__ it seems to me that Logistic regression is an example of *linear* classification method because z is linear in terms of the parameters theta.  However, I've read elsewhere that Logistic regression is linear because the decision boundary is linear - Andrew Ng gives examples of non-linear decision boundaries produced by logistic regression. So .. I'm not sure ..