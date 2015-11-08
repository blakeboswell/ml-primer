
g <- function(z) 1 / (1 + exp(-z))

plot(x = seq(-5, 5, 0.1), y = g(seq(-5, 5, 0.1)),
     main = 'Logistic Function, g(z) = 1 / (1 + exp(z))',
     xlab = 'z', ylab='g(z)', 
     col = ifelse(g(seq(-5, 5, 0.1)) < 0.5, "red", "blue"))
