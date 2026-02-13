# Simulate data
n <- 100
x <- rnorm(n)
y <- 3 + 2 * x + rnorm(n)
plot(y ~ x, col = "blue", pch = 19)

# Standard linear regression in R with lm
summary(lm(y ~ x))

# Gradient descent from scratch in R
alpha <- vector()
beta <- vector()
dEdalpha <- vector()
dEdbeta <- vector()
eta <- 0.01; alpha[1] <- 1; beta[1] <- 1
for(i in 1:1000)
{
  print(alpha[i])
  print(beta[i])
  
  dEdalpha[i] <- -sum(2 * (y - alpha[i] - beta[i] * x)) / n
  dEdbeta[i] <- -sum(2 * x * (y - alpha[i] - beta[i] * x)) / n
  
  alpha[i+1] <- alpha[i] - eta*dEdalpha[i]
  beta[i+1] <- beta[i] - eta*dEdbeta[i]
}
tail(alpha, 1)
tail(beta, 1)

