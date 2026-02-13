########################################## ANN from scratch, in matrix form ##########################################
d <- matrix(c(0, 0, 1, 1), ncol = 1) # true labels
X <- matrix(c(c(0, 0, 1, 1), c(0, 1, 0, 1)), ncol = 2)
X

phi <- function(x){return(1/(1 + exp(-x)))} # activation function

mu <- 0.1; N_epochs <- 10000; E <- vector()
w <- matrix(c(0.1, 0.5), ncol = 1) # initialization of weights

for(epochs in 1:N_epochs)
{
  #Forward propagation
  y <- phi(X %*% w - 3) # for simplicity we use a fixed bias -3
  
  #Backward propagation
  E <- append(E, sum((d-y)^2))
  dE_dw <- - (d-y) * y * (1-y)
  w <- w - mu * (t(X) %*% dE_dw)
}
plot(E ~ seq(1:N_epochs), xlab="Epochs", ylab="Error", col="red")
y


########################################## ANN from scratch, not in matrix form ######################################
d <- c(0, 0, 1, 1) # true labels
x1 <- c(0, 0, 1, 1); x2 <- c(0, 1, 0, 1)

phi <- function(x){return(1/(1 + exp(-x)))} # activation function

mu <- 0.1; N_epochs <- 10000
w1 <- 0.1; w2 <- 0.5; E <- vector()
for(epochs in 1:N_epochs)
{
  #Forward propagation
  y <- phi(w1*x1 + w2*x2 - 3) # for simplicity we use a fixed bias -3
  
  #Backward propagation
  E[epochs] <- sum((d-y)^2)
  dE_dw1 <- - (1 / length(d)) * sum((d-y) * y * (1-y) * x1)
  dE_dw2 <- - (1 / length(d)) * sum((d-y) * y * (1-y) * x2)
  w1 <- w1 - mu * dE_dw1
  w2 <- w2 - mu * dE_dw2
}
plot(E ~ seq(1:N_epochs), xlab="Epochs", ylab="Error", col="red")
y
