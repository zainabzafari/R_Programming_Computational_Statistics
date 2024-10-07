#Statistical Tests

library ( dplyr )
library ( ggplot2 )
# Experimental Design
alpha <- 0.05
n_samples <- 1000
mu1 <- mu2 <- 2
mu3 <- 4
sigma <- 1
set.seed (42)
pop1 <- rnorm (1000 , mu1 , sigma )
pop2 <- rnorm (1000 , mu2 , sigma )
pop3 <- rnorm (1000 , mu3 , sigma )
sample1 <- replicate ( n = n_samples , sample ( pop1 , size =5))
sample2 <- replicate ( n = n_samples , sample ( pop2 , size =5))
sample3 <- replicate ( n = n_samples , sample ( pop3 , size =5))
sample4 <- cbind (
  replicate ( n = n_samples /2 , sample ( pop2 , size =5)) ,
  replicate ( n = n_samples /2 , sample ( pop3 , size =5)))
