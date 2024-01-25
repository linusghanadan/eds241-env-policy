# Generate data on potential outcomes and pre-treatment covariates:

rm(list=ls())
library(MASS)
# Define variables and generate data:
N <- 20000
Xi <- sample(c(1,2,3,4,5),N, replace=TRUE)
m0_Xi <- 0.5*Xi
m1_Xi <- 1*Xi
# Generate correlated error terms:
ei <- mvrnorm(n=N,mu=c(0, 0),Sigma=matrix(c(1,0.75,0.75,1), ncol=2))
# Calculate potential outcomes:
Yi_0 = m0_Xi + ei[,1]		
Yi_1 = m1_Xi + ei[,2]
# Output the mean of the potential outcomes:
mean(Yi_0)
mean(Yi_1)

if ((Xi + ei) <= 3)
  Di = 1 
if ((Xi + ei) > 3)
  Di = 0

# Create a dataframe from the vectors:
df <- data.frame(Xi, Di, Yi_0, Yi_1)

