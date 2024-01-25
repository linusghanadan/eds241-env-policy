### Generate data on potential outcomes and pre-treatment covariates:
# Here we build on our work from Tuesday....
rm(list=ls())
library(MASS)
N <- 20000
Xi <- sample(c(1,2,3,4,5), N, replace=TRUE)
m0_Xi <- 0.5 * Xi
m1_Xi <- 1 * Xi
ei <- mvrnorm(n=N, mu=c(0, 0), Sigma=matrix(c(1, 0.75, 0.75, 1), ncol=2))
Yi_0 <- m0_Xi + ei[,1]		
Yi_1 <- m1_Xi + ei[,2]

# Treatment assignment
Ti <- rbinom(N, 1, 0.8)      # Random treatment assignment (50% probability). 

# Potential outcomes
Yi <- Ti * Yi_1 + (1 - Ti) * Yi_0

# Regression analysis
model <- lm(Yi ~ Ti + Xi)
summary(model)

# Histograms
hist(Yi[Ti == 1], breaks=50, col=rgb(1,0,0,0.5), main="Histogram of Treated vs. Untreated",
     xlab="Outcome", xlim=range(c(Yi[Ti == 1], Yi[Ti == 0])), ylim=c(0, 800))
hist(Yi[Ti == 0], breaks=50, col=rgb(0,0,1,0.5), add=TRUE)
legend("topright", legend=c("Treated", "Untreated"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

# Mess around with the probability of treatment assignment and how standard error changes- what kind of overlap do we see in the histograms?