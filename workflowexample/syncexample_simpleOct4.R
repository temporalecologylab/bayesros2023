## Started 4 Oct 2024 ##
## By Lizzie ##

## Based off: git/teaching/hotstats/examples/exampleday1/syncexampleday1.R ##

# Simulate test data (data to test our model) on species phenology over time
# Fit the model and check a few diagnostics

rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/Lizzie/Documents/git/teaching/bayesROS2023/workflowexample")

######################
## One real example ##
######################

# Our example is shifts in phenology over time
d <- read.csv("input/rawlong.tot.csv")

# Pick one species to look at ... 
table(d$species)


####################
## Build test data ##
####################

## Build
# y = a + bx + error
# error ~ normal(0, sigma_y) 

# ypred = a + bx 
# y ~ normal(ypred, sigma_y)

b <- -0.6
a <- 120
sigma_y <- 20

x <- seq(from=-5, to=35, by=1)
N <- length(x)

y <- a +b*x+rnorm(N, 0, sigma_y)


##################
## Prior checks ##
##################

bmu <- 0
bsigma  <- 2
hist(rnorm(10000, bmu, bsigma), cex.axis=1)

# Just vary the slope and look at possible outcomes
bpossible  <- rnorm(100, bmu, bsigma)
plot(range(x), range(y), type="n", xlab="Year", ylab="Day of year", cex.axis=1)
for (i in c(1:100)){
  y = a + bpossible[i]*x + rnorm(N, 0, sigma_y)
  points(y~x)
  abline(a=a, b=bpossible[i])
}

amu  <- 100
asigma <- 20
hist(rnorm(10000, amu, asigma), cex.axis=1)


# Now vary the slope and intercept  and look at possible outcomes
# And I made the axes bigger 
apossible  <- rnorm(100, amu, asigma)
plot(range(x), range(y), type="n", xlab="Year", ylab="Day of year", 
  xlim=c(-50, 50), ylim=c(0, 400), cex.axis=1)

for (i in c(1:100)){
  y = apossible[i] + bpossible[i]*x + rnorm(N, 0, sigma_y)
  # points(y~x)
  abline(a=apossible[i], b=bpossible[i])
}


## End of code from class ... 

## Another way to do this and a model fit to test data, see syncexample_simple.R