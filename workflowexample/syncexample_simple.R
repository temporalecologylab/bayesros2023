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

d <- read.csv("input/rawlong.tot.csv")
# Pick one species ...
table(d$species)


####################
## Build test data ##
####################

# Create the modl parameters
Nspp <- 1
intercept <- 125 # a
shift <- 0.5 # b
sigma_y <- 5

# Create the data
year_0 <- 1980
n_data_per_species <- round(runif(Nspp, 5, 40))
species <- rep(1:Nspp, n_data_per_species)
N <- length(species)
year <- rep(NA, N)

for (j in 1:Nspp){
  year[species==j] <- rev(2009 - 1:(n_data_per_species[j])) - year_0
}

ypred <- length(N)

for (n in 1:N){
  s <- species[n]
  ypred[n] <- intercept + shift*year[n]
}

y <- rnorm(N, ypred, sigma_y)

# Plot the data

par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Test data")
for (j in 1:Nspp)
  lines(year[species==j], y[species==j])


####################### 
## Start prior check ##
#######################

# Let's do b, we think it's normally distributed so we need to define mean (mu) and variation (sigma)
mub <- -2 # mean (mu), I start with what seemed okay above
sigmab <- 0.5 # variation

# Step 1 -- always plot the basic histogram first!
hist(rnorm(10000, mub, sigmab))

# Seems okay, on to Step 2...

ppcreps <- 200 # up to you, more is better generally
bprior <- rnorm(ppcreps, mub, sigmab)

plot(1, type="n", ylim=c(0, 365), xlim=c(5, 40), # zoom in on relevant temperatures
     xlab="year", ylab="day of event")
for (i in c(1:ppcreps)){
    abline(a=intercept, b=bprior[i])
    }


####################
## Model fitting ##
####################

library("rstan")
options(mc.cores = 4) # set to how many cores you want to run (for 4, you need 4 cores on your machine)


# Fit a model with a trend line (this model is the same as how we generated the data)
fit <- stan("stan/synchrony1_notype_onespecies.stan", data=c("N","y","year"), iter=2000, chains=4) # make sure the names in your Stan data block match the data part here
# Look at the results
print(fit)
library("shinystan")
launch_shinystan(fit)


####################
## Start PPC ##
####################

# grep stan output
sumer <- summary(fit)$summary
bpred <- sumer[grep("b", rownames(sumer)), "mean"]
# ... 




