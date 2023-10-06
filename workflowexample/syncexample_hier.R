## Started 4 Oct 2024 ##
## By Lizzie ##

## Based off: git/teaching/hotstats/examples/exampleday1/syncexampleday1.R ##

# Simulate test data (data to test our model) on species phenology over time
# Fit the model and check a few diagnostics

rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("/Users/Lizzie/Documents/git/teaching/bayesROS2023/workflowexample")
set.seed(167)

####################
## Build test data ##
####################

# Create the species-level parameters
Nspp <- 50
mu_doy <- 125
sigma_doy <- 20
mu_shift <- 0.5
sigma_shift <- 5
species_doy <- rnorm(Nspp, mu_doy, sigma_doy)
species_trend <- rnorm(Nspp, mu_shift, sigma_shift)

# Create the overall `error'
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
  ypred[n] <- species_doy[s] + species_trend[s]*year[n]
}

y <- rnorm(N, ypred, sigma_y)

# Plot the data

# pdf("testdata.pdf", height=4, width=6)
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0), tck=-.01)
plot(range(year), range(y), type="n", xlab="Year", ylab="Day of year",
     bty="l", main="Test data")
for (j in 1:Nspp)
  lines(year[species==j], y[species==j])
# dev.off()

####################
## Model fitting ##
####################

library("rstan")
options(mc.cores = 4) # set to how many cores you want to run (for 4, you need 4 cores on your machine)


# Fit a model with a trend line (this model is the same as how we generated the data)
fit <- stan("stan/synchrony1_notype.stan", data=c("N","y","Nspp","species","year"), iter=1000, chains=4)
# Look at the results
print(fit)
library("shinystan")
launch_shinystan(fit)

# grep stan output
sumer <- summary(fit)$summary
spslopes <- sumer[grep("b\\[", rownames(sumer)), "mean"]

plot(spslopes~species_trend)
abline(0,1)

if(FALSE){ # Too slow... 
# ppc not working with my generated quantities ...
library(rstanarm)
fitarm <- stan_lmer(y~(year|species))
launch_shinystan(fitarm)
}

#################################################
## Some plots to show what mixed modeling does ##
#################################################

n_data_per_species
df <- data.frame(species=species, year=year, y=y)
par(mfrow=c(1,2))

# One species with less data
plot(y~year, data=subset(df, species==34), ylab="Day of year", pch=16)
abline(lm(y~year, data=subset(df, species==34)))

mixed34 <- sumer[grep("34", rownames(sumer)),] 

abline(a=mixed34[1,1], b=mixed34[2,1], col="dodgerblue")

# One species with more data and a trend
plot(y~year, data=subset(df, species==49), ylab="Day of year", pch=16)
abline(lm(y~year, data=subset(df, species==49)))

mixed49 <- sumer[grep("49", rownames(sumer)),] 

abline(a=mixed49[1,1], b=mixed49[2,1], col="dodgerblue")


