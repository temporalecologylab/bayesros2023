// Synchrony model with trend, no partial pooling

data {
  int N;                                // # data points
  vector[N] y;                          // DOY of pheno event
  vector[N] year;                       // year of data point
}

parameters {
  real a;                          // the intercept for each species
  real b;                          // the slope for each species
  real<lower=0> sigma_y;                // measurement error, noise, etc.

}

model {
real ypred[N];
    for (i in 1:N){
    ypred[i] = a + b * year[i];
  }
  y ~ normal(ypred, sigma_y);
   // Priors ...
} 
