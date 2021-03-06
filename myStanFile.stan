data {
  int<lower=0> N; 
  int<lower=0> N_test;
  int<lower=0> J;
  int<lower=0> y[N,J];
  matrix<lower=0, upper=1>[N,J] r;
  matrix<lower=0, upper=1>[N_test,J] r_test;
  int<lower=0> total[N,J];
  int<lower=0> total_test[N_test,J];
  int<lower=0,upper=1> samplePriors;
  real<lower=0> musd;
  real<lower=0> alphasd_center;
  real<lower=0> alphasd_sigma;
}
parameters {
  real mu;
  real<lower=0> alphasd;
  vector[J] lalpha_raw;
}
transformed parameters{
  vector[J] alpha;
  vector[J] lalpha; 
  real<lower=0, upper=1> theta[N,J];
  real<lower=0, upper=1> theta_test[N_test,J];
  lalpha = mu + alphasd * lalpha_raw;
  alpha = exp(lalpha);
  
   for (j in 1:J){
      for (n in 1:N){
       theta[n,j] = inv_logit(logit(r[n,j])+lalpha[j]);
      }
      for (n in 1:N_test){
       theta_test[n,j] = inv_logit(logit(r_test[n,j])+lalpha[j]);
      }
   }
  
}
model {
   mu ~ normal(0, musd);
   alphasd ~ normal(alphasd_center, alphasd_sigma);
   for (j in 1:J){
     lalpha_raw[j] ~ normal(0, 1);
     if(!samplePriors){
      for (n in 1:N){
        y[n,j] ~ binomial(total[n,j], theta[n,j]);
      } 
     }
   }
}
generated quantities {
  int<lower=0> y_test[N_test, J];
  for (j in 1:J){
      for (n in 1:N_test){
        y_test[n,j] = binomial_rng(total_test[n,j], theta_test[n,j]);
      } 
   }
}


