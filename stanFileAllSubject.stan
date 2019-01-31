data {
  int<lower=0> N;
  int<lower=0> J;
  //int<lower=0> y[N];
  int<lower=0> y[N,J];
  matrix<lower=0, upper=1>[N,J] r;
  int<lower=0> total[N,J];
  int<lower=0,upper=1> samplePriors;
  real<lower=0> musd;
  real<lower=0> alphasd;
}
parameters {
  real<lower=0> mu;
  vector<lower=0>[J] alpha;
  //real<lower=0, upper=1/max(r)> alpha;
}
transformed parameters{
  real<lower=0, upper=1> theta[N,J];
   for (j in 1:J){
      for (n in 1:N){
       theta[n,j] = r[n,j] * alpha[j];
     }
   }
  
}
model {
   //mu ~ normal(1,musd);
   mu ~ gamma(musd,musd);
   for (j in 1:J){
     alpha[j] ~ normal(mu, alphasd)T[0,];
     if(!samplePriors){
      for (n in 1:N){
        //y[n,j] ~ binomial(total[n,j], r[n,j] * alpha[j]);
        y[n,j] ~ binomial(total[n,j], theta[n,j]);
      } 
     }
   }
  //y ~ binomial(total,theta);
  //y ~ binomial(total,alpha * r);
}


