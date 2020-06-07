// https://discourse.mc-stan.org/t/bayesian-structural-time-series-modeling/2256/2

data {
  int <lower=0> T;
  vector[T] y;
}
parameters {
  vector[T] mu_err;
  vector[T] delta_err;
  real <lower=0> s_obs;
  real <lower=0> s_slope;
  real <lower=0> s_level;
}
transformed parameters {
  vector[T] mu;
  vector[T] delta;
  mu[1] = mu_err[1];
  delta[1] = delta_err[1];
  for (t in 2:T) {
    mu[t] = mu[t-1] + delta[t-1] + s_level * mu_err[t];
    delta[t] = delta[t-1] + s_slope * delta_err[t];
  }
}
model {
  s_obs ~ normal(5,10);
  s_slope ~ normal(0,1);
  s_level ~ normal(0,1);
  
  mu_err ~ normal(0,1);
  delta_err ~ normal(0,1);
  
  y ~ normal(mu, s_obs);
}
