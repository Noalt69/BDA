 data {
 int<lower=0> N_train;
 int<lower=0> K;
 int<lower=0> L_train;
 matrix[N_train, K] X_train;
 // Number of observation in train set
 // Number of columns
 // Number of groups in train set
 // Observations in train set
 int<lower=0, upper=L_train> ll_train[N_train]; // Group level in train set
 int<lower=0, upper=1> y_train[N_train];
 // Target value in train set
 int<lower=0> N_test;
 int<lower=0> L_test;
 matrix[N_test, K] X_test;
 int<lower=0, upper=L_test> ll_test[N_test];
 }
 parameters {
 real mu_0;
 real<lower=0> sigma_0;
 real beta_0[L_train];
 real mu[K];
 real<lower=0> sigma[K];
 vector[K] beta[L_train];
 }
 model {
 // Set priors
 mu_0 ~ normal(0, 1);
 sigma_0 ~ inv_gamma(0.5, 1);
 beta_0 ~ normal(mu_0, sigma_0);
 mu ~ normal(0, 1);
 sigma ~ inv_gamma(0.5, 1);
 // Set regularization priors
 //mu[3] ~ normal(0, 0.01);
 //sigma[3] ~ inv_gamma(1, 0.01);
 // Set priors
 for(i in 1:L_train) {
 beta[i] ~ normal(mu, sigma);
 }
 // Compute likelihood
 for(i in 1:N_train)
 // Number of observation in test set
 // Number of groups in test set
 // Observations in test set
 // Group level in test set
 y_train[i] ~ bernoulli_logit(beta_0[ll_train[i]] + X_train[i] * beta[ll_train[i]]);
 }
 generated quantities {
   int<lower=0, upper=1> y_pred[N_test];
 vector[N_train] log_lik;
 // Compute predictions
 for(i in 1:N_test)
 y_pred[i] = bernoulli_logit_rng(beta_0[ll_test[i]] + X_test[i] * beta[ll_test[i]]);
 // Compute log likelihood
 for(i in 1:N_train)
 log_lik[i] = bernoulli_logit_lpmf(y_train[i]|beta_0[ll_train[i]] + X_train[i] * beta[ll_train[i]]);
 }