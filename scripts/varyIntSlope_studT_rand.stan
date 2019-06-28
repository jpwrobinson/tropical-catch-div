data {
  int<lower=1> K; // number of fishers
  int<lower=1> J; // number of years
  int<lower=1> N; // rows of data
  int<lower=1, upper=K> fisher_id[N]; // vector of fisher indices
  int<lower=1, upper=J> yr_id[N]; // vector of yr indices
  vector[N] x1; //exp var 1
  vector[N] y; // vector to hold observations
}
parameters {
  vector[K] a_fisher; //group level deviate
  matrix[2,J] z_yr; //matrix for yr slope/intercept estimates
  real b1;
  real mu_a; //global intercept 
  real<lower=0> sigma; //SD of ind. obs
  real<lower=0> sigma_a_fisher; //SD of group intercept
  vector<lower=0>[2] phi_yr; //SD of yr slope and intercept
  cholesky_factor_corr[2] L_rho_yr; //for covariance matrix
  real<lower=2> nu;
}
transformed parameters {
  matrix[J,2] v_yr; //transformed covariance matrix
  vector[J] a_yr; //group-level deviates
  vector[J] b1_yr; //group-level slope
  matrix[2,2] rho_yr; //covariance estimate for yr slope/int
  v_yr = (diag_pre_multiply(phi_yr,L_rho_yr)*z_yr)';
  a_yr = col(v_yr,1);
  b1_yr = col(v_yr,2);
  rho_yr = L_rho_yr * L_rho_yr';
}
model {
  vector[N] B1;
  vector[N] A;
  vector[N] mu; //linear predictor

  //group-level parameters and priors
  L_rho_yr ~ lkj_corr_cholesky(2);
  phi_yr ~ student_t(5, 0, 3);
  sigma_a_fisher ~ student_t(5, 0, 3);
  sigma ~ student_t(5, 0, 3);
  nu ~ gamma(2, 0.1);
  b1 ~ normal(0, 5);
  mu_a ~ normal(0, 5);
  a_fisher ~ normal(0, sigma_a_fisher);
  to_vector(z_yr) ~ normal(0, 1);
  
  
  for (i in 1:N) {
    B1[i] = b1 + b1_yr[yr_id[i]];
    A[i] = mu_a + a_fisher[fisher_id[i]] + a_yr[yr_id[i]];
    mu[i] = A[i] + B1[i]*x1[i];
  }

  //likelihood
  y ~ student_t(nu, mu, sigma);
}
generated quantities {
  real y_rep[N];
  vector[N] B1;
  vector[N] A;
  vector[N] mu; //linear predictor as above (has to be defined in this code chunk)

  for (i in 1:N) {
    B1[i] = b1 + b1_yr[yr_id[i]];
    A[i] = mu_a + a_fisher[fisher_id[i]] + a_yr[yr_id[i]];
    mu[i] = A[i] + B1[i]*x1[i];    
    y_rep[i] = student_t_rng(nu, mu[i], sigma);
  }
}
