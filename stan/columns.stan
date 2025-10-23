data {
	int <lower=1> n_obs; // number of observations
	int <lower=1> n_col; // number of columns - note this is physical columns, not data columns
	int <lower=1> n_time; // number of time steps
	// int <lower=1> n_chain; // number of chains of columns (NOT MCMC chains!)

	vector [n_obs] y_obs; // observations
	// array [n_obs] int <lower=0, upper = n_chain> chain_id; //grouping variable for chains/replicates
	array [n_obs] int <lower=0, upper= n_col> column_id; // grouping variable for individual (non-nested) columns
	
	// prior hyperparameters
	real <lower=0> a_scale;
	real <lower=0> sig_scale;
	// real <lower=0> chain_scale;
	// real <lower=0> chain_sig_scale;
	real <lower=0> col_scale;
	real <lower=0> col_sig_scale;
}
parameters {
	real <lower=0> sigma; // global residual variance
	real a; // global intercept
	// vector[n_chain] gamm_chain_sc; // random effect for chains, scaled
	vector[n_col] gamm_col_sc; // random effect for columns, scaled

  // hyperparameters for chain random effect
	// real mu_chain;
	// real sig_chain;
	real mu_col;
	real sig_col;
}
transformed parameters {
	vector [n_obs] mu; // expected value for y_obs
	// vector[n_chain] gamm_chain; // random effect for chains
	vector[n_col] gamm_col; // random effect for columns
	
	// re-centre the random effects
	// gamm_chain = mu_chain + gamm_chain_sc * sig_chain;
	gamm_col = mu_col + gamm_col_sc * sig_col;
	
  for(i in 1:n_obs) {
    mu[i] = a + gamm_col[column_id[i]];
    // mu[i] = a + gamm_chain[chain_id[i]] + gamm_col[column_id[i]];
  }

}
model {
	y_obs ~ normal(mu, sigma);

	// hierachical priors
	// gamm_chain_sc ~ std_normal();
	gamm_col_sc ~ std_normal();

	// priors
	a ~ normal(0, a_scale);
	sigma ~ cauchy(0, sig_scale);

  // hyperpriors
  // mu_chain ~ normal(0, chain_scale);
  // sig_chain ~ cauchy(0, chain_sig_scale);
  mu_col ~ normal(0, col_scale);
  sig_col ~ cauchy(0, col_sig_scale);
}
