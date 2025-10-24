data {
	int <lower=1> n_obs; // number of observations
	int <lower=1> n_chains; // number replicate chains
	int <lower=1> n_days; // number of days
	array [n_obs] int <lower=1, upper = n_chains> chain_id; 
	array [n_obs] int <lower=1, upper = n_days> day_id; // actually day number + 1 (day 0 is day_id 1)
	array [n_obs] int <lower=1, upper=3> pos; // in which position is each observation (after the change)
	
	vector [n_obs] y_obs; // observations
}
parameters {
	real <lower=0> sig_obs; // global residual variance
  vector[3] b; // position effect
  vector[2] g; // autocorrelation strength;

  array [3] matrix [n_chains, n_days] y_latent_scaled;

  // hyperparameters for latent effect
	real <lower = 0> y_lat_sig;
	real y_lat_mu;
}
transformed parameters {
  array [3] matrix [n_chains, n_days] y_latent;
  
  // autoregressive portion of the model
  for(p in 1:3) {
    // scale the latent variable
    y_latent[p] = y_latent_scaled[p] * y_lat_sig + y_lat_mu;
    for(ch in 1:n_chains) {
      // different step size for the first day
      y_latent[p][ch,2] = b[p] + g[1] * y_latent[p][ch, 1];
      for(t in 3:n_days) {
        y_latent[p][ch,t] = b[p] + g[2] * y_latent[p][ch, t-1];
      }
    }
  }
}
model {
  for(i in 1:n_obs) {
    int ch = chain_id[i];
    int t = day_id[i];
    int p = pos[i];
    y_obs[i] ~ normal(y_latent[p][ch,t], sig_obs);
  }

  for(p in 1:3) {
    for(ch in 1:n_chains) {
      for(t in 1:n_days) {
        y_latent_scaled[p][ch,t] ~ std_normal();
      }
    }
  }

	// priors
	b ~ normal(0, 5);
	g ~ normal(0, 1);
	sig_obs ~ cauchy(0, 5);
	
	// hyperpriors
  y_lat_sig ~ cauchy(0, 5);
  y_lat_mu ~ normal(0, 5);
}
generated quantities {
 // simulated hypothetical chain with complete time series
 matrix [3, n_days] y_sim;
 for(p in 1:3) {
   for(t in 1_n_days) {
     
   }
 }
  
}