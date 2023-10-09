data {
	int <lower=1> no; // number of observations
	// int <lower=1> nm; // number of missing cells in the dataset
	int <lower=1> ncol; // number of columns
	int <lower=1> nt; // number of time steps

	vector [no] y_obs;
	vector <lower=0, upper = 1> [no] rev;
	int <lower=0, upper = no> i_prev [no];
	vector <lower=0, upper = nt> [no] dt; // the time interval (i.e., day_id) of each obs
	// int <lower=0, upper = nt> interval [no]; // the time interval (i.e., day_id) of each obs
	// int <lower=1, upper=ncol> col_id_obs [no]; // column id of the observations
	// int <lower=1, upper=nt> t_obs [no]; // time step of each observation
	// int <lower=1, upper=ncol> col_id_m [nm]; // column id of missing cells
	// int <lower=1, upper=nt> t_m [nm]; // time step of missing cells

	// int <lower=1> nchn; // number of chains
	// int <lower = 1, upper = nchn> chain_id [n];
}
// transformed data {
// 	matrix <lower=0, upper=1> [nt,ncol] reversed = rep_matrix(1, nt, ncol);
// 	reversed[1,] = rep_row_vector(0, ncol);
// }
parameters {
	// latent variable
	// vector <lower=0> [nm] y_mis;

	real <lower=0> sigma;
	real a;
	real b_rev;
	real r;
	// vector [nt-1] b_time; // effect of each time step

	// real a_mu;
	// real a_sig;
}
transformed parameters {
	vector [no] mu; // expected value for y_obs
	for(i in 1:no) {
	// 	mu[i] = a;
	// 	if(i_prev[i] != 0)
	// 		mu[i] += b_time[interval[i]] * y_obs[i_prev[i]];
	// 	mu[i] = exp(mu[i]);

		// different idea: exponential decay
		if(i_prev[i] != 0) {
			mu[i] = a + b_rev * rev[i] + y_obs[i_prev[i]] * exp(-r * dt[i]);
		} else {
			mu[i] = a + b_rev * rev[i];
		}
	}
}
model {
	y_obs ~ normal(mu, sigma);

	// hierachical priors
	// a ~ normal(a_mu, a_sig);

	// priors
	a ~ cauchy(0, 10);
	b_rev ~ cauchy(0, 5);
	// a_mu ~ cauchy(0, 10);
	// a_sig ~ cauchy(0, 10);
	sigma ~ cauchy(0, 5);
	// b_time ~ cauchy(0, 5);
	r ~ normal(0, 5);
}
