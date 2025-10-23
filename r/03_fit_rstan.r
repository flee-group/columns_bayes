library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat = readRDS("data/cleaned_data.rds")

# drop NAs for now, imputations may follow?
dat <- dat[complete.cases(dat),]

# for now, working with DOC
# convert to a list expected by rstan
stan_data = with(dat, list(
	n_obs = nrow(dat),
  n_chain = length(levels(replicate)),
	n_col = length(levels(columnID)),
  n_time = max(day_number),
  y_obs = log_ratio_DOC,
	chain_id = as.integer(replicate),
	column_id = as.integer(columnID),
	
	# priors here
	a_scale = 5,
	sig_scale =2.5,
	chain_scale = 1,
	chain_sig_scale = 1,
	col_scale = 1,
	col_sig_scale = 1
))

mod = stan_model("stan/columns.stan")
fit = sampling(mod, data = stan_data, open_progress = FALSE, control = list(max_treedepth = 14))



