library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat = readRDS("data/cleaned_data.rds")

# drop NAs for now, imputations may follow?
dat <- dat[complete.cases(dat),]

# for now, working with DOC
# convert to a list expected by rstan
stan_data = with(dat, list(
	no = nrow(dat),
  n_chain = length(levels(replicate)),
	n_col = length(levels(columnID)),
  nt = max(day_number),
  y_obs = log_ratio_DOC
))

mod = stan_model("stan/columns.stan")
fit = sampling(mod, data = stan_data, open_progress = FALSE)



