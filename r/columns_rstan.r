library(data.table)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat = fread("data/absorbance_indices_reduced_data.csv")
dat[[1]] = NULL # row numbers

# drop reservoir from the data, we will treat it separately later
dat_res = dat[replicate == "Reservoir"]
dat = dat[replicate != "Reservoir"]


# temporary, work with a single replicate
dat = dat[replicate == "A"]


# work out a unique id for each column
dat[, day_no := as.integer(substr(Sampling_Day, 4, 5))]
dat[, column_id := paste0(replicate, substr(col_no, 2, 2))]
dat[day_no != 0 & col_no == "C1", column_id := paste0(replicate, 3)]
dat[day_no != 0 & col_no == "C3", column_id := paste0(replicate, 1)]
dat[, day_id := as.integer(factor(day_no))]
dat[,column_id := as.integer(factor(column_id))]

# figure out the index of the row for the time step before each observation
# uses witchcraft
dat$prev_id = sapply(paste0(dat$column_id, dat$day_id - 1), 
	match, paste0(dat$column_id, dat$day_id))
dat[is.na(prev_id), prev_id := 0]

dat$increment = 0
dat[prev_id != 0]$increment = dat[prev_id != 0]$day_no - dat[dat[prev_id != 0]$prev_id]$day_no


# we need to figure out what cells are missing
missing = data.table(expand.grid(day_id = unique(dat$day_id), col_id = unique(dat$column_id)))
missing = merge(missing, dat[,.(sample_date, day_id, column_id)], by.x = c("day_id", "col_id"), 
	by.y = c("day_id", "column_id"), all.x = TRUE)
missing = missing[is.na(sample_date), .(day_id, col_id)]



stan_data = with(dat, list(
	no = nrow(dat),
	# nm = nrow(missing),
	ncol = max(column_id),
	nt = max(day_id),

	y_obs = a254,
	i_prev = dat$prev_id,
	# col_id_obs = column_id,
	# interval = day_id - 1
	dt = increment,
	rev = as.integer(day_no != 0)
	# col_id_m = missing$col_id,
	# t_m = missing$day_id

	# position = as.integer(substr(col_no, 2, 2)),
	# chain_id = as.integer(factor(replicate)),	
))

mod = stan_model("stan/columns.stan")

fit = sampling(mod, data = stan_data, open_progress = FALSE)

## ugh, this is hard. it's impossible to know why things are not working well
## I think simplest/most flexible is to model additive increments
## so E(y[i]) = a + b_t[i-1] * y[i-1] + ...
## with no covars, there is no EV for y[0], so it also should get no parameters/likelihood
## probably should do this first with a simulation; if this works can add complexity
## if that works then maybe it goes with real data

