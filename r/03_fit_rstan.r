library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat = readRDS("data/cleaned_data.rds")


# remove rows with NAs for now, imputations to follow?
dat <- dat[complete.cases(dat),]

# for now, working with bix
# convert to a list expected by rstan
stan_data = with(dat, list(
	n_obs = nrow(dat),
  n_chains = length(levels(replicate)),
	n_days = 18,
	chain_id = as.integer(replicate),
	day_id = day_number + 1,
	pos = as.integer(substr(col_no, 8,8)),
  y_obs = bix
))

mod = stan_model("stan/columns.stan")
fit = sampling(mod, data = stan_data, open_progress = FALSE, iter = 10000,
               control = list(adapt_delta = 0.99, max_treedepth = 14))
saveRDS(fit, "results/car_latent_fit.rds")



samps = as.matrix(fit, pars = "y_latent")
plot_data = data.table::rbindlist(lapply(1:3, \(p) {
  res = sapply(1:18, \(t) {
    cols = grep(paste0("y_latent\\[", p, ",.+,",t,'\\]'), colnames(samps))
    quantile(samps[,cols], c(0.5, 0.05, 0.95))
  })
  res = data.frame(t(res))
  colnames(res) = c("median", "lower", "upper")
  res$day = 0:17
  res
}), idcol = "position")
plot_data$position = factor(plot_data$position)

raw_data_plot = data.frame(day = stan_data$day_id - 1, y = stan_data$y_obs, position = factor(stan_data$pos))

library(ggplot2)
ggplot() + # geom_point(data = raw_data_plot, aes(x = day, y = y, col = position), size = 0.1) + 
  geom_line(data = plot_data, aes(x = day, y = median, col = position)) + 
  geom_point(data = plot_data, aes(x = day, y = median, col = position)) + 
  geom_errorbar(data = plot_data, aes(x = day, ymin = lower, ymax = upper, col = position), width = 0.2) +
  ylab("E(bix)")




