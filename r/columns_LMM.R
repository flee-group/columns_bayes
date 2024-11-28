library(data.table)
library(lme4)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat = fread("data/absorbance_indices_reduced_data.csv", drop=1)

# drop reservoir from the data, we will treat it separately later
dat_res = dat[replicate == "Reservoir"]
dat = dat[replicate != "Reservoir"]

# work out a unique id for each column = column_id
# replicate is the unique id per chain (set of 3 columns)
dat[, day_no := as.integer(substr(Sampling_Day, 4, 5))]
dat[, column_id := paste0(replicate, substr(col_no, 2, 2))]
dat[day_no != 0 & col_no == "C1", column_id := paste0(replicate, 3)]
dat[day_no != 0 & col_no == "C3", column_id := paste0(replicate, 1)]
dat[, day_id := as.integer(factor(day_no))]
dat[,column_id := as.integer(factor(column_id))]

llm_bix <- lmer(bix ~ day_no * col_no + (1 | replicate) + (1|column_id), data = dat)
summary(llm_bix)


fm <-list()
# Apply experiment_lmer to columns 2 to 9 in ER_data
fm <- lapply(names(dat)[c(2:4, 8:10)], function(colname) {
  # Get the formula to be used in lmer
  formula <- as.formula(paste0(colname, " ~ sample_date * col_no + (1 | replicate) + (1|column_id)"))

  # Fit the model using lmer
  fit <- lme4::lmer(formula, data = dat)
  # warning in case of isSingular
  if (isSingular(fit)) {
    warning(paste("Singular fit detected for variable:", colname))
  }

  # Get the summary of the model
  summary <- summary(fit)
  # Returns the fit and the summary of each variable
  # Do not return fit if just checking out the summaries
  return(list(fit, summary))
})
names(fm) <- names(dat)[c(2:4, 8:10)]
