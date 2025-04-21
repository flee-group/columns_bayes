library(rstan)
library(dplyr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat <- read.csv("data/absorbance_indices_reduced_data.csv")[, -1]

# drop reservoir from the data, we will treat it separately later
dat_res <- dat |>
  filter(replicate == "Reservoir")

dat_columns <- dat |>
  filter(replicate != "Reservoir")

# Calculate the log ratios of the variables to the average of the day00 and day0

variables <- c("bix", "fi", "hix", "a254", "E2_E3", "SR")

# The averaged before the reversal
data_before_averages <- dat_columns |>
  filter(sample_date %in% c("S08", "S10")) |>
  group_by(replicate, col_no) |>
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"))

# Calculate the log ratios of all the variables by joining the data_before_averages and mutating over
# Remember that col_no is always after the reversal, hence equal of the "position" from our discussions.
data <- dat_columns |>
  filter(!sample_date %in% c( "S08", "S10")) |>
  left_join(data_before_averages, by = c("replicate", "col_no")) |>
  group_by(replicate, col_no) |>
  mutate(across(all_of(variables),
                ~ log(.x / get(paste0("mean_", cur_column()))),
                .names = "log_ratio_{.col}")) |>
  ungroup() |>
  mutate(across(c(Sampling_Day, replicate, col_no), as.factor)) |>
  select(day_no = Sampling_Day, replicate, col_no, starts_with("log_ratio"))

# Add chainID as the concatenation of replicate and col_no. It is repeated for some but not all.
data <- data |>
  mutate(chainID = as.factor(paste0(replicate, "_",col_no))) |>
  relocate(chainID, .before = 1)





