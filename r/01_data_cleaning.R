library(dplyr)
source("r/plotting_functions.R")


dat <- read.csv("data/absorbance_indices_reduced_data.csv")[, -1]
dat_DOC <- read.csv("data/DOC_final_pretreated_all.csv", sep = ";")[, -2]

# drop reservoir from the data, we will treat it separately later
dat_res <- dat |>
  filter(replicate == "Reservoir")

dat_columns <- dat |>
  filter(replicate != "Reservoir")

# Bring the DOC data to the same shape
dat_DOC <- dat_DOC |>
  mutate(sample_date = substr(Sample, 1, 3),
         replicate = substr(Sample, 5, 5),
         col_no = substr(Sample, 7,8))

# Remove everything beefore S08 since these are growth daysno
# Remove the reservoirs coded as C0 (column 0)

dat_DOC_columns <- dat_DOC |>
  filter(col_no != "C0") |>
  filter(sample_date >= "S10")

# Merge the 2 data sets if DOC will be part of the analysis
# The missing data is randomly missing (sample lost)
dat_columns <- dat_columns |>
  full_join(dat_DOC_columns, by = c("Sampling_Day", "sample_date", "replicate", "col_no"))

# Calculate the log ratios of the variables to the average of the day00 and day0

variables <- c("bix", "fi", "hix", "a254", "E2_E3", "SR", "DOC", "DN")

# The averaged before the reversal
data_before_averages <- dat_columns |>
  filter(sample_date %in% c("S10")) |>
  group_by(replicate, col_no) |>
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"))

data_day00 <- dat_columns |>
  filter(sample_date %in% c("S10"))

# Calculate the log ratios of all the variables by joining the data_before_averages and mutating over
# Remember that col_no is always after the reversal, hence equal of the "position" from our discussions.
data <- dat_columns |>
  filter(!sample_date %in% c( "S08", "S10")) |>
  group_by(replicate, col_no) |>
  ungroup() |>
  mutate(across(c(Sampling_Day, replicate, col_no), as.factor)) |>
  select(day_no = Sampling_Day, replicate, col_no, all_of(variables)) |>
  filter(replicate != "O") # we remove replicate O because sadly it is not reversed

data <- convert_column_labels(data)

# Add chainID as the concatenation of replicate and col_no. It is repeated for some but not all.
data <- data |>
  mutate(columnID = as.factor(paste0(replicate, "_",col_no))) |>
  relocate(replicate, .before = 1)

# combine with before reversal Day 00 data (coded as S08)
# Prepare data_day00 to match the structure of data
data_day00_formatted <- dat_columns |>
  filter(sample_date %in% c("S10")) |>
  select(replicate, day_no = Sampling_Day, col_no, all_of(variables) ) |>
  mutate(
    day_no = as.factor(day_no),
    replicate = as.factor(replicate),
    col_no = as.factor(col_no),
    columnID = NA_character_,  # Leave empty
    after_reversal_position = case_when(
      col_no == "C1" ~ "Column 3",
      col_no == "C3" ~ "Column 1",
      col_no == "C2" ~ "Column 2")) |>
  select(replicate, day_no, col_no, all_of(variables), columnID, after_reversal_position) |>
  convert_column_labels()

# Combine the datasets
data_combined <- bind_rows(data, data_day00_formatted)

# create the columnID to match the after reversal ID's
data_all <- data_combined |>
  mutate(columnID = case_when(
    day_no == "Day0" ~ as.factor(paste0(replicate, "_", after_reversal_position)), 
    TRUE ~ as.factor(columnID))) |>
  select(!after_reversal_position)


# parse day number into an integer column
data_all$day_number <- as.integer(sub("Day(.+)", "\\1", data_all$day_no))
saveRDS(data_all, "data/cleaned_data.rds")
