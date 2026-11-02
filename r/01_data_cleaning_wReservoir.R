# Data cleaning that KEEPS the reservoir as an extra state ("C0").
# Builds the column track and then adds a single reservoir observation per day,
# so that trajectories can read C0 -> C1 -> C2 -> C3.

library(dplyr)
source("r/plotting_functions.R")


dat_wReservoir <- read.csv("data/absorbance_indices_reduced_data.csv")[, -1]
dat_DOC_wReservoir <- read.csv("data/DOC_final_pretreated_all.csv", sep = ";")[, -2]

# Split reservoir vs columns. dat_res_wReservoir is kept and re-introduced as
# the "C0" state further down.
dat_res_wReservoir <- dat_wReservoir |>
  filter(replicate == "Reservoir")

dat_columns_wReservoir <- dat_wReservoir |>
  filter(replicate != "Reservoir")

# Bring the DOC data to the same shape
dat_DOC_wReservoir <- dat_DOC_wReservoir |>
  mutate(sample_date = substr(Sample, 1, 3),
         replicate = substr(Sample, 5, 5),
         col_no = substr(Sample, 7,8))

# Keep S10 onward: S10 (Day0) is the before-reversal state the analysis uses, and
# everything before it is either growth days or the discarded S08 sampling.
# Remove the reservoirs coded as C0 (column 0).
dat_DOC_columns <- dat_DOC_wReservoir |>
  filter(col_no != "C0") |>
  filter(sample_date >= "S10")

# Merge the 2 data sets if DOC will be part of the analysis
# The missing data is randomly missing (sample lost)
dat_columns_wReservoir <- dat_columns_wReservoir |>
  full_join(dat_DOC_columns,
            by = c("Sampling_Day", "sample_date", "replicate", "col_no"))

variables <- c("bix", "fi", "hix", "a254", "E2_E3", "SR", "DOC", "DN")

# The after-reversal sampling days (S11 onward).
# col_no here is the column's position AFTER the reversal -- the "position" from
# our discussions. On the before-reversal day (S10 / Day0, handled below) col_no
# is instead the position the column occupied BEFORE the reversal. In both cases
# it is simply the position at the time of sampling.
#
# NOTE: this script keeps only S10 as the before-reversal state. S08 (Day00) is
# dropped here but is still read straight from the raw objects by r/05_SI_table.R,
# which reports the S08+S10 two-day average.
data_wReservoir <- dat_columns_wReservoir |>
  filter(!sample_date %in% c( "S08", "S10")) |>
  group_by(replicate, col_no) |>
  ungroup() |>
  mutate(across(c(Sampling_Day, replicate, col_no), as.factor)) |>
  select(day_no = Sampling_Day, replicate, col_no, all_of(variables)) |>
  filter(replicate != "O") # we remove replicate O because sadly it is not reversed

data_wReservoir <- convert_column_labels(data_wReservoir)

data_wReservoir <- data_wReservoir |>
  relocate(replicate, .before = 1)

# Combine with the before-reversal (S10 / Day0) data.
# Prepare it to match the structure of data_wReservoir.
data_day00_formatted_wReservoir <- dat_columns_wReservoir |>
  filter(sample_date %in% c("S10")) |>
  select(replicate, day_no = Sampling_Day, col_no, all_of(variables) ) |>
  mutate(
    day_no = as.factor(day_no),
    replicate = as.factor(replicate),
    col_no = as.factor(col_no)) |>
  convert_column_labels()

# Combine the datasets
data_all_wReservoir <- bind_rows(data_wReservoir, data_day00_formatted_wReservoir)


# ============================================================================
# RESERVOIR TRACK ("C0")
# ----------------------------------------------------------------------------
# The reservoir is the source water every column is fed from, so it is the
# natural starting state. It is repeated measures (in the DOC file the same C0
# value is duplicated once per replicate), so we keep exactly ONE reservoir
# observation per day to avoid distorting the PCA weights.
#
# The reservoir is matched to a day by the SAMPLING EVENT (sample_date / S#) the
# columns of that day use -- NOT by the reservoir's own Sampling_Day label, which
# is offset (e.g. the S10 reservoir is labelled "Day1" but S10 is column Day0).
# This is the absolute Day# <-> S# mapping already encoded in the column pipeline.
# ============================================================================

# Day# <-> S# lookup, taken straight from the column pipeline (no invented days).
# Restricted to the days that actually survive into the column data_all (S08 /
# Day00, the before-reversal experiment start, is dropped there and so here too).
valid_days_wReservoir <- unique(as.character(data_all_wReservoir$day_no))
day_sampledate_lookup <- dat_columns_wReservoir |>
  distinct(sample_date, Sampling_Day) |>
  filter(!is.na(Sampling_Day), Sampling_Day %in% valid_days_wReservoir)

# Reservoir DOC: the C0 rows. Identical duplicates per date -> one value per S#.
res_doc_wReservoir <- dat_DOC_wReservoir |>
  filter(col_no == "C0") |>
  distinct(sample_date, DOC, DN)

# Reservoir optical, matched to the column day via sample_date.
res_optical_wReservoir <- dat_res_wReservoir |>
  select(sample_date, bix, fi, hix, a254, E2_E3, SR) |>
  left_join(day_sampledate_lookup, by = "sample_date")

# Warn for any reservoir rows whose sampling event has no column day match
# (e.g. S08 / before the reversal): these are dropped, not invented into a day.
res_unmatched_wReservoir <- res_optical_wReservoir |>
  filter(is.na(Sampling_Day))
if (nrow(res_unmatched_wReservoir) > 0) {
  warning("Dropping reservoir rows with no matching column day (sample_date): ",
          paste(sort(unique(res_unmatched_wReservoir$sample_date)), collapse = ", "))
}

# Keep only the days that exist in the column data, average duplicate reservoir
# optical rows within a single sampling event (e.g. S15 has two), and attach DOC.
reservoir_rows_wReservoir <- res_optical_wReservoir |>
  filter(!is.na(Sampling_Day)) |>
  left_join(res_doc_wReservoir, by = "sample_date") |>
  group_by(day_no = Sampling_Day, sample_date) |>
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# Every day that has an a254 measurement must also have a DOC (needed for
# suva254 = a254 / (DOC/1000) downstream); warn otherwise.
res_missing_doc_wReservoir <- reservoir_rows_wReservoir |>
  filter(!is.na(a254) & is.nan(a254) == FALSE & (is.na(DOC) | is.nan(DOC)))
if (nrow(res_missing_doc_wReservoir) > 0) {
  warning("Reservoir day(s) with a254 but no DOC: ",
          paste(res_missing_doc_wReservoir$day_no, collapse = ", "))
}

# Shape the reservoir rows like data_all_wReservoir: replicate is a placeholder
# (it is duplicated per real replicate later, in the trajectory step), col_no = "C0".
reservoir_rows_wReservoir <- reservoir_rows_wReservoir |>
  mutate(replicate = "Reservoir",
         col_no = factor("C0",
                         levels = c("C0", "Column 1", "Column 2", "Column 3"))) |>
  select(replicate, day_no, col_no, all_of(variables))

# Bind the reservoir onto the column data.
data_all_wReservoir <- bind_rows(data_all_wReservoir, reservoir_rows_wReservoir)

# Keep col_no ordered C0 < C1 < C2 < C3 after the bind.
data_all_wReservoir$col_no <- factor(
  data_all_wReservoir$col_no,
  levels = c("C0", "Column 1", "Column 2", "Column 3"))


# parse day number into an integer column
data_all_wReservoir$day_number <-
  as.integer(sub("Day(.+)", "\\1", data_all_wReservoir$day_no))
saveRDS(data_all_wReservoir, "data/cleaned_data_wReservoir.rds")
