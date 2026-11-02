# Create combined dataset for the Supplementary Information table.
# Three deliberate differences from the PCA:
#   - DOC and DN are reported, and a254 is converted to SUVA254 = a254/(DOC/1000).
#     The PCA drops DOC/DN after computing SUVA254; the SI table keeps them.
#   - No day reduction. The PCA keeps only Day0/Day3/Day10/Day17 (and drops
#     Day9); the SI table documents every sampling day.
#   - Before reversal is the average of BOTH pre-reversal sampling days, S08 and
#     S10. The analysis uses S10 (Day0) alone and drops S08 entirely, so those
#     rows are rebuilt from source below -- see the BEFORE REVERSAL block.

source("r/01_data_cleaning_wReservoir.R")

variables <- c("bix", "fi", "hix", "suva254", "E2_E3", "SR", "DOC", "DN")

# ---------------------------------------------------------------------------
# AFTER REVERSAL (S11-S19), straight from the analysed dataset.
# data_all_wReservoir carries day_no but not sample_date; day_sampledate_lookup
# (built in the cleaning script from the column pipeline) maps them back so the
# table can keep its S# "Sample Date" column.
# Day0 is excluded here because it is S10, which the before-reversal block below
# handles together with S08.
# ---------------------------------------------------------------------------
dat_si <- data_all_wReservoir |>
  filter(day_no != "Day0") |>
  left_join(day_sampledate_lookup, by = c("day_no" = "Sampling_Day")) |>
  mutate(suva254 = a254 / (DOC / 1000),
         col_no = recode(as.character(col_no),
                         "C0" = "Reservoir",
                         "Column 1" = "C1",
                         "Column 2" = "C2",
                         "Column 3" = "C3"))

# Columns: average across replicates within each sampling day, "mean (sd)"
dat_combined <- dat_si |>
  filter(col_no != "Reservoir") |>
  group_by(sample_date, col_no, day_no) |>
  summarise(across(all_of(variables),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop") |>
  # Format as "mean (sd)" for each variable
  transmute(
    sample_date, col_no, day_no,
    bix = paste0(round(bix_mean, 2), " (", round(bix_sd, 2), ")"),
    fi = paste0(round(fi_mean, 2), " (", round(fi_sd, 2), ")"),
    hix = paste0(round(hix_mean, 2), " (", round(hix_sd, 2), ")"),
    suva254 = paste0(round(suva254_mean, 2), " (", round(suva254_sd, 2), ")"),
    E2_E3 = paste0(round(E2_E3_mean, 2), " (", round(E2_E3_sd, 2), ")"),
    SR = paste0(round(SR_mean, 2), " (", round(SR_sd, 2), ")"),
    DOC = paste0(round(DOC_mean, 2), " (", round(DOC_sd, 2), ")"),
    DN = paste0(round(DN_mean, 2), " (", round(DN_sd, 2), ")")
  )

# Reservoir: the cleaning script already reduces it to ONE row per sampling day,
# so there is nothing to average over and no SD to report.
dat_reservoir_formatted <- dat_si |>
  filter(col_no == "Reservoir") |>
  group_by(sample_date, col_no, day_no) |>
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(across(all_of(variables), ~ as.character(round(.x, 2))))

# ---------------------------------------------------------------------------
# BEFORE REVERSAL (S08 + S10), rebuilt from source.
#
# The analysis keeps only S10 as the before-reversal state: 01_data_cleaning_
# wReservoir.R filters DOC to `sample_date >= "S10"` and drops S08/Day00 from
# day_sampledate_lookup, so neither the S08 column DOC nor the S08 reservoir row
# survives into data_all_wReservoir. The SI table reports both pre-reversal days,
# so they are re-joined here. Do NOT "fix" this to read from data_all_wReservoir.
#
# Each day is first averaged over its replicates, then the two day-means are
# averaged 50/50 so neither day dominates (S08 has 15 replicates, S10 has 4).
# Because this is the mean of two days, no SD is reported -- as for the reservoir.
# ---------------------------------------------------------------------------
before_reversal <- c("S08", "S10")

# Columns: re-join the S08 DOC that the cleaning script filters out.
dat_columns_before <- dat_wReservoir |>
  filter(replicate != "Reservoir", sample_date %in% before_reversal) |>
  full_join(dat_DOC_wReservoir |>
              filter(col_no != "C0", sample_date %in% before_reversal),
            by = c("Sampling_Day", "sample_date", "replicate", "col_no")) |>
  filter(replicate != "O") |>   # never reversed, as in the cleaning script
  mutate(suva254 = a254 / (DOC / 1000))

# Reservoir: one row per pre-reversal day (S08 is the one dropped upstream).
dat_reservoir_before <- dat_res_wReservoir |>
  filter(sample_date %in% before_reversal) |>
  select(sample_date, bix, fi, hix, a254, E2_E3, SR) |>
  left_join(dat_DOC_wReservoir |>
              filter(col_no == "C0", sample_date %in% before_reversal) |>
              distinct(sample_date, DOC, DN),
            by = "sample_date") |>
  mutate(col_no = "Reservoir", suva254 = a254 / (DOC / 1000))

# Two-stage average: within day, then across the two days.
dat_before_formatted <- bind_rows(
    dat_columns_before |> select(sample_date, col_no, all_of(variables)),
    dat_reservoir_before |> select(sample_date, col_no, all_of(variables))) |>
  group_by(sample_date, col_no) |>
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  group_by(col_no) |>
  summarise(across(all_of(variables), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(sample_date = "Before_Reversal",
         day_no = "Before_Reversal",
         across(all_of(variables), ~ as.character(round(.x, 2))))

# Combine and then rename
dat_final <- bind_rows(dat_reservoir_formatted, dat_combined, dat_before_formatted) |>
  arrange(sample_date,
          factor(col_no, levels = c("Reservoir", "C1", "C2", "C3"))) |>
  select(sample_date, col_no, day_no, all_of(variables)) |>
  setNames(c("Sample Date", "Column", "Sampling Day", "BIX", "FI",
             "HIX", "SUVA254", "E2/E3", "SR", "DOC", "DN"))

# Save to CSV
write.csv(dat_final, "output/optical_data_table.csv", row.names = FALSE)
