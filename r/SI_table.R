# Create combined dataset for the Supplementary Information table

source("R/01_data_cleaning.R")

#clean reservoir data
doc_res <- dat_DOC |>
  filter(col_no == "C0" & sample_date >= "S08") |>
  select(-c(replicate, col_no, Sample)) |>
  unique()

# The final dataset that has all the reservoir data joined
reservoir_combined <- doc_res |>
  left_join(dat_res, by = c("sample_date"))


dat_combined <- dat_columns |>
  # Replace S08 and S10 with "Before_Reversal" for both sample_date and Sampling_Day
  mutate(sample_date = if_else(sample_date %in% c("S08", "S10"), 
                               "Before_Reversal", 
                               sample_date),
         Sampling_Day = if_else(sample_date == "Before_Reversal",
                                "Before_Reversal",
                                Sampling_Day)) |>
  # Group by sample_date, col_no, and Sampling_Day, then average across replicates
  group_by(sample_date, col_no, Sampling_Day) |>
  summarise(across(all_of(variables), 
                   ~ mean(.x, na.rm = TRUE), 
                   .names = "mean {.col}"),
            .groups = "drop")


dat_combined <- dat_columns |>
  # Replace S08 and S10 with "Before_Reversal"
  mutate(sample_date = if_else(sample_date %in% c("S08", "S10"), 
                               "Before_Reversal", 
                               sample_date),
         Sampling_Day = if_else(sample_date == "Before_Reversal",
                                "Before_Reversal",
                                Sampling_Day)) |>
  # Group and calculate both mean and SD
  group_by(sample_date, col_no, Sampling_Day) |>
  summarise(across(all_of(variables), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop") |>
  # Format as "mean (sd)" for each variable
  mutate(
    bix = paste0(round(bix_mean, 2), " (", round(bix_sd, 2), ")"),
    fi = paste0(round(fi_mean, 2), " (", round(fi_sd, 2), ")"),
    hix = paste0(round(hix_mean, 2), " (", round(hix_sd, 2), ")"),
    a254 = paste0(round(a254_mean, 2), " (", round(a254_sd, 2), ")"),
    E2_E3 = paste0(round(E2_E3_mean, 2), " (", round(E2_E3_sd, 2), ")"),
    SR = paste0(round(SR_mean, 2), " (", round(SR_sd, 2), ")"),
    DOC = paste0(round(DOC_mean, 2), " (", round(DOC_sd, 2), ")"),
    DN = paste0(round(DN_mean, 2), " (", round(DN_sd, 2), ")")
  ) |>
  # Select only the formatted columns
  select(sample_date, col_no, Sampling_Day, all_of(variables)) 

# Process reservoir data (average S08 and S10, format others)
dat_reservoir_formatted <- reservoir_combined |>
  # Use Sampling_Day.y (which appears to be correct) and rename
  select(sample_date, col_no, Sampling_Day = Sampling_Day.y, all_of(variables)) |>
  mutate(sample_date = if_else(sample_date %in% c("S08", "S10"), 
                               "Before_Reversal", 
                               sample_date),
         Sampling_Day = if_else(sample_date == "Before_Reversal",
                                "Before_Reversal",
                                Sampling_Day)) |>
  group_by(sample_date, col_no, Sampling_Day) |>
  summarise(across(all_of(variables), 
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(across(all_of(variables), 
                ~ as.character(round(.x, 2))))

# Combine and then rename
dat_final <- bind_rows(dat_reservoir_formatted, dat_combined) |>
  arrange(sample_date, 
          factor(col_no, levels = c("Reservoir", "C1", "C2", "C3"))) |>
  setNames(c("Sample Date", "Column", "Sampling Day", "BIX", "FI", 
             "HIX", "A254", "E2/E3", "SR", "DOC", "DN"))

# Save to CSV
write.csv(dat_final, "output/optical_data_table.csv", row.names = FALSE)