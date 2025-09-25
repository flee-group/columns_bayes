# A Bayesian Model for the Column Experiment
A Bayesian model trial for column experiment data analyzing dissolved organic matter dynamics before and after flow reversal.

## Data Description
This dataset contains dissolved organic carbon (DOC), dissolved nitrogen (DN), and optical measurements from column experiment samples with different substrate inputs, collected across multiple sampling days with treatment replicates.

Data for analyses is excluded from this repository, please contact the authors.

## Data Files
The `/data` folder contains 2 main datasets:

### 1. Absorbance and Optical Indices (`absorbance_indices_reduced_data.csv`)
Contains optical measurements from 1 day before flow reversal onwards (starting from index 92, sample "S08_A_C1"):
- **Sample format**: `<sampling_day>_<replicate>_<column>` (e.g., S08_A_C1 = Day 00, replicate A, Column 1)
- **Optical indices**: BIX (biological index), FI (fluorescence index), HIX (humification index)
- **Absorbance metrics**: a254 (decadal absorbance at 254 nm), E2_E3 ratio, SR (slope ratio)
- **Metadata**: sample_day, replicate, col_no, and assigned day labels

### 2. DOC and DN Measurements (`DOC_final_pretreated_all.csv`)
Contains dissolved organic carbon and nitrogen data:
- **Sample format**: `<sampling_day>_<replicate>_<column_number>`
- **Measurements**: DOC (mg C/L), DN (mg N/L)
- **Temporal framework**: 
  - Pre-reversal: "Day_minusX" (X days before flow reversal)
  - Day 00: Complete replicate collection
  - Day 0: Four replicates collected minutes before flow reversal
  - Post-reversal: Days numbered sequentially after flow reversal

## Analysis Structure
- `01_data_cleaning.R`: Data preprocessing and log-ratio calculations
- `02_plot.R`: Visualization of temporal patterns
- `columns_rstan.r`: Bayesian model implementation using RStan

## Script Overview

### `01_data_cleaning.R`
Data preprocessing pipeline that:
- Loads and merges optical indices and DOC/DN datasets
- Filters data to post-flow reversal period (S08 onwards)
- Calculates baseline averages from pre-reversal samples
- Transforms variables to log ratios relative to baseline values
- Creates analysis-ready dataset with normalized measurements for Bayesian modeling

### `02_plot.R`
Visualization script that:
- Creates multi-panel boxplots showing temporal patterns across all variables
- Displays log-ratio changes over time for each column treatment
- Generates publication-ready figures saved as PDF output

### `columns_rstan.r`
Bayesian modeling implementation using RStan for time series analysis of column experiment data.