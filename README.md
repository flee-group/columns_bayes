# Column Experiment: Dissolved Organic Matter Dynamics

Analysis of dissolved organic matter dynamics before and after flow reversal in a
column experiment, using PCA and trajectory analysis of optical indices with the
feed reservoir included as the source state.

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
- **Reservoir rows** are marked `replicate == "Reservoir"` and are split out as the `C0` state

### 2. DOC and DN Measurements (`DOC_final_pretreated_all.csv`)
Contains dissolved organic carbon and nitrogen data:
- **Sample format**: `<sampling_day>_<replicate>_<column_number>`
- **Measurements**: DOC and DN in µg/L (note: SUVA254 divides by `DOC/1000` to convert to mg/L)
- **Reservoir rows** are encoded as column `C0`, duplicated once per replicate
- **Temporal framework**:
  - Pre-reversal: "Day_minusX" (X days before flow reversal)
  - Day 00 (S08): Complete replicate collection
  - Day 0 (S10): Four replicates collected minutes before flow reversal
  - Post-reversal: Days numbered sequentially after flow reversal

## Analysis Structure

Scripts are numbered in run order and live in `/r`:

| Script | Purpose | Writes |
|---|---|---|
| `01_data_cleaning_wReservoir.R` | Merge optical + DOC data, build the column track, add one reservoir (`C0`) observation per day | `data/cleaned_data_wReservoir.rds` |
| `02_plot.R` | Multi-panel boxplots of each variable over time, by column position | `output/plots/DOC_and_optical.pdf` |
| `03_pca_wReservoir.R` | PCA on the optical indices with the reservoir as a single source point; arrow and loadings plots | `output/plots/PCA_wReservoir_active_arrows.pdf` |
| `04_trajectory_analysis_wReservoir.R` | 4-state trajectory analysis (C0 → C1 → C2 → C3): lengths, angles, directionality | `output/plots/trajectory_wReservoir_*.pdf` |
| `05_SI_table.R` | Supplementary table of all optical/DOC values per sampling day and column | `output/optical_data_table.csv` |

`r/plotting_functions.R` holds shared ggplot helpers and `convert_column_labels()`.

### Run order matters

`01` and `05` are self-contained (`05` sources `01`). But **`02` and `04` depend on
objects left in the global environment by earlier scripts** — `04` in particular needs
`scores_active` and `figs_active` from `03`. Run them in numeric order in a single
session.

## Analysis Notes

- **Before reversal.** The analysis pipeline treats **S10 (Day0) alone** as the
  before-reversal state; S08 is dropped. The SI table (`05`) instead reports the
  **average of S08 and S10**, re-reading S08 directly from the raw files, with each
  day averaged over its replicates and the two day-means then averaged 50/50.
- **Replicate `O`** is excluded throughout — it was never reversed.
- **SUVA254** is computed as `a254 / (DOC/1000)`, since DOC is stored in µg/L.
- **The reservoir** is reduced to one observation per sampling day, so it does not
  outweigh the columns in the PCA. Duplicate reservoir rows within a sampling event
  (e.g. S15) are averaged.
