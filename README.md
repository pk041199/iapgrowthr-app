# iapgrowthr

R package scaffold for Indian pediatric anthropometry references and score calculation.

Team: Epipulse Team

Current scope:

- percentile and z-score helpers
- percentile-table scoring for `height`, `weight`, `bmi`, and `wc`
- blood pressure scoring for `sbp` and `dbp` using age, sex, and height percentile
- batch dataset scoring with output columns appended
- dropdown-based column mapping for uploaded CSV files
- Base R chart functions for IAP-style `height`, `weight`, and `BMI` plotting
- Separate readers for IAP `ht/wt`, `BMI`, `WC`, and `BP` CSV files
- Separate CDC head-circumference reference reader
- Root-level Shiny app for deployment
- One-click Windows launcher scripts for offline local use

Current source files found in the repo:

- `iap_ht__wt_percentile_ref.csv`
- `iap_bmi_pecentiles_5-18.csv`
- `iap_wc_ref.csv`
- `iap_bp_ref.csv`

Reference readers:

- `read_iap_anthro_ref()` auto-detects the supported anthropometry CSV shape
- `read_iap_hw_ref()` reads the current height/weight percentile file
- `read_iap_bmi_ref()` reads the current BMI percentile file
- `read_iap_wc_ref()` reads the current waist-circumference percentile file
- `read_iap_bp_ref()` reads the wide blood-pressure table
- `read_cdc_hc_ref()` reads CDC head-circumference data in long format

Core scoring functions:

- `score_percentile_ref()` for a single value
- `score_bp_ref()` for `sbp` and `dbp`
- `score_dataset_ref()` for a whole dataset
- `plot_iap_height_weight_chart()`
- `plot_iap_bmi_chart()`

## Offline one-click run on Windows

Files added for local offline use:

- `run_iapgrowthr.bat`
- `setup_and_run_iapgrowthr.bat`

Use them like this:

1. Install R on the machine.
2. Make sure `Rscript` is available on PATH.
3. Double-click `setup_and_run_iapgrowthr.bat` the first time.
4. After setup works, you can use `run_iapgrowthr.bat` directly.

What they do:

- `setup_and_run_iapgrowthr.bat` checks `Rscript`, installs needed packages, then starts the app.
- `run_iapgrowthr.bat` starts the local Shiny app at `http://127.0.0.1:3838`.

## Batch upload workflow

1. Upload your CSV file.
2. Select the matching source column for each field from dropdown lists.
3. Process the dataset.
4. Preview the result and download the output CSV.

Supported target fields in the mapping UI:

- `age`
- `sex`
- `height`
- `weight`
- `bmi`
- `wc`
- `sbp`
- `dbp`
- `height_percentile`

Notes:

- If `bmi` is not mapped but `height` and `weight` are mapped, BMI is calculated automatically.
- For `bp`, map either `height_percentile` directly or map `height` so height percentile can be estimated first.
- Any unmapped column is kept in the uploaded dataset and returned in the output file.

The output appends columns like:

- `height_percentile`, `height_zscore`, `height_status`
- `weight_percentile`, `weight_zscore`, `weight_status`
- `bmi_percentile`, `bmi_zscore`, `bmi_status`
- `wc_percentile`, `wc_zscore`, `wc_status`
- `sbp_percentile`, `sbp_zscore`, `sbp_status`
- `dbp_percentile`, `dbp_zscore`, `dbp_status`

## Run locally from R

From the repo root in R:

```r
install.packages(c("shiny", "rsconnect"))
shiny::runApp(".")
```

Or from terminal in the repo root:

```powershell
Rscript -e "shiny::runApp('.', host='0.0.0.1', port=3838)"
```

## Deploy as a Shiny app

### Option 1. shinyapps.io

From the repo root in R:

```r
install.packages(c("shiny", "rsconnect"))
rsconnect::setAccountInfo(name = "YOUR_ACCOUNT", token = "YOUR_TOKEN", secret = "YOUR_SECRET")
rsconnect::deployApp(appDir = ".")
```

### Option 2. Posit Connect / Shiny Server

Deploy the whole repository directory, not just a subfolder, because the app depends on:

- `app.R`
- `R/`
- the reference CSV files in the repo root

## Current repo status

- `ht/wt`, `BMI`, `WC`, and `BP` CSV files are present
- batch dataset upload is supported for `ht/wt/bmi/wc/sbp/dbp`
- CDC `hc` CSV is still not present
- this repo has not been built or tested on this machine yet because `Rscript` is not available on PATH here
