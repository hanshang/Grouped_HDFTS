# High-dimensional functional time series forecasting with a group structure

Replication code for

> Shang, H. L. *High-dimensional functional time series forecasting with a group structure.*
> Department of Actuarial Studies and Business Analytics, Macquarie University.

The paper forecasts Japanese age-specific mortality rates disaggregated by geography
and sex, and reconciles the forecasts so that they respect the summability constraints
of the group structure. Base forecasts are produced with univariate (UFTS), multivariate
(MFTS) and multilevel (MLFTS) functional time series methods and with a one-way
functional ANOVA plus functional factor model (FANOVA+FFM). The forecasts are then
reconciled bottom-up and by optimal combination (OLS and WLS).

---

## How to run

**Set the working directory to the `code_submission/` folder before sourcing anything.**
Every `source()` path in the repository is relative to that folder.

```r
setwd("")
```

**Every script is numbered in the order in which it must be run.** The scripts share a
single global environment, and later stages rely on objects created by earlier ones, so
run them in ascending numerical order. The whole pipeline is:

```r
source("01_load_packages.R")
source("02_read_data.R")
source("03_auxiliary_point.R")
# ... and so on through 51
```

Scripts 01–22 produce the base forecasts and Tables 2 and 4. Scripts 23–51 reconcile
those forecasts and produce Tables 3 and 5. If you only want the reconciled results for
one model, you still need 01–05 plus the base-forecast scripts for that model.

---

## Script index

### Setup (01–03)

| # | File | Description |
|---|---|---|
| 01 | `01_load_packages.R` | Attaches the four required packages: `HMDHFDplus`, `demography`, `ftsa`, `hdftsa`. |
| 02 | `02_read_data.R` | Downloads rates and exposures from the Japanese Mortality Database, writes the plain-text copies into `read_data/`, and builds the `demogdata` objects for the national, 8 regional and 47 prefectural series by sex. Needs an internet connection; skip it if you are using the committed text files. |
| 03 | `03_auxiliary_point.R` | Defines the MFTS model and the point-forecast helper routines shared by all base-forecast scripts. |

### Base forecasts — shared helpers (04–05)

| # | File | Description |
|---|---|---|
| 04 | `04_base_forecast_UFTS_fun.R` | Helper that converts UFTS forecasts into MAE and RMSE forecast errors over the expanding window. |
| 05 | `05_base_forecast_MLFTS_fun.R` | Same for the MLFTS model, wrapping `MLFTS_model()` from `hdftsa`. |

### Base forecasts — UFTS (06–09)

| # | File | Description |
|---|---|---|
| 06 | `06_base_forecast_UFTS_ets_EVR.R` | UFTS base forecasts, ETS scores, number of components chosen by the EVR criterion. |
| 07 | `07_base_forecast_UFTS_ets_K6.R` | UFTS base forecasts, ETS scores, components fixed at *O* = 6. |
| 08 | `08_base_forecast_UFTS_arima_EVR.R` | UFTS base forecasts, ARIMA scores, components by the EVR criterion. |
| 09 | `09_base_forecast_UFTS_arima_K6.R` | UFTS base forecasts, ARIMA scores, components fixed at *O* = 6. |

### Base forecasts — MFTS (10–13)

| # | File | Description |
|---|---|---|
| 10 | `10_base_forecast_MFTS_ets_EVR.R` | MFTS base forecasts, ETS scores, components by the EVR criterion. |
| 11 | `11_base_forecast_MFTS_ets_K6.R` | MFTS base forecasts, ETS scores, components fixed at *O* = 6. |
| 12 | `12_base_forecast_MFTS_arima_EVR.R` | MFTS base forecasts, ARIMA scores, components by the EVR criterion. |
| 13 | `13_base_forecast_MFTS_arima_K6.R` | MFTS base forecasts, ARIMA scores, components fixed at *O* = 6. |

### Base forecasts — MLFTS (14–17)

| # | File | Description |
|---|---|---|
| 14 | `14_base_forecast_MLFTS_EVR_ets.R` | MLFTS base forecasts, ETS scores, components by the EVR criterion. |
| 15 | `15_base_forecast_MLFTS_K6_ets.R` | MLFTS base forecasts, ETS scores, components fixed at *O* = 6. |
| 16 | `16_base_forecast_MLFTS_EVR_arima.R` | MLFTS base forecasts, ARIMA scores, components by the EVR criterion. |
| 17 | `17_base_forecast_MLFTS_K6_arima.R` | MLFTS base forecasts, ARIMA scores, components fixed at *O* = 6. |

### Base forecasts — FANOVA and summaries (18–22)

| # | File | Description |
|---|---|---|
| 18 | `18_base_forecast_OWFANOVA_ets.R` | One-way FANOVA plus functional factor model base forecasts, ETS scores. |
| 19 | `19_base_forecast_OWFANOVA_arima.R` | One-way FANOVA plus functional factor model base forecasts, ARIMA scores. |
| 20 | `20_base_forecast_TWFANOVA.R` | Two-way FANOVA base forecasts. Exploratory; results are not reported in the paper. |
| 21 | `21_summary_results_ets.R` | Collects the ETS base-forecast errors across all methods and levels into **Table 2**. |
| 22 | `22_summary_results_arima.R` | Collects the ARIMA base-forecast errors into **Table 4** (Appendix A). |

### Reconciliation setup (23–26)

| # | File | Description |
|---|---|---|
| 23 | `23_Japan_population_at_risk_forecast.R` | Forecasts the exposure-to-risk ratios that populate the summing matrix at each horizon, treating the age-0 ratio separately. |
| 24 | `24_Japan_S_mat.R` | Builds the summing matrix **S**<sub>t</sub> linking the 94 bottom-level series to all 168 series, using the forecast exposure ratios. |
| 25 | `25_UFTS_BU_optim_fun.R` | The reconciliation engine: bottom-up, OLS and WLS optimal combination, plus the MAE and RMSE errors at every level of the group structure. Used by all reconciliation scripts. |
| 26 | `26_base_forecast_MLFTS_forecasts_fun.R` | Helper that extracts fitted MLFTS forecast curves in the shape the reconciliation engine expects. |

### Reconciliation — UFTS (27–30)

| # | File | Description |
|---|---|---|
| 27 | `27_UFTS_BU_optim_ets_EVR.R` | Reconciles the UFTS (ETS, EVR) base forecasts; writes `figures/UFTS_EVR_ets_forecast_accuracy_by_level.pdf`. |
| 28 | `28_UFTS_BU_optim_ets_K6.R` | Reconciles the UFTS (ETS, *O* = 6) base forecasts. |
| 29 | `29_UFTS_BU_optim_arima_EVR.R` | Reconciles the UFTS (ARIMA, EVR) base forecasts. |
| 30 | `30_UFTS_BU_optim_arima_K6.R` | Reconciles the UFTS (ARIMA, *O* = 6) base forecasts. |

### Reconciliation — MFTS (31–38)

Each variant assembles the base forecast values first, then reconciles them.

| # | File | Description |
|---|---|---|
| 31 | `31_base_forecast_MFTS_values_ets_EVR_forecasts.R` | Assembles the MFTS (ETS, EVR) base forecast values into per-prefecture arrays. |
| 32 | `32_MFTS_BU_optim_ets_EVR.R` | Reconciles them; writes `figures/MFTS_EVR_ets_forecast_accuracy_by_level.pdf`. |
| 33 | `33_base_forecast_MFTS_values_ets_K6_forecasts.R` | Assembles the MFTS (ETS, *O* = 6) base forecast values. |
| 34 | `34_MFTS_BU_optim_ets_K6.R` | Reconciles them. |
| 35 | `35_base_forecast_MFTS_values_arima_EVR.R` | Assembles the MFTS (ARIMA, EVR) base forecast values. |
| 36 | `36_MFTS_BU_optim_arima_EVR.R` | Reconciles them. |
| 37 | `37_base_forecast_MFTS_values_arima_K6_forecasts.R` | Assembles the MFTS (ARIMA, *O* = 6) base forecast values. |
| 38 | `38_MFTS_BU_optim_arima_K6.R` | Reconciles them. |

### Reconciliation — MLFTS (39–46)

| # | File | Description |
|---|---|---|
| 39 | `39_base_forecast_MLFTS_ets_EVR.R` | Assembles the MLFTS (ETS, EVR) base forecast values. |
| 40 | `40_MLFTS_BU_optim_ets_EVR.R` | Reconciles them; writes `figures/MLFTS_EVR_ets_forecast_accuracy_by_level.pdf`. |
| 41 | `41_base_forecast_MLFTS_ets_K6.R` | Assembles the MLFTS (ETS, *O* = 6) base forecast values. Originally named `base_forecast_MLFTS_arima_K6.R`; renamed because its contents are the ETS code. |
| 42 | `42_MLFTS_BU_optim_ets_K6.R` | Reconciles them. |
| 43 | `43_base_forecast_MLFTS_arima_EVR.R` | Assembles the MLFTS (ARIMA, EVR) base forecast values. |
| 44 | `44_MLFTS_BU_optim_arima_EVR.R` | Reconciles them. |
| 45 | `45_base_forecast_MLFTS_arima_K6.R` | Assembles the MLFTS (ARIMA, *O* = 6) base forecast values. |
| 46 | `46_MLFTS_BU_optim_arima_K6.R` | Reconciles them. |

### Reconciliation — FANOVA+FFM and final summary (47–51)

| # | File | Description |
|---|---|---|
| 47 | `47_OWFANOVA_base_forecasts_ets_forecasts.R` | Assembles the FANOVA+FFM (ETS) base forecast values. |
| 48 | `48_OWFANOVA_BU_optim_ets_K6.R` | Reconciles them; writes `figures/OWFANOVA_ets_forecast_accuracy_by_level.pdf`. |
| 49 | `49_OWFANOVA_base_forecasts_arima_forecasts.R` | Assembles the FANOVA+FFM (ARIMA) base forecast values. |
| 50 | `50_OWFANOVA_BU_optim_arima_K6.R` | Reconciles them. |
| 51 | `51_summary_point_forecasts.R` | Collects the reconciled point-forecast summaries into **Table 3** (MAFE) and **Table 5** (RMSFE, Appendix C). |

---

## Other contents

All 51 scripts sit in a single flat folder; the numeric prefix carries the
ordering that the sub-folders used to. Two sub-folders are created at run time
rather than shipped:

- `read_data/` — script 02 creates it, downloads the Japanese Mortality Database
  rates and exposures into it as plain text, and reads them back. The pre-built
  text files are no longer bundled, so script 02 must be run once (it needs an
  internet connection and takes several minutes).
- `figures/` — each reconciliation script creates it if needed and writes its
  `*_forecast_accuracy_by_level.pdf` there. Twelve PDFs in total.

## Requirements

R (>= 4.0) and four packages:

| Package | Purpose |
|---|---|
| [`HMDHFDplus`](https://cran.r-project.org/package=HMDHFDplus) | download Japanese Mortality Database files |
| [`demography`](https://cran.r-project.org/package=demography) | `demogdata` objects, `smooth.demogdata` |
| [`ftsa`](https://cran.r-project.org/package=ftsa) | functional time series forecasting |
| [`hdftsa`](https://cran.r-project.org/package=hdftsa) | `MLFTS_model`, high-dimensional FTS routines |

```r
install.packages(c("HMDHFDplus", "demography", "ftsa", "hdftsa"))
```

<!-- TODO: paste the output of sessionInfo() from the machine used for the
     reported results, so the exact package versions are on record. -->

## Data

Mortality rates and exposures come from the
[Japanese Mortality Database](https://www.ipss.go.jp/p-toukei/JMD/index-en.html)
(National Institute of Population and Social Security Research), covering 1975–2024 for
ages 0–100 (final group 100+). The structure has 168 series across three levels:

| Level | Series | Count |
|---|---|---|
| 0 | Japan; Japan × sex | 3 |
| 1 | Region; Region × sex | 24 |
| 2 | Prefecture; Prefecture × sex | 141 |
| | **Total** | **168** |

The text files in `read_data/` are a convenience snapshot so the analysis runs without a
download. Please cite the Japanese Mortality Database as the source and check its terms
of use before redistributing them further.

## Relationship to the original submission

This is a tidied version of the original `code_submission` folder. The executable
content of all 51 scripts is unchanged; the edits were confined to:

- numbering every script by execution order, and updating all `source()` calls and
  header comments to match;
- flattening the `base_forecasts/` and `gfts/` sub-folders so that all 51 scripts
  live in one directory, and dropping the pre-built `read_data/` text files, which
  script 02 regenerates;
- renaming `gfts/MLFTS/ets_K6/base_forecast_MLFTS_arima_K6.R` to `..._ets_K6.R`, since
  its contents are the ETS code (see script 41);
- removing 57 hard-coded `setwd()` calls containing absolute Dropbox and
  username-specific paths, and rewriting the `source()` calls they supported to paths
  relative to `code_submission/`;
- adding a header comment to each script recording its purpose, prerequisites and
  outputs;
- normalising whitespace: tabs to four spaces, trailing whitespace removed, runs of
  blank lines collapsed, a single terminating newline;
- sending generated PDFs to `figures/` rather than the working directory;
- `require()` to `library()` in script 01, so a missing package stops the run rather
  than warning;
- deleting macOS and RStudio artefacts (`.DS_Store`, `.Rhistory`, `.Rapp.history`,
  `__MACOSX/`).

Assignment operators, brace style, variable names, numeric constants and all
algorithmic content were deliberately left untouched. Every file was verified to parse
under R 4.3.3, and the executable content was diffed line by line against the original
to confirm the transformations above are the only differences.

## Citation

See `CITATION.cff`.

## License

Code is released under the MIT License (see `LICENSE`). The data files in `read_data/`
are derived from the Japanese Mortality Database and are subject to its terms, not the
MIT License.
