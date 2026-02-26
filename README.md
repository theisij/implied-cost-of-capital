# Overview
This repository generates implied cost of capital (ICC) estimates, as used in [In Search of the True Greenium](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4744608) (Eskildsen, Ibert, Jensen, and Pedersen, 2026). If you use this code or data, please cite:
```
@article{greenium2026,
  title={In Search of the True Greenium},
  author={Eskildsen, Marc and Ibert, Markus and Jensen, Theis Ingerslev and Pedersen, Lasse Heje},
  year={2026}
}
```

# Implied cost of capital - Data
The data is available here:
https://www.dropbox.com/scl/fo/j10kaoqlxe4vc1exw2efa/h?rlkey=coqucul5f6uuhrgy259v18v6n&dl=0
The folder also contains a description of the columns in the data sets.

# Implied cost of capital - Code
To generate the data yourself:
- Step 1: Download/clone the repository to your local PC
- Step 2: Set the project folder as your working directory (e.g., open `implied-cost-of-capital.Rproj` in RStudio)
- Step 3: Run `renv::restore()` to install the required R packages
- Step 4: Modify the file `main.R`, by adding WRDS login details and an API key to the St. Louis's FRED database (https://fred.stlouisfed.org/docs/api/api_key.html):
```
# User defined inputs ------------------------
# Users need to modify the three variables below to their own credentials
wrds_username <- "YOURWRDSUSENAME"
wrds_password <- "YOURWRDSPASSWORD"
fred_apikey <- "YOURFREDAPIKEY"
```
- Step 5: Run the script `main.R` to generate the data

`main.R` executes the following scripts in order:
| Script | Description |
|---|---|
| `0-icc_functions.R` | Shared functions (FF49 classification, ICC calculations) |
| `1-wrds_download.R` | Downloads data from WRDS and FRED to `WRDS-DATA/` |
| `2-icc_comp.R` | Computes ICC using Compustat prices (US + global) |
| `3-icc_us.R` | Computes ICC using CRSP prices (US only) |
| `4-analyze.R` | Summary plots of ICC estimates |

After executing these steps, the `OUTPUT/` folder should contain the following files:
- `icc_us.parquet`: ICC estimates for the US based on prices from CRSP
- `icc_comp.parquet`: ICC estimates for the US and globally, based on prices from Compustat
- `forward_eps_us.parquet`: Forward earnings-to-price ratios (CRSP)
- `forward_eps_comp.parquet`: Forward earnings-to-price ratios (Compustat)

