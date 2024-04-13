# Implied cost of capital - Data
Code to generate the implied cost of capital measures used in Eskildsen, Ibert, Jensen, and Pedersen (2024). The data is available here:
https://www.dropbox.com/scl/fo/j10kaoqlxe4vc1exw2efa/h?rlkey=coqucul5f6uuhrgy259v18v6n&dl=0 
The folder also contains a description of the columns in the data sets.

# Implied cost of capital - Code
To generate the data yourself:
- Step 1: Download files to your local PC
- Step 2: Open `implied-cost-of-capital.Rproj` in R
- Step 3: Modify the file `main.R`, by adding WRDS login details and an API key to the St. Louis's FRED database (https://fred.stlouisfed.org/docs/api/api_key.html):
```
# User defined inputs ------------------------
# Users need to modify the three variables below to their own credentials
wrds_username <- "YOURWRDSUSENAME"
wrds_password <- "YOURWRDSPASSWORD"
fread_apikey <- "YOURFREDAPIKEY"
```
- Step 4: Run the script `main.R` to generate the data

After executing these four steps, your project folder should contain the following files:
- `icc_us.csv`: ICC's in the US based on prices from CRSP
- `icc_comp.csv`: ICC's in the US and globally, based on prices from Compustat

