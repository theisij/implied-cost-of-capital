# User defined inputs ------------------------
# Users need to modify the three variables below to their own credentials
wrds_username <- "YOURWRDSUSENAME"
wrds_password <- "YOURWRDSPASSWORD"
fread_apikey <- "YOURFREDAPIKEY"

# Libraries ---------------------------------
library(lubridate)
library(tidyverse)
library(data.table)
library(RPostgres)

# Run code chunks ----------------------------
source("icc_functions.R")
source("wrds_download.R")
source("icc_comp.R")
source("icc_us.R")
