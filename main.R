# User defined inputs ------------------------
# Users need to modify the three variables below to their own credentials
wrds_username <- "YOURWRDSUSENAME"
wrds_password <- "YOURWRDSPASSWORD"
fred_apikey <- "YOURFREDAPIKEY"

# Libraries ---------------------------------
library(lubridate)
library(tidyverse)
library(data.table)
library(RPostgres)
library(arrow)

# Run code chunks ----------------------------
ECHO <- TRUE
source("0-icc_functions.R", echo = ECHO)
source("1-wrds_download.R", echo = ECHO)
source("2-icc_comp.R", echo = ECHO)
source("3-icc_us.R", echo = ECHO)
source("4-analyze.R", echo = ECHO)
