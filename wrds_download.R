# Establish connection ----------------------
# Connect to WRDS
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user=wrds_username,              # <-- add your WRDS username here
                  password=wrds_password)          # <-- add your WRDS password here
# Function to fetch data 
wrds_fetch <- function(wrds, sql_string, n=-1){
  res <- dbSendQuery(wrds, sql_string)
  data <- dbFetch(res, n=n)
  dbClearResult(res)
  data |> setDT()
  return(data)
}

# Download data for Global ICC ---------------------------
if (T) {
  # IBES-Compustat linking - Global
  sql_string <- "
    SELECT  gvkey, iid, epf, excntry, ibtic  
    FROM comp.g_security;
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/comp_g_security.parquet")
  # IBES-Compustat linking - NA
  sql_string <- "
    SELECT  gvkey, iid, epf, excntry, ibtic  
    FROM comp.security;
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/comp_security.parquet")
  # EPS forecasts
  sql_string <- "
    SELECT ticker, cusip, fpi, statpers, fpedats, curcode, numest, medest, measure, estflag 
    FROM ibes.statsumu_epsint
    WHERE fpi IN ('0', '1', '2');
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/statsumu_epsint.parquet")
  # Dividend forecasts
  sql_string <- "
    SELECT ticker, statpers, curcode, numest, medest 
    FROM ibes.statsumu_xepsint
    WHERE measure='DPS' and fiscalp='ANN' and fpi='1';
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/ibes_statsumu_xepsint_dps.parquet")
  # Actual earnings 
  sql_string <- "
    SELECT ticker, anndats, pends, pdicity, curr_act, measure, value 
    FROM ibes.actu_epsint;
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/ibes_actu_epsint.parquet")
  # Realized book per share 
  sql_string <- "
    SELECT ticker, anndats, pends, curr_act, value 
    FROM ibes.actu_xepsint
    WHERE pdicity='ANN' AND measure='BPS' AND value IS NOT NULL;
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/ibes_actu_xepsint_bps.parquet") # bps_g[, .N, by = year(pends)][order(year)] # NOT AVAILABLE UNTIL 1996...
  # Global prices daily
  sql_string <- "
    SELECT a.gvkey, a.iid, a.datadate, a.curcdd, a.ajexdi, a.prccd, a.monthend, b.ibtic
    FROM comp.g_secd as a
    LEFT JOIN comp.g_security as b
    ON a.gvkey=b.gvkey AND a.iid=b.iid
    WHERE b.tpci='0' and b.ibtic<>'' and a.ajexdi IS NOT NULL and a.prccd IS NOT NULL and a.curcdd<>'';
  " 
  system.time(wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/comp_g_secd.parquet"))
  # NA prices daily
  sql_string <- "
    SELECT a.gvkey, a.iid, a.datadate, a.curcdd, a.ajexdi, a.prccd, b.ibtic
    FROM comp.secd as a
    LEFT JOIN comp.security as b
    ON a.gvkey=b.gvkey AND a.iid=b.iid
    WHERE b.tpci='0' and b.ibtic<>'' and a.ajexdi IS NOT NULL and a.prccd IS NOT NULL and a.curcdd<>'';
  " 
  system.time(wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/comp_secd.parquet"))  # 16min
  # Global FUNDA
  sql_string <- "
    SELECT gvkey, datadate, ajexi, sich, curcd, at, dvt, seq, ib, cshpria, epsexcon, iid 
    FROM comp.g_funda
    WHERE indfmt in ('INDL', 'FS') and datafmt='HIST_STD' and popsrc='I' and consol='C';
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/comp_g_funda.parquet")
}

# Download data for US ICC -------------------------------
if (T) {
  # CRSP-COMP linking 
  sql_string <- "
    SELECT lpermno as permno, gvkey, liid as iid, linkdt as start, linkenddt as end, linkprim
    FROM crsp.ccmxpf_lnkhist
    WHERE linktype in ('LC', 'LU', 'LS');
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/crsp_comp_link.parquet")
  # IBES-CRSP linking 
  sql_string <- "
    SELECT ticker as ibtic, permno, sdate, edate 
    FROM wrdsapps.ibcrsphist
    WHERE permno IS NOT NULL and score=1;
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/crsp_ibes_link.parquet")
  # EPS forecasts
  sql_string <- "
    SELECT ticker, cusip, fpi, statpers, fpedats, curcode, numest, medest, measure, estflag 
    FROM ibes.statsumu_epsus
    WHERE fpi IN ('0', '1', '2');
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/statsumu_epsus.parquet")
  # Dividend forecasts
  sql_string <- "
    SELECT ticker, statpers, curcode, numest, medest 
    FROM ibes.statsumu_xepsus
    WHERE measure='DPS' and fiscalp='ANN' and fpi='1';
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/ibes_statsumu_xepsus_dps.parquet")
  # Actual earnings 
  sql_string <- "
    SELECT ticker, anndats, pends, pdicity, curr_act, measure, value 
    FROM ibes.actu_epsus;
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/ibes_actu_epsus.parquet")
  # Realized book per share 
  sql_string <- "
    SELECT ticker, anndats, pends, curr_act, value 
    FROM ibes.actu_xepsus
    WHERE pdicity='ANN' AND measure='BPS' AND value IS NOT NULL;
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/ibes_actu_xepsus_bps.parquet") # bps_g[, .N, by = year(pends)][order(year)] # NOT AVAILABLE UNTIL 1996...
  # Prices from CRSP 
  sql_string <- "
    SELECT permno, date, abs(prc) as prc, cfacshr
    FROM crsp.dsf
    WHERE date >= '01JAN1976';
  "
  system.time(wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/crsp_dsf.parquet"))  # 5min
  # SIC codes from CRSP
  sql_string <- "
    SELECT distinct permno, namedt, nameendt, siccd 
    FROM crsp.dsenames;
  "
  system.time(wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/crsp_dsenames_sic.parquet"))
  
  # US FUNDA [Also used in global ICC for industry ROE's back in time]
  sql_string <- "
    SELECT gvkey, iid, datadate, ajex, sich, curcd, at, dvt, seq, ib, cshpri, epspx, bkvlps 
    FROM comp.funda
    WHERE indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C';
  "
  wrds |> wrds_fetch(sql_string) |> write_parquet("WRDS-DATA/comp_funda.parquet")
}

# Risk-free rates (from FRED) ----------------------------------------------
if (T) {
  library(fredr)
  fredr_set_key(fred_apikey) # Get key form st. louis FED
  # 10-year
  rf10 <- fredr(series_id = "DGS10")
  rf10 |> write_parquet("WRDS-DATA/rf10_fred.parquet")
}


# EXCHANGE RATES ---------------------------------------------------------------
# Replicates 01-exchange_rates.sas: computes USD exchange rates using
# comp.exrt_dly with GBP as intermediate currency, then fills date gaps (LOCF)
if (T) {
  # Download raw exchange rates from WRDS
  sql_string <- "
    SELECT fromcurd, tocurd, datadate, exratd
    FROM comp.exrt_dly
    WHERE fromcurd = 'GBP'
  "
  exrt_dly <- wrds |> wrds_fetch(sql_string)
  exrt_dly[, datadate := as.Date(datadate)]
  # Compute cross-rates locally: fx = (GBP->USD) / (GBP->X) = X->USD
  gbp_usd <- exrt_dly[tocurd == "USD", .(datadate, exratd_usd = exratd)]
  fx_raw <- exrt_dly[gbp_usd, on = "datadate", nomatch = NULL]
  fx_raw <- unique(fx_raw[, .(curcdd = tocurd, datadate, fx = exratd_usd / exratd)])
  rm(exrt_dly, gbp_usd)
  # Add USD = 1 starting from 1950 (comp.exrt_dly only starts in 1982)
  usd <- data.table(curcdd = "USD", datadate = as.Date("1950-01-01"), fx = 1)
  fx <- rbind(fx_raw, usd)
  # Fill date gaps with LOCF per currency
  max_date <- fx[, max(datadate)]
  fx_filled <- fx[order(curcdd, datadate), {
    all_dates <- data.table(datadate = seq.Date(min(datadate), max_date, by = "day"))
    out <- .SD[all_dates, on = "datadate"]
    out[, fx := nafill(fx, type = "locf")]
    out
  }, by = curcdd]
  # Format to match SAS output (date as YYYYMMDD string)
  ex_rates <- fx_filled[, .(date = format(datadate, "%Y%m%d"), curcdd, fx)]
  ex_rates |> setorder(curcdd, date)
  ex_rates |> write_parquet("WRDS-DATA/exchange_rates.parquet")
  rm(fx_raw, usd, fx, max_date, fx_filled, ex_rates)
}
