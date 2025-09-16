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
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/comp_g_security.csv")
  # IBES-Compustat linking - NA
  sql_string <- "
    SELECT  gvkey, iid, epf, excntry, ibtic  
    FROM comp.security;
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/comp_security.csv")
  # EPS forecasts
  sql_string <- "
    SELECT ticker, cusip, fpi, statpers, fpedats, curcode, numest, medest, measure, estflag 
    FROM ibes.statsumu_epsint
    WHERE fpi IN ('0', '1', '2');
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/statsumu_epsint.csv")
  # Dividend forecasts
  sql_string <- "
    SELECT ticker, statpers, curcode, numest, medest 
    FROM ibes.statsumu_xepsint
    WHERE measure='DPS' and fiscalp='ANN' and fpi='1';
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/ibes_statsumu_xepsint_dps.csv")
  # Actual earnings 
  sql_string <- "
    SELECT ticker, anndats, pends, pdicity, curr_act, measure, value 
    FROM ibes.actu_epsint;
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/ibes_actu_epsint.csv")
  # Realized book per share 
  sql_string <- "
    SELECT ticker, anndats, pends, curr_act, value 
    FROM ibes.actu_xepsint
    WHERE pdicity='ANN' AND measure='BPS' AND value IS NOT NULL;
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/ibes.actu_xepsint_bps.csv") # bps_g[, .N, by = year(pends)][order(year)] # NOT AVAILABLE UNTIL 1996...
  # Global prices daily
  sql_string <- "
    SELECT a.gvkey, a.iid, a.datadate, a.curcdd, a.ajexdi, a.prccd, a.monthend, b.ibtic
    FROM comp.g_secd as a
    LEFT JOIN comp.g_security as b
    ON a.gvkey=b.gvkey AND a.iid=b.iid
    WHERE b.tpci='0' and b.ibtic<>'' and a.ajexdi IS NOT NULL and a.prccd IS NOT NULL and a.curcdd<>'';
  " 
  system.time(wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/comp_g_secd.csv"))
  # NA prices daily
  sql_string <- "
    SELECT a.gvkey, a.iid, a.datadate, a.curcdd, a.ajexdi, a.prccd, b.ibtic
    FROM comp.secd as a
    LEFT JOIN comp.security as b
    ON a.gvkey=b.gvkey AND a.iid=b.iid
    WHERE b.tpci='0' and b.ibtic<>'' and a.ajexdi IS NOT NULL and a.prccd IS NOT NULL and a.curcdd<>'';
  " 
  system.time(wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/comp_secd.csv"))  # 16min
  # Global FUNDA
  sql_string <- "
    SELECT gvkey, datadate, ajexi, sich, curcd, at, dvt, seq, ib, cshpria, epsexcon, iid 
    FROM comp.g_funda
    WHERE indfmt in ('INDL', 'FS') and datafmt='HIST_STD' and popsrc='I' and consol='C';
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/comp_g_funda.csv")
}

# Download data for US ICC -------------------------------
if (T) {
  # CRSP-COMP linking 
  sql_string <- "
    SELECT lpermno as permno, gvkey, liid as iid, linkdt as start, linkenddt as end, linkprim
    FROM crsp.ccmxpf_lnkhist
    WHERE linktype in ('LC', 'LU', 'LS');
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/crsp_comp_link.csv")
  # IBES-CRSP linking 
  sql_string <- "
    SELECT ticker as ibtic, permno, sdate, edate 
    FROM wrdsapps.ibcrsphist
    WHERE permno IS NOT NULL and score=1;
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/crsp_ibes_link.csv")
  # EPS forecasts
  sql_string <- "
    SELECT ticker, cusip, fpi, statpers, fpedats, curcode, numest, medest, measure, estflag 
    FROM ibes.statsumu_epsus
    WHERE fpi IN ('0', '1', '2');
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/statsumu_epsus.csv")
  # Dividend forecasts
  sql_string <- "
    SELECT ticker, statpers, curcode, numest, medest 
    FROM ibes.statsumu_xepsus
    WHERE measure='DPS' and fiscalp='ANN' and fpi='1';
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/ibes_statsumu_xepsus_dps.csv")
  # Actual earnings 
  sql_string <- "
    SELECT ticker, anndats, pends, pdicity, curr_act, measure, value 
    FROM ibes.actu_epsus;
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/ibes_actu_epsus.csv")
  # Realized book per share 
  sql_string <- "
    SELECT ticker, anndats, pends, curr_act, value 
    FROM ibes.actu_xepsus
    WHERE pdicity='ANN' AND measure='BPS' AND value IS NOT NULL;
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/ibes.actu_xepsus_bps.csv") # bps_g[, .N, by = year(pends)][order(year)] # NOT AVAILABLE UNTIL 1996...
  # Prices from CRSP 
  sql_string <- "
    SELECT permno, date, abs(prc) as prc, cfacshr
    FROM crsp.dsf
    WHERE date >= '01JAN1976';
  "
  system.time(wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/crsp_dsf.csv"))  # 5min
  # SIC codes from CRSP
  sql_string <- "
    SELECT distinct permno, namedt, nameendt, siccd 
    FROM crsp.dsenames;
  "
  system.time(wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/crsp_dsenames_sic.csv"))
  
  # US FUNDA [Also used in global ICC for industry ROE's back in time]
  sql_string <- "
    SELECT gvkey, iid, datadate, ajex, sich, curcd, at, dvt, seq, ib, cshpri, epspx, bkvlps 
    FROM comp.funda
    WHERE indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C';
  "
  wrds |> wrds_fetch(sql_string) |> fwrite("WRDS-DATA/comp_funda.csv")
}

# Risk-free rates (from FRED) ----------------------------------------------
if (T) {
  library(fredr)
  fredr_set_key(fred_apikey) # Get key form st. louis FED
  # 10-year
  rf10 <- fredr(series_id = "DGS10")
  rf10 %>% fwrite("WRDS-DATA/rf10_fred.csv")
}


# EXCHANGE RATES (COMPUTED IN WRDS CLOUD) ----------------------------------
if (FALSE) {
  "
  /* USD to Foreign FX Conversion Rate from Compustat*/
    %macro compustat_fx(out=);
  data usd_curcdd; 
  curcdd='USD';
  datadate=input(put(19500101,8.),yymmdd8.);
  fx=1;
  format datadate yymmddn8.;
  run;  /* comp.exrt_dly only starts in 1982 and since we convert to USD we know that the fx for USD is 1 */
    
    proc sql; 
  create table __fx1 as
  select distinct a.tocurd as  curcdd  , a.datadate,  b.exratd/a.exratd as fx /*fx is quoted as x/USD so to go from x to USD do x*fx*/
    from comp.exrt_dly a , comp.exrt_dly b
  where a.fromcurd = 'GBP' and b.tocurd = 'USD' /*b.exratd is always from GBP to USD, a.exratd is from GBP to currency X*/
    and a.fromcurd = b.fromcurd and a.datadate = b.datadate;
  quit;
  
  data __fx2; set __fx1 usd_curcdd; run; 
  
  proc sort data = __fx2;  by curcdd descending datadate; run ; 
  
  /* Carry forward fx observations in case gaps*/
    data __fx3; format date YYMMDDN8.; 
  set __fx2;
  by curcdd;
  date = datadate;
  output;
  following = lag(date); 
  if first.curcdd then
  following = date+1;
  n = following-date;
  do i=1 to n-1;
  date = date+1; output;
  end;
  
  drop datadate following n i;
  run;
  
  proc sort data=__fx3 out=&out nodupkey; by curcdd date; run;
  
  proc delete data=usd_curcdd __fx1 __fx2 __fx3; run;
  %mend compustat_fx; 
  
  %compustat_fx(out=ex_rates);
  
  proc export data=ex_rates
  outfile='~/exchange_rates.csv'   
  dbms=CSV
  replace;
  run;
  
  "
  
}
