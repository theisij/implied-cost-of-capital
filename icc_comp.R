# Prepare data ------------------------------
# Linking table -----
# Comp-IBES link - Global
comp_ibes_g_link <- fread("WRDS-DATA/comp_g_security.csv", colClasses = c("gvkey"="character", "iid"="character"))
comp_ibes_g_link[, type := "int"]
# Comp-IBES link - NA
comp_ibes_na_link <- fread("WRDS-DATA/comp_security.csv", colClasses = c("gvkey"="character", "iid"="character"))
comp_ibes_na_link[, type := "na"]
# Comp-IBES link - comb
comp_ibes_link <- rbind(comp_ibes_g_link, comp_ibes_na_link)
comp_ibes_link <- comp_ibes_link[ibtic!=""]
comp_ibes_link[, n := .N, by = .(ibtic)]
comp_ibes_link <- comp_ibes_link[n==1 | (n==1 & type=="na")][, type := NULL]

# CRSP-COMP link 
crsp_comp_link <- fread("WRDS-DATA/crsp_comp_link.csv", colClasses = c("gvkey"="character", "iid"="character"))
crsp_comp_link[, end := if_else(is.na(end), max(end, na.rm=T), end)]

# Compustat prices (Global) -------------------------
comp_p_g <- fread("WRDS-DATA/comp_g_secd.csv", colClasses = c("gvkey"="character", "iid"="character"))
comp_p_g <- comp_p_g[curcdd != "" & !is.na(ajexdi) & !is.na(prccd)]
# Get adjfct 
comp_p_g |> setorder(gvkey, iid, datadate)
comp_p_g[, counter := rleid(ajexdi), by = .(gvkey, iid)]
adjfct_g <- comp_p_g[, .(
  start=min(datadate), 
  end=max(datadate)
), by = .(gvkey, iid, counter, ajexdi)][, counter := NULL]
adjfct_g |> setorder(gvkey, iid, start)
adjfct_g[, next_start := shift(start, 1, type = "lead"), by = .(gvkey, iid)] # Ensure no gaps (carry forward last value)
adjfct_g[!is.na(next_start), end := next_start-1][, next_start := NULL]
# Compress comp prices
comp_p_g <- comp_p_g[,.(gvkey, iid, datadate, curcdd, prc_adj = prccd/ajexdi)]
# Compustat prices (NA) -------------------------
comp_p_na <- fread("WRDS-DATA/comp_secd.csv", colClasses = c("gvkey"="character", "iid"="character"))
comp_p_na <- comp_p_na[curcdd != "" & !is.na(ajexdi) & !is.na(prccd)]
# Get adjfct 
comp_p_na |> setorder(gvkey, iid, datadate)
comp_p_na[, counter := rleid(ajexdi), by = .(gvkey, iid)]
adjfct_na <- comp_p_na[, .(
  start=min(datadate), 
  end=max(datadate)
), by = .(gvkey, iid, counter, ajexdi)][, counter := NULL]
adjfct_na |> setorder(gvkey, iid, start)
adjfct_na[, next_start := shift(start, 1, type = "lead"), by = .(gvkey, iid)] # Ensure no gaps (carry forward last value)
adjfct_na[!is.na(next_start), end := next_start-1][, next_start := NULL]
# Compress comp prices
comp_p_na <- comp_p_na[,.(gvkey, iid, datadate, curcdd, prc_adj = prccd/ajexdi)]
# Combine adjustment factors -----------
adjfct <- rbind(adjfct_na, adjfct_g)
rm(adjfct_g, adjfct_na)
# Exchange rates ------------------
ex_rates <- fread("WRDS-DATA/exchange_rates.csv", colClasses = c("date"="character"))
ex_rates[, datadate := as.Date(date, format = "%Y%m%d")][, date := NULL]
# Statsumu --------------------------------
statsumu_int <- fread("WRDS-DATA/statsumu_epsint.csv")
statsumu_us <- fread("WRDS-DATA/statsumu_epsus.csv")
statsumu <- rbind(statsumu_int, statsumu_us) 
rm(statsumu_int, statsumu_us)
statsumu |> setnames(old = c("ticker", "curcode", "statpers"), new = c("ibtic", "curcdd", "datadate"))
statsumu[, value := medest]
statsumu <- statsumu[!is.na(value)]
# Compress compustat data and add exchange rate info ----
ib_dates <- unique(statsumu$datadate)
comp_p_g <- comp_p_g[datadate %in% c(ib_dates, ib_dates-1, ib_dates-2, ib_dates-3, ib_dates-4, ib_dates-5, ib_dates-6)]  # Allow up to 6 days lags (to account for holidays)
comp_p_na <- comp_p_na[datadate %in% c(ib_dates, ib_dates-1, ib_dates-2, ib_dates-3, ib_dates-4, ib_dates-5, ib_dates-6)]  # Allow up to 6 days lags (to account for holidays)
# Combine price files 
comp_p <- rbind(comp_p_g, comp_p_na)
rm(comp_p_g, comp_p_na)
# Keep price closest to ibes date
comp_p[, eom := ceiling_date(datadate, unit="m")-1]
comp_p <- data.table(ib_date=ib_dates, eom = ceiling_date(ib_dates, unit="m")-1)[comp_p, on = "eom"]
comp_p[datadate<=ib_date, max_date := max(datadate), by = .(gvkey, iid, eom)]
comp_p <- comp_p[datadate==max_date][, .(gvkey, iid, datadate=ib_date, curcdd, prc_adj)]
# Add exchange info
comp_p <- ex_rates[comp_p, on = .(curcdd, datadate)]
comp_p[, prc_adj := prc_adj*fx][, c("curcdd", "fx") := NULL]
# Actuals - EPS ---------------------------------------
actu_int <- fread("WRDS-DATA/ibes_actu_epsint.csv")
actu_us <- fread("WRDS-DATA/ibes_actu_epsus.csv")
actu <- rbind(actu_int, actu_us) 
rm(actu_int, actu_us)
actu |> setnames(old = c("ticker", "curr_act", "pends"), new = c("ibtic", "curcdd", "datadate"))
actu <- actu[!is.na(value)]
# Actuals - BPS ---------------------------------------
bps_int <- fread("WRDS-DATA/ibes.actu_xepsint_bps.csv")
bps_us <- fread("WRDS-DATA/ibes.actu_xepsus_bps.csv")
bps <- rbind(bps_int, bps_us) 
rm(bps_int, bps_us)
bps |> setnames(old = c("ticker", "curr_act", "pends"), new = c("ibtic", "curcdd", "datadate"))
bps <- bps[!is.na(value)] # Already in SQL statement
# Forecasts - DPS ---------------------------------------
dps_int <- fread("WRDS-DATA/ibes_statsumu_xepsint_dps.csv")
dps_us <- fread("WRDS-DATA/ibes_statsumu_xepsus_dps.csv")
dps_ib <- rbind(dps_int, dps_us) 
rm(dps_int, dps_us)
dps_ib |> setnames(old = c("ticker", "curcode", "statpers"), new = c("ibtic", "curcdd", "datadate"))
dps_ib[, value := medest]
dps_ib <- dps_ib[!is.na(value)]
# CRSP SIC codes (for historical)
crsp_sic <- fread("WRDS-DATA/crsp_dsenames_sic.csv")
# Compustat g_funda
funda_g <- fread("WRDS-DATA/comp_g_funda.csv", colClasses = c("gvkey"="character"))
# Compustat funda
funda <- fread("WRDS-DATA/comp_funda.csv", colClasses = c("gvkey"="character"))
# 10 year US treasury rate 
rf10 <- fread("WRDS-DATA/rf10_fred.csv")
rf10 <- rf10[!is.na(value), .(start=date, rf10=value/100)][order(start)]
rf10[, end := shift(start, 1, type="lead")-1]
rf10[is.na(end), end := start]
# Prepare IBES -----------------------------
prepare_ibes <- function(data) {
  # Add gvkey-iid 
  data <- comp_ibes_link[, .(gvkey, iid, ibtic)][data, on = .(ibtic)]
  print(paste0("Missing IBES-COMP match excludes ", data[, round(mean(is.na(gvkey))*100, 2)], "%"))
  data <- data[!is.na(gvkey)]
  # Adjustment factor to statsumu -------
  data[, merge_date := datadate]
  data <- adjfct[data, on = .(gvkey, iid, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
  print(paste0("Missing AJEXDI excludes ", data[, round(mean(is.na(ajexdi))*100, 2)], "%"))
  data <- data[!is.na(ajexdi)]
  # Conver British/Irish pence to British/Irish pound
  data[curcdd=="BPN", curcdd := "GBP"] # BPN==British pence (P.15: https://inside.rotman.utoronto.ca/financelab/files/2022/08/IBES_Summary-History-Utilization-Guide.pdf)
  data[curcdd=="IPN", curcdd := "IEP"] # IPN==Irish pence (P.14: https://inside.rotman.utoronto.ca/financelab/files/2022/08/IBES_Summary-History-Utilization-Guide.pdf)
  # Add exchange rate
  data <- ex_rates[data, on = .(datadate, curcdd)]
  print(paste0("Missing exchange rate excludes ", data[, round(mean(is.na(fx))*100, 2)], "% for codes ", data[is.na(fx), paste0(unique(curcdd), collapse = "+")]))
  data <- data[!is.na(fx)]
  # Adjust value---
  # Convert British/Irish pence to British/Irish pound
  data[ , value_adj := if_else(curcdd %in% c("GBP", "IEP"), value/100, value)] 
  # Adjust for splits and exchange rates
  data[, value_adj := value_adj/ajexdi*fx]
  # Output
  return(data)
}
# Prepare actu
actu <- actu |> prepare_ibes()
# Prepare bps_g
bps <- bps |> prepare_ibes()
# Prepare statsumu_g
statsumu <- statsumu |> prepare_ibes()
# Prepare dps 
dps_ib <- dps_ib |> prepare_ibes()

## Prepare funda_g ------------------------------
funda_g[!is.na(sich), ff49 := sich |> ff49_fun()]
funda[!is.na(sich), ff49 := sich |> ff49_fun()]
crsp_sic[, ff49_crsp := siccd |> ff49_fun()]
# Add FF49 from CRSP if neccesary (comp sic starts in 1980)
funda[, merge_date := datadate]
funda <- crsp_comp_link[linkprim=="P", .(gvkey, permno, start=as.Date(start), end=as.Date(end))][funda, on = .(gvkey, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
funda[, merge_date := datadate]
funda <- crsp_sic[, .(permno, namedt, nameendt, ff49_crsp)][funda, on = .(permno, namedt<=merge_date, nameendt>=merge_date)][, c("namedt", "nameendt") := NULL]
funda[is.na(ff49), ff49 := ff49_crsp]
funda[, ff49_crsp := NULL]
# Add FX
funda_g <- ex_rates[, .(curcd=curcdd, fx, datadate)][funda_g, on = .(datadate, curcd)]
funda <- ex_rates[, .(curcd=curcdd, fx, datadate)][funda, on = .(datadate, curcd)]
# FF49 classification ---
ff49_ind <- rbind(
  funda[, .(gvkey, datadate, ff49, type = "us")],
  funda_g[, .(gvkey, datadate, ff49, type = "int")]
)[!is.na(ff49)]
ff49_ind[, n := .N, by = .(gvkey, datadate)]
ff49_ind <- ff49_ind[n==1 | (n==2 & type=="int")][order(gvkey, datadate)]
ff49_ind |> setnames(old = "datadate", new = "start")
ff49_ind[, start := as.Date(start)]
ff49_ind[, end := shift(start, 1, type = "lead")-1, by = gvkey]
ff49_ind[is.na(end), end := start+1+years(1)-1]
ff49_ind <- ff49_ind[, .(gvkey, ff49, start, end)]

# Book-per-share ----------------
# (FUNDA has bkvlps)
bps_comb <- rbind(
  funda[, .(gvkey, iid, datadate, bps_adj = bkvlps/ajex*fx, type = "funda")],
  funda_g[, .(gvkey, iid, datadate, bps_adj = (seq/cshpria)/ajexi*fx, type = "g_funda")],
  bps[, .(gvkey, iid, datadate, bps_adj = value_adj, type = "ibes")]
)
bps_comb <- bps_comb[!is.na(bps_adj)]
bps_comb[, n := .N, by = .(gvkey, iid, datadate)]
bps_comb <- bps_comb[n==1 | (n != 1 & type=="ibes")]
# Very few cases hae multiple BPS in IBES. Exclude those to be conservative
bps_comb[, n := .N, by = .(gvkey, iid, datadate)] # bps_comb[, mean(n==1)] 99.97%
bps_comb <- bps_comb[n==1][, n := NULL]

# Prepare ICC data ----------------- --------------
comp_id <- function(gvkey, iid) { # Following JKP
  case_when(
    str_detect(iid, "W") ~ paste0('3', gvkey, substr(iid, 1, 2)),
    str_detect(iid, "C") ~ paste0('2', gvkey, substr(iid, 1, 2)),
    TRUE ~ paste0('1', gvkey, substr(iid, 1, 2)),
  )
}
# Treat daily p separately (to keep gvkey)
comp_p[, id := comp_id(gvkey, iid)]
comp_p[, iid := NULL]
# Change gvkey/iid to id
for (x in list(statsumu, actu, bps_comb, dps_ib)) {
  x[, id := comp_id(gvkey, iid)]
  x[, c("gvkey", "iid") := NULL]
}
icc_data <- prepare_icc_data(
  daily_p = comp_p,
  epsf = statsumu,
  epsa = actu,
  acc_g = funda_g,
  acc_us = funda,
  po_pref = "g_funda", # Prefer payout ratio from funda or g_funda?
  bps = bps_comb,
  dps = dps_ib,
  rf10 = rf10,
  ff49_ind = ff49_ind
)
# Create ICC estimates -------------------------
system.time(icc_oj <- icc_data |> icc_oj_fun())
system.time(icc_peg <- icc_data |> icc_peg_fun())
system.time(icc_gls <- icc_data |> icc_gls_fun(root_low = 0.01, root_high = 0.5))  # 7min
system.time(icc_ct <- icc_data |> icc_ct_fun(root_low = 0.01, root_high = 0.5))    # 2min
# Combine --------------------------------------
icc <- icc_oj[icc_data[, .(id, datadate, numest1_avg)], on = .(id, datadate)]
icc <- icc_peg[icc, on = .(id, datadate)]
icc <- icc_gls[icc, on = .(id, datadate)]
icc <- icc_ct[icc, on = .(id, datadate)]
icc <- icc[!is.na(icc_ct) | !is.na(icc_gls) | !is.na(icc_peg) | !is.na(icc_oj)]
# Save -----------------------------------------
icc %>% fwrite("icc_comp.csv")




# Forwards E/P -----------
if (FALSE) {
  icc_data[, .(id, datadate, eps0_p = eps0/prc_adj, eps1_p = eps1/prc_adj, eps2_p = eps2/prc_adj)] |> 
    fwrite("forward_eps_comp.csv")
}





# Analysis
if (FALSE) {
  # Missing obs 
  icc_data[, lapply(.SD, function(x) mean(is.na(x)))]
  icc_data[numest1_avg>=3, lapply(.SD, function(x) mean(is.na(x)))]
  
  icc_data[, lapply(.SD, function(x) sum(!is.na(x))), by=datadate] |> 
    pivot_longer(-datadate) |> 
    filter(year(datadate)>=1980) |> 
    ggplot(aes(datadate, value)) +
    geom_line() +
    facet_wrap(~name, scales = "free_y")
  
  icc_data[numest1_avg>=3, lapply(.SD, function(x) sum(!is.na(x))), by=datadate] |> 
    pivot_longer(-datadate) |> 
    filter(year(datadate)>=1980) |> 
    ggplot(aes(datadate, value)) +
    geom_line() +
    facet_wrap(~name, scales = "free_y")
  
  statsumu_g[!is.na(value_adj), .N, by = .(fpi, datadate)] |> 
    ggplot(aes(datadate, N)) + 
    geom_line() +
    facet_wrap(~factor(fpi))
  # Analysis 
  icc_ss <- rf10[copy(icc)[, merge_date := datadate], on = .(start<=merge_date, end>=merge_date)][, c("start", "end") := NULL] |>
    pivot_longer(-c(id, datadate, rf10, numest1_avg)) |>
    filter(numest1_avg >= 3) |>
    filter(!is.na(value)) |> 
    group_by(datadate, name) |> 
    summarise(
      n = n(), 
      q25=quantile(value-rf10, 0.25), 
      q50=quantile(value-rf10, 0.5), 
      q75=quantile(value-rf10, 0.75)
    ) 
  # Coverage
  icc_ss |> 
    ggplot(aes(datadate, n, colour = name)) + 
    geom_line() +
    facet_wrap(~name)
  # Level/dispersion
  icc_ss |> 
    ggplot(aes(datadate, q50, colour = name)) + 
    geom_line()
  icc_ss |> 
    ggplot(aes(datadate, q50, colour = name)) + 
    geom_line() +
    geom_line(aes(y=q25), linetype="dotted") +
    geom_line(aes(y=q75), linetype="dotted") +
    facet_wrap(~name)
  
  
  
  
  # Screening measure=="EPSPAR"
  # Not entirely sure what measure==EPSPAR means: 
  statsumu_g[fpi==1 & curcode=="JPY" & ibtic == "@ZV" & statpers == as.Date("2021-05-20")]  
  
  # Check currency code overlap from IBES and Compustat
  ibes_cur <- actu_g[, .(n_ibes = .N), by = curcdd]
  comp_cur <- ex_rates[, .(n_comp = .N), by = curcdd]
  test <- ibes_cur[, .(curcdd, n_ibes)][comp_cur, on = "curcdd"]
  test[!is.na(n_ibes)]
  # Same number as in IBES?
  ibes_cur[!(curcdd %in% comp_cur$curcdd)]
  
  # Statsumu stats
  statsumu_g <- wrds |> wrds_fetch(sql_string)
  statsumu_g[, .N, by = measure][, pct := N/sum(N)][]
  statsumu_g[, .(N =mean(measure=="EPS")), by = curcode][order(-N)]
  statsumu_g[, .N, by = curcode][, pct := round(N/sum(N), 3)][order(N)]
  # Long term growth around the world
  test <- statsumu_g[fpi=='0', .(n=.N, med = median(medest)), by = .(statpers, curcode)]
  test <- test[n>=20 & year(statpers)>=2000]
  test[, N := .N, by = curcode]
  test <- test[N>=12*20] 
  test[, med_n := median(n), by = curcode]
  
  test[med_n>=200] |> 
    ggplot(aes(statpers, med, colour = curcode)) + 
    geom_line() + 
    geom_hline(yintercept = 10) #+
  # facet_wrap(~curcode)
  
  # TEST: Compare COMP and IBES EPS
  if (FALSE) {
    # Compute EPS from observables in Compustat
    eps_comp <- funda_g[, .(gvkey, iid, "pends"=datadate, "merge_date"=datadate, fx,
                            eps_comp = ib/cshpria,
                            eps_comp_adj1 = (ib/cshpria)/ajexi*fx)][!is.na(eps_comp)]
    eps_comp <- adjfct_g[eps_comp, on = .(gvkey, iid, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
    eps_comp[, eps_comp_adj2 := eps_comp/ajexdi*fx]
    
    # Compare to EPS from IBES
    test <- actu_g[pdicity=="ANN" & measure=="EPS", .(gvkey, iid, pends=datadate, value, value_adj)][eps_comp, on = .(gvkey, iid, pends)]
    test <- test |> na.omit()
    test <- test[!is.infinite(eps_comp)]
    # Correlation
    test[, cor(value, eps_comp, method = "spearman")]
    test[, cor(value_adj, eps_comp_adj1, method = "spearman")]
    test[, cor(value_adj, eps_comp_adj2, method = "spearman")]
    # Scale
    test[, quantile(value/eps_comp, probs = c(0.25, 0.5, 0.75))]
    test[, quantile(value_adj/eps_comp_adj1, probs = c(0.25, 0.5, 0.75))]
    test[, quantile(value_adj/eps_comp_adj2, probs = c(0.25, 0.5, 0.75))]
    # Conclusion: IBES/COMP seem comparable and fine to use ajex from funda
  }
  
  # ICC over time --------------
  icc |> 
    pivot_longer(-c(gvkey, iid, datadate)) |> 
    filter(!is.na(value)) |> 
    group_by(datadate, name) |> 
    summarise(n=n(), med = median(value)) |> 
    ggplot(aes(datadate, med, colour = name)) +
    geom_line() +
    facet_wrap(~name)
  icc |> 
    pivot_longer(-c(gvkey, iid, datadate)) |> 
    filter(!is.na(value)) |> 
    group_by(datadate, name) |> 
    summarise(n=n(), med = median(value)) |> 
    ggplot(aes(datadate, n, colour = name)) +
    geom_line() +
    facet_wrap(~name)
  
  # Missing for GLS -----------
  icc_gls[,.N, by =datadate][order(datadate)][, chg := N-lag(N)][order(chg)]
  icc_data[year(datadate)==2008, lapply(.SD, function(x) sum(!is.na(x))),by=datadate]
  # Statsumu isn't the issue
  test <- statsumu_g[fpi=='1' & measure=="EPS" & year(datadate)==2008]
  test[!is.na(medest),.N, by = datadate][order(datadate)]
  # Is price the issue?
  test <- comp_p_g[year(datadate)==2008]
  test[!is.na(prc_adj),.N, by = datadate][order(datadate)]
}
