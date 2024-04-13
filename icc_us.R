# Prepare data ------------------------------
# Linking table -----
# Comp-IBES link 
crsp_ibes_link <- fread("WRDS-DATA/crsp_ibes_link.csv")
# Current link stops in Mar-2023. Extend to Dec-2023
crsp_ibes_link[edate==as.Date("2022-03-31"), edate := as.Date("2022-12-31")]
# CRSP-COMP link 
crsp_comp_link <- fread("WRDS-DATA/crsp_comp_link.csv", colClasses = c("gvkey"="character", "iid"="character"))
crsp_comp_link[, end := if_else(is.na(end), max(end, na.rm=T), end)]


# CRSP prices ------------------------------
crsp_dsf <- fread("WRDS-DATA/crsp_dsf.csv")
crsp_dsf |> setnames(old = "date", new = "datadate")
# Adjustment factor ------------------------
crsp_dsf |> setorder(permno, datadate)
crsp_dsf[, counter := rleid(cfacshr), by = .(permno)]
adjfct <- crsp_dsf[, .(
  start=min(datadate), 
  end=max(datadate)
), by = .(permno, counter, cfacshr)][, counter := NULL]
adjfct |> setorder(permno, start)
adjfct[, next_start := shift(start, 1, type = "lead"), by = .(permno)] # Ensure no gaps (carry forward last value)
adjfct[!is.na(next_start), end := next_start-1][, next_start := NULL]
# Compress comp prices
crsp_dsf <- crsp_dsf[,.(permno, datadate, prc_adj = prc/cfacshr)]

# Remaining data ----------------------------------
# Exchange rates
ex_rates <- fread("WRDS-DATA/exchange_rates.csv", colClasses = c("date"="character"))
ex_rates[, datadate := as.Date(date, format = "%Y%m%d")][, date := NULL]

# Statsumu  
statsumu <- fread("WRDS-DATA/statsumu_epsus.csv")
statsumu |> setnames(old = c("ticker", "curcode", "statpers"), new = c("ibtic", "curcdd", "datadate"))
statsumu[, value := medest]
statsumu <- statsumu[!is.na(value)]
# Compress price data and add exchange rate info ----
ib_dates <- unique(statsumu$datadate)
crsp_dsf <- crsp_dsf[datadate %in% c(ib_dates, ib_dates-1, ib_dates-2, ib_dates-3, ib_dates-4, ib_dates-5, ib_dates-6)]  # Allow up to 6 days lags (to account for holidays)

# Keep price closest to ibes date
crsp_dsf[, eom := ceiling_date(datadate, unit="m")-1]
crsp_dsf <- data.table(ib_date=ib_dates, eom = ceiling_date(ib_dates, unit="m")-1)[crsp_dsf, on = "eom"]
crsp_dsf[datadate<=ib_date, max_date := max(datadate), by = .(permno, eom)]
crsp_dsf <- crsp_dsf[datadate==max_date][, .(permno, datadate=ib_date, prc_adj)]

# Actu 
actu <- fread("WRDS-DATA/ibes_actu_epsus.csv")
actu |> setnames(old = c("ticker", "curr_act", "pends"), new = c("ibtic", "curcdd", "datadate"))
actu <- actu[!is.na(value)]

# BPS 
bps <- fread("WRDS-DATA/ibes.actu_xepsus_bps.csv")
bps |> setnames(old = c("ticker", "curr_act", "pends"), new = c("ibtic", "curcdd", "datadate"))
bps <- bps[!is.na(value)] # Already in SQL statement

# DPS IBES 
dps_ib <- fread("WRDS-DATA/ibes_statsumu_xepsus_dps.csv")
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
  # Add permno
  data <- copy(data)
  data[, merge_date := datadate]
  data <- crsp_ibes_link[data, on = .(ibtic, sdate<=merge_date, edate>=merge_date)][, c("sdate", "edate") := NULL]
  print(paste0("Missing IBES-CRSP match excludes ", data[, round(mean(is.na(permno))*100, 2)], "%"))
  data <- data[!is.na(permno)]
  # Adjustment factor to statsumu -------
  data[, merge_date := datadate]
  data <- adjfct[data, on = .(permno, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
  print(paste0("Missing CFACSHR excludes ", data[, round(mean(is.na(cfacshr))*100, 2)], "%"))
  data <- data[!is.na(cfacshr)]
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
  data[, value_adj := value_adj/cfacshr*fx]
  # Output
  return(data)
}
# Prepare actu_g
actu <- actu |> prepare_ibes()
# Prepare bps
bps <- bps |> prepare_ibes()
# Prepare statsumu
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
ff49_ind <- ff49_ind[n==1 | (n==2 & type=="us")][order(gvkey, datadate)]
ff49_ind |> setnames(old = "datadate", new = "start")
ff49_ind[, start := as.Date(start)]
ff49_ind[, end := shift(start, 1, type = "lead")-1, by = gvkey]
ff49_ind[is.na(end), end := start+1+years(1)-1]
ff49_ind <- ff49_ind[, .(gvkey, ff49, start, end)]

# Book-per-share ----------------
# Adjust with CRSP factor
funda[, merge_date := datadate]
funda <- adjfct[funda, on = .(permno, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
funda[, adj_check := abs(log(cfacshr)-log(ajex))<0.2] # Check that adjustment factors are comparable
funda[adj_check==T, bps_adj := bkvlps/cfacshr*fx]

# (FUNDA has bkvlps)
bps_comb <- rbind(
  funda[, .(permno, datadate, bps_adj, type = "comp")],
  bps[, .(permno, datadate, bps_adj = value_adj, type = "ibes")]
)
bps_comb <- bps_comb[!is.na(bps_adj)]
bps_comb[, n := .N, by = .(permno, datadate)]
bps_comb <- bps_comb[n==1 | (n != 1 & type=="ibes")]
# Very few cases hae multiple BPS in IBES. Exclude those to be conservative
bps_comb[, n := .N, by = .(permno, datadate)] # bps_comb[, mean(n==1)] 99.97%
bps_comb <- bps_comb[n==1][, n := NULL]

# Prepare ICC data -------------------------------
# Add gvkey to daily p for accounting data
crsp_dsf[, merge_date := as.Date(datadate)]
crsp_dsf <- crsp_comp_link[,.(permno, gvkey, start=as.Date(start), end=as.Date(end))][crsp_dsf, on = .(permno, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
# Change permno to id
for (x in list(crsp_dsf, statsumu, actu, bps_comb, dps_ib)) {
  x %>% setnames(old = "permno", new = "id")
}

# Data
icc_data <- prepare_icc_data(
  daily_p = crsp_dsf,
  epsf = statsumu,
  epsa = actu,
  acc_g = funda_g,
  acc_us = funda,
  po_pref = "funda", # Prefer payout ratio from funda or g_funda?
  bps = bps_comb,
  dps = dps_ib,
  rf10 = rf10,
  ff49_ind = ff49_ind
)
# Create ICC estimates -------------------------
system.time(icc_oj <- icc_data |> icc_oj_fun())
system.time(icc_peg <- icc_data |> icc_peg_fun())
system.time(icc_gls <- icc_data |> icc_gls_fun(root_low = 0.01, root_high = 0.5))  # 5min
system.time(icc_ct <- icc_data |> icc_ct_fun(root_low = 0.01, root_high = 0.5))    # 2min
# Combine --------------------------------------
icc <- icc_oj[icc_data[, .(id, datadate, numest1_avg)], on = .(id, datadate)]
icc <- icc_peg[icc, on = .(id, datadate)]
icc <- icc_gls[icc, on = .(id, datadate)]
icc <- icc_ct[icc, on = .(id, datadate)]
icc <- icc[!is.na(icc_ct) | !is.na(icc_gls) | !is.na(icc_peg) | !is.na(icc_oj)]
# Save -----------------------------------------
icc %>% fwrite("icc_us.csv")

# Forwards E/P -----------
if (FALSE) {
  icc_data[, .(id, datadate, eps0_p = eps0/prc_adj, eps1_p = eps1/prc_adj, eps2_p = eps2/prc_adj)] |> 
    fwrite("forward_eps_us.csv")
}

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
  
  statsumu[!is.na(value_adj), .N, by = .(fpi, datadate)] |> 
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
    labs(title = "numest1_avg>=3")
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
  # Is extension by linear extrapolation reasonable? [this should be done before the interpolation!!!]
  icc_data[, median(eps2/eps1-1,na.rm=T)] # Call it 15%
  icc_data[, eps2_proxy1 := eps1]
  icc_data[, eps2_proxy2 := eps1*(1+0.15)]
  icc_data[, eps2_proxy3 := eps0 + (eps1-eps0)*2]
  
  test_sub <- icc_data[!is.na(eps2) & !is.na(eps2_proxy1) & !is.na(eps2_proxy2)& !is.na(eps2_proxy3) & eps2!=0]
  test_sub[, ratio1 := eps2_proxy1/eps2]
  test_sub[, ratio2 := eps2_proxy2/eps2]
  test_sub[, ratio3 := eps2_proxy3/eps2]
  test_sub[ratio1 >= quantile(ratio1, 0.05) & ratio1 <= quantile(ratio1, 0.95) &
           ratio2 >= quantile(ratio2, 0.05) & ratio2 <= quantile(ratio2, 0.95) &
           ratio3 >= quantile(ratio3, 0.05) & ratio3 <= quantile(ratio3, 0.95)
           ] |> 
    pivot_longer(c(ratio1, ratio2, ratio3)) |> 
    ggplot(aes(x=value)) +
    geom_histogram() +
    facet_wrap(~name, ncol=1)
  # Mean/median squared error
  test_sub |> 
    select(id, datadate, eps2, eps2_proxy1, eps2_proxy2, eps2_proxy3) |> 
    pivot_longer(c(eps2_proxy1, eps2_proxy2, eps2_proxy3)) |> 
    group_by(name) |> 
    summarise(
      r2 = 1 - sum((value-eps2)^2)/sum((eps2-mean(eps2))^2),
      med_rse = sqrt(median((eps2-value)^2)),
      scor = cor(value, eps2, method = "spearman")
    )
  # Mean/median squared error (non-extreme subset)
  w <- 0.01
  test_sub[, se1 := (eps2-eps2_proxy1)^2]
  test_sub[, se2 := (eps2-eps2_proxy2)^2]
  test_sub[, se3 := (eps2-eps2_proxy3)^2]
  test_sub[se1 >= quantile(se1, w) & se1 <= quantile(se1, 1-w) &
             se2 >= quantile(se2, w) & se2 <= quantile(se2, 1-w) &
             se3 >= quantile(se3, w) & se3 <= quantile(se3, 1-w)] |> 
    select(id, datadate, eps2, eps2_proxy1, eps2_proxy2, eps2_proxy3) |> 
    pivot_longer(c(eps2_proxy1, eps2_proxy2, eps2_proxy3)) |> 
    group_by(name) |> 
    summarise(
      mean_err = mean(value-eps2), 
      r2 = 1 - sum((value-eps2)^2)/sum((eps2-mean(eps2))^2),
      med_rse = sqrt(median((eps2-value)^2)),
      scor = cor(value, eps2, method = "spearman")
    )
  # It not unambigious but I prefer proxy2
}