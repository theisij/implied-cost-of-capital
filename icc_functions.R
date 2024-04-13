# FF49 classification -----------------------------------
ff49_fun <- function(sic) {
  case_when(
    sic == 2048 | (100 <= sic & sic <= 299) | (700 <= sic & sic <= 799) | (910 <= sic & sic <= 919) ~ 1,
    sic %in% c(2095, 2098, 2099) | (2000 <= sic & sic <= 2046) | (2050 <= sic & sic <= 2063) | 
      (2070 <= sic & sic <= 2079) | (2090 <= sic & sic <= 2092) ~ 2,
    sic %in% c(2086, 2087, 2096, 2097) | (2064 <= sic & sic <= 2068) ~ 3,
    sic == 2080 | (2082 <= sic & sic <= 2085) ~ 4,
    (2100 <= sic & sic <= 2199) ~ 5,
    sic %in% c(3732, 3930, 3931) | (920 <= sic & sic <= 999) | (3650 <= sic & sic <= 3652) | 
      (3940 <= sic & sic <= 3949) ~ 6,
    sic %in% c(7840, 7841, 7900, 7910, 7911, 7980) | (7800 <= sic & sic <= 7833) | (7920 <= sic & sic <= 7933) | 
      (7940 <= sic & sic <= 7949) | (7990 <= sic & sic <= 7999) ~ 7,
    sic %in% c(2770, 2771) | (2700 <= sic & sic <= 2749) | (2780 <= sic & sic <= 2799) ~ 8,
    sic %in% c(2047, 2391, 2392, 3160, 3161, 3229, 3260, 3262, 3263, 3269, 3230, 3231, 3750, 3751, 3800, 3860, 
               3861, 3910, 3911, 3914, 3915, 3991, 3995) | (2510 <= sic & sic <= 2519) | (2590 <= sic & sic <= 2599) | 
      (2840 <= sic & sic <= 2844) | (3170 <= sic & sic <= 3172) | (3190 <= sic & sic <= 3199) | (3630 <= sic & sic <= 3639) | 
      (3870 <= sic & sic <= 3873) | (3960 <= sic & sic <= 3962) ~ 9,
    sic %in% c(3020, 3021, 3130, 3131, 3150, 3151) | (2300 <= sic & sic <= 2390) | (3100 <= sic & sic <= 3111) | 
      (3140 <= sic & sic <= 3149) | (3963 <= sic & sic <= 3965) ~ 10,
    (8000 <= sic & sic <= 8099) ~ 11,
    sic %in% c(3693, 3850, 3851) | (3840 <= sic & sic <= 3849) ~ 12,
    sic %in% c(2830, 2831) | (2833 <= sic & sic <= 2836) ~ 13,
    (2800 <= sic & sic <= 2829) | (2850 <= sic & sic <= 2879) | (2890 <= sic & sic <= 2899) ~ 14,
    sic %in% c(3031, 3041) | (3050 <= sic & sic <= 3053) | (3060 <= sic & sic <= 3099) ~ 15,
    (2200 <= sic & sic <= 2284) | (2290 <= sic & sic <= 2295) | (2297 <= sic & sic <= 2299) | (2393 <= sic & sic <= 2395) | 
      (2397 <= sic & sic <= 2399) ~ 16,
    sic %in% c(2660, 2661, 3200, 3210, 3211, 3240, 3241, 3261, 3264, 3280, 3281, 3446, 3996) | 
      (800 <= sic & sic <= 899) | (2400 <= sic & sic <= 2439) | (2450 <= sic & sic <= 2459) | (2490 <= sic & sic <= 2499) | 
      (2950 <= sic & sic <= 2952) | (3250 <= sic & sic <= 3259) | (3270 <= sic & sic <= 3275) | (3290 <= sic & sic <= 3293) | 
      (3295 <= sic & sic <= 3299) | (3420 <= sic & sic <= 3429) | (3430 <= sic & sic <= 3433) | (3440 <= sic & sic <= 3442) | 
      (3448 <= sic & sic <= 3452) | (3490 <= sic & sic <= 3499) ~ 17,
    (1500 <= sic & sic <= 1511) | (1520 <= sic & sic <= 1549) | (1600 <= sic & sic <= 1799) ~ 18,
    sic == 3300 | (3310 <= sic & sic <= 3317) | (3320 <= sic & sic <= 3325) | (3330 <= sic & sic <= 3341) | 
      (3350 <= sic & sic <= 3357) | (3360 <= sic & sic <= 3379) | (3390 <= sic & sic <= 3399) ~ 19,
    sic %in% c(3400, 3443, 3444) | (3460 <= sic & sic <= 3479) ~ 20,
    sic %in% c(3538, 3585, 3586) | (3510 <= sic & sic <= 3536) | (3540 <= sic & sic <= 3569) | (3580 <= sic & sic <= 3582) | 
      (3589 <= sic & sic <= 3599) ~ 21,
    sic %in% c(3600, 3620, 3621, 3648, 3649, 3660, 3699) | (3610 <= sic & sic <= 3613) | (3623 <= sic & sic <= 3629) | 
      (3640 <= sic & sic <= 3646) | (3690 <= sic & sic <= 3692) ~ 22,
    sic %in% c(2296, 2396, 3010, 3011, 3537, 3647, 3694, 3700, 3710, 3711, 3799) | (3713 <= sic & sic <= 3716) | 
      (3790 <= sic & sic <= 3792) ~ 23,
    sic %in% c(3720, 3721, 3728, 3729) | (3723 <= sic & sic <= 3725) ~ 24,
    sic %in% c(3730, 3731) | (3740 <= sic & sic <= 3743) ~ 25,
    sic == 3795 | (3760 <= sic & sic <= 3769) | (3480 <= sic & sic <= 3489) ~ 26,
    (1040 <= sic & sic <= 1049) ~ 27,
    (1000 <= sic & sic <= 1039) | (1050 <= sic & sic <= 1119) | (1400 <= sic & sic <= 1499) ~ 28,
    (1200 <= sic & sic <= 1299) ~ 29,
    sic %in% c(1300, 1389) | (1310 <= sic & sic <= 1339) | (1370 <= sic & sic <= 1382) | (2900 <= sic & sic <= 2912) | 
      (2990 <= sic & sic <= 2999) ~ 30,
    sic %in% c(4900, 4910, 4911, 4939) | (4920 <= sic & sic <= 4925) | (4930 <= sic & sic <= 4932) | (4940 <= sic & sic <= 4942) ~ 31,
    sic %in% c(4800, 4899) | (4810 <= sic & sic <= 4813) | (4820 <= sic & sic <= 4822) | (4830 <= sic & sic <= 4841) | 
      (4880 <= sic & sic <= 4892) ~ 32,
    sic %in% c(7020, 7021, 7200, 7230, 7231, 7240, 7241, 7250, 7251, 7395, 7500, 7600, 7620, 7622, 7623, 7640, 
               7641) | (7030 <= sic & sic <= 7033) | (7210 <= sic & sic <= 7212) | (7214 <= sic & sic <= 7217) | (7219 <= sic & sic <= 7221) | 
      (7260 <= sic & sic <= 7299) | (7520 <= sic & sic <= 7549) | (7629 <= sic & sic <= 7631) | (7690 <= sic & sic <= 7699) | 
      (8100 <= sic & sic <= 8499) | (8600 <= sic & sic <= 8699) | (8800 <= sic & sic <= 8899) | (7510 <= sic & sic <= 7515) ~ 33,
    sic %in% c(3993, 7218, 7300, 7374, 7396, 7397, 7399, 7519, 8700, 8720, 8721) | (2750 <= sic & sic <= 2759) | 
      (7310 <= sic & sic <= 7342) | (7349 <= sic & sic <= 7353) | (7359 <= sic & sic <= 7369) | (7376 <= sic & sic <= 7385) | 
      (7389 <= sic & sic <= 7394) | (8710 <= sic & sic <= 8713) | (8730 <= sic & sic <= 8734) | (8740 <= sic & sic <= 8748) | 
      (8900 <= sic & sic <= 8911) | (8920 <= sic & sic <= 8999) | (4220 <= sic & sic <= 4229) ~ 34,
    sic == 3695 | (3570 <= sic & sic <= 3579) | (3680 <= sic & sic <= 3689) ~ 35,
    sic == 7375 | (7370 <= sic & sic <= 7373) ~ 36,
    sic %in% c(3622, 3810, 3812) | (3661 <= sic & sic <= 3666) | (3669 <= sic & sic <= 3679) ~ 37,
    sic == 3811 | (3820 <= sic & sic <= 3827) | (3829 <= sic & sic <= 3839) ~ 38,
    sic %in% c(2760, 2761) | (2520 <= sic & sic <= 2549) | (2600 <= sic & sic <= 2639) | (2670 <= sic & sic <= 2699) | 
      (3950 <= sic & sic <= 3955) ~ 39,
    sic %in% c(3220, 3221) | (2440 <= sic & sic <= 2449) | (2640 <= sic & sic <= 2659) | (3410 <= sic & sic <= 3412) ~ 40,
    sic %in% c(4100, 4130, 4131, 4150, 4151, 4230, 4231, 4780, 4789) | (4000 <= sic & sic <= 4013) | (4040 <= sic & sic <= 4049) | 
      (4110 <= sic & sic <= 4121) | (4140 <= sic & sic <= 4142) | (4170 <= sic & sic <= 4173) | (4190 <= sic & sic <= 4200) | 
      (4210 <= sic & sic <= 4219) | (4240 <= sic & sic <= 4249) | (4400 <= sic & sic <= 4700) | (4710 <= sic & sic <= 4712) | 
      (4720 <= sic & sic <= 4749) | (4782 <= sic & sic <= 4785) ~ 41,
    sic %in% c(5000, 5099, 5100) | (5010 <= sic & sic <= 5015) | (5020 <= sic & sic <= 5023) | (5030 <= sic & sic <= 5060) | 
      (5063 <= sic & sic <= 5065) | (5070 <= sic & sic <= 5078) | (5080 <= sic & sic <= 5088) | (5090 <= sic & sic <= 5094) | 
      (5110 <= sic & sic <= 5113) | (5120 <= sic & sic <= 5122) | (5130 <= sic & sic <= 5172) | (5180 <= sic & sic <= 5182) | 
      (5190 <= sic & sic <= 5199) ~ 42,
    sic %in% c(5200, 5250, 5251, 5260, 5261, 5270, 5271, 5300, 5310, 5311, 5320, 5330, 5331, 5334, 5900, 5999) | 
      (5210 <= sic & sic <= 5231) | (5340 <= sic & sic <= 5349) | (5390 <= sic & sic <= 5400) | (5410 <= sic & sic <= 5412) | 
      (5420 <= sic & sic <= 5469) | (5490 <= sic & sic <= 5500) | (5510 <= sic & sic <= 5579) | (5590 <= sic & sic <= 5700) | 
      (5710 <= sic & sic <= 5722) | (5730 <= sic & sic <= 5736) | (5750 <= sic & sic <= 5799) | (5910 <= sic & sic <= 5912) | 
      (5920 <= sic & sic <= 5932) | (5940 <= sic & sic <= 5990) | (5992 <= sic & sic <= 5995) ~ 43,
    sic %in% c(7000, 7213) | (5800 <= sic & sic <= 5829) | (5890 <= sic & sic <= 5899) | (7010 <= sic & sic <= 7019) | 
      (7040 <= sic & sic <= 7049) ~ 44,
    sic == 6000 | (6010 <= sic & sic <= 6036) | (6040 <= sic & sic <= 6062) | (6080 <= sic & sic <= 6082) | 
      (6090 <= sic & sic <= 6100) | (6110 <= sic & sic <= 6113) | (6120 <= sic & sic <= 6179) | (6190 <= sic & sic <= 6199) ~ 45,
    sic %in% c(6300, 6350, 6351, 6360, 6361) | (6310 <= sic & sic <= 6331) | (6370 <= sic & sic <= 6379) | 
      (6390 <= sic & sic <= 6411) ~ 46,
    sic %in% c(6500, 6510, 6540, 6541, 6610, 6611) | (6512 <= sic & sic <= 6515) | (6517 <= sic & sic <= 6532) | 
      (6550 <= sic & sic <= 6553) | (6590 <= sic & sic <= 6599) ~ 47,
    sic %in% c(6700, 6798, 6799) | (6200 <= sic & sic <= 6299) | (6710 <= sic & sic <= 6726) | (6730 <= sic & sic <= 6733) | 
      (6740 <= sic & sic <= 6779) | (6790 <= sic & sic <= 6795) ~ 48,
    sic %in% c(4970, 4971, 4990, 4991) | (4950 <= sic & sic <= 4961) ~ 49,
    TRUE ~ NA_real_  # Default case
  )
}

# Prepare ICC data -----------------------------------------
# Datadates much be names datadate, and data must be split and currency adjusted
# Must also have excluded missing gvkey and iid/PERMNO
prepare_icc_data <- function(daily_p, epsf, epsa, acc_g, acc_us, po_pref, bps, dps, rf10, ff49_ind) {
  # EPS forecasts
  epsf_st <- epsf[measure == "EPS" & fpi %in% 1:2, .(id, datadate, fpi = paste0("eps", fpi), value_adj)]
  epsf_st <- epsf_st %>% dcast(id+datadate~fpi, value.var = "value_adj")
  # Extend eps2 (lacking in early years)
  epsf_st[is.na(eps2), eps2 := eps1*(1+0.15)]
  # Coverage [require coverage in past 12 months and compute average]
  numest <- epsf[measure=="EPS" & fpi==1, .(id, datadate, numest)]
  ibes_dates <- sort(unique(numest$datadate))
  numest <- 12:length(ibes_dates) |> lapply(function(i) {
    x <- numest[datadate >= ibes_dates[i-11] & datadate<=ibes_dates[i], .(n=.N, numest1_avg = mean(numest)), by = id]
    x[n==12, .(id, datadate = ibes_dates[i], numest1_avg)]
  }) |> rbindlist()
  # LTG forecasts
  epsf_ltg <- epsf[measure == "EPS" & fpi==0, .(id, datadate, "ltg"=medest/100)]
  # Actuals 
  epsa_ann <- epsa[measure=="EPS" & pdicity=="ANN" &!is.na(value_adj) & !is.infinite(value_adj), .(id, datadate, value_adj)]
  epsa_ann <- epsa_ann[, .(id, "start"=datadate, eps0 = value_adj)][order(id, start)]
  epsa_ann[, end := shift(start, 1, type="lead")-1, by = .(id)]
  epsa_ann[is.na(end), end := floor_date(start, unit="m")+months(13)-1]
  # DPS estimate from I/B/E/S 
  dps_ib <- dps[,.(id, datadate, "ib_dps1"=value_adj)]
  # DPS from Comp
  po_comp <- rbind(
    acc_g[, .(gvkey, datadate, at, dvt, ib, type = "g_funda")],
    acc_us[, .(gvkey, datadate, at, dvt, ib, type = "funda")]
  )
  po_comp <- po_comp[at>0] # non-negative assets seems like a minimal requirement
  po_comp[ib>0, po := dvt/ib]
  po_comp[ib<=0 & at>0, po := dvt/(at*0.06)]
  # Screen firms in both funda and g_funda
  po_comp[, n := .N, by = .(gvkey, datadate)]
  po_comp <- po_comp[n==1 | (n==2 & type==po_pref)][, type := NULL]
  po_comp <- po_comp[!is.na(po)][order(gvkey, datadate)]
  po_comp[, public_date := as.Date(datadate)+1+months(4)-1][, datadate := NULL]  # Assume a lag of 4 months which I think is non-standard
  po_comp[, start := public_date]
  po_comp[, end := lead(public_date, 1)-1, by = gvkey]
  po_comp[is.na(end), end := start+1+years(1)-1]
  po_comp <- po_comp[, .(gvkey, start, end, po)]  # po_comp[, .(gvkey, start, end, po)][end<start] (none)
  po_comp[, p01 := quantile(po, 0.01)]
  po_comp[, p99 := quantile(po, 0.99)] # Maybe just cap at 0 and 1 (but the latter is questionable?
  po_comp[, po := pmin(p99, pmax(po, p01))]
  po_comp <- po_comp[, .(gvkey, start, end, po)]
  # Industry ROE from Compustat (exclude negative earnings) ----
  # Combine US and Int FUNDA
  acc <- rbind(
    funda_g[, .(gvkey, datadate, curcd, fx, ff49, seq, ib, int=T)],
    funda[, .(gvkey, datadate, curcd, fx, ff49, seq, ib, int=F)]
  )
  roe_comp <- acc[, .(gvkey, datadate, curcd, fx, ff49, seq, ib, int)][order(gvkey, int, datadate)]
  roe_comp[, roe := ib/lag(seq, 1), by = .(gvkey, int)]
  roe_comp[, roe_usd := (ib*fx)/lag((seq*fx), 1), by = .(gvkey, int)]
  # Replace ROE with ROE_usd if currency is different 
  roe_comp[, curcd_lag := lag(curcd, 1), by = .(gvkey, int)]
  roe_comp[curcd!=curcd_lag, roe := roe_usd]
  # Check 1 year diff 
  roe_comp[, yr_diff := as.integer(datadate-lag(datadate, 1)), by = .(gvkey, int)]
  roe_comp <- roe_comp[yr_diff %in% 365:366 & !is.na(yr_diff)]# Excludes less than 2%: roe_comp[!is.na(yr_diff), mean(yr_diff %in% 365:366)]
  # MG only use firms with positive ROE
  roe_comp <- roe_comp[roe>0 & !is.na(roe)] 
  # Require industry relationship
  roe_comp <- roe_comp[!is.na(ff49)]
  # Avoid double counting (choose int)
  roe_comp[, n := .N, by = .(gvkey, datadate)]
  roe_comp <- roe_comp[n==1 | (n==2 & int==T)]
  # Industry average ROE over past 10 years
  roe_comp <- roe_comp[, .(gvkey, "public_date"=as.Date(datadate)+1+months(4)-1, roe, ff49)]
  roe_comp <- as.Date(unique(epsf_st$datadate)) %>% lapply(function(m) {
    roe_comp[public_date>=(m+1-years(10)-1) & public_date<=m, .(
      n = .N, 
      roe_ind = median(roe)
    ), by = ff49][, datadate := m]
  }) %>% rbindlist()
  roe_comp[, p01 := quantile(roe_ind, 0.01)]
  roe_comp[, p99 := quantile(roe_ind, 0.99)] # Maybe just cap at 0 and 1 (but the latter is questionable?
  roe_comp[, roe_ind := pmin(p99, pmax(roe_ind, p01))][, c("p01", "p99") := NULL]
  # Book-per-share data
  bps0 <- bps[, .(id, "start"=as.Date(datadate)+1+months(4)-1, "bps0"=bps_adj)][order(id, start)]
  bps0[, end := lead(start, 1), by = .(id)]
  bps0[is.na(end), end := start+1+months(12)-1]
  # Combine -------
  icc_data <- epsf_st[daily_p, on = .(id, datadate)]
  # Screen: Require EPS1 and Price
  icc_data <- icc_data[!is.na(prc_adj) & !is.na(eps1)]
  # Add remaining data
  n_start <- icc_data[,.N]
  icc_data <- numest[icc_data, on = .(id, datadate)]
  icc_data <- epsf_ltg[icc_data, on = .(id, datadate)] 
  icc_data <- dps_ib[icc_data, on = .(id, datadate)]
  icc_data[, merge_date := datadate]
  icc_data <- po_comp[icc_data, on = .(gvkey, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
  icc_data[, merge_date := datadate]
  icc_data <- bps0[icc_data, on = .(id, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
  icc_data[, merge_date := datadate]
  icc_data <- epsa_ann[icc_data, on = .(id, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
  icc_data[, merge_date := datadate]
  icc_data <- rf10[icc_data, on = .(start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
  # Add industry data
  icc_data[, merge_date := datadate]
  icc_data <- ff49_ind[icc_data, on = .(gvkey, start<=merge_date, end>=merge_date)][, c("start", "end") := NULL]
  icc_data <- roe_comp[icc_data, on = .(ff49, datadate)]
  if (nrow(icc_data)!=n_start) {
    warning(paste0("DATA HAS DUPLICATES"))
  }
  # Output
  return(icc_data)
}

# Actual ICC function -------------------------------------------
# Implied Costs of Capital --------------------------
# Ohlson and Juettner-Nauroth (2005, OJ) 
icc_oj_fun <- function(data) {
  oj <- icc_data[!is.na(eps1) & !is.na(eps2) & !is.na(ltg) & !is.na(po), .(id, datadate, prc_adj, rf10, po, eps1, eps2, ltg)]
  oj[, gam_min1 := rf10-0.03]
  oj[, A := 0.5*(gam_min1+eps1*po/prc_adj)]
  # oj[, stg := ((eps2-eps1)/eps1*ltg)^0.5] # Max ensures that STG=LTG if (see p. 450 of Mohanram and Gode)
  oj[, stg := pmax(((eps2-eps1)/eps1*ltg)^0.5, ltg)]
  oj[, icc_oj := A + sqrt(A^2+eps1/prc_adj*(stg-gam_min1))] 
  # Output
  oj[, .(id, datadate, icc_oj)]
}
# PEG (Easton (2004) and Mohanram and Gode)
icc_peg_fun <- function(data) {
  peg <- icc_data[!is.na(eps1) & !is.na(eps2) & !is.na(ltg), .(id, datadate, prc_adj, rf10, po, eps1, eps2, ltg)]
  # peg[, stg := ((eps2-eps1)/eps1*ltg)^0.5] # Max ensures that STG=LTG if (see p. 450 of Mohanram and Gode)
  peg[, stg := pmax(((eps2-eps1)/eps1*ltg)^0.5, ltg)]
  peg[, icc_peg := sqrt(eps1/prc_adj*stg)] # Max ensures that STG=LTG if (see p. 450 of Mohanram and Gode)
  # Output
  peg[, .(id, datadate, icc_peg)]
}
# Gebhardt, Lee, and Swaminathan (2001)
icc_gls_fun <- function(data, root_low, root_high) { 
  gls <- data[, .(id, datadate, eps1, eps2, bps0, prc_adj, po, roe_ind)] %>% na.omit()
  gls <- gls[!is.infinite(eps1) & !is.infinite(bps0)]
  gls[, bps1 := bps0+eps1*(1-po)]
  gls[, bps2 := bps1+eps2*(1-po)]
  gls[, roe1 := eps1/bps0]
  gls[, roe2 := eps2/bps1]
  gls[, g := (roe_ind-roe2)/10]
  for (h in 3:12) {
    gls[, (paste0("roe", h)) := roe2+g*(h-2)]
    if (TRUE) {
      gls[, (paste0("bps", h)) := get(paste0("bps", h-1))*(1+get(paste0("roe", h))*(1-po))]
    } else { # Write it like below
      gls[, (paste0("eps", h)) := get(paste0("roe", h))*get(paste0("bps", h-1))]
      gls[, (paste0("bps", h)) := get(paste0("bps", h-1))+get(paste0("eps", h))*(1-po)]
    }
  }
  gls_func <- function(r, p, 
                       bps0, bps1, bps2, bps3, bps4, bps5, bps6, bps7, bps8, bps9, bps10, bps11,
                       roe1, roe2, roe3, roe4, roe5, roe6, roe7, roe8, roe9, roe10, roe11, roe12) {
    p - (
      bps0 +
        ((roe1-r)*bps0)/(1+r)^1 +
        ((roe2-r)*bps1)/(1+r)^2 +
        ((roe3-r)*bps2)/(1+r)^3 +
        ((roe4-r)*bps3)/(1+r)^4 +
        ((roe5-r)*bps4)/(1+r)^5 +
        ((roe6-r)*bps5)/(1+r)^6 +
        ((roe7-r)*bps6)/(1+r)^7 +
        ((roe8-r)*bps7)/(1+r)^8 +
        ((roe9-r)*bps8)/(1+r)^9 +
        ((roe10-r)*bps9)/(1+r)^10 +
        ((roe11-r)*bps10)/(1+r)^11 +
        ((roe12-r)*bps11)/((r*(1+r)^11))) 
  }
  
  # Check that range is of same sign
  gls <- gls %>%
    rowwise() %>%
    mutate(
      int_low = gls_func(r = root_low, p=prc_adj, 
                         bps0=bps0, bps1=bps1, bps2=bps2, bps3=bps3, bps4=bps4, bps5=bps5,
                         bps6=bps6, bps7=bps7, bps8=bps8, bps9=bps9, bps10=bps11, bps11=bps11,
                         roe1=roe1, roe2=roe2, roe3=roe3, roe4=roe4, roe5=roe5, roe6=roe6, 
                         roe7=roe7, roe8=roe8, roe9=roe9, roe10=roe10, roe11=roe11, roe12=roe12),
      int_high = gls_func(r = root_high, p=prc_adj, 
                          bps0=bps0, bps1=bps1, bps2=bps2, bps3=bps3, bps4=bps4, bps5=bps5,
                          bps6=bps6, bps7=bps7, bps8=bps8, bps9=bps9, bps10=bps11, bps11=bps11,
                          roe1=roe1, roe2=roe2, roe3=roe3, roe4=roe4, roe5=roe5, roe6=roe6, 
                          roe7=roe7, roe8=roe8, roe9=roe9, roe10=roe10, roe11=roe11, roe12=roe12)
    )
  gls %>% ungroup() %>% summarise(mean(sign(int_low)==sign(int_high), na.rm=T))
  # Fit
  gls <- gls %>%
    filter(sign(int_high) != sign(int_low) & !is.na(int_low)) %>%
    mutate(
      icc_gls = uniroot(gls_func, interval = c(root_low, root_high), p=prc_adj, 
                        bps0=bps0, bps1=bps1, bps2=bps2, bps3=bps3, bps4=bps4, bps5=bps5,
                        bps6=bps6, bps7=bps7, bps8=bps8, bps9=bps9, bps10=bps11, bps11=bps11,
                        roe1=roe1, roe2=roe2, roe3=roe3, roe4=roe4, roe5=roe5, roe6=roe6, 
                        roe7=roe7, roe8=roe8, roe9=roe9, roe10=roe10, roe11=roe11, roe12=roe12)$root
    )
  # Output
  gls %>% ungroup() %>% select(id, datadate, icc_gls) %>% setDT()
}
# Claus and Thomas (2001) --------------
icc_ct_fun <- function(data, root_low, root_high) {
  ct <- data[, .(id, datadate, eps1, eps2, ltg, bps0, prc_adj, po, roe_ind, g=rf10-0.03)] %>% na.omit()
  ct <- ct[!is.infinite(eps1) & !is.infinite(bps0)]
  ct[, bps1 := bps0+eps1*(1-po)]
  ct[, bps2 := bps1+eps2*(1-po)]
  ct[, roe1 := eps1/bps0]
  ct[, roe2 := eps2/bps1]
  for (h in 3:5) {
    ct[, (paste0("eps", h)) := get(paste0("eps", h-1))*(1+ltg)]
    ct[, (paste0("roe", h)) := get(paste0("eps", h))/get(paste0("bps", h-1))]
    ct[, (paste0("bps", h)) := get(paste0("bps", h-1))+get(paste0("eps", h))*(1-po)]
  }
  
  ct_func <- function(r, p, g,
                      bps0, bps1, bps2, bps3, bps4,
                      roe1, roe2, roe3, roe4, roe5) {
    p - (
      bps0 +
        ((roe1-r)*bps0)/(1+r)^1 +
        ((roe2-r)*bps1)/(1+r)^2 +
        ((roe3-r)*bps2)/(1+r)^3 +
        ((roe4-r)*bps3)/(1+r)^4 +
        ((roe5-r)*bps4)/(1+r)^5 +
        ((roe5-r)*(1+g)*bps4)/((r-g)*(1+r)^5)) 
  }
  
  # Check that range is of same sign
  ct <- ct %>%
    rowwise() %>%
    mutate(
      r_low = pmax(root_low, g+0.001),
      int_low = ct_func(r = r_low, p=prc_adj, g=g, 
                        bps0=bps0, bps1=bps1, bps2=bps2, bps3=bps3, bps4=bps4, 
                        roe1=roe1, roe2=roe2, roe3=roe3, roe4=roe4, roe5=roe5),
      int_high = ct_func(r = root_high, p=prc_adj, g=g, 
                         bps0=bps0, bps1=bps1, bps2=bps2, bps3=bps3, bps4=bps4,
                         roe1=roe1, roe2=roe2, roe3=roe3, roe4=roe4, roe5=roe5)
    )
  
  ct %>% ungroup() %>% summarise(mean(sign(int_low)==sign(int_high), na.rm=T))
  
  # Fit
  ct <- ct %>%
    filter(sign(int_high) != sign(int_low) & !is.na(int_low)) %>%
    mutate(
      icc_ct = uniroot(ct_func, interval = c(r_low, root_high), p=prc_adj, g=g, 
                       bps0=bps0, bps1=bps1, bps2=bps2, bps3=bps3, bps4=bps4, 
                       roe1=roe1, roe2=roe2, roe3=roe3, roe4=roe4, roe5=roe5)$root
    )
  # Output
  ct %>% ungroup() %>% select(id, datadate, icc_ct) %>% setDT()
}