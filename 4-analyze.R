# Analyze ICC estimates ---------------------------------------------------------
icc <- rbind(
  read_parquet("OUTPUT/icc_comp.parquet") |> setDT() |> _[, source := "Compustat"],
  read_parquet("OUTPUT/icc_us.parquet") |> setDT() |> _[, source := "CRSP"]
)

# Summarize: median ICC and number of firms by month and source
icc_long <- icc |>
  melt(id.vars = c("id", "datadate", "numest1_avg", "source"),
       measure.vars = c("icc_oj", "icc_peg", "icc_gls", "icc_ct"),
       variable.name = "method", value.name = "icc") |>
  _[!is.na(icc)]

icc_summary <- icc_long[, .(median_icc = median(icc), n_firms = uniqueN(id)),
                         by = .(datadate, method, source)]

# Plot 1: Median ICC over time
icc_summary |> 
  ggplot(aes(datadate, median_icc, colour = method)) +
  geom_line() +
  facet_wrap(~source) +
  labs(x = NULL, y = "Median ICC", colour = "Method") +
  theme_minimal()

# Plot 2: Number of firms over time
icc_summary |> 
  ggplot(aes(datadate, n_firms, colour = method)) +
  geom_line() +
  facet_wrap(~source) +
  labs(x = NULL, y = "Number of firms", colour = "Method") +
  theme_minimal()
