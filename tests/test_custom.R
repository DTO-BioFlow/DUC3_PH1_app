library(here)

# Ensure path is relative to project root
source(here("modules/PH1_function.R"))

df <- read.csv(
  "https://raw.githubusercontent.com/DTO-BioFlow/DUC3_dataset_inventory/refs/heads/main/data_sets/EDITO_dasid_4687_SCHPM1_holo_mero.csv"
)

head(df, Inf)
sapply(df, class)

results <- run_ph1_analysis(
  df = df,
  ref_years  = c(2015, 2018),
  comp_years = c(2018, 2025),
  lf1 = "meroplankton",
  lf2 = "holoplankton",
  mon_thr    = 8
)

results$datasets$PI_results
results$env_plots
results$ts_plots