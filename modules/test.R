
df <- read.csv("https://raw.githubusercontent.com/DTO-BioFlow/DUC3_dataset_inventory/refs/heads/main/data_sets/EDITO_dasid_4687_SCHPM1_holo_mero.csv")

source("modules/PH1_function.R")




results <- run_ph1_analysis(
  df = df,
  ref_years  = c(2015, 2018),
  comp_years = c(2018, 2025),
  mon_thr    = 8
)

# Access components
results$datasets$PI_results
results$env_plots
results$ts_plots