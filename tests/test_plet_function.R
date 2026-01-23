library(arrow)
library(dplyr)
library(here)


# Ensure path is relative to project root
source(here("modules/PH1_function.R"))

# ------------------------------------------
# GET DATA
# ------------------------------------------
fs <- S3FileSystem$create(
  anonymous = TRUE,
  scheme = "https",
  endpoint_override = "minio.dive.edito.eu"
)

parquet_path <- "oidc-willemboone/PLET/assessment_data.parquet"


assessment <- read_parquet(
  fs$path(parquet_path)
)


my_selection <- assessment |> 
  filter(region_id == 'SNS',
         dataset_name == 'BE_Flanders_Marine_Institute_VLIZ_LW_VLIZ_zoo') |> 
  collect()  # collect() works if assessment is a Dataset, for Arrow Table it's already in memory, so collect() is optional

# 5. Inspect the result
print(my_selection)


convert_lifeform_df <- function(df, lf1, lf2) {
  df %>%
    mutate(period = as.character(period)) %>%
    filter(grepl("^\\d{4}-\\d{2}$", period)) %>%
    select(period, numSamples, all_of(c(lf1, lf2))) %>%
    pivot_longer(
      cols = all_of(c(lf1, lf2)),
      names_to = "lifeform",
      values_to = "abundance"
    ) %>%
    rename(num_samples = numSamples) %>%
    # Ensure every period has both lifeforms (abundance stays NA if missing)
    complete(period, lifeform = c(lf1, lf2), fill = list(abundance = NA, num_samples = NA)) %>%
    arrange(period, lifeform)
}


new_df = convert_lifeform_df(df = my_selection,
                             lf1= "meroplankton",
                             lf2 = "holoplankton")


results <- run_ph1_analysis(
  df = new_df,
  ref_years  = c(2015, 2018),
  comp_years = c(2018, 2025),
  mon_thr    = 8
)

results$datasets$PI_results
results$env_plots
results$ts_plots

