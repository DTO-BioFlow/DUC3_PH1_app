library(dplyr)
library(tidyr)

df <- read.csv(
  "https://raw.githubusercontent.com/DTO-BioFlow/DUC3_dataset_inventory/refs/heads/main/data_sets/EDITO_dasid_4687_SCHPM1_holo_mero.csv"
)


# Convert from wide to long
df_long <- df %>%
  pivot_longer(
    cols = c(holoplankton, meroplankton),
    names_to = "lifeform",
    values_to = "abundance"
  ) %>%
  select(period, lifeform, abundance, num_samples)

# Check the result
head(df_long, n=Inf)
