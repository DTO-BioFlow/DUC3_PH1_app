library(data.table)
library(janitor)
library(pracma)
library(broom)
library(EnvStats)
library(patchwork)
library(rnaturalearth)
library(zoo)
library(purrr)

library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(ggplot2)

source("modules/Supporting_scripts/PI_functions_v1.R")
source("modules/Supporting_scripts/Supporting_functions_v2.R")

run_ph1_analysis <- function(df,
                             lf1,
                             lf2,
                             ref_years,
                             comp_years,
                             mon_thr = 8) {
  
  df <- df %>%
    pivot_longer(
      cols = c(lf1, lf2),
      names_to = "lifeform",
      values_to = "abundance"
    ) %>%
    select(period, lifeform, abundance, num_samples)
  
  print(df)
  
  # ---------------------------------------------------------------------------
  # Parse dates
  # ---------------------------------------------------------------------------
  dates <- read.table(
    text = as.character(df$period),
    sep = "-",
    stringsAsFactors = FALSE
  )
  colnames(dates) <- c("year", "month")
  
  df <- cbind(dates, df) %>%
    dplyr::select(-period) %>%
    dplyr::filter(
      year >= min(c(ref_years, comp_years)),
      year <= max(c(ref_years, comp_years))
    )
  
  rm(dates)
  
  # ---------------------------------------------------------------------------
  # Create assessment IDs
  # ---------------------------------------------------------------------------
  assess_list <- create_assess_id(x = df)
  
  df <- assess_list[[1]]
  df_assess_id <- assess_list[[2]]
  
  # ---------------------------------------------------------------------------
  # Polygon maps
  # ---------------------------------------------------------------------------
  polygon_maps <- plot_polys(x = df_assess_id, buff = 2)
  
  # ---------------------------------------------------------------------------
  # Transform, clean, and gap-fill
  # ---------------------------------------------------------------------------
  df <- log_transform(x = df, method = 1)
  
  df <- clean_years(x = df, thr = mon_thr)
  
  df <- fill_gaps(x = df, max_gap = 3)
  
  # ---------------------------------------------------------------------------
  # Split reference and comparison periods
  # ---------------------------------------------------------------------------
  df_ref  <- dataSelect(x = df, lf = df_lf, lims = ref_years)
  df_comp <- dataSelect(x = df, lf = df_lf, lims = comp_years)
  
  # ---------------------------------------------------------------------------
  # Reference envelopes and PI calculation
  # ---------------------------------------------------------------------------
  envAll <- find_envAll(x = df_ref, lf = df_lf)
  
  piResults <- PIcalcAll(
    x = envAll,
    y = df_comp,
    z = df_ref,
    lf = df_lf
  )
  
  piResultsAnnual <- suppressWarnings(
    PIcalcAnnual(
      x = envAll,
      y = df_comp,
      z = df_ref,
      lf = df_lf
    )
  )
  
  # ---------------------------------------------------------------------------
  # Plot envelopes
  # ---------------------------------------------------------------------------
  env_plots <- plot_env(
    x = envAll,
    y = df_ref,
    z = df_comp,
    lf = df_lf,
    pi = piResults
  )
  
  # ---------------------------------------------------------------------------
  # Kendall trend analysis
  # ---------------------------------------------------------------------------
  df_fits_tot <- kendallAll(x = df, seasonal = FALSE)
  
  # ---------------------------------------------------------------------------
  # Time-series preparation and plotting
  # ---------------------------------------------------------------------------
  df_plot <- create_ts(x = df, y = df_fits_tot)
  
  ts_plots <- plot_ts(x = df_plot)
  
  # ---------------------------------------------------------------------------
  # Assemble outputs
  # ---------------------------------------------------------------------------
  if (nrow(df_assess_id) > 1) {
    datasets <- list(
      Kendall_results     = df_fits_tot,
      PI_results          = piResults,
      PI_annual_results   = piResultsAnnual,
      Assessment_ids      = df_assess_id
    )
  } else {
    datasets <- list(
      Kendall_results     = df_fits_tot,
      PI_results          = piResults,
      PI_annual_results   = piResultsAnnual
    )
  }
  
  # ---------------------------------------------------------------------------
  # Return everything in memory
  # ---------------------------------------------------------------------------
  return(list(
    env_plots     = env_plots,
    ts_plots      = ts_plots,
    polygon_maps  = polygon_maps,
    datasets      = datasets,
    df_plot       = df_plot,
    envAll        = envAll
  ))
}
