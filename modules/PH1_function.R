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

#' Run PH1 analysis pipeline (Matthew Holland methodology)
#'
#' Executes the full PH1 analytical workflow as defined in the Matthew Holland
#' reference notebook. This function performs data restructuring, temporal
#' parsing, cleaning, transformation, gap-filling, reference envelope
#' construction, Pelagic Habitat (PH1) indicator calculation, trend analysis,
#' and visualization. The function is designed as a complete in-memory
#' processing pipeline and returns all major outputs required for interpretation
#' and reporting.
#'
#'
#' @param df A data frame containing the raw monitoring data. Must contain at
#'   least the following columns:
#'   \itemize{
#'     \item \code{period}: Character string of the form \code{"YYYY-MM"}
#'       representing year and month.
#'     \item \code{num_samples}: Integer number of samples per time step.
#'     \item Two or more lifeform columns containing abundance values (numeric,
#'       floating point). Column names are user-defined and selected using
#'       \code{lf1} and \code{lf2}.
#'   }
#'
#' @param lf1 Character string giving the column name of the first lifeform
#'   variable in \code{df}.
#'
#' @param lf2 Character string giving the column name of the second lifeform
#'   variable in \code{df}.
#'
#' @param ref_years Integer vector of years defining the reference period
#'   (e.g., \code{c(2000, 2010)}).
#'
#' @param comp_years Integer vector of years defining the comparison period
#'   (e.g., \code{c(2011, 2020)}).
#'
#' @param mon_thr Integer threshold specifying the minimum number of monthly
#'   observations required per year for data retention. Defaults to 8.
#'
#' @return A named list containing the full PH1 analysis outputs:
#'   \itemize{
#'     \item \code{env_plots}: Envelope plots for reference and comparison
#'       periods.
#'     \item \code{ts_plots}: Time-series plots of lifeform abundance and trend
#'       fits.
#'     \item \code{polygon_maps}: Spatial polygon maps corresponding to
#'       assessment units.
#'     \item \code{datasets}: A list containing:
#'       \itemize{
#'         \item \code{Kendall_results}: Kendall trend test results.
#'         \item \code{PI_results}: PH1 indicator values.
#'         \item \code{PI_annual_results}: Annual PH1 indicator values.
#'         \item \code{Assessment_ids}: Assessment unit identifiers (if more
#'           than one unit exists).
#'       }
#'     \item \code{df_plot}: Processed time-series dataset used for plotting.
#'     \item \code{envAll}: Computed reference envelope objects.
#'   }
#'
#'
#' @examples
#' \dontrun{
#' results <- run_ph1_analysis(
#'   df = my_data,
#'   lf1 = "diatoms",
#'   lf2 = "dinoflagellates",
#'   ref_years = c(2000, 2010),
#'   comp_years = c(2011, 2020),
#'   mon_thr = 8
#' )
#' }
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
  
  # Create df_lf_user
  df_lf_user <- data.frame(V1 = lf1, V2 = lf2)
  
  # Check if df_lf_user is already in df_lf
  if (!any(apply(df_lf, 1, function(row) all(row == df_lf_user)))) {
    df_lf <- rbind(df_lf, df_lf_user)
  }
  
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
