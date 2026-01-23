check_data_PH1 <- function(x) {
  
  result <- list(
    pass = FALSE,
    message = NULL
  )
  
  tryCatch({
    
    # ---- Load data ----
    if (is.character(x)) {
      df <- read.csv(x, stringsAsFactors = FALSE)
    } else if (is.data.frame(x)) {
      df <- x
    } else {
      stop("Input must be a data.frame or CSV path/URL.")
    }
    
    # ---- Column count ----
    if (ncol(df) != 4) {
      stop("Data must contain exactly 4 columns: period, two abundance columns, num_samples")
    }
    
    # ---- Column positions ----
    period_col <- df[[1]]
    abundance_cols <- df[, 2:3]
    num_samples_col <- df[[4]]
    
    # ---- period ----
    if (!is.character(period_col)) {
      stop("First column 'period' must be character.")
    }
    if (any(!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", period_col))) {
      stop("'period' must follow YYYY-MM format.")
    }
    
    # ---- abundance columns ----
    if (!all(sapply(abundance_cols, is.numeric))) {
      stop("The two middle columns must be numeric (abundance data).")
    }
    
    # ---- num_samples ----
    if (!is.numeric(num_samples_col)) {
      stop("Last column 'num_samples' must be numeric.")
    }
    if (any(num_samples_col %% 1 != 0)) {
      stop("'num_samples' must contain integers only.")
    }
    
    # ---- Success ----
    result$pass <- TRUE
    result$message <- "Validation passed"
    
  }, error = function(e) {
    result$message <- e$message
  })
  
  return(result)
}