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
      stop("Data must contain exactly 4 columns.")
    }
    
    # ---- Column names & order ----
    expected_names <- c("period", "lifeform", "abundance", "num_samples")
    if (!identical(names(df), expected_names)) {
      stop("Column names or order are incorrect.")
    }
    
    # ---- period ----
    if (!is.character(df$period)) {
      stop("'period' must be character.")
    }
    if (any(!grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", df$period))) {
      stop("'period' must follow YYYY-MM format.")
    }
    
    # ---- lifeform ----
    if (!is.character(df$lifeform)) {
      stop("'lifeform' must be character.")
    }
    
    # ---- abundance ----
    if (!is.numeric(df$abundance)) {
      stop("'abundance' must be numeric.")
    }
    
    # ---- num_samples ----
    if (!is.numeric(df$num_samples)) {
      stop("'num_samples' must be numeric.")
    }
    if (any(df$num_samples %% 1 != 0)) {
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