# =========================================
# Script: create_renv_lock.R
# Purpose: Automatically generate renv.lock for your Shiny app
# =========================================

# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

library(renv)

# 1️⃣ Initialize renv in the project (does not modify existing packages)
if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
}

# 2️⃣ Detect all R and Rmd files recursively
files <- list.files(
  path = ".", 
  pattern = "\\.(R|Rmd)$", 
  recursive = TRUE, 
  full.names = TRUE
)

# 3️⃣ Scan each file individually to avoid "length > 1" bug
deps_list <- lapply(files, function(f) {
  message("Scanning dependencies in: ", f)
  renv::dependencies(f)
})

# Flatten results
deps <- do.call(rbind, deps_list)

# 4️⃣ Remove duplicates and base/system packages
deps <- deps[!deps$Package %in% c("R", "base", "stats", "utils", "methods", "graphics", "grDevices"), ]
pkgs_to_snapshot <- unique(deps$Package)

# terra 1.8-29 requires an Rcpp version that supports its eight-argument
# Rcpp module constructors. Install the compatible version before snapshotting
# so the generated lockfile does not retain an older local Rcpp installation.
renv::install("Rcpp@1.1.0")
pkgs_to_snapshot <- union(pkgs_to_snapshot, "Rcpp")

message("Packages to snapshot:")
print(pkgs_to_snapshot)

# 5️⃣ Snapshot only these packages to renv.lock
renv::snapshot(packages = pkgs_to_snapshot, prompt = FALSE)

message("✅ renv.lock created successfully!")
