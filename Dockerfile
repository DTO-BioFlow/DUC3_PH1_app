# Use official R Shiny image
FROM rocker/shiny:latest

# Disable renv auto-activation
ENV RENV_CONFIG_AUTOLOAD=FALSE

# Install system dependencies including build tools for compiled packages
RUN apt-get update && apt-get install -y \
    build-essential \
    xz-utils \
    cmake \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libcairo2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server/

# Copy only requirements first to leverage Docker cache
COPY requirements.txt .

# Install R packages listed in requirements.txt with full debugging
RUN R -e 'cat("===== R VERSION =====\n"); \
          print(R.version.string); \
          cat("\n===== LIB PATHS =====\n"); \
          print(.libPaths()); \
          cat("\n===== SYSTEM LIB VERSIONS =====\n"); \
          system("gdal-config --version"); \
          system("proj 2>&1 | head -n 1"); \
          system("geos-config --version"); \
          cat("\n===== READING REQUIREMENTS =====\n"); \
          packages <- trimws(readLines("requirements.txt")); \
          packages <- packages[packages != "" & !startsWith(packages, "#")]; \
          print(packages); \
          cat("\n===== INSTALLING PACKAGES =====\n"); \
          options(repos = c(CRAN="https://cran.rstudio.com/")); \
          install_success <- sapply(packages, function(p) { \
              tryCatch({ \
                  install.packages(p, dependencies=TRUE); \
                  TRUE \
              }, warning=function(w) { \
                  cat("WARNING installing", p, ":\n"); print(w); TRUE \
              }, error=function(e) { \
                  cat("ERROR installing", p, ":\n"); print(e); FALSE \
              }) \
          }); \
          cat("\n===== INSTALL RESULTS =====\n"); \
          print(data.frame(package=packages, installed=install_success)); \
          cat("\n===== INSTALLED PACKAGES =====\n"); \
          installed <- installed.packages()[,"Package"]; \
          print(installed); \
          cat("\n===== MISSING PACKAGES =====\n"); \
          missing <- packages[!install_success | !(packages %in% installed)]; \
          print(missing); \
          if(length(missing)) { \
              cat("\nBuild failing because these packages are missing:\n"); \
              stop(paste(missing, collapse=", ")); \
          } else { \
              cat("\nAll requested packages installed successfully.\n"); \
          }'

# Copy the rest of the app
COPY . .

# Expose Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]
