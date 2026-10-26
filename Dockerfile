# Use the Rocker Shiny image
FROM rocker/shiny:4.3

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies required by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    cmake \
    curl \
    gdal-bin \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libv8-dev \
    libpng-dev \
    libjpeg-dev \
    zlib1g-dev \
    libproj-dev \
    libgdal-dev \
    libgeos-dev \
    libudunits2-dev \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server

# Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Copy the lockfile first to improve Docker layer caching
COPY renv.lock ./renv.lock

# Restore the R environment from the lockfile
RUN R -e "renv::restore(lockfile='/srv/shiny-server/renv.lock', prompt=FALSE)"

# Copy the Shiny application after restoring packages
COPY . .

# Expose Shiny port
EXPOSE 3838

# Start the Shiny application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
