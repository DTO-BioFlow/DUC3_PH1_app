# Use current Shiny image (Ubuntu 24.04 base)
FROM rocker/shiny:4.5

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
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

# Install renv first (separate layer for caching)
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Copy renv files first (so package restore is cached unless lockfile changes)
COPY renv.lock renv.lock
COPY renv/ renv/

# Restore packages
RUN R -e "renv::restore(lockfile='renv.lock', prompt=FALSE, clean=TRUE)"

# Copy entire app AFTER packages are restored
COPY . .

# Expose port
EXPOSE 3838

# Run Shiny
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
