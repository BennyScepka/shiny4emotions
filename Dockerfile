# Use R base image
FROM r-base:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    libsodium-dev \
    libgeos-dev \
    libudunits2-dev \
    libgdal-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Set CRAN mirror
RUN echo "options(repos = c(CRAN = 'https://cloud.r-project.org/'))" > /usr/lib/R/etc/Rprofile.site

# Install remotes package first for better dependency handling
RUN R -e "install.packages('remotes', dependencies=TRUE)"

# Create app directory
RUN mkdir /app
WORKDIR /app

# Copy all files except those in .dockerignore
COPY . .

# Install core tidyverse packages first
RUN R -e "remotes::install_version('tidyverse', dependencies=TRUE, upgrade=FALSE)"

# Install each package separately with error handling
RUN R -e "\
    packages <- c('shiny', 'shinyBS', 'bslib', 'bsicons', 'jsonlite', 'data.table', \
                 'visNetwork', 'wordcloud2', 'stringr', 'syuzhet', 'igraph', 'udpipe', \
                 'text2vec', 'tidyr', 'ggplot2', 'DT', 'dplyr', 'tibble', 'radarchart', \
                 'fmsb', 'ggraph', 'tidygraph', 'purrr', 'stringdist', 'plotly', \
                 'networkD3', 'scales', 'glmnet', 'broom', 'digest', 'circlize', 'shinyjs'); \
    for(pkg in packages) { \
        tryCatch({ \
            if (!requireNamespace(pkg, quietly = TRUE)) { \
                message(sprintf('Installing %s', pkg)); \
                install.packages(pkg, dependencies=TRUE, quiet=TRUE); \
            } \
        }, error = function(e) { \
            message(sprintf('Error installing %s: %s', pkg, e$message)); \
        }) \
    }"

# Verify installation
RUN R -e "installed <- installed.packages()[,'Package']; \
    required <- c('shiny', 'shinyBS', 'bslib', 'bsicons', 'jsonlite', 'data.table', \
                 'visNetwork', 'wordcloud2', 'stringr', 'syuzhet', 'igraph', 'udpipe', \
                 'text2vec', 'tidyr', 'ggplot2', 'DT', 'dplyr', 'tibble', 'radarchart', \
                 'fmsb', 'ggraph', 'tidygraph', 'purrr', 'stringdist', 'plotly', \
                 'networkD3', 'scales', 'glmnet', 'broom', 'digest', 'circlize', 'shinyjs'); \
    missing <- setdiff(required, installed); \
    if(length(missing) > 0) stop('Missing packages: ', paste(missing, collapse=', '))"

# Expose port 3838 (default Shiny port)
EXPOSE 3838

# Create script to run app with error handling
RUN echo '#!/usr/bin/env Rscript \n\
tryCatch({ \n\
    library(shiny) \n\
    options(shiny.port = 3838, shiny.host = "0.0.0.0") \n\
    message("Starting Shiny app...") \n\
    runApp("/app") \n\
}, error = function(e) { \n\
    message("Error starting Shiny app: ", e$message) \n\
    quit(status = 1) \n\
})' > /app/run.R \
    && chmod +x /app/run.R

# Run the script
CMD ["/usr/bin/Rscript", "/app/run.R"]
