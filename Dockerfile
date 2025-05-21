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

# Set CRAN mirror and R library path
ENV R_LIBS_USER=/usr/local/lib/R/site-library
RUN mkdir -p $R_LIBS_USER
RUN echo "options(repos = c(CRAN = 'https://cloud.r-project.org/'))" > /usr/lib/R/etc/Rprofile.site

# Create app directory
RUN mkdir /app
WORKDIR /app

# Copy all files except those in .dockerignore
COPY . .

# Install all required R packages
RUN Rscript -e '\
    install.packages("remotes"); \
    packages <- c("shiny", "shinyBS", "bslib", "bsicons", "jsonlite", "data.table", \
                 "visNetwork", "wordcloud2", "stringr", "syuzhet", "igraph", "udpipe", \
                 "text2vec", "tidyr", "ggplot2", "DT", "dplyr", "tibble", "radarchart", \
                 "fmsb", "ggraph", "tidygraph", "purrr", "stringdist", "plotly", \
                 "networkD3", "scales", "glmnet", "broom", "digest", "circlize", "shinyjs"); \
    install.packages(packages)'

# Verify installation and create a startup script that loads required packages
RUN Rscript -e '\
    required <- c("shiny", "shinyBS", "bslib", "bsicons", "jsonlite", "data.table", \
                 "visNetwork", "wordcloud2", "stringr", "syuzhet", "igraph", "udpipe", \
                 "text2vec", "tidyr", "ggplot2", "DT", "dplyr", "tibble", "radarchart", \
                 "fmsb", "ggraph", "tidygraph", "purrr", "stringdist", "plotly", \
                 "networkD3", "scales", "glmnet", "broom", "digest", "circlize", "shinyjs"); \
    missing <- setdiff(required, installed.packages()[,"Package"]); \
    if(length(missing) > 0) stop("Missing packages: ", paste(missing, collapse=", "))' && \
    echo '#!/usr/bin/env Rscript \n\
    tryCatch({ \n\
        library(shiny) \n\
        library(shinyBS) \n\
        library(bslib) \n\
        library(bsicons) \n\
        library(jsonlite) \n\
        library(data.table) \n\
        library(visNetwork) \n\
        library(wordcloud2) \n\
        library(stringr) \n\
        library(syuzhet) \n\
        library(igraph) \n\
        library(udpipe) \n\
        library(text2vec) \n\
        library(tidyr) \n\
        library(ggplot2) \n\
        library(DT) \n\
        library(dplyr) \n\
        library(tibble) \n\
        library(radarchart) \n\
        library(fmsb) \n\
        library(ggraph) \n\
        library(tidygraph) \n\
        library(purrr) \n\
        library(stringdist) \n\
        library(plotly) \n\
        library(networkD3) \n\
        library(scales) \n\
        library(glmnet) \n\
        library(broom) \n\
        library(digest) \n\
        library(circlize) \n\
        library(shinyjs) \n\
        options(shiny.port = 3838, shiny.host = "0.0.0.0") \n\
        message("Starting Shiny app...") \n\
        runApp("/app") \n\
    }, error = function(e) { \n\
        message("Error starting Shiny app: ", e$message) \n\
        quit(status = 1) \n\
    })' > /app/run.R && chmod +x /app/run.R

# Expose port 3838 (default Shiny port)
EXPOSE 3838

# Run the script
CMD ["/usr/bin/Rscript", "/app/run.R"]
