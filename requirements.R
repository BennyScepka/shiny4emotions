#-------------------------------------------------------------
# Requirements Script for Persuasion Analysis App
#-------------------------------------------------------------

# List of required packages with usage verified
required_packages <- c(
  # UI and Shiny packages - all used
  "shiny",       # Core Shiny functionality
  "shinyBS",     # Bootstrap components for Shiny
  "bslib",       # Bootstrap theming for Shiny
  "bsicons",     # Icons for the UI
  "shinyjs",     # JavaScript operations in Shiny
  
  # Data handling - all used
  "jsonlite",    # Used for JSON data reading
  "data.table",  # Fast data manipulation
  "dplyr",       # Data transformation
  "tidyr",       # Data tidying
  "tibble",      # Modern data frames
  "stringr",     # String manipulation
  
  
  # Visualization - all needed
  "ggplot2",     # Plot generation
  "plotly",      # Interactive plots
  "DT",          # Data tables
  "visNetwork",  # Network visualization
  "networkD3",   # Additional network viz
  "fmsb",        # For radar charts
  "radarchart",  # Alternative radar charts
  "circlize",    # For chord diagrams
  "ggraph",      # Graph visualization
  "tidygraph",   # Graph data structures
  "scales",      # Scale functions for visualization
  "wordcloud2",  # Used in some views
  
  # Text analysis
  "syuzhet",     # Sentiment analysis
  "text2vec",    # Text vectorization
  "udpipe",      # NLP processing
  
  # Network analysis
  "igraph",      # Graph theory and analysis
  
  # Statistical modeling
  "glmnet",      # Used for regression models
  "broom",       # Tidying model outputs
  "purrr",       # Functional programming tools
  
  # Utilities
  "digest",      # Used for caching
  "stringdist",  # String distance calculations
  "reticulate"   # Python interoperability
)

# Function to check and install missing packages
check_and_install_packages <- function() {
  # Get the list of installed packages
  installed_packages <- installed.packages()[, "Package"]
  
  # Find which required packages are not installed
  missing_packages <- required_packages[!(required_packages %in% installed_packages)]
  
  # Install missing packages if any
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
    
    # Check if all packages were successfully installed
    still_missing <- missing_packages[!(missing_packages %in% installed.packages()[, "Package"])]
    if (length(still_missing) > 0) {
      warning("Failed to install the following packages: ", paste(still_missing, collapse = ", "))
    }
  } else {
    message("All required packages are already installed.")
  }
  
  # Report success
  message("Package check completed. Ready to run the application.")
}

# Run the check and install function
check_and_install_packages()

# Load Python environment for BERT analysis
if (requireNamespace("reticulate", quietly = TRUE)) {
  message("Checking Python environment for BERT analysis...")
  tryCatch({
    # This path will be overridden by helpers.R if needed
    reticulate::use_condaenv("dreizehn", required = FALSE)
    message("Python environment found. Ready for BERT analysis.")
  }, error = function(e) {
    message("Note: Python environment not configured. BERT analysis will use cached results only.")
  })
}

message("Requirements check complete. Ready to launch the application.")
