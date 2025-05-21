# ───────────────────────────────────────────────
# 🏗️ Reddit BERT Preprocessing Script with Debug Mode
# ───────────────────────────────────────────────
# This script processes Reddit data for BERT analysis

# Load required libraries
library(jsonlite)
library(reticulate)
library(data.table)
library(purrr)
library(dplyr)
library(stringr)

# Create output directory
bert_dir <- "data/bert"
if (!dir.exists(bert_dir)) {
  dir.create(bert_dir, recursive = TRUE)
  message("Created directory: ", bert_dir)
}

# Set up logging
log_file <- file.path(bert_dir, "bert_processing.log")
file.create(log_file)

# Function to log messages both to console and file
log_message <- function(msg) {
  message(msg)
  cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", msg, "\n"), 
      file = log_file, append = TRUE)
}

log_message("Setting up Python environment...")

# Set the path to your conda executable
Sys.setenv(RETICULATE_CONDA = "C:/Users/bensc/anaconda3/Scripts/conda.exe")  # Adjust this path

# Then proceed with your existing conda environment setup
Sys.setenv(RETICULATE_PYTHON = "C:/Users/bensc/anaconda3/envs/dreizehn/python.exe")

# Debug Python paths
log_message(paste0("Python path: ", Sys.getenv("RETICULATE_PYTHON")))
log_message(paste0("Conda path: ", Sys.getenv("RETICULATE_CONDA")))

# Initialize Python and BERT
use_condaenv("dreizehn", required = TRUE)

# Load the needed Python modules with proper error handling
tryCatch({
  log_message("Importing transformers and torch...")
  transformers <- import("transformers")
  torch <- import("torch")
  log_message("Successfully imported Python modules")
}, error = function(e) {
  log_message(paste0("Error importing Python modules: ", e$message))
  stop("Failed to import required Python modules")
})

log_message("Loading BERT models...")

# Set up a no-op default BERT model for testing without the actual model
create_dummy_model <- function() {
  # Create a simple function that returns dummy outputs compatible with our processing
  list(
    dummy_predict = function(input_text) {
      # Return a dummy tensor-like object with 3 values
      # that our processing code can handle
      scores <- c(0.333, 0.333, 0.334)
      return(scores)
    }
  )
}

# Load the zero-shot classification pipeline from transformers
tryCatch({
  log_message("Loading zero-shot classification pipeline...")
  # Use zero-shot classification pipeline instead of direct model loading
  zs_classifier <- transformers$pipeline(
    task = "zero-shot-classification",
    model = "facebook/bart-large-mnli",
    device = if(torch$cuda$is_available()) 0 else -1  # Use GPU if available
  )
  use_dummy_model <- FALSE
  log_message("Successfully loaded zero-shot classification pipeline")
}, error = function(e) {
  log_message(paste0("Error loading zero-shot classification pipeline: ", e$message))
  log_message("Falling back to dummy model")
  zs_classifier <- NULL
  use_dummy_model <- TRUE
})

log_message("Loading Reddit data...")

# Try to load the Reddit data
tryCatch({
  # Load the data
  reddit_data <- fromJSON("data/reddit_data_anonymized.json", simplifyVector = FALSE)
  log_message(paste0("Successfully loaded Reddit data with ", length(reddit_data), " top-level items"))
}, error = function(e) {
  log_message(paste0("Error loading Reddit data: ", e$message))
  stop("Failed to load Reddit data")
})

# ───────────────────────────────────────────────
# 🔨 Helper Functions For Reddit Data Processing
# ───────────────────────────────────────────────

# Create empty dataframe to store text data
reddit_text_df <- data.frame(
  id = character(),
  text = character(),
  parent_id = character(),
  type = character(),
  author = character(),
  stringsAsFactors = FALSE
)

# Recursive function to extract text from Reddit data structure
extract_reddit_data <- function(item, parent_id = NA) {
  # Check if valid item
  if (is.null(item)) return(NULL)
  
  # Extract fields safely
  id <- if (!is.null(item$id)) as.character(item$id) else NA
  text <- if (!is.null(item$text)) as.character(item$text) else NA
  item_type <- if (!is.null(item$type)) as.character(item$type) else NA
  author <- if (!is.null(item$author)) as.character(item$author) else NA
  
  # Create row for this item
  row <- data.frame(
    id = id,
    text = text,
    parent_id = parent_id,
    type = item_type,
    author = author,
    stringsAsFactors = FALSE
  )
  
  # Process comments if present
  comment_rows <- NULL
  if (!is.null(item$comments) && length(item$comments) > 0) {
    for (comment in item$comments) {
      comment_rows <- rbind(comment_rows, extract_reddit_data(comment, id))
    }
  }
  
  # Process replies if present
  reply_rows <- NULL
  if (!is.null(item$replies) && length(item$replies) > 0) {
    for (reply in item$replies) {
      reply_rows <- rbind(reply_rows, extract_reddit_data(reply, id))
    }
  }
  
  # Combine all rows
  result <- rbind(row, comment_rows, reply_rows)
  return(result)
}

log_message("Extracting text from Reddit data...")
# Process all posts
for (post in reddit_data) {
  post_data <- extract_reddit_data(post)
  reddit_text_df <- rbind(reddit_text_df, post_data)
}

# Remove rows with NA or empty text
reddit_text_df <- reddit_text_df[!is.na(reddit_text_df$text) & nchar(reddit_text_df$text) > 0, ]

log_message(paste0("Found ", nrow(reddit_text_df), " text items"))

# Save the extracted Reddit data for future reference
saveRDS(reddit_text_df, file.path(bert_dir, "reddit_text_data.rds"))
log_message(paste0("Saved Reddit text data to ", file.path(bert_dir, "reddit_text_data.rds")))

# ───────────────────────────────────────────────
# 🔍 Debug Mode Functions
# ───────────────────────────────────────────────

# Create a separate debug file
debug_file <- file.path(bert_dir, "bert_debug.log")
file.create(debug_file)

# Debug function to write to debug file
debug <- function(msg, text_sample = NULL) {
  formatted_msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", msg)
  if (!is.null(text_sample)) {
    # Add a sample of the text (first 50 chars) if provided
    formatted_msg <- paste0(formatted_msg, "\nText sample: ", 
                            substr(text_sample, 1, 50), 
                            if(nchar(text_sample) > 50) "..." else "")
  }
  cat(paste0(formatted_msg, "\n"), file = debug_file, append = TRUE)
}

# Test BERT functions with direct Python access
test_bert_functions <- function() {
  test_text <- "This is a test text to see if BERT functions work properly."
  
  debug("Testing BERT functions with sample text", test_text)
  
  # Test tokenizer
  tryCatch({
    debug("Testing tokenizer...")
    if (use_dummy_model) {
      debug("Using dummy tokenizer")
      inputs <- list(input_ids = NULL)
    } else {
      debug("Using real tokenizer")
      inputs <- tokenizer(test_text, return_tensors = "pt", 
                          truncation = TRUE, max_length = as.integer(512))
      debug(paste0("Tokenizer result type: ", class(inputs)[1]))
      debug(paste0("Tokenizer result keys: ", 
                   paste(names(inputs), collapse = ", ")))
    }
    debug("Tokenizer test succeeded")
  }, error = function(e) {
    debug(paste0("Tokenizer test failed: ", e$message))
  })
  
  # Test model
  tryCatch({
    debug("Testing model predictions...")
    if (use_dummy_model) {
      debug("Using dummy model")
      outputs <- c(0.333, 0.333, 0.334)
      debug(paste0("Model result: ", paste(outputs, collapse = ", ")))
    } else {
      debug("Using real model")
      result <- zs_classifier(
        test_text,
        candidate_labels = c("logos", "pathos", "ethos"),
        multi_label = FALSE
      )
      debug(paste0("Zero-shot classification result: ", paste(result$scores, collapse = ", ")))
    }
    debug("Model test succeeded")
  }, error = function(e) {
    debug(paste0("Model test failed: ", e$message))
  })
}

# Run BERT function tests
test_bert_functions()

# ───────────────────────────────────────────────
# 🧠 BERT Analysis Functions
# ───────────────────────────────────────────────

# Preprocess text for BERT
preprocess_for_bert <- function(text) {
  # Basic text cleaning
  text <- gsub("[^[:alnum:][:space:]]", "", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}

# Helper function to check if a value is a valid text string
is_valid_text <- function(text) {
  return(!is.null(text) && is.character(text) && length(text) == 1 && nchar(text) >= 5)
}

# Update detect_appeal_types_bert function to use zero-shot classification
detect_appeal_types_bert <- function(text_str) {
  # Check if text is valid
  if (!is_valid_text(text_str)) {
    debug(paste0("Invalid text in detect_appeal_types_bert: ", 
                 class(text_str)[1], ", length: ", length(text_str)))
    return(data.frame(
      logos_score = 0.333,
      pathos_score = 0.333,
      ethos_score = 0.334,
      stringsAsFactors = FALSE
    ))
  }
  
  # Clean the text
  cleaned_text <- preprocess_for_bert(text_str)
  debug("Processing text in detect_appeal_types_bert", cleaned_text)
  
  # BERT-based appeal type detection
  tryCatch({
    if (use_dummy_model || is.null(zs_classifier)) {
      # Use dummy model for testing
      scores <- c(0.333, 0.333, 0.334)
      debug("Using dummy scores for appeal types")
    } else {
      # Use zero-shot classification
      candidate_labels <- c("logos", "pathos", "ethos")
      result <- zs_classifier(
        cleaned_text,
        candidate_labels = candidate_labels,
        multi_label = FALSE
      )
      
      # Extract scores from result
      scores <- result$scores
      debug(paste0("Zero-shot appeal scores: ", paste(scores, collapse=", ")))
    }
    
    # Ensure we have 3 scores
    if (length(scores) < 3) {
      debug("Not enough scores returned, padding with defaults")
      scores <- c(scores, rep(1/3, 3 - length(scores)))
    }
    
    return(data.frame(
      logos_score = scores[1],
      pathos_score = scores[2],
      ethos_score = scores[3],
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    debug(paste0("Error in detect_appeal_types_bert: ", e$message))
    return(data.frame(
      logos_score = 0.333,
      pathos_score = 0.333,
      ethos_score = 0.334,
      stringsAsFactors = FALSE
    ))
  })
}

# Similarly update the other BERT functions
detect_fallacies_bert <- function(text_str) {
  # Check if text is valid
  if (!is_valid_text(text_str)) {
    debug(paste0("Invalid text in detect_fallacies_bert: ", 
                 class(text_str)[1], ", length: ", length(text_str)))
    return(data.frame(
      ad_hominem = 0.2,
      straw_man = 0.2,
      false_dichotomy = 0.2,
      slippery_slope = 0.2,
      appeal_to_popularity = 0.2,
      stringsAsFactors = FALSE
    ))
  }
  
  # Clean the text
  cleaned_text <- preprocess_for_bert(text_str)
  debug("Processing text in detect_fallacies_bert", cleaned_text)
  
  # BERT-based fallacy detection
  tryCatch({
    if (use_dummy_model || is.null(zs_classifier)) {
      # Use dummy scores for testing
      scores <- c(0.2, 0.2, 0.2, 0.2, 0.2)
      debug("Using dummy scores for fallacies")
    } else {
      # Use zero-shot classification
      candidate_labels <- c("ad_hominem", "straw_man", "false_dichotomy", 
                            "slippery_slope", "appeal_to_popularity")
      result <- zs_classifier(
        cleaned_text,
        candidate_labels = candidate_labels,
        multi_label = TRUE
      )
      
      # Extract scores from result, ensuring correct order
      scores <- numeric(length(candidate_labels))
      for (i in 1:length(candidate_labels)) {
        idx <- match(candidate_labels[i], result$labels)
        if (!is.na(idx)) {
          scores[i] <- result$scores[idx]
        } else {
          scores[i] <- 0.2  # Default if label not found
        }
      }
      debug(paste0("Zero-shot fallacy scores: ", paste(scores, collapse=", ")))
    }
    
    return(data.frame(
      ad_hominem = scores[1],
      straw_man = scores[2],
      false_dichotomy = scores[3],
      slippery_slope = scores[4],
      appeal_to_popularity = scores[5],
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    debug(paste0("Error in detect_fallacies_bert: ", e$message))
    return(data.frame(
      ad_hominem = 0.2,
      straw_man = 0.2,
      false_dichotomy = 0.2,
      slippery_slope = 0.2,
      appeal_to_popularity = 0.2,
      stringsAsFactors = FALSE
    ))
  })
}

analyze_argument_components_bert <- function(text_str) {
  # Check if text is valid
  if (!is_valid_text(text_str)) {
    debug(paste0("Invalid text in analyze_argument_components_bert: ", 
                 class(text_str)[1], ", length: ", length(text_str)))
    return(data.frame(
      claims = 0.2,
      premises = 0.2,
      rebuttals = 0.2,
      concessions = 0.2,
      evidence = 0.2,
      stringsAsFactors = FALSE
    ))
  }
  
  # Clean the text
  cleaned_text <- preprocess_for_bert(text_str)
  debug("Processing text in analyze_argument_components_bert", cleaned_text)
  
  # Analyze argument components
  tryCatch({
    if (use_dummy_model || is.null(zs_classifier)) {
      # Use dummy scores for testing
      predictions <- c(0.2, 0.2, 0.2, 0.2, 0.2)
      debug("Using dummy scores for argument components")
    } else {
      # Use zero-shot classification
      candidate_labels <- c("claim", "premise", "rebuttal", "concession", "evidence")
      result <- zs_classifier(
        cleaned_text,
        candidate_labels = candidate_labels,
        multi_label = TRUE
      )
      
      # Extract scores from result, ensuring correct order
      predictions <- numeric(length(candidate_labels))
      for (i in 1:length(candidate_labels)) {
        idx <- match(candidate_labels[i], result$labels)
        if (!is.na(idx)) {
          predictions[i] <- result$scores[idx]
        } else {
          predictions[i] <- 0.2  # Default if label not found
        }
      }
      debug(paste0("Zero-shot argument component scores: ", paste(predictions, collapse=", ")))
    }
    
    # Return argument components analysis
    return(data.frame(
      claims = predictions[1],
      premises = predictions[2],
      rebuttals = predictions[3],
      concessions = predictions[4],
      evidence = predictions[5],
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    debug(paste0("Error in analyze_argument_components_bert: ", e$message))
    return(data.frame(
      claims = 0.2,
      premises = 0.2,
      rebuttals = 0.2,
      concessions = 0.2,
      evidence = 0.2,
      stringsAsFactors = FALSE
    ))
  })
}

# Set to FALSE to use the actual model instead of dummy values
use_dummy_model <- FALSE  # Change this to FALSE to use real model
log_message(paste0("Using dummy model: ", use_dummy_model))

# ───────────────────────────────────────────────
# 🧪 Test BERT functions on a small sample
# ───────────────────────────────────────────────

# Test BERT functions on a small subset before full processing
test_batch_size <- 5
if (nrow(reddit_text_df) > 0) {
  test_df <- reddit_text_df[1:min(test_batch_size, nrow(reddit_text_df)), ]
  log_message(paste0("Testing BERT functions on ", nrow(test_df), " samples"))
  
  for (i in 1:nrow(test_df)) {
    text_id <- test_df$id[i]
    text_content <- test_df$text[i]
    
    log_message(paste0("Testing text ID: ", text_id))
    debug(paste0("Testing text ID: ", text_id), text_content)
    
    # Test each function
    appeal_result <- detect_appeal_types_bert(text_content)
    fallacy_result <- detect_fallacies_bert(text_content)
    arg_result <- analyze_argument_components_bert(text_content)
    
    # Log results
    log_message(paste0("Test results for ID ", text_id, ":"))
    log_message(paste0("  Appeal types: ", 
                       paste(names(appeal_result), appeal_result, sep="=", collapse=", ")))
    
    # More detailed results to debug file
    debug(paste0("Full results for ID ", text_id, ":"))
    debug(paste0("  Appeal types: ", 
                 paste(names(appeal_result), appeal_result, sep="=", collapse=", ")))
    debug(paste0("  Fallacies: ", 
                 paste(names(fallacy_result), fallacy_result, sep="=", collapse=", ")))
    debug(paste0("  Argument components: ", 
                 paste(names(arg_result), arg_result, sep="=", collapse=", ")))
  }
  
  log_message("Test processing completed")
}

# ───────────────────────────────────────────────
# 🔄 Process Reddit Text in Batches
# ───────────────────────────────────────────────

# Initialize empty data frames to store results
analysis_results <- list(
  appeal_types = data.frame(),
  fallacies = data.frame(),
  arg_components = data.frame()
)

# Function to process a batch of Reddit texts
process_reddit_batch <- function(reddit_df, start_idx, end_idx) {
  # Initialize data frames for this batch
  batch_appeal_types <- data.frame(
    id = character(),
    logos_score = numeric(),
    pathos_score = numeric(),
    ethos_score = numeric(),
    stringsAsFactors = FALSE
  )
  
  batch_fallacies <- data.frame(
    id = character(),
    ad_hominem = numeric(),
    straw_man = numeric(),
    false_dichotomy = numeric(),
    slippery_slope = numeric(),
    appeal_to_popularity = numeric(),
    stringsAsFactors = FALSE
  )
  
  batch_arg_components <- data.frame(
    id = character(),
    claims = numeric(),
    premises = numeric(),
    rebuttals = numeric(),
    concessions = numeric(),
    evidence = numeric(),
    stringsAsFactors = FALSE
  )
  
  log_message(sprintf("Processing batch %d to %d of %d texts...", 
                      start_idx, end_idx, nrow(reddit_df)))
  
  # Get subset of data for this batch
  batch_df <- reddit_df[start_idx:min(end_idx, nrow(reddit_df)), ]
  
  # Process each text in the batch
  for (i in 1:nrow(batch_df)) {
    row_idx <- start_idx + i - 1
    if (row_idx %% 10 == 0) {
      log_message(sprintf("  Processing %d/%d...", row_idx, nrow(reddit_df)))
    }
    
    # Get Reddit text ID and content
    text_id <- batch_df$id[i]
    text_content <- batch_df$text[i]
    
    # Skip if text is too short
    if (nchar(text_content) < 5) {
      log_message(sprintf("  Skipping text ID %s: Too short", text_id))
      next
    }
    
    # Process with BERT
    success <- TRUE
    
    # Appeal types
    tryCatch({
      appeal_row <- detect_appeal_types_bert(text_content)
      if (!is.null(appeal_row) && is.data.frame(appeal_row)) {
        appeal_row$id <- text_id
        batch_appeal_types <- rbind(batch_appeal_types, appeal_row)
      } else {
        success <- FALSE
        debug(paste0("Invalid appeal_row result for ID ", text_id))
      }
    }, error = function(e) {
      success <- FALSE
      debug(paste0("Error in appeal types for ID ", text_id, ": ", e$message))
    })
    
    # Fallacies
    tryCatch({
      fallacy_row <- detect_fallacies_bert(text_content)
      if (!is.null(fallacy_row) && is.data.frame(fallacy_row)) {
        fallacy_row$id <- text_id
        batch_fallacies <- rbind(batch_fallacies, fallacy_row)
      } else {
        success <- FALSE
        debug(paste0("Invalid fallacy_row result for ID ", text_id))
      }
    }, error = function(e) {
      success <- FALSE
      debug(paste0("Error in fallacies for ID ", text_id, ": ", e$message))
    })
    
    # Argument components
    tryCatch({
      arg_row <- analyze_argument_components_bert(text_content)
      if (!is.null(arg_row) && is.data.frame(arg_row)) {
        arg_row$id <- text_id
        batch_arg_components <- rbind(batch_arg_components, arg_row)
      } else {
        success <- FALSE
        debug(paste0("Invalid arg_row result for ID ", text_id))
      }
    }, error = function(e) {
      success <- FALSE
      debug(paste0("Error in argument components for ID ", text_id, ": ", e$message))
    })
    
    if (success) {
      log_message(sprintf("  Successfully processed text ID %s", text_id))
    } else {
      log_message(sprintf("  Some errors occurred while processing text ID %s", text_id))
    }
  }
  
  # Return batch results
  return(list(
    appeal_types = batch_appeal_types,
    fallacies = batch_fallacies,
    arg_components = batch_arg_components
  ))
}

# Process in batches
batch_size <- 100
num_batches <- ceiling(nrow(reddit_text_df) / batch_size)

# Check for existing checkpoints
checkpoint_file_pattern <- file.path(bert_dir, "reddit_bert_batch_%d_of_%d.rds")
last_successful_batch <- 0

# Look for existing checkpoints
checkpoint_files <- list.files(bert_dir, pattern = "reddit_bert_batch_.*\\.rds", full.names = TRUE)
if (length(checkpoint_files) > 0) {
  # Extract batch numbers from filenames
  batch_numbers <- as.numeric(sub(".*reddit_bert_batch_(\\d+)_of_\\d+\\.rds", "\\1", checkpoint_files))
  if (length(batch_numbers) > 0) {
    last_successful_batch <- max(batch_numbers)
    log_message(paste0("Resuming from batch ", last_successful_batch, " of ", num_batches))
    
    # Load last checkpoint
    last_checkpoint_file <- sprintf(checkpoint_file_pattern, last_successful_batch, num_batches)
    if (file.exists(last_checkpoint_file)) {
      last_results <- readRDS(last_checkpoint_file)
      
      # Merge with existing results
      analysis_results$appeal_types <- rbind(analysis_results$appeal_types, 
                                             last_results$appeal_types)
      analysis_results$fallacies <- rbind(analysis_results$fallacies, 
                                          last_results$fallacies)
      analysis_results$arg_components <- rbind(analysis_results$arg_components, 
                                               last_results$arg_components)
      
      log_message(paste0("Loaded checkpoint from ", last_checkpoint_file))
    }
  }
}

# Process remaining batches
log_message(paste0("Processing ", nrow(reddit_text_df), " Reddit texts in ", 
                   num_batches, " batches..."))

for (batch in (last_successful_batch + 1):num_batches) {
  start_idx <- ((batch - 1) * batch_size) + 1
  end_idx <- min(batch * batch_size, nrow(reddit_text_df))
  
  tryCatch({
    # Process batch
    batch_results <- process_reddit_batch(reddit_text_df, start_idx, end_idx)
    
    # Merge with existing results
    analysis_results$appeal_types <- rbind(analysis_results$appeal_types, 
                                           batch_results$appeal_types)
    analysis_results$fallacies <- rbind(analysis_results$fallacies, 
                                        batch_results$fallacies)
    analysis_results$arg_components <- rbind(analysis_results$arg_components, 
                                             batch_results$arg_components)
    
    # Save checkpoint
    checkpoint_file <- sprintf(checkpoint_file_pattern, batch, num_batches)
    saveRDS(batch_results, checkpoint_file)
    log_message(paste0("Saved checkpoint to ", checkpoint_file))
  }, error = function(e) {
    log_message(paste0("Error processing batch ", batch, ": ", e$message))
    log_message(paste0("Please restart the script to resume from batch ", batch))
    stop("Batch processing stopped due to error")
  })
}

# Save final results
final_output_file <- file.path(bert_dir, "reddit_bert_analysis.rds")
saveRDS(analysis_results, final_output_file)
log_message(paste0("✓ Completed BERT preprocessing! Results saved to ", final_output_file))

# Create joined view with text and analysis results
joined_results <- list()

# Join appeal types
if (nrow(analysis_results$appeal_types) > 0) {
  joined_results$appeal_types <- merge(
    reddit_text_df, 
    analysis_results$appeal_types,
    by = "id",
    all.x = TRUE,   # <-- changed from FALSE to TRUE
    all.y = FALSE
  )
}

# Join fallacies
if (nrow(analysis_results$fallacies) > 0) {
  joined_results$fallacies <- merge(
    reddit_text_df, 
    analysis_results$fallacies,
    by = "id",
    all.x = TRUE,   # <-- changed from FALSE to TRUE
    all.y = FALSE
  )
}

# Join argument components
if (nrow(analysis_results$arg_components) > 0) {
  joined_results$arg_components <- merge(
    reddit_text_df, 
    analysis_results$arg_components,
    by = "id",
    all.x = TRUE,   # <-- changed from FALSE to TRUE
    all.y = FALSE
  )
}

# Optionally, log missing analysis
missing_appeal <- sum(is.na(joined_results$appeal_types$logos_score))
missing_fallacy <- sum(is.na(joined_results$fallacies$ad_hominem))
missing_arg <- sum(is.na(joined_results$arg_components$claims))
log_message(paste0("Missing appeal analysis for ", missing_appeal, " texts"))
log_message(paste0("Missing fallacy analysis for ", missing_fallacy, " texts"))
log_message(paste0("Missing argument component analysis for ", missing_arg, " texts"))

# Save joined results
joined_output_file <- file.path(bert_dir, "reddit_bert_joined_analysis.rds")
saveRDS(joined_results, joined_output_file)
log_message(paste0("✓ Joined analysis with text data saved to ", joined_output_file))
log_message("✓ The Shiny app will now load much faster using these precomputed results")
