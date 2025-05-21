# ===============================================
# HELPERS
# ===============================================



# ===============================================
# CRITICAL FUNCTIONS 
# ===============================================

# Utility function to build user network for visualization
buildUserNetwork <- function(nodes, edges) {
  # Extract unique users and create nodes
  users <- unique(nodes$author)
  user_nodes <- data.frame(
    id = users,
    label = users,
    stringsAsFactors = FALSE
  )
  
  # Create edges between users based on reply structure
  user_edges <- data.frame()
  if (nrow(edges) > 0) {
    for (i in 1:nrow(edges)) {
      from_user <- nodes$author[nodes$id == edges$from[i]]
      to_user <- nodes$author[nodes$id == edges$to[i]]
      if (length(from_user) > 0 && length(to_user) > 0) {
        user_edges <- rbind(user_edges, 
                            data.frame(from = from_user, 
                                       to = to_user, 
                                       stringsAsFactors = FALSE))
      }
    }
  }
  
  # Aggregate edge weights
  if (nrow(user_edges) > 0) {
    user_edges <- user_edges %>%
      group_by(from, to) %>%
      summarise(weight = n(), .groups = "drop")
  }
  
  list(nodes = user_nodes, edges = user_edges)
}

# Function to process discussion arguments
process_discussion_arguments <- function(node, is_root = FALSE, parent_id = NA) {
  if (is.null(node)) return(NULL)
  
  # Skip bots
  node_author <- if (is.null(node$author) || length(node$author) == 0) "" else node$author
  if (tolower(node_author) %in% c("deltabot", "automoderator")) return(NULL)
  
  text <- node$text
  node_id <- node$id
  
  if (is.null(text) || nchar(text) == 0) return(NULL)
  
  # Get argument and persuasion analysis using BERT
  arg_analysis <- analyze_argument_components(text)
  persuasion_analysis <- analyze_persuasion(text)
  
  # Create result structure
  result <- list(
    node_id = node_id,
    parent_id = parent_id,
    author = node_author,
    is_root = is_root,
    text_preview = substr(text, 1, 100),
    arg_components = arg_analysis,
    persuasion_metrics = persuasion_analysis
  )
  
  # Process children recursively
  children_results <- list()
  
  if (!is.null(node$comments) && length(node$comments) > 0) {
    for (comment in node$comments) {
      child_result <- process_discussion_arguments(comment, is_root = FALSE, parent_id = node_id)
      if (!is.null(child_result)) {
        children_results <- c(children_results, list(child_result))
      }
    }
  }
  
  if (!is.null(node$replies) && length(node$replies) > 0) {
    for (reply in node$replies) {
      child_result <- process_discussion_arguments(reply, is_root = FALSE, parent_id = node_id)
      if (!is.null(child_result)) {
        children_results <- c(children_results, list(child_result))
      }
    }
  }
  
  result$children <- children_results
  return(result)
}

# Function to flatten argument structure
flatten_argument_structure <- function(arg_structure) {
  if (is.null(arg_structure)) return(NULL)
  
  result <- data.frame(
    node_id = arg_structure$node_id,
    parent_id = arg_structure$parent_id,
    author = arg_structure$author,
    is_root = arg_structure$is_root,
    text_preview = arg_structure$text_preview,
    stringsAsFactors = FALSE
  )
  
  # Combine argument components and persuasion metrics
  result <- cbind(result, 
                  arg_structure$arg_components,
                  arg_structure$persuasion_metrics)
  
  children_data <- lapply(arg_structure$children, flatten_argument_structure)
  children_data <- do.call(rbind, children_data)
  
  result <- rbind(result, children_data)
  return(result)
}

# Calculate argument flow between posts
calculate_argument_flow <- function(flat_args) {
  if (is.null(flat_args) || nrow(flat_args) < 2) return(NULL)
  
  # Ensure argument_type is set correctly
  component_cols <- c("claims", "premises", "rebuttals", "concessions", "evidence")
  if (!"argument_type" %in% colnames(flat_args)) {
    present_cols <- intersect(component_cols, colnames(flat_args))
    if (length(present_cols) > 0) {
      flat_args$argument_type <- apply(flat_args[, present_cols, drop = FALSE], 1, function(row) {
        present_cols[which.max(row)]
      })
    } else {
      flat_args$argument_type <- "claims"
    }
  }
  
  edges <- flat_args[!is.na(flat_args$parent_id), c("parent_id", "node_id")]
  colnames(edges) <- c("from", "to")
  
  # Merge with argument_type
  edges <- merge(edges,
                 flat_args[, c("node_id", "argument_type")],
                 by.x = "from", by.y = "node_id", all.x = TRUE)
  colnames(edges)[3] <- "from_type"
  
  edges <- merge(edges,
                 flat_args[, c("node_id", "argument_type")],
                 by.x = "to", by.y = "node_id", all.x = TRUE)
  colnames(edges)[4] <- "to_type"
  
  # Ensure from_type and to_type are character vectors, not lists
  edges$from_type <- as.character(edges$from_type)
  edges$to_type <- as.character(edges$to_type)
  
  # Count transitions between argument types
  transitions <- as.data.frame(table(edges$from_type, edges$to_type))
  colnames(transitions) <- c("from_type", "to_type", "count")
  
  # Filter for transitions that occur at least once
  transitions <- transitions[transitions$count > 0, ]
  
  return(transitions)
}


# Function to add missing columns and standardize data frames
standardize_bert_data <- function() {
  # Add necessary columns to argument component data frames
  if (length(precomputed_analysis$arg_components) > 0) {
    for (i in seq_along(precomputed_analysis$arg_components)) {
      df <- precomputed_analysis$arg_components[[i]]
      
      # Add missing columns that the UI expects
      if (is.data.frame(df)) {
        # Add argument_type if missing
        if (!"argument_type" %in% colnames(df)) {
          # Determine argument type based on highest score
          component_cols <- c("claims", "premises", "rebuttals", "concessions", "evidence")
          scores <- as.numeric(df[1, component_cols])
          arg_type <- component_cols[which.max(scores)]
          df$argument_type <- arg_type
        }
        
        # Add evidence_to_claim_ratio if missing
        if (!"evidence_to_claim_ratio" %in% colnames(df)) {
          if (all(c("evidence", "claims") %in% colnames(df))) {
            evidence_val <- as.numeric(df[1, "evidence"])
            claims_val <- as.numeric(df[1, "claims"])
            df$evidence_to_claim_ratio <- if(claims_val > 0) evidence_val / claims_val else evidence_val
          } else {
            df$evidence_to_claim_ratio <- 1.0 # Default value
          }
        }
        
        # Add rebuttal_presence if missing
        if (!"rebuttal_presence" %in% colnames(df)) {
          if ("rebuttals" %in% colnames(df)) {
            rebuttal_val <- as.numeric(df[1, "rebuttals"])
            df$rebuttal_presence <- if(rebuttal_val > 0.4) 1 else 0
          } else {
            df$rebuttal_presence <- 0 # Default value
          }
        }
        
        # Add concession_presence if missing
        if (!"concession_presence" %in% colnames(df)) {
          if ("concessions" %in% colnames(df)) {
            concession_val <- as.numeric(df[1, "concessions"])
            df$concession_presence <- if(concession_val > 0.4) 1 else 0
          } else {
            df$concession_presence <- 0 # Default value
          }
        }
        
        # Add complexity_score if missing
        if (!"complexity_score" %in% colnames(df)) {
          # Calculate a complexity score based on the component values
          if (all(c("claims", "premises", "rebuttals", "concessions", "evidence") %in% colnames(df))) {
            component_scores <- as.numeric(df[1, c("claims", "premises", "rebuttals", "concessions", "evidence")])
            df$complexity_score <- sum(component_scores) / length(component_scores)
          } else {
            df$complexity_score <- 0.5 # Default value
          }
        }
        
        # Update the data frame in the list
        precomputed_analysis$arg_components[[i]] <- df
      }
    }
  }
  
  # Add necessary columns to persuasion data frames
  if (length(precomputed_analysis$persuasion) > 0) {
    for (i in seq_along(precomputed_analysis$persuasion)) {
      df <- precomputed_analysis$persuasion[[i]]
      
      if (is.data.frame(df)) {
        # Add missing columns that the UI expects
        
        # Add persuasiveness_index if missing
        if (!"persuasiveness_index" %in% colnames(df)) {
          if (all(c("logos_score", "pathos_score", "ethos_score") %in% colnames(df))) {
            appeal_scores <- as.numeric(df[1, c("logos_score", "pathos_score", "ethos_score")])
            df$persuasiveness_index <- max(appeal_scores)
          } else {
            df$persuasiveness_index <- 0.5 # Default value
          }
        }
        
        # Add dominant_appeal if missing
        if (!"dominant_appeal" %in% colnames(df)) {
          if (all(c("logos_score", "pathos_score", "ethos_score") %in% colnames(df))) {
            appeal_types <- c("logos", "pathos", "ethos")
            appeal_scores <- as.numeric(df[1, c("logos_score", "pathos_score", "ethos_score")])
            df$dominant_appeal <- if(all(appeal_scores == 0)) "neutral" else appeal_types[which.max(appeal_scores)]
          } else {
            df$dominant_appeal <- "neutral" # Default value
          }
        }
        
        # Update the data frame in the list
        precomputed_analysis$persuasion[[i]] <- df
      }
    }
  }
}

# ───────────────────────────────────────────────
# 📦 Load Precomputed BERT Results
# ───────────────────────────────────────────────
# Check if cached analysis exists in the new location
bert_dir <- "data/bert"
reddit_cache_file <- file.path(bert_dir, "reddit_bert_analysis.rds")
reddit_text_file <- file.path(bert_dir, "reddit_text_data.rds")
reddit_joined_file <- file.path(bert_dir, "reddit_bert_joined_analysis.rds")

# Legacy cache file path (from original script)
legacy_cache_file <- "data/precomputed_bert_analysis.rds"

# Function to convert the new data format to the old format expected by the app
convert_bert_format <- function(new_analysis, text_data) {
  # Initialize the structure expected by the app
  old_format <- list(
    appeal_types = list(),
    fallacies = list(),
    arg_components = list(),
    persuasion = list()
  )
  
  # Map text IDs to their content
  text_map <- setNames(text_data$text, text_data$id)
  
  # Process appeal types
  if (!is.null(new_analysis$appeal_types) && is.data.frame(new_analysis$appeal_types) && nrow(new_analysis$appeal_types) > 0) {
    for (i in 1:nrow(new_analysis$appeal_types)) {
      id <- new_analysis$appeal_types$id[i]
      if (id %in% names(text_map)) {
        text <- text_map[id]
        old_format$appeal_types[[text]] <- new_analysis$appeal_types[i, !colnames(new_analysis$appeal_types) %in% "id", drop = FALSE]
      }
    }
  }
  
  # Process fallacies
  if (!is.null(new_analysis$fallacies) && is.data.frame(new_analysis$fallacies) && nrow(new_analysis$fallacies) > 0) {
    for (i in 1:nrow(new_analysis$fallacies)) {
      id <- new_analysis$fallacies$id[i]
      if (id %in% names(text_map)) {
        text <- text_map[id]
        old_format$fallacies[[text]] <- new_analysis$fallacies[i, !colnames(new_analysis$fallacies) %in% "id", drop = FALSE]
      }
    }
  }
  
  # Process argument components
  if (!is.null(new_analysis$arg_components) && is.data.frame(new_analysis$arg_components) && nrow(new_analysis$arg_components) > 0) {
    for (i in 1:nrow(new_analysis$arg_components)) {
      id <- new_analysis$arg_components$id[i]
      if (id %in% names(text_map)) {
        text <- text_map[id]
        old_format$arg_components[[text]] <- new_analysis$arg_components[i, !colnames(new_analysis$arg_components) %in% "id", drop = FALSE]
      }
    }
  }
  
  # Process persuasion
  if (!is.null(new_analysis$persuasion) && is.data.frame(new_analysis$persuasion) && nrow(new_analysis$persuasion) > 0) {
    for (i in 1:nrow(new_analysis$persuasion)) {
      id <- new_analysis$persuasion$id[i]
      if (id %in% names(text_map)) {
        text <- text_map[id]
        old_format$persuasion[[text]] <- new_analysis$persuasion[i, !colnames(new_analysis$persuasion) %in% "id", drop = FALSE]
      }
    }
  }
  
  return(old_format)
}

# Load precomputed data - simplified without Python dependencies
if (file.exists(reddit_joined_file)) {
  precomputed_analysis <- readRDS(reddit_joined_file)
} else if (file.exists(reddit_cache_file) && file.exists(reddit_text_file)) {
  new_analysis <- readRDS(reddit_cache_file)
  text_data <- readRDS(reddit_text_file)
  precomputed_analysis <- convert_bert_format(new_analysis, text_data)
} else if (file.exists(legacy_cache_file)) {
  precomputed_analysis <- readRDS(legacy_cache_file)
} else {
  stop("No precomputed BERT analysis found. Please ensure the data files are in the correct location.")
}

# NOW call the standardization function AFTER precomputed_analysis has been initialized
standardize_bert_data()

# 3. Patch function for the persuasion_data reactive
patch_persuasion_data <- function(flat_args) {
  if (is.null(flat_args)) return(flat_args)
  
  # Ensure the data frame has all required columns
  if (!"evidence_to_claim_ratio" %in% colnames(flat_args)) {
    # Create from claims and evidence if available
    if (all(c("claims", "evidence") %in% colnames(flat_args))) {
      flat_args$evidence_to_claim_ratio <- 
        ifelse(flat_args$claims > 0, 
               flat_args$evidence / flat_args$claims, 
               flat_args$evidence)
    } else {
      # Add default value
      flat_args$evidence_to_claim_ratio <- 1.0
    }
  }
  
  # Add other columns needed by the UI
  if (!"complexity_score" %in% colnames(flat_args)) {
    component_cols <- intersect(
      c("claims", "premises", "rebuttals", "concessions", "evidence"),
      colnames(flat_args)
    )
    
    if (length(component_cols) > 0) {
      flat_args$complexity_score <- rowMeans(flat_args[, component_cols])
    } else {
      flat_args$complexity_score <- 0.5
    }
  }
  
  # Add boolean flags for rebuttals and concessions
  if (!"rebuttal_presence" %in% colnames(flat_args) && "rebuttals" %in% colnames(flat_args)) {
    flat_args$rebuttal_presence <- ifelse(flat_args$rebuttals > 0.4, 1, 0)
  }
  
  if (!"concession_presence" %in% colnames(flat_args) && "concessions" %in% colnames(flat_args)) {
    flat_args$concession_presence <- ifelse(flat_args$concessions > 0.4, 1, 0)
  }
  
  # Add argument_type if missing
  if (!"argument_type" %in% colnames(flat_args)) {
    component_cols <- intersect(
      c("claims", "premises", "rebuttals", "concessions", "evidence"),
      colnames(flat_args)
    )
    
    if (length(component_cols) > 0) {
      flat_args$argument_type <- apply(flat_args[, component_cols], 1, function(row) {
        component_cols[which.max(row)]
      })
    } else {
      flat_args$argument_type <- "claims" # Default
    }
  }
  
  # Always set dominant_appeal if possible
  appeal_cols <- c("logos_score", "pathos_score", "ethos_score")
  if (all(appeal_cols %in% colnames(flat_args))) {
    flat_args$dominant_appeal <- apply(flat_args[, appeal_cols], 1, function(row) {
      appeal_types <- c("logos", "pathos", "ethos")
      if (all(is.na(row))) return("neutral")
      if (all(row == 0)) return("neutral")
      appeal_types[which.max(row)]
    })
  } else if (!"dominant_appeal" %in% colnames(flat_args)) {
    flat_args$dominant_appeal <- "neutral"
  }
  
  return(flat_args)
}

# Add global variables for warning suppression and caching
miss_counter <- 0
max_warnings <- 20  # Increased from 5 to 20 to allow more informative warnings
warning_shown <- FALSE
bert_cache <- new.env(hash = TRUE, parent = emptyenv())

# ───────────────────────────────────────────────
# 🔄 Enhanced Cached BERT Helper Functions (using precomputed data)
# ───────────────────────────────────────────────
detect_appeal_types <- function(text) {
  # Check cache first
  cache_key <- digest::digest(text)
  if (exists(cache_key, envir = bert_cache)) {
    cached_result <- get(cache_key, envir = bert_cache)
    if (!is.null(cached_result$appeal_types)) {
      return(cached_result$appeal_types)
    }
  }
  
  # First try to get from cache using exact match
  if (!is.null(precomputed_analysis$appeal_types[[text]])) {
    result <- precomputed_analysis$appeal_types[[text]]
    # Save in cache for future use
    if (!exists(cache_key, envir = bert_cache)) {
      assign(cache_key, list(), envir = bert_cache)
    }
    cached_data <- get(cache_key, envir = bert_cache)
    cached_data$appeal_types <- result
    assign(cache_key, cached_data, envir = bert_cache)
    return(result)
  }
  
  # If we have a joined analysis file, try to use that for text lookup
  if (file.exists(reddit_joined_file)) {
    joined_data <- readRDS(reddit_joined_file)
    if (!is.null(joined_data$appeal_types)) {
      # Try to find a close match in the text column
      matches <- joined_data$appeal_types$text
      best_match_idx <- which(sapply(matches, function(t) {
        stringdist::stringdist(text, t, method = "jw") < 0.2  # Adjust threshold as needed
      }))
      if (length(best_match_idx) > 0) {
        row_data <- joined_data$appeal_types[best_match_idx[1], ]
        result <- data.frame(
          logos_score = row_data$logos_score,
          pathos_score = row_data$pathos_score,
          ethos_score = row_data$ethos_score,
          stringsAsFactors = FALSE
        )
        
        # Save in cache for future use
        if (!exists(cache_key, envir = bert_cache)) {
          assign(cache_key, list(), envir = bert_cache)
        }
        cached_data <- get(cache_key, envir = bert_cache) 
        cached_data$appeal_types <- result
        assign(cache_key, cached_data, envir = bert_cache)
        
        return(result)
      }
    }
  }
  
  # Fallback if not found (e.g. for new data)
  # Use global counter to limit warnings
  if (miss_counter < max_warnings && !warning_shown) {
    message("Appeal types not found in precomputed data for: ", substr(text, 1, 30), "...")
    miss_counter <<- miss_counter + 1
    
    # Show message about suppressing future warnings
    if (miss_counter >= max_warnings) {
      message("Suppressing further 'not found' warnings. Set max_warnings higher to see more.")
      warning_shown <<- TRUE
    }
  }
  
  result <- data.frame(
    logos_score = 0.333,
    pathos_score = 0.333,
    ethos_score = 0.334,
    stringsAsFactors = FALSE
  )
  
  # Save in cache for future use
  if (!exists(cache_key, envir = bert_cache)) {
    assign(cache_key, list(), envir = bert_cache)
  }
  cached_data <- get(cache_key, envir = bert_cache)
  cached_data$appeal_types <- result
  assign(cache_key, cached_data, envir = bert_cache)
  
  return(result)
}

# Similar approach for detect_fallacies
detect_fallacies <- function(text) {
  # Check cache first
  cache_key <- digest::digest(text)
  if (exists(cache_key, envir = bert_cache)) {
    cached_result <- get(cache_key, envir = bert_cache)
    if (!is.null(cached_result$fallacies)) {
      return(cached_result$fallacies)
    }
  }
  
  # First try to get from cache using exact match
  if (!is.null(precomputed_analysis$fallacies[[text]])) {
    result <- precomputed_analysis$fallacies[[text]]
    # Save in cache
    if (!exists(cache_key, envir = bert_cache)) {
      assign(cache_key, list(), envir = bert_cache)
    }
    cached_data <- get(cache_key, envir = bert_cache)
    cached_data$fallacies <- result
    assign(cache_key, cached_data, envir = bert_cache)
    return(result)
  }
  
  # If we have a joined analysis file, try to use that for text lookup
  if (file.exists(reddit_joined_file)) {
    joined_data <- readRDS(reddit_joined_file)
    if (!is.null(joined_data$fallacies)) {
      # Try to find a close match in the text column
      matches <- joined_data$fallacies$text
      best_match_idx <- which(sapply(matches, function(t) {
        stringdist::stringdist(text, t, method = "jw") < 0.2  # Adjust threshold as needed
      }))
      if (length(best_match_idx) > 0) {
        row_data <- joined_data$fallacies[best_match_idx[1], ]
        result <- data.frame(
          ad_hominem = row_data$ad_hominem,
          straw_man = row_data$straw_man,
          false_dichotomy = row_data$false_dichotomy,
          slippery_slope = row_data$slippery_slope,
          appeal_to_popularity = row_data$appeal_to_popularity,
          stringsAsFactors = FALSE
        )
        
        # Save in cache
        if (!exists(cache_key, envir = bert_cache)) {
          assign(cache_key, list(), envir = bert_cache)
        }
        cached_data <- get(cache_key, envir = bert_cache)
        cached_data$fallacies <- result
        assign(cache_key, cached_data, envir = bert_cache)
        
        return(result)
      }
    }
  }
  
  # Fallback if not found - limit warnings
  if (miss_counter < max_warnings && !warning_shown) {
    message("Fallacies not found in precomputed data for: ", substr(text, 1, 30), "...")
    miss_counter <<- miss_counter + 1
    
    # Show message about suppressing future warnings
    if (miss_counter >= max_warnings) {
      message("Suppressing further 'not found' warnings. Set max_warnings higher to see more.")
      warning_shown <<- TRUE
    }
  }
  
  result <- data.frame(
    ad_hominem = 0.2,
    straw_man = 0.2,
    false_dichotomy = 0.2,
    slippery_slope = 0.2,
    appeal_to_popularity = 0.2,
    stringsAsFactors = FALSE
  )
  
  # Save in cache
  if (!exists(cache_key, envir = bert_cache)) {
    assign(cache_key, list(), envir = bert_cache)
  }
  cached_data <- get(cache_key, envir = bert_cache)
  cached_data$fallacies <- result
  assign(cache_key, cached_data, envir = bert_cache)
  
  return(result)
}

analyze_argument_components <- function(text) {
  # Check cache first
  cache_key <- digest::digest(text)
  if (exists(cache_key, envir = bert_cache)) {
    cached_result <- get(cache_key, envir = bert_cache)
    if (!is.null(cached_result$arg_components)) {
      return(cached_result$arg_components)
    }
  }
  
  # Get from precomputed data
  if (!is.null(precomputed_analysis$arg_components[[text]])) {
    result <- precomputed_analysis$arg_components[[text]]
    
    # Save in cache
    if (!exists(cache_key, envir = bert_cache)) {
      assign(cache_key, list(), envir = bert_cache)
    }
    cached_data <- get(cache_key, envir = bert_cache)
    cached_data$arg_components <- result
    assign(cache_key, cached_data, envir = bert_cache)
    
    return(result)
  }
  
  # If we have a joined analysis file, try to use that for text lookup
  if (file.exists(reddit_joined_file)) {
    joined_data <- readRDS(reddit_joined_file)
    if (!is.null(joined_data$arg_components)) {
      # Try to find a close match in the text column
      matches <- joined_data$arg_components$text
      best_match_idx <- which(sapply(matches, function(t) {
        stringdist::stringdist(text, t, method = "jw") < 0.2  # Adjust threshold as needed
      }))
      if (length(best_match_idx) > 0) {
        row_data <- joined_data$arg_components[best_match_idx[1], ]
        result <- data.frame(
          claims = row_data$claims,
          premises = row_data$premises,
          rebuttals = row_data$rebuttals,
          concessions = row_data$concessions,
          evidence = row_data$evidence,
          stringsAsFactors = FALSE
        )
        
        # Save in cache
        if (!exists(cache_key, envir = bert_cache)) {
          assign(cache_key, list(), envir = bert_cache)
        }
        cached_data <- get(cache_key, envir = bert_cache)
        cached_data$arg_components <- result
        assign(cache_key, cached_data, envir = bert_cache)
        
        return(result)
      }
    }
  }
  
  # Try to use zero-shot classifier directly if available
  result <- tryCatch({
    if (exists("zs_classifier") && !is.null(zs_classifier)) {
      # If we have access to the zero-shot classifier, use it
      candidate_labels <- c("claim", "premise", "rebuttal", "concession", "evidence")
      out <- zs_classifier(text, candidate_labels = candidate_labels, multi_label = TRUE)
      
      # Extract scores and ensure all components are present
      df <- setNames(as.list(out$scores), out$labels)
      for (lbl in candidate_labels) {
        if (is.null(df[[lbl]])) df[[lbl]] <- 0
      }
      df <- as.data.frame(df[candidate_labels], stringsAsFactors = FALSE)
      
      # Return the freshly computed result
      df
    } else {
      # Fallback for missing data - limit warnings
      if (miss_counter < max_warnings && !warning_shown) {
        message("Argument components not found in precomputed data for: ", substr(text, 1, 30), "...")
        miss_counter <<- miss_counter + 1
        
        # Show message about suppressing future warnings
        if (miss_counter >= max_warnings) {
          message("Suppressing further 'not found' warnings. Set max_warnings higher to see more.")
          warning_shown <<- TRUE
        }
      }
      
      data.frame(
        claims = 0.2,
        premises = 0.2,
        rebuttals = 0.2,
        concessions = 0.2,
        evidence = 0.2,
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) {
    # If live classification fails, return default values
    data.frame(
      claims = 0.2,
      premises = 0.2,
      rebuttals = 0.2,
      concessions = 0.2,
      evidence = 0.2,
      stringsAsFactors = FALSE
    )
  })
  
  # Save in cache
  if (!exists(cache_key, envir = bert_cache)) {
    assign(cache_key, list(), envir = bert_cache)
  }
  cached_data <- get(cache_key, envir = bert_cache)
  cached_data$arg_components <- result
  assign(cache_key, cached_data, envir = bert_cache)
  
  return(result)
}

analyze_persuasion <- function(text, bulk_mode = FALSE) {
  # Add a simple check to avoid processing extremely short or empty text
  if (is.null(text) || nchar(text) < 5) {
    return(data.frame(
      logos_score = 0.33,
      pathos_score = 0.33,
      ethos_score = 0.34,
      fallacy_score = 0.2,
      persuasiveness_index = 0.5,
      dominant_appeal = "neutral",
      stringsAsFactors = FALSE
    ))
  }
  
  # Check cache first
  cache_key <- digest::digest(text)
  if (exists(cache_key, envir = bert_cache)) {
    cached_result <- get(cache_key, envir = bert_cache)
    if (!is.null(cached_result$persuasion)) {
      return(cached_result$persuasion)
    }
  }
  
  # Get from precomputed data
  if (!is.null(precomputed_analysis$persuasion[[text]])) {
    result <- precomputed_analysis$persuasion[[text]]
    
    # Save in cache
    if (!exists(cache_key, envir = bert_cache)) {
      assign(cache_key, list(), envir = bert_cache)
    }
    cached_data <- get(cache_key, envir = bert_cache)
    cached_data$persuasion <- result
    assign(cache_key, cached_data, envir = bert_cache)
    
    return(result)
  }
  
  # If we have a joined analysis file, try to use that for text lookup
  if (file.exists(reddit_joined_file)) {
    joined_data <- readRDS(reddit_joined_file)
    if (!is.null(joined_data$persuasion)) {
      # Try to find a close match in the text column
      matches <- joined_data$persuasion$text
      best_match_idx <- which(sapply(matches, function(t) {
        stringdist::stringdist(text, t, method = "jw") < 0.2  # Adjust threshold as needed
      }))
      if (length(best_match_idx) > 0) {
        row_data <- joined_data$persuasion[best_match_idx[1], ]
        result <- data.frame(
          logos_score = row_data$logos_score,
          pathos_score = row_data$pathos_score,
          ethos_score = row_data$ethos_score,
          fallacy_score = row_data$fallacy_score,
          persuasiveness_index = row_data$persuasiveness_index,
          dominant_appeal = row_data$dominant_appeal,
          stringsAsFactors = FALSE
        )
        
        # Save in cache
        if (!exists(cache_key, envir = bert_cache)) {
          assign(cache_key, list(), envir = bert_cache)
        }
        cached_data <- get(cache_key, envir = bert_cache)
        cached_data$persuasion <- result
        assign(cache_key, cached_data, envir = bert_cache)
        
        return(result)
      }
    }
  }
  
  # Try to use the zero-shot classifier directly if available
  result <- tryCatch({
    if (exists("zs_classifier") && !is.null(zs_classifier)) {
      # Generate persuasion data using zero-shot classification
      
      # Appeal types classification
      appeal_labels <- c("logos", "pathos", "ethos")
      appeal_result <- zs_classifier(text, candidate_labels = appeal_labels, multi_label = FALSE)
      
      # Create result dataframe
      result <- data.frame(
        logos_score = appeal_result$scores[match("logos", appeal_result$labels)] %||% 0.33,
        pathos_score = appeal_result$scores[match("pathos", appeal_result$labels)] %||% 0.33,
        ethos_score = appeal_result$scores[match("ethos", appeal_result$labels)] %||% 0.34,
        fallacy_score = 0.2,  # Default value
        persuasiveness_index = max(appeal_result$scores),
        stringsAsFactors = FALSE
      )
      
      # Determine dominant appeal type
      result$dominant_appeal <- appeal_labels[which.max(c(
        result$logos_score, result$pathos_score, result$ethos_score
      ))]
      
      result
    } else {
      # Fallback for missing data - with limited warnings
      if (!bulk_mode && miss_counter < max_warnings && !warning_shown) {
        # Create a cleaner text preview for the warning message
        text_preview <- gsub("[\r\n\t]", " ", substr(text, 1, 30))
        text_preview <- gsub("\\s+", " ", text_preview)
        message("Persuasion analysis not cached: \"", text_preview, "...\" (using defaults)")
        miss_counter <<- miss_counter + 1
        
        # Show message about suppressing future warnings
        if (miss_counter >= max_warnings) {
          message("Note: Suppressing further 'not found' warnings. Use clear_bert_cache() to reset.")
          warning_shown <<- TRUE
        }
      } else if (bulk_mode) {
        # In bulk mode, just increment the counter silently
        miss_counter <<- miss_counter + 1
      }
      
      result <- data.frame(
        logos_score = 0.33,
        pathos_score = 0.33,
        ethos_score = 0.34,
        fallacy_score = 0.2,
        persuasiveness_index = 0.5,
        stringsAsFactors = FALSE
      )
      
      # Determine dominant appeal type
      appeal_types <- c("logos", "pathos", "ethos")
      appeal_scores <- c(result$logos_score, result$pathos_score, result$ethos_score)
      result$dominant_appeal <- if(all(appeal_scores == 0)) "neutral" else appeal_types[which.max(appeal_scores)]
      
      result
    }
  }, error = function(e) {
    # In case of error, return default values
    result <- data.frame(
      logos_score = 0.33,
      pathos_score = 0.33,
      ethos_score = 0.34,
      fallacy_score = 0.2,
      persuasiveness_index = 0.5,
      stringsAsFactors = FALSE
    )
    
    # Determine dominant appeal type
    appeal_types <- c("logos", "pathos", "ethos")
    appeal_scores <- c(result$logos_score, result$pathos_score, result$ethos_score)
    result$dominant_appeal <- if(all(appeal_scores == 0)) "neutral" else appeal_types[which.max(appeal_scores)]
    
    result
  })
  
  # Save in cache
  if (!exists(cache_key, envir = bert_cache)) {
    assign(cache_key, list(), envir = bert_cache)
  }
  cached_data <- get(cache_key, envir = bert_cache)
  cached_data$persuasion <- result
  assign(cache_key, cached_data, envir = bert_cache)
  
  return(result)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

# Add a function to clear cache if needed
clear_bert_cache <- function(reset_warnings = TRUE) {
  rm(list = ls(envir = bert_cache), envir = bert_cache)
  if (reset_warnings) {
    miss_counter <<- 0
    warning_shown <<- FALSE
  }
  message("BERT cache cleared", if(reset_warnings) " and warning counters reset" else "")
}

# Add a batch analysis function to process multiple texts at once
batch_analyze_persuasion <- function(texts) {
  message("Batch analyzing ", length(texts), " posts. This may take a moment...")
  start_time <- Sys.time()
  
  # Process all texts in bulk mode (suppressing individual warnings)
  results <- lapply(texts, function(text) {
    analyze_persuasion(text, bulk_mode = TRUE)
  })
  
  # Report summary at the end
  end_time <- Sys.time()
  missing_count <- miss_counter
  message("Batch analysis complete: processed ", length(texts), 
          " posts in ", round(difftime(end_time, start_time, units = "secs"), 1), 
          " seconds. Missing from cache: ", missing_count)
  
  return(results)
}

# Add a more comprehensive check_bert_setup function
check_bert_setup <- function(reset_stats = FALSE) {
  if (reset_stats) {
    miss_counter <<- 0
    warning_shown <<- FALSE
  }
  
  bert_status <- list(
    classifier_loaded = exists("zs_classifier") && !is.null(zs_classifier),
    cache_entries = length(ls(envir = bert_cache)),
    warnings_shown = miss_counter,
    warnings_suppressed = warning_shown,
    max_warnings = max_warnings
  )
  
  # Output status message
  message("BERT Status:")
  message("- Zero-Shot Classifier Loaded: ", bert_status$classifier_loaded)
  message("- Cache Entries: ", bert_status$cache_entries)
  message("- Warning Count: ", bert_status$warnings_shown, "/", bert_status$max_warnings)
  message("- Warnings Suppressed: ", bert_status$warnings_suppressed)
  message("- Max Warning Threshold: ", bert_status$max_warnings)
  
  invisible(bert_status)
}

