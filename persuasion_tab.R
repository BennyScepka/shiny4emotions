# ───────────────────────────────────────────────
# Persuasion Tab
# ───────────────────────────────────────────────

# ───────────────────────────────────────────────
#  Persuasion Chart Descriptions
# ───────────────────────────────────────────────
persuasion_chart_descriptions <- list(
  persuasion_coef_plot = list(
    title = "Feature Effects on Δ Odds",
    desc = "Shows which persuasion factors have the strongest impact on earning a delta. The model uses ridge regression to account for correlated features, displaying the odds ratio for each persuasive factor."
  ),
  sentiment_shift_patterns = list(
    title = "Sentiment Shift Patterns",
    desc = "Visualizes how emotional tone evolves throughout successful discussions compared to unsuccessful ones. This chart tracks emotional transitions to identify patterns that lead to view changes."
  ),
  narrative_arc = list(
    title = "Narrative Arc Components",
    desc = "Shows the progression of argument complexity, evidence usage, and sentiment as the discussion unfolds. Peaks represent high engagement points, often preceding delta awards."
  ),
  evidence_impact = list(
    title = "Evidence Type Impact",
    desc = "Compares the effectiveness of different evidence types in persuading the original poster. Personal anecdotes, logical arguments, expert opinions, and data-driven evidence are analyzed for their persuasive impact."
  ),
  persuasion_effectiveness = list(
    title = "Enhanced Persuasion Effectiveness",
    desc = "Shows not just the success rate by appeal type, but also the average time-to-delta and response rates, providing a comprehensive view of persuasion effectiveness."
  )
)

# ───────────────────────────────────────────────
# Load BERT Preprocessed Data
# ───────────────────────────────────────────────
bert_data <- reactive({
  tryCatch({
    bert_file <- "data/bert/reddit_bert_joined_analysis.rds"
    if (file.exists(bert_file)) {
      readRDS(bert_file)
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
})

# ───────────────────────────────────────────────
# Card UI and Rendering
# ───────────────────────────────────────────────

# Card 1 Description UI
output$persuasion_card1_desc <- renderUI({
  chart <- input$persuasion_chart1
  desc <- persuasion_chart_descriptions[[chart]]
  if (is.null(desc)) return(NULL)
  tags$div(
    style = "margin-bottom: 0.5rem;",
    tags$strong(desc$title),
    tags$p(desc$desc)
  )
})

# Card 2 Description UI
output$persuasion_card2_desc <- renderUI({
  chart <- input$persuasion_chart2
  desc <- persuasion_chart_descriptions[[chart]]
  if (is.null(desc)) return(NULL)
  tags$div(
    style = "margin-bottom: 0.5rem;",
    tags$strong(desc$title),
    tags$p(desc$desc)
  )
})

# Card 1 Content
output$persuasion_card1 <- renderUI({
  plotOutput("persuasion_card1_plot", height = "100%")
})

# Card 2 Content (always show plot for global charts)
output$persuasion_card2 <- renderUI({
  plotOutput("persuasion_card2_plot", height = "100%")
})


# ───────────────────────────────────────────────
# Local Persuasion Model for Selected Network
# ───────────────────────────────────────────────
persuasion_model <- reactive({
  # require that we have a network to work on
  req(networkData())
  
  # fetch BERT‐derived components & appeal scores
  arg_data    <- get_bert_data_for_selection("arg_components")
  appeal_data <- get_bert_data_for_selection("appeal_types")
  
  # need at least 10 posts in each
  if (is.null(arg_data) || is.null(appeal_data) ||
      nrow(arg_data) < 10 || nrow(appeal_data) < 10) {
    return(NULL)
  }
  
  # join on id
  model_data <- merge(arg_data, appeal_data, by = "id", suffixes = c("", "_appeal"))
  
  # delta_flag should already be in the BERT subset
  req("delta_flag" %in% names(model_data))
  
  # fit an un‐penalized logistic
  fit <- tryCatch({
    glm(
      delta_flag ~ claims + premises + rebuttals + concessions + evidence +
        logos_score + pathos_score + ethos_score,
      data   = model_data,
      family = binomial(link = "logit")
    )
  }, error = function(e) {
    message("Error fitting persuasion model: ", e$message)
    NULL
  })
  
  fit
})


# ───────────────────────────────────────────────
# Card 2 Plot Rendering
# ───────────────────────────────────────────────

output$persuasion_card2_plot <- renderPlot({
  # Get chart type from input
  chart_type <- input$persuasion_chart2
  
  if (chart_type == "persuasion_coef_plot") {
    # Get the global model
    fit <- global_persuasion_model()
    
    # Handle missing model case
    if (is.null(fit)) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Insufficient data for global model analysis.\nEnsure that preprocessed data includes delta information.", cex = 1.2)
      return()
    }
    
    # Make sure we can extract coefficients
    if (!inherits(fit, "glm") || is.null(coef(fit)) || length(coef(fit)) <= 1) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Model contains no usable coefficients.", cex = 1.2)
      return()
    }
    
    # Extract model summary and coefficients safely
    model_summary <- tryCatch({
      summary(fit)
    }, error = function(e) {
      message("Error extracting model summary: ", e$message)
      NULL
    })
    
    if (is.null(model_summary) || is.null(model_summary$coefficients)) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Cannot extract model coefficients.", cex = 1.2)
      return()
    }
    
    # Get coefficients (excluding intercept)
    model_coefs <- model_summary$coefficients
    
    # Check if we have at least one non-intercept coefficient
    if (nrow(model_coefs) <= 1) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Model contains only intercept.", cex = 1.2)
      return()
    }
    
    # Extract non-intercept estimates
    coefs <- model_coefs[-1, "Estimate"]
    
    # Create dataframe for plotting
    df_coefs <- data.frame(
      term = names(coefs),
      estimate = as.numeric(coefs),
      odds = exp(as.numeric(coefs)),
      stringsAsFactors = FALSE
    ) %>% 
      # Clean up term names for display
      mutate(term = gsub("_score", "", term)) %>%
      # Sort by absolute effect size
      arrange(desc(abs(estimate)))
    
    # Create the plot
    ggplot(df_coefs, aes(x = odds, y = reorder(term, odds))) +
      geom_segment(aes(x = 1, xend = odds, y = term, yend = term),
                   color = "#0063A6", linewidth = 1.2) +
      geom_point(size = 3, color = "#0063A6") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "#E4003A") +
      scale_x_log10() +
      labs(
        title = "Global Feature Effects on Delta Awards",
        subtitle = paste("Based on", nrow(model_summary$model), "data points"),
        x = "Odds ratio (glm coefficient)",
        y = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(t = 10))
      )
  } else if (chart_type == "sentiment_shift_patterns") {
    shift_data <- extract_sentiment_shifts(is_global = TRUE)
    
    if (is.null(shift_data) || nrow(shift_data) < 2) {
      plot.new()
      title("Insufficient data for global sentiment shift analysis")
      return()
    }
    
    # Calculate average emotion scores by depth across all threads
    emotions <- c("anger", "fear", "disgust", "sadness", "joy", "trust", "anticipation", "surprise")
    
    # Filter for reasonable depths (0-5)
    filtered_data <- shift_data[shift_data$depth <= 5, ]
    if (nrow(filtered_data) < 2) {
      plot.new()
      title("Global threads not deep enough for analysis")
      return()
    }
    
    # Reshape for plotting
    emotion_data <- filtered_data %>%
      group_by(depth, on_delta_path) %>%
      summarize(across(all_of(emotions), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      pivot_longer(cols = all_of(emotions), names_to = "emotion", values_to = "score")
    
    # Define color mapping for emotions
    emotion_colors <- c(
      "anger" = "red",
      "fear" = "purple",
      "disgust" = "darkgreen",
      "sadness" = "blue",
      "joy" = "yellow",
      "trust" = "lightblue",
      "anticipation" = "orange",
      "surprise" = "pink"
    )
    
    # Calculate the range for consistent y-axis limits
    y_min <- max(0.001, min(emotion_data$score, na.rm = TRUE))
    y_max <- max(1, max(emotion_data$score, na.rm = TRUE))
    
    # Plot emotion trajectories with enhanced visualization
    ggplot(emotion_data, aes(x = depth, y = score, 
                             color = emotion, 
                             fill = emotion,
                             linetype = on_delta_path)) +
      # Add subtle area fill for emphasis
      geom_area(aes(alpha = on_delta_path), position = "identity") +
      scale_alpha_manual(values = c("TRUE" = 0.15, "FALSE" = 0.05)) +
      # Keep line and points
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      # Reference line for consistent comparison
      geom_hline(yintercept = 0.1, linetype = "dotted", color = "darkgray") +
      # Apply log scale to y-axis
      scale_y_log10(limits = c(y_min, y_max)) +
      facet_wrap(~ emotion, ncol = 2) +
      scale_color_manual(values = emotion_colors) +
      scale_fill_manual(values = emotion_colors) +
      scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"),
                            labels = c("TRUE" = "Delta Path", "FALSE" = "Non-Delta Path")) +
      labs(
        title = "Global Emotion Trajectories (Log Scale)",
        subtitle = "Averaged across multiple discussions",
        x = "Discussion Depth",
        y = "Emotion Score (log scale)",
        color = "Emotion",
        fill = "Emotion",
        linetype = "Path Type"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill = "lightgray", color = NA),
        panel.grid.minor = element_blank()
      ) +
      guides(alpha = "none") # Hide the alpha legend
  } else if (chart_type == "evidence_impact") {
    # Get global evidence type data
    evidence_data <- analyze_evidence_types(is_global = TRUE)
    
    if (is.null(evidence_data) || nrow(evidence_data) == 0) {
      plot.new()
      title("Insufficient data for global evidence analysis")
      return()
    }
    
    # Plot global evidence type effectiveness
    ggplot(evidence_data, aes(x = reorder(type, rate), y = rate, fill = type)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = sprintf("%.0f%%", rate * 100)), 
                position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
      geom_text(aes(y = 0.05, label = sprintf("n=%d", count)), 
                hjust = 0, color = "black", size = 3.5) +
      scale_fill_manual(values = c(
        "Personal Anecdote" = "#0063A6",
        "Logical Argument" = "#009A93",
        "Expert Opinion" = "#F6A800",
        "Data-Driven" = "#E4003A"
      )) +
      coord_flip() +
      labs(
        title = "Global Evidence Type Impact on Persuasion",
        subtitle = "Success rates across all analyzed discussions",
        x = "",
        y = "Global Persuasion Success Rate"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 12),
        panel.grid.major.y = element_blank()
      )
  }
})



# ───────────────────────────────────────────────
# Helper Functions for Global Analysis
# ───────────────────────────────────────────────

global_persuasion_model <- reactive({
  # Get all BERT data
  bert <- bert_data()
  if (is.null(bert)) {
    message("No BERT data available for global persuasion model")
    return(NULL)
  }
  
  # Debugging - check what data we actually have
  message("BERT data structure: ", paste(names(bert), collapse=", "))
  
  # Get argument components and appeal types data
  arg_data <- bert$arg_components
  appeal_data <- bert$appeal_types
  
  # More debugging - check what the data looks like
  message("Argument components rows: ", ifelse(is.null(arg_data), "NULL", nrow(arg_data)))
  message("Appeal types rows: ", ifelse(is.null(appeal_data), "NULL", nrow(appeal_data)))
  
  if (is.null(arg_data) || is.null(appeal_data)) {
    message("Missing required data components")
    return(NULL)
  }
  
  # Don't restrict by minimum row counts - work with what we have
  # Join the datasets on the id column
  combined_data <- merge(arg_data, appeal_data, by = "id", suffixes = c("", "_appeal"))
  message("Merged data rows: ", nrow(combined_data))
  
  # Check if delta_flag exists
  if (!"delta_flag" %in% colnames(combined_data)) {
    # Check if on_delta_path exists instead
    if ("on_delta_path" %in% colnames(combined_data)) {
      message("Using on_delta_path as delta_flag")
      combined_data$delta_flag <- combined_data$on_delta_path
    } else {
      # Try to derive it from the network data if possible
      message("delta_flag not found, attempting to derive from network data")
      # Get all posts
      posts <- data_raw()
      if (!is.null(posts) && length(posts) > 0) {
        # Flag posts containing deltas
        has_delta <- sapply(posts, function(p) contains_delta(p, original_author = p$author))
        message("Posts with delta: ", sum(has_delta, na.rm = TRUE), " of ", length(posts))
        
        # Find all nodes on delta paths
        delta_paths <- character(0)
        for (i in which(has_delta)) {
          post <- posts[[i]]
          all_nodes <- get_nodes(post, is_root = TRUE)
          all_edges <- get_edges(post)
          
          if (length(all_nodes) > 0 && length(all_edges) > 0) {
            nodes_dt <- unique(data.table::rbindlist(lapply(all_nodes, as.data.table), fill = TRUE))
            edges_dt <- data.table::rbindlist(lapply(all_edges, as.data.table), fill = TRUE)
            
            g <- tryCatch({
              graph_from_data_frame(d = edges_dt, vertices = nodes_dt, directed = TRUE)
            }, error = function(e) NULL)
            
            if (!is.null(g)) {
              delta_nodes <- nodes_dt$id[nodes_dt$shape == "triangle"]
              root_nodes <- nodes_dt$id[!(nodes_dt$id %in% edges_dt$to)]
              if (length(root_nodes) == 0) root_nodes <- nodes_dt$id[1]
              
              for (root in root_nodes) {
                for (delta in delta_nodes) {
                  try({
                    path <- shortest_paths(g, from = root, to = delta)$vpath[[1]]
                    delta_paths <- union(delta_paths, as_ids(path))
                  }, silent = TRUE)
                }
              }
            }
          }
        }
        
        message("Total nodes on delta paths: ", length(delta_paths))
        
        # Apply to combined data
        combined_data$delta_flag <- combined_data$id %in% delta_paths
      } else {
        message("Cannot derive delta_flag - no post data")
        return(NULL)
      }
    }
  }
  
  # Select predictors
  predictors <- c("claims", "premises", "rebuttals", "concessions", "evidence",
                  "logos_score", "pathos_score", "ethos_score")
  
  # Filter rows with complete predictor data
  complete_rows <- complete.cases(combined_data[, c(predictors, "delta_flag")])
  message("Complete rows: ", sum(complete_rows))
  
  if (sum(complete_rows) == 0) {
    message("No complete rows for model")
    return(NULL)
  }
  
  # Create a minimal model with whatever data we have
  model_data <- combined_data[complete_rows, ]
  
  # Make sure we have variation in the outcome
  if (length(unique(model_data$delta_flag)) < 2) {
    message("No variation in outcome variable")
    return(NULL)
  }
  
  # Use only available predictors
  available_predictors <- intersect(predictors, colnames(model_data))
  message("Available predictors: ", paste(available_predictors, collapse=", "))
  
  if (length(available_predictors) == 0) {
    message("No available predictors")
    return(NULL)
  }
  
  # Build formula dynamically based on available predictors
  formula_str <- paste("delta_flag ~", paste(available_predictors, collapse = " + "))
  message("Using formula: ", formula_str)
  
  # Fit logistic regression model
  tryCatch({
    fit <- glm(
      as.formula(formula_str),
      data = model_data,
      family = binomial(link = "logit")
    )
    
    # Basic validation of model
    if (is.null(fit) || is.null(coef(fit)) || length(coef(fit)) <= 1) {
      message("Invalid model fit")
      return(NULL)
    }
    
    message("Model fitted successfully with ", length(coef(fit)), " coefficients")
    return(fit)
  }, error = function(e) {
    message("Error in model fitting: ", e$message)
    return(NULL)
  })
})

# ───────────────────────────────────────────────
# Helper Functions to Build Visualizations
# ───────────────────────────────────────────────

# Function to get BERT data for the current selection
get_bert_data_for_selection <- function(type = "arg_components") {
  bert <- bert_data()
  if (is.null(bert) || is.null(bert[[type]])) {
    return(NULL)
  }
  
  # Get current network data
  net <- networkData()
  if (is.null(net) || is.null(net$nodes)) {
    return(NULL)
  }
  
  # Filter BERT data for current discussion
  current_ids <- net$nodes$id
  bert_subset <- bert[[type]][bert[[type]]$id %in% current_ids, ]
  
  # Add delta path information
  if (nrow(bert_subset) > 0) {
    g <- graph_from_data_frame(d = net$edges, vertices = net$nodes, directed = TRUE)
    delta_ids <- net$nodes$id[net$nodes$shape == "triangle"]
    root_nodes <- net$nodes$id[!(net$nodes$id %in% net$edges$to)]
    if (length(root_nodes) == 0) root_nodes <- net$nodes$id[1]
    
    # Find all nodes on paths to delta
    delta_path_ids <- character(0)
    for (r in root_nodes) {
      for (d in delta_ids) {
        # Use try to handle cases where no path exists
        try({
          path <- shortest_paths(g, from = r, to = d)$vpath[[1]]
          delta_path_ids <- union(delta_path_ids, as_ids(path))
        }, silent = TRUE)
      }
    }
    
    # Add delta path flag
    bert_subset$delta_flag <- bert_subset$id %in% delta_path_ids
  }
  
  return(bert_subset)
}

# Function to get global BERT data for a specific type
get_global_bert_data <- function(type = "arg_components") {
  bert <- bert_data()
  if (is.null(bert) || is.null(bert[[type]])) {
    return(NULL)
  }
  
  # Sample no more than 1000 rows for better performance
  if (nrow(bert[[type]]) > 1000) {
    set.seed(123) # For reproducibility
    return(bert[[type]][sample(nrow(bert[[type]]), 1000), ])
  }
  
  return(bert[[type]])
}

# Function to create time-ordered data for narrative arc visualization
prepare_narrative_arc_data <- function(is_global = FALSE) {
  if (is_global) {
    # Sample a few threads for global view
    posts <- data_raw()
    if (is.null(posts) || length(posts) == 0) return(NULL)
    
    sample_size <- min(5, length(posts))
    sampled_posts <- posts[sample(length(posts), sample_size)]
    
    all_arc_data <- list()
    for (i in seq_along(sampled_posts)) {
      thread_data <- extract_narrative_arc_data(sampled_posts[[i]])
      if (!is.null(thread_data) && nrow(thread_data) > 0) {
        thread_data$thread_id <- i
        all_arc_data[[i]] <- thread_data
      }
    }
    
    if (length(all_arc_data) == 0) return(NULL)
    return(do.call(rbind, all_arc_data))
  } else {
    # Get data for current selection
    net <- networkData()
    if (is.null(net) || is.null(net$selected_post)) return(NULL)
    
    return(extract_narrative_arc_data(net$selected_post))
  }
}

extract_narrative_arc_data <- function(post) {
  if (is.null(post)) return(NULL)
  
  # Helper to flatten the thread structure
  flatten_thread <- function(node, depth = 0, parent = NULL, path = NULL) {
    if (is.null(node)) return(NULL)
    
    # Skip automod and deltabot
    if (tolower(node$author) %in% c("deltabot", "automoderator")) return(NULL)
    
    # Get BERT data
    bert <- bert_data()
    if (!is.null(bert)) {
      arg_data <- bert$arg_components[bert$arg_components$id == node$id, ]
      appeal_data <- bert$appeal_types[bert$appeal_types$id == node$id, ]
    } else {
      arg_data <- NULL
      appeal_data <- NULL
    }
    
    # Extract sentiment
    sentiment <- get_nrc_sentiment(node$text)
    positive <- sum(sentiment[c("joy", "trust", "anticipation", "surprise")])
    negative <- sum(sentiment[c("anger", "fear", "disgust", "sadness")])
    sentiment_score <- (positive - negative) / (positive + negative + 0.001)
    
    # Calculate text complexity
    complexity <- nchar(node$text) / (str_count(node$text, "\\s+") + 1)
    
    # Extract values from BERT data
    if (!is.null(arg_data) && nrow(arg_data) > 0) {
      evidence_score <- arg_data$evidence
      claims_score <- arg_data$claims
      rebuttals_score <- arg_data$rebuttals
    } else {
      evidence_score <- NA
      claims_score <- NA
      rebuttals_score <- NA
    }
    
    if (!is.null(appeal_data) && nrow(appeal_data) > 0) {
      logos_score <- appeal_data$logos_score
      pathos_score <- appeal_data$pathos_score
      ethos_score <- appeal_data$ethos_score
    } else {
      logos_score <- NA
      pathos_score <- NA
      ethos_score <- NA
    }
    
    # Check for delta
    has_delta <- grepl("Delta|delta|Δ|∆", node$text, ignore.case = TRUE)
    
    # Create data for this node
    current <- data.frame(
      id = node$id,
      depth = depth,
      parent_id = if (is.null(parent)) NA_character_ else parent$id,
      author = node$author,
      time = if (!is.null(node$created_utc)) as.POSIXct(node$created_utc, origin = "1970-01-01", tz = "UTC") else NA,
      sentiment = sentiment_score,
      complexity = complexity,
      evidence = evidence_score,
      claims = claims_score,
      rebuttals = rebuttals_score,
      logos = logos_score,
      pathos = pathos_score,
      ethos = ethos_score,
      has_delta = has_delta,
      stringsAsFactors = FALSE
    )
    
    # Process children
    children_data <- NULL
    if (!is.null(node$comments) && length(node$comments) > 0) {
      for (child in node$comments) {
        child_data <- flatten_thread(child, depth + 1, node, c(path, node$id))
        children_data <- rbind(children_data, child_data)
      }
    }
    if (!is.null(node$replies) && length(node$replies) > 0) {
      for (child in node$replies) {
        child_data <- flatten_thread(child, depth + 1, node, c(path, node$id))
        children_data <- rbind(children_data, child_data)
      }
    }
    
    # Combine and return
    return(rbind(current, children_data))
  }
  
  thread_data <- flatten_thread(post)
  
  # Sort by time if available
  if (!is.null(thread_data) && nrow(thread_data) > 0 && !all(is.na(thread_data$time))) {
    thread_data <- thread_data[order(thread_data$time), ]
    
    # Add sequence number
    thread_data$seq <- 1:nrow(thread_data)
    
    # Calculate cumulative metrics
    thread_data$cum_sentiment <- cumsum(thread_data$sentiment) / seq_along(thread_data$sentiment)
    thread_data$cum_complexity <- cumsum(thread_data$complexity) / seq_along(thread_data$complexity)
    thread_data$cum_evidence <- cumsum(thread_data$evidence) / seq_along(thread_data$evidence)
    
    # Normalize components to ensure they are on the same scale
    thread_data$norm_complexity <- scale(thread_data$complexity)[, 1]
    thread_data$norm_evidence <- scale(thread_data$evidence)[, 1]
    thread_data$norm_sentiment <- scale(thread_data$sentiment)[, 1]
  }
  
  return(thread_data)
}

# Function to extract sentiment shifts from thread
extract_sentiment_shifts <- function(is_global = FALSE) {
  if (is_global) {
    # Sample threads for global view
    posts <- data_raw()
    if (is.null(posts) || length(posts) == 0) return(NULL)
    
    sample_size <- min(10, length(posts))
    sampled_posts <- posts[sample(length(posts), sample_size)]
    
    all_shift_data <- list()
    for (i in seq_along(sampled_posts)) {
      thread_shifts <- get_thread_sentiment_shifts(sampled_posts[[i]])
      if (!is.null(thread_shifts) && nrow(thread_shifts) > 0) {
        thread_shifts$thread_id <- i
        all_shift_data[[i]] <- thread_shifts
      }
    }
    
    if (length(all_shift_data) == 0) return(NULL)
    return(do.call(rbind, all_shift_data))
  } else {
    # Get data for current selection
    net <- networkData()
    if (is.null(net) || is.null(net$selected_post)) return(NULL)
    
    return(get_thread_sentiment_shifts(net$selected_post))
  }
}



get_thread_sentiment_shifts <- function(post) {
  if (is.null(post)) return(NULL)
  
  # Recursive helper
  extract_path_sentiment <- function(node, parent_sentiment = NULL, depth = 0) {
    if (is.null(node)) return(NULL)
    if (tolower(node$author) %in% c("deltabot","automoderator")) return(NULL)
    
    # Compute NRC sentiment
    sent <- get_nrc_sentiment(node$text)
    emotions <- c("anger","fear","disgust","sadness",
                  "joy","trust","anticipation","surprise")
    
    # CORRECTED: build a *numeric* named vector
    emotion_values <- as.numeric(sent[emotions])
    names(emotion_values) <- emotions
    
    # dominant emotion
    if (all(is.na(emotion_values)) || sum(emotion_values, na.rm = TRUE)==0) {
      dominant <- "neutral"
    } else {
      dominant <- names(emotion_values)[which.max(emotion_values)]
    }
    
    # shift from parent (Euclidean)
    shift <- NA_real_
    if (!is.null(parent_sentiment)) {
      shift <- sqrt(sum((emotion_values - parent_sentiment)^2, na.rm = TRUE))
    }
    
    has_delta <- grepl("Delta|Δ|∆", node$text, ignore.case = TRUE)
    
    current <- data.frame(
      id              = node$id,
      depth           = depth,
      dominant_emotion= dominant,
      anger           = sent$anger,
      fear            = sent$fear,
      disgust         = sent$disgust,
      sadness         = sent$sadness,
      joy             = sent$joy,
      trust           = sent$trust,
      anticipation    = sent$anticipation,
      surprise        = sent$surprise,
      shift           = shift,
      has_delta       = has_delta,
      stringsAsFactors= FALSE
    )
    
    # recurse into replies/comments
    children <- lapply(c(node$comments, node$replies), function(child) {
      extract_path_sentiment(child, emotion_values, depth+1)
    })
    children <- do.call(rbind, children)
    
    rbind(current, children)
  }
  
  # run it
  paths <- extract_path_sentiment(post)
  
  # now mark on‐delta‐path
  net <- networkData()
  if (!is.null(net) && nrow(net$edges)>0) {
    g <- graph_from_data_frame(net$edges, vertices=net$nodes, directed=TRUE)
    delta_ids <- net$nodes$id[net$nodes$shape=="triangle"]
    roots     <- setdiff(net$nodes$id, net$edges$to)
    if (length(roots)==0) roots <- net$nodes$id[1]
    
    onpath <- character(0)
    for (r in roots) for (d in delta_ids) {
      p <- try(shortest_paths(g, from=r, to=d)$vpath[[1]], silent=TRUE)
      if (inherits(p,"vpath")) onpath <- union(onpath, as_ids(p))
    }
    paths$on_delta_path <- paths$id %in% onpath
  } else {
    paths$on_delta_path <- paths$has_delta
  }
  
  paths
}


# Function to analyze evidence types
analyze_evidence_types <- function(is_global = FALSE) {
  if (is_global) {
    # Get global BERT data
    arg_data <- get_global_bert_data("arg_components")
    if (is.null(arg_data)) return(NULL)
    
    # Add dummy delta flag for testing if needed
    if (!"delta_flag" %in% colnames(arg_data)) {
      set.seed(123)
      arg_data$delta_flag <- sample(c(TRUE, FALSE), nrow(arg_data), replace = TRUE, prob = c(0.3, 0.7))
    }
  } else {
    # Get data for current selection
    arg_data <- get_bert_data_for_selection("arg_components")
    if (is.null(arg_data)) return(NULL)
  }
  
  # Perform text analysis to categorize evidence types
  # This is a simplified example - you would need to implement more sophisticated 
  # NLP to truly classify evidence types
  arg_data$personal_anecdote <- grepl("I|my|me|mine|we|our|us", arg_data$text, ignore.case = TRUE) & 
    arg_data$evidence > 0.5
  
  arg_data$logical_argument <- arg_data$premises > 0.6 & arg_data$claims > 0.5
  
  arg_data$expert_opinion <- grepl("according to|expert|study|research|professor|Dr\\.|Ph\\.D", 
                                   arg_data$text, ignore.case = TRUE) & 
    arg_data$evidence > 0.5
  
  arg_data$data_driven <- grepl("data|statistics|percent|study|studies|researcher|analysis", 
                                arg_data$text, ignore.case = TRUE) & 
    arg_data$evidence > 0.5
  
  # For each evidence type, calculate persuasion rate
  calc_rate <- function(evidence_col) {
    delta_with_evidence <- sum(arg_data[[evidence_col]] & arg_data$delta_flag, na.rm = TRUE)
    total_with_evidence <- sum(arg_data[[evidence_col]], na.rm = TRUE)
    
    if (total_with_evidence > 0) {
      return(delta_with_evidence / total_with_evidence)
    } else {
      return(0)
    }
  }
  
  # Calculate rates
  evidence_rates <- data.frame(
    type = c("Personal Anecdote", "Logical Argument", "Expert Opinion", "Data-Driven"),
    rate = c(
      calc_rate("personal_anecdote"),
      calc_rate("logical_argument"),
      calc_rate("expert_opinion"),
      calc_rate("data_driven")
    ),
    count = c(
      sum(arg_data$personal_anecdote, na.rm = TRUE),
      sum(arg_data$logical_argument, na.rm = TRUE),
      sum(arg_data$expert_opinion, na.rm = TRUE),
      sum(arg_data$data_driven, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  
  return(evidence_rates)
}


# ───────────────────────────────────────────────
# Card 1 Plot Rendering (all local persuasion charts)
# ───────────────────────────────────────────────
output$persuasion_card1_plot <- renderPlot({
  chart_type <- input$persuasion_chart1
  
  # 1) Feature Effects on Δ Odds
  if (chart_type == "persuasion_coef_plot") {
    fit <- persuasion_model()
    if (is.null(fit) || length(coef(fit)) <= 1) {
      plot.new(); title("Insufficient data for model"); return()
    }
    coefs <- coef(summary(fit))[-1, "Estimate"]
    df_coefs <- data.frame(
      term     = names(coefs),
      estimate = as.numeric(coefs),
      odds     = exp(coefs),
      stringsAsFactors = FALSE
    ) %>% arrange(desc(abs(estimate)))
    
    ggplot(df_coefs, aes(x = odds, y = reorder(term, odds))) +
      geom_segment(aes(x = 1, xend = odds, y = term, yend = term),
                   color = "#0063A6", linewidth = 1.2) +
      geom_point(size = 3, color = "#0063A6") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "#E4003A") +
      scale_x_log10() +
      labs(
        title = "Feature Effects on Δ Odds",
        x     = "Odds‐ratio (glm coefficient)",
        y     = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y      = element_text(size = 12),
        axis.title.x     = element_text(size = 12, margin = margin(t = 10))
      )
    
    # 2) Sentiment Shift Patterns
  } else if (chart_type == "sentiment_shift_patterns") {
    shift_data <- extract_sentiment_shifts(is_global = FALSE)
    if (is.null(shift_data) || nrow(shift_data) < 2) {
      plot.new(); title("Insufficient data for sentiment shift analysis"); return()
    }
    # restrict to depths 0–5
    shift_data <- shift_data[shift_data$depth <= 5, ]
    if (nrow(shift_data) < 2) {
      plot.new(); title("Thread not deep enough for analysis"); return()
    }
    
    # If path-finding found no TRUEs, fall back to has_delta
    if (!"on_delta_path" %in% colnames(shift_data) ||
        !any(shift_data$on_delta_path, na.rm = TRUE)) {
      shift_data$on_delta_path <- shift_data$has_delta
    }
    
    emotions <- c("anger","fear","disgust","sadness","joy","trust","anticipation","surprise")
    emotion_summary <- shift_data %>%
      group_by(depth, on_delta_path) %>%
      summarise(across(all_of(emotions), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      pivot_longer(cols      = all_of(emotions),
                   names_to  = "emotion",
                   values_to = "score") %>%
      mutate(on_delta_path = factor(on_delta_path,
                                    levels = c(FALSE, TRUE),
                                    labels = c("Non-Δ Path", "Δ Path")))
    
    # Calculate the range for consistent y-axis limits
    y_min <- max(0.001, min(emotion_summary$score, na.rm = TRUE))
    y_max <- max(1, max(emotion_summary$score, na.rm = TRUE))
    
    ggplot(emotion_summary,
           aes(x = depth, y = score,
               color     = on_delta_path,
               fill      = on_delta_path,
               linetype  = on_delta_path)) +
      # Add subtle area fill for emphasis
      geom_area(alpha = 0.15, position = "identity") +
      # Keep line and points
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      # Reference line at 0.1 for consistent comparison
      geom_hline(yintercept = 0.1, linetype = "dotted", color = "darkgray") +
      # Apply log scale to y-axis only
      scale_y_log10(limits = c(y_min, y_max)) +
      facet_wrap(~ emotion, ncol = 2) +
      scale_color_manual(values = c("Non-Δ Path" = "#E4003A",
                                    "Δ Path"     = "#0063A6")) +
      scale_fill_manual(values = c("Non-Δ Path" = "#E4003A",
                                  "Δ Path"     = "#0063A6")) +
      scale_linetype_manual(values = c("Non-Δ Path" = "dashed",
                                      "Δ Path"     = "solid")) +
      labs(
        title    = "Sentiment Shift Patterns (Log Scale)",
        subtitle = "Δ-path vs non-Δ-path emotion trajectories",
        x        = "Discussion Depth",
        y        = "Average Emotion Score (log scale)",
        color    = "Path Type",
        fill     = "Path Type",
        linetype = "Path Type"
      ) +
      theme_minimal() +
      theme(
        legend.position  = "top",
        strip.text       = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill = "lightgray", color = NA),
        panel.grid.minor = element_blank()
      )
    
    # 3) Narrative Arc Visualization

  } else if (chart_type == "narrative_arc") {
    # 3) Narrative Arc Visualization
    arc_data <- prepare_narrative_arc_data(is_global = FALSE)
    if (is.null(arc_data) || nrow(arc_data) < 3) {
      plot.new()
      title("Insufficient data for narrative arc")
      return()
    }
    
    # Standardize each metric and scale between -1 and 1
    arc_data <- arc_data %>%
      mutate(
        norm_complexity = scale(complexity)[,1] / max(abs(scale(complexity)[,1]), na.rm = TRUE),
        norm_evidence   = scale(evidence)[,1] / max(abs(scale(evidence)[,1]), na.rm = TRUE),
        norm_sentiment  = scale(sentiment)[,1] / max(abs(scale(sentiment)[,1]), na.rm = TRUE)
      )
    
    # Pivot to long form
    plot_df <- arc_data %>%
      select(seq, norm_complexity, norm_evidence, norm_sentiment) %>%
      pivot_longer(-seq, names_to = "metric", values_to = "value")
    
    # **Only** find the true Δ‐award nodes by shape
    net       <- networkData()
    delta_ids <- net$nodes$id[net$nodes$shape == "triangle"]
    delta_pts <- arc_data[arc_data$id %in% delta_ids, , drop = FALSE]
    
    ggplot(plot_df, aes(x = seq, y = value, color = metric)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      # draw a dashed red line *only* at real Δ awards
      geom_vline(
        data       = delta_pts,
        aes(xintercept = seq),
        linetype   = "dashed",
        color      = "#E4003A",
        linewidth  = 1
      ) +
      scale_color_manual(
        values = c(
          norm_complexity = "#0063A6",
          norm_evidence   = "#009A93",
          norm_sentiment  = "#F6A800"
        ),
        labels = c(
          norm_complexity = "Text Complexity",
          norm_evidence   = "Evidence Usage",
          norm_sentiment  = "Sentiment"
        )
      ) +
      labs(
        title    = "Narrative Arc Through Discussion",
        subtitle = "Red dashed lines = Δ awards",
        x        = "Sequence in Discussion",
        y        = "Standardized Value",
        color    = "Metric"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title    = element_blank()
      )
    
    # 4) Evidence Type Impact
  } else if (chart_type == "evidence_impact") {
    evidence_data <- analyze_evidence_types(is_global = FALSE)
    if (is.null(evidence_data) || nrow(evidence_data) == 0) {
      plot.new(); title("Insufficient data for evidence analysis"); return()
    }
    ggplot(evidence_data, aes(x = reorder(type, rate), y = rate, fill = type)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = sprintf("%.0f%%", rate * 100)),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold") +
      geom_text(aes(y = 0.05, label = sprintf("n=%d", count)),
                hjust = 0, color = "black", size = 3.5) +
      scale_fill_manual(values = c(
        "Personal Anecdote" = "#0063A6",
        "Logical Argument"  = "#009A93",
        "Expert Opinion"    = "#F6A800",
        "Data-Driven"       = "#E4003A"
      )) +
      coord_flip() +
      labs(
        title    = "Evidence Type Impact on Persuasion",
        subtitle = "Local discussion success rates",
        x        = NULL,
        y        = "Success Rate"
      ) +
      theme_minimal() +
      theme(
        legend.position    = "none",
        axis.text.y        = element_text(size = 12),
        panel.grid.major.y = element_blank()
      )
    
    # 5) Enhanced Persuasion Effectiveness
  } else if (chart_type == "persuasion_effectiveness") {
    appeal_data <- get_bert_data_for_selection("appeal_types")
    if (is.null(appeal_data) || nrow(appeal_data) < 3) {
      plot.new(); title("Insufficient data for analysis"); return()
    }
    calc_metrics <- function(col) {
      sel   <- appeal_data[appeal_data[[col]] > 0.6, ]
      n_sel <- nrow(sel)
      list(
        success_rate  = if (n_sel > 0) mean(sel$delta_flag) else 0,
        response_rate = if (n_sel > 0) mean(sel$id %in% appeal_data$parent_id) else 0,
        count         = n_sel
      )
    }
    logos  <- calc_metrics("logos_score")
    pathos <- calc_metrics("pathos_score")
    ethos  <- calc_metrics("ethos_score")
    plot_df <- data.frame(
      appeal       = c("Logos","Pathos","Ethos"),
      success_rate = c(logos$success_rate, pathos$success_rate, ethos$success_rate),
      response_rate= c(logos$response_rate,pathos$response_rate,ethos$response_rate),
      count        = c(logos$count,     pathos$count,     ethos$count),
      stringsAsFactors = FALSE
    ) %>%
      pivot_longer(c(success_rate,response_rate),
                   names_to = "metric", values_to = "value") %>%
      mutate(metric = factor(metric,
                             levels = c("success_rate","response_rate"),
                             labels = c("Success Rate","Response Rate")))
    ggplot(plot_df, aes(x = appeal, y = value, fill = metric)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = sprintf("%.0f%%", value * 100)),
                position = position_dodge(width = 0.7), vjust = -0.5) +
      geom_text(aes(y = 0.05, label = sprintf("n=%d", count)),
                position = position_dodge(width = 0.7)) +
      scale_fill_manual(values = c("Success Rate" = "#0063A6", "Response Rate" = "#F6A800")) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(
        title    = "Persuasion Effectiveness by Appeal Type",
        subtitle = "Local discussion metrics",
        x        = "Appeal Type",
        y        = "Rate",
        fill     = "Metric"
      ) +
      theme_minimal() +
      theme(
        legend.position    = "top",
        axis.text.x        = element_text(size = 12),
        panel.grid.major.x = element_blank()
      )
    
    # No chart selected
  } else {
    plot.new()
    title("No chart selected")
  }
})

