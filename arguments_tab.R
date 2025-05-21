# ───────────────────────────────────────────────
# Arguments Card Descriptions
# ───────────────────────────────────────────────
# Argument Structure chart descriptions (move from UI to server)
argument_chart_descriptions <- list(
  argument_components_radar = list(
    title = "Argument Component Balance",
    desc = "Compares the average presence of argument components (claims, premises, rebuttals, concessions, evidence) between posts on the delta path and those not on the delta path. Helps identify which argument styles are more common in successful persuasion."
  ),
  argument_flow_chord = list(
    title = "Flow Between Components",
    desc = "Shows how argument components transition from one to another in the reply structure. Visualizes the flow of argument types between parent and child posts. Colors: Claims: Blue, Premises: Orange, Rebuttals: Green, Concessions: Red, Evidence: Purple."
  ),
  appeal_types_plot = list(
    title = "Appeal Types Distribution",
    desc = "Displays the distribution of rhetorical appeals (logos, pathos, ethos) across posts, highlighting differences between delta and non-delta paths."
  ),
  argument_temporal_evolution = list(
    title = "Temporal Evolution",
    desc = "Tracks how argument components change over the course of the discussion, showing trends in argument style as the conversation progresses."
  ),
  global_argument_components_radar = list(
    title = "Global Argument Component Balance",
    desc = "Shows the average argument component scores across all discussions in the dataset, comparing delta-winning and non-delta discussions."
  ),
  global_argument_flow_chord = list(
    title = "Global Flow Between Components",
    desc = "Aggregates argument component transitions across all discussions, visualizing common flows in argument structure. Colors: Claims: Blue, Premises: Orange, Rebuttals: Green, Concessions: Red, Evidence: Purple."
  ),
  global_appeal_types_plot = list(
    title = "Global Appeal Types Distribution",
    desc = "Displays the overall distribution of rhetorical appeals (logos, pathos, ethos) in the dataset, comparing successful and unsuccessful persuasion."
  ),
  global_argument_temporal_evolution = list(
    title = "Global Temporal Evolution Mean",
    desc = "Shows the average evolution of argument components over time across all discussions, highlighting general trends."
  )
)

# ───────────────────────────────────────────────
# Card UI and Rendering
# ───────────────────────────────────────────────
output$argument_card1 <- renderUI({
  plotOutput("argument_card1_plot", height = "100%")
})
output$argument_card2 <- renderUI({
  plotOutput("argument_card2_plot", height = "100%")
})

output$argument_card1_plot <- renderPlot({ 
  chart_fun <- get(input$argument_chart1)
  chart_fun()
})
output$argument_card2_plot <- renderPlot({
  chart_fun <- get(input$argument_chart2)
  chart_fun()
})


output$argument_card1_desc <- renderUI({
  chart <- input$argument_chart1
  desc <- argument_chart_descriptions[[chart]]
  if (is.null(desc)) return(NULL)
  tags$div(
    style = "margin-bottom: 0.5rem;",
    tags$strong(desc$title),
    tags$p(desc$desc)
  )
})

output$argument_card2_desc <- renderUI({
  chart <- input$argument_chart2
  desc <- argument_chart_descriptions[[chart]]
  if (is.null(desc)) return(NULL)
  tags$div(
    style = "margin-bottom: 0.5rem;",
    tags$strong(desc$title),
    tags$p(desc$desc)
  )
})


# ───────────────────────────────────────────────
# Appeal Types Plots
# ───────────────────────────────────────────────

global_appeal_types_plot <- function() {
  # Check if precomputed BERT data exists
  bert_data_path <- "data/bert/reddit_bert_joined_analysis.rds"
  if (!file.exists(bert_data_path)) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No BERT data available. Run preprocessing script first.", cex = 1.2)
    return(NULL)
  }
  
  # Load precomputed BERT data
  bert_data <- tryCatch({
    readRDS(bert_data_path)
  }, error = function(e) {
    message(paste0("Error loading BERT data: ", e$message))
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "Error loading BERT data file", cex = 1.2)
    return(NULL)
  })
  
  # Check for appeal types data
  if (is.null(bert_data) || is.null(bert_data$appeal_types) || nrow(bert_data$appeal_types) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "BERT data file contains no appeal types data", cex = 1.2)
    return(NULL)
  }
  
  # Get posts data to identify delta paths
  posts <- data_raw()
  if (is.null(posts) || length(posts) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No posts data available", cex = 1.5)
    return(NULL)
  }
  
  # Initialize data to track delta paths
  all_delta_ids <- character(0)
  post_count <- 0
  delta_post_count <- 0
  
  # Process each post to identify delta paths
  message("Identifying delta paths across posts...")
  for (post_index in seq_along(posts)) {
    post <- posts[[post_index]]
    post_count <- post_count + 1
    
    # Check if post contains a delta
    has_delta <- contains_delta(post, original_author = post$author)
    if (has_delta) {
      delta_post_count <- delta_post_count + 1
      
      # Extract network structure
      all_nodes <- get_nodes(post, is_root = TRUE)
      all_edges <- get_edges(post)
      
      if (length(all_nodes) > 0 && length(all_edges) > 0) {
        nodes_dt <- unique(data.table::rbindlist(lapply(all_nodes, as.data.table), fill = TRUE))
        edges_dt <- data.table::rbindlist(lapply(all_edges, as.data.table), fill = TRUE)
        
        # Only process if we have valid network data
        if (nrow(nodes_dt) > 0 && nrow(edges_dt) > 0) {
          # Find delta nodes
          delta_nodes <- nodes_dt$id[nodes_dt$shape == "triangle"]
          
          if (length(delta_nodes) > 0) {
            # Build graph
            g <- tryCatch({
              graph_from_data_frame(d = edges_dt, vertices = nodes_dt, directed = TRUE)
            }, error = function(e) NULL)
            
            if (!is.null(g)) {
              # Find root nodes
              root_nodes <- nodes_dt$id[!(nodes_dt$id %in% edges_dt$to)]
              if (length(root_nodes) == 0) root_nodes <- nodes_dt$id[1]
              
              # Find all nodes on paths to delta
              for (root in root_nodes) {
                for (delta in delta_nodes) {
                  # Use try to handle cases where no path exists
                  try({
                    path <- shortest_paths(g, from = root, to = delta)$vpath[[1]]
                    all_delta_ids <- union(all_delta_ids, as_ids(path))
                  }, silent = TRUE)
                }
              }
            }
          }
        }
      }
    }
  }
  
  message(paste0("Found ", length(all_delta_ids), " nodes on delta paths across ", 
                 delta_post_count, " posts with deltas"))
  
  # Add delta path flag to appeal types data
  appeal_data <- bert_data$appeal_types
  appeal_data$on_delta_path <- appeal_data$id %in% all_delta_ids
  
  # Define appeal columns
  appeal_cols <- c("logos_score", "pathos_score", "ethos_score")
  
  # Check if we have the required columns
  if (!all(appeal_cols %in% colnames(appeal_data))) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "BERT data doesn't contain required appeal scores", cex = 1.2)
    return(NULL)
  }
  
  # Only keep rows with complete appeal data
  complete_rows <- complete.cases(appeal_data[, appeal_cols])
  if (sum(complete_rows) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No complete appeal type data found in BERT analysis", cex = 1.2)
    return(NULL)
  }
  appeal_data <- appeal_data[complete_rows, ]
  
  # Reshape to long format for plotting
  appeal_long <- tidyr::pivot_longer(
    appeal_data,
    cols = all_of(appeal_cols),
    names_to = "appeal_type",
    values_to = "score"
  )
  
  # Clean up appeal type names
  appeal_long$appeal_type <- gsub("_score", "", appeal_long$appeal_type)
  appeal_long$appeal_type <- tools::toTitleCase(appeal_long$appeal_type)
  
  # Group by delta path and appeal type
  appeal_summary <- appeal_long %>%
    dplyr::group_by(on_delta_path, appeal_type) %>%
    dplyr::summarise(
      avg_score = mean(score, na.rm = TRUE),
      std_error = sd(score, na.rm = TRUE) / sqrt(n()),
      count = n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(path_type = ifelse(on_delta_path, "Delta Path", "Non-Delta Path"))
  
  # Count delta and non-delta nodes
  delta_nodes <- sum(appeal_data$on_delta_path)
  non_delta_nodes <- sum(!appeal_data$on_delta_path)
  
  # Create a bar chart with error bars
  ggplot(appeal_summary, aes(x = appeal_type, y = avg_score, fill = path_type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_errorbar(
      aes(ymin = pmax(0, avg_score - std_error), ymax = pmin(1, avg_score + std_error)),
      position = position_dodge(width = 0.9),
      width = 0.25
    ) +
    scale_fill_manual(values = c("Delta Path" = "#0063A6", "Non-Delta Path" = "#E4003A")) +
    scale_y_continuous(limits = c(0, max(appeal_summary$avg_score + appeal_summary$std_error) * 1.1), 
                       breaks = seq(0, 1, 0.1)) +
    labs(
      title = "Global Appeal Types Analysis: Delta vs Non-Delta Paths (BERT Analysis)",
      subtitle = paste0(
        "Across ", post_count, " discussions (", delta_post_count, " with deltas)\n",
        "Delta paths: ", delta_nodes, " nodes | Non-delta paths: ", non_delta_nodes, " nodes"
      ),
      x = "Appeal Type",
      y = "Average Score",
      fill = "Path Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "top"
    )
}



appeal_types_plot <- function() {
  # Get argument structure data
  arg_data <- argument_structure_data()
  if (is.null(arg_data) || is.null(arg_data$flat)) return(NULL)
  
  # Get network data to identify delta paths
  net <- networkData()
  if (is.null(net)) return(NULL)
  
  # Check if precomputed BERT data exists
  bert_data_path <- "data/bert/reddit_bert_joined_analysis.rds"
  if (!file.exists(bert_data_path)) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No BERT data available. Run preprocessing script first.", cex = 1.2)
    return(NULL)
  }
  
  # Load precomputed BERT data
  bert_data <- tryCatch({
    readRDS(bert_data_path)
  }, error = function(e) {
    message(paste0("Error loading BERT data: ", e$message))
    return(NULL)
  })
  
  if (is.null(bert_data) || is.null(bert_data$appeal_types) || nrow(bert_data$appeal_types) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "BERT data file contains no appeal types data", cex = 1.2)
    return(NULL)
  }
  
  # Build graph to find paths to deltas
  g <- graph_from_data_frame(d = net$edges, vertices = net$nodes, directed = TRUE)
  delta_nodes <- net$nodes$id[net$nodes$shape == "triangle"]
  
  # If no delta nodes, create a placeholder plot
  if (length(delta_nodes) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No delta awards found in this discussion", cex = 1.5)
    return(NULL)
  }
  
  # Find root nodes
  root_nodes <- net$nodes$id[!(net$nodes$id %in% net$edges$to)]
  if (length(root_nodes) == 0) root_nodes <- net$nodes$id[1]
  
  # Find all nodes on paths to delta
  delta_path_ids <- character(0)
  for (root in root_nodes) {
    for (delta in delta_nodes) {
      # Use try to handle cases where no path exists
      try({
        path <- shortest_paths(g, from = root, to = delta)$vpath[[1]]
        delta_path_ids <- union(delta_path_ids, as_ids(path))
      }, silent = TRUE)
    }
  }
  
  # Get all node IDs in the current discussion
  node_ids <- net$nodes$id
  
  # Extract BERT data for nodes that are in the current discussion
  bert_appeal_data <- bert_data$appeal_types[bert_data$appeal_types$id %in% node_ids, ]
  
  # If no matches were found, show a message
  if (nrow(bert_appeal_data) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No BERT data matches found for current discussion nodes", cex = 1.2)
    return(NULL)
  }
  
  # Add delta path information
  bert_appeal_data$on_delta_path <- bert_appeal_data$id %in% delta_path_ids
  
  # Define appeal columns
  appeal_cols <- c("logos_score", "pathos_score", "ethos_score")
  
  # Check if we have the required columns
  if (!all(appeal_cols %in% colnames(bert_appeal_data))) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "BERT data doesn't contain required appeal scores", cex = 1.2)
    return(NULL)
  }
  
  # Reshape to long format for plotting
  appeal_long <- tidyr::pivot_longer(
    bert_appeal_data,
    cols = appeal_cols,
    names_to = "appeal_type",
    values_to = "score"
  )
  
  # Clean up appeal type names
  appeal_long$appeal_type <- gsub("_score", "", appeal_long$appeal_type)
  appeal_long$appeal_type <- tools::toTitleCase(appeal_long$appeal_type)
  
  # Group by delta path and appeal type to get average scores
  appeal_summary <- appeal_long %>%
    dplyr::group_by(on_delta_path, appeal_type) %>%
    dplyr::summarise(
      avg_score = mean(score, na.rm = TRUE),
      std_error = sd(score, na.rm = TRUE) / sqrt(n()),
      count = n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(path_type = ifelse(on_delta_path, "Delta Path", "Non-Delta Path"))
  
  # Determine node counts for better subtitling
  delta_count <- sum(bert_appeal_data$on_delta_path)
  non_delta_count <- sum(!bert_appeal_data$on_delta_path)
  
  # Create a bar chart with error bars
  ggplot(appeal_summary, aes(x = appeal_type, y = avg_score, fill = path_type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_errorbar(
      aes(ymin = pmax(0, avg_score - std_error), ymax = pmin(1, avg_score + std_error)),
      position = position_dodge(width = 0.9),
      width = 0.25
    ) +
    scale_fill_manual(values = c("Delta Path" = "#0063A6", "Non-Delta Path" = "#E4003A")) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = "Appeal Types in Delta vs Non-Delta Paths (BERT Analysis)",
      subtitle = paste0(
        "Delta paths (", delta_count, " nodes) vs Non-delta paths (", 
        non_delta_count, " nodes) - ", nrow(bert_appeal_data), " nodes analyzed"
      ),
      x = "Appeal Type",
      y = "Average Score",
      fill = "Path Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "top"
    )
}



# ───────────────────────────────────────────────
# Argument Flow Chord diagram
# ───────────────────────────────────────────────

argument_flow_chord <- function() {
  flow <- argument_structure_data()$flow
  if (is.null(flow) || nrow(flow) == 0) {
    plot.new()
    title("No argument flow data")
    return(invisible())
  }

  # --- PATCH: Filter out empty or NA types before proceeding ---
  flow <- flow[
    !is.na(flow$from_type) & flow$from_type != "" &
    !is.na(flow$to_type) & flow$to_type != "",
  ]

  # Ensure all argument types are present as both rows and columns
  all_types <- c("claims", "premises", "rebuttals", "concessions", "evidence")
  types <- union(all_types, unique(c(flow$from_type, flow$to_type)))
  # Remove empty string or NA from types
  types <- types[types != "" & !is.na(types)]
  mat <- matrix(0, nrow = length(types), ncol = length(types),
                dimnames = list(types, types))
  for (i in seq_len(nrow(flow))) {
    from <- flow$from_type[i]
    to <- flow$to_type[i]
    if (from %in% types && to %in% types) {
      mat[from, to] <- flow$count[i]
    }
  }
  
  # Set colors for each component
  comp_colors <- c(
    claims = "#1f77b4",
    premises = "#ff7f0e",
    rebuttals = "#2ca02c",
    concessions = "#d62728",
    evidence = "#9467bd"
  )
  
  # Ensure all types have a color
  missing_cols <- setdiff(types, names(comp_colors))
  if (length(missing_cols) > 0) {
    comp_colors <- c(comp_colors, setNames(rep("gray", length(missing_cols)), missing_cols))
  }
  comp_colors <- comp_colors[types]
  
  # Only plot if there is at least one nonzero transition
  if (sum(mat) == 0) {
    plot.new()
    title("No argument flow data")
    return(invisible())
  }
  
  # --- PATCH: Set up plotting area and margins before plotting ---
  op <- par(no.readonly = TRUE)
  # Increase right margin for legend, top for title
  par(mar = c(1, 1, 5, 10))
  plot.new()
  # Draw chord diagram
  circlize::chordDiagram(
    mat,
    grid.col = comp_colors,
    transparency = 0.2,
    annotationTrack = c("grid"),
    preAllocateTracks = 1
  )
  # Title
  mtext("Flow Between Argument Components", side = 3, line = 2, cex = 1.3, font = 2)
  # --- PATCH: Draw legend while device is open, after chordDiagram ---
  legend("right",
         legend = tools::toTitleCase(names(comp_colors)),
         fill = comp_colors,
         border = "gray40",
         bty = "n",
         title = "Component",
         cex = 0.9,
         inset = c(-0.05, 0))
  par(op)
}


global_argument_flow_chord <- function() {
  # Check if precomputed BERT data exists
  bert_data_path <- "data/bert/reddit_bert_joined_analysis.rds"
  if (!file.exists(bert_data_path)) {
    plot.new()
    title("No BERT data available\nRun preprocessing script first", cex.main = 1.2)
    return(invisible())
  }
  
  # Load precomputed BERT data
  bert_data <- tryCatch({
    readRDS(bert_data_path)
  }, error = function(e) {
    message(paste0("Error loading BERT data: ", e$message))
    plot.new()
    title("Error loading BERT data", cex.main = 1.2)
    return(invisible())
  })
  
  # Check for argument components data
  if (is.null(bert_data) || is.null(bert_data$arg_components) || nrow(bert_data$arg_components) == 0) {
    plot.new()
    title("BERT data contains no argument components", cex.main = 1.2)
    return(invisible())
  }
  
  # Get all posts data
  posts <- data_raw()
  if (is.null(posts) || length(posts) == 0) {
    plot.new()
    title("No posts data available", cex.main = 1.2)
    return(invisible())
  }
  
  message("Processing argument flow across all discussions...")
  
  # Define argument component columns
  component_cols <- c("claims", "premises", "rebuttals", "concessions", "evidence")
  
  # Check if the component columns exist in the data
  if (!all(component_cols %in% colnames(bert_data$arg_components))) {
    plot.new()
    title("Required argument component columns missing in BERT data", cex.main = 1.2)
    return(invisible())
  }
  
  # Initialize counters for statistics
  post_count <- 0
  delta_post_count <- 0
  node_count <- 0
  delta_node_count <- 0
  
  # Initialize a matrix to hold the global flow counts
  all_types <- component_cols
  global_flow_mat <- matrix(0, nrow = length(all_types), ncol = length(all_types),
                            dimnames = list(all_types, all_types))
  
  # Process each post
  for (post_index in seq_along(posts)) {
    post <- posts[[post_index]]
    post_count <- post_count + 1
    
    # Safe check for has_delta - handle NA values
    has_delta <- tryCatch({
      contains_delta(post, original_author = post$author)
    }, error = function(e) {
      message(paste0("Error checking for delta in post ", post_index, ": ", e$message))
      return(FALSE)
    })
    
    # Handle NA value in has_delta
    if (is.na(has_delta)) has_delta <- FALSE
    
    if (has_delta) delta_post_count <- delta_post_count + 1
    
    # Extract network structure
    all_nodes <- tryCatch({
      get_nodes(post, is_root = TRUE)
    }, error = function(e) {
      message(paste0("Error getting nodes for post ", post_index, ": ", e$message))
      return(list())
    })
    
    all_edges <- tryCatch({
      get_edges(post)
    }, error = function(e) {
      message(paste0("Error getting edges for post ", post_index, ": ", e$message))
      return(list())
    })
    
    # Skip empty or invalid structures
    if (length(all_nodes) == 0 || length(all_edges) == 0) next
    
    # Safely convert to data tables
    nodes_dt <- tryCatch({
      unique(data.table::rbindlist(lapply(all_nodes, as.data.table), fill = TRUE))
    }, error = function(e) {
      message(paste0("Error creating nodes data table for post ", post_index, ": ", e$message))
      return(data.table::data.table())
    })
    
    edges_dt <- tryCatch({
      data.table::rbindlist(lapply(all_edges, as.data.table), fill = TRUE)
    }, error = function(e) {
      message(paste0("Error creating edges data table for post ", post_index, ": ", e$message))
      return(data.table::data.table())
    })
    
    # Only process if we have valid network data
    if (nrow(nodes_dt) > 0 && nrow(edges_dt) > 0) {
      # Find delta nodes
      delta_nodes <- character(0)
      if ("shape" %in% colnames(nodes_dt)) {
        delta_nodes <- nodes_dt$id[nodes_dt$shape == "triangle"]
      }
      delta_present <- length(delta_nodes) > 0
      
      # Get node IDs from this discussion
      discussion_node_ids <- nodes_dt$id
      
      # Extract BERT data for nodes in this discussion
      discussion_arg_data <- bert_data$arg_components[bert_data$arg_components$id %in% discussion_node_ids, ]
      
      if (nrow(discussion_arg_data) > 0) {
        # Add node count to the total
        node_count <- node_count + nrow(discussion_arg_data)
        
        # Build graph for this discussion - with error handling
        g <- tryCatch({
          graph_from_data_frame(d = edges_dt, vertices = nodes_dt, directed = TRUE)
        }, error = function(e) {
          message(paste0("Error creating graph for post ", post_index, ": ", e$message))
          return(NULL)
        })
        
        if (!is.null(g) && igraph::ecount(g) > 0) {
          # Get all edges
          edge_list <- tryCatch({
            as_edgelist(g, names = TRUE)
          }, error = function(e) {
            message(paste0("Error creating edge list for post ", post_index, ": ", e$message))
            return(matrix(character(0), ncol = 2))
          })
          
          # Only process if we have edges
          if (is.matrix(edge_list) && nrow(edge_list) > 0) {
            # For each edge, find the component with highest score in source and target nodes
            for (i in 1:nrow(edge_list)) {
              from_id <- edge_list[i, 1]
              to_id <- edge_list[i, 2]
              
              # Make sure both IDs are not NA or empty
              if (is.na(from_id) || is.na(to_id) || from_id == "" || to_id == "") next
              
              # Find these nodes in the BERT data
              from_idx <- which(discussion_arg_data$id == from_id)
              to_idx <- which(discussion_arg_data$id == to_id)
              
              # Proceed only if we found both nodes
              if (length(from_idx) > 0 && length(to_idx) > 0) {
                # Get component scores for source and target nodes
                from_scores <- as.numeric(discussion_arg_data[from_idx, component_cols])
                to_scores <- as.numeric(discussion_arg_data[to_idx, component_cols])
                
                # Skip if any scores are NA
                if (any(is.na(from_scores)) || any(is.na(to_scores))) next
                
                # Find dominant component types (highest scoring)
                from_type <- component_cols[which.max(from_scores)]
                to_type <- component_cols[which.max(to_scores)]
                
                # Check for NA in types
                if (is.na(from_type) || is.na(to_type)) next
                
                # Only count if both scores are above a threshold (e.g., 0.2)
                threshold <- 0.2
                max_from_score <- max(from_scores, na.rm = TRUE)
                max_to_score <- max(to_scores, na.rm = TRUE)
                
                # Safe check for max scores
                if (is.na(max_from_score) || is.na(max_to_score)) next
                
                if (max_from_score > threshold && max_to_score > threshold) {
                  # Increment the flow count in the matrix
                  global_flow_mat[from_type, to_type] <- global_flow_mat[from_type, to_type] + 1
                  
                  # Safe check for delta_present before proceeding
                  if (delta_present && length(delta_nodes) > 0) {
                    # Find all nodes on paths to delta
                    delta_path_ids <- character(0)
                    
                    # Find root nodes safely
                    root_nodes <- character(0)
                    if ("to" %in% colnames(edges_dt)) {
                      root_nodes <- nodes_dt$id[!(nodes_dt$id %in% edges_dt$to)]
                    }
                    
                    # If no root nodes found, use first node
                    if (length(root_nodes) == 0 && nrow(nodes_dt) > 0) {
                      root_nodes <- nodes_dt$id[1]
                    }
                    
                    # Only proceed if we have root nodes and delta nodes
                    if (length(root_nodes) > 0) {
                      for (root in root_nodes) {
                        for (delta in delta_nodes) {
                          # Skip invalid root or delta
                          if (is.na(root) || is.na(delta) || root == "" || delta == "") next
                          
                          # Use try to handle cases where no path exists
                          delta_path <- tryCatch({
                            path <- shortest_paths(g, from = root, to = delta)$vpath[[1]]
                            as_ids(path)
                          }, error = function(e) {
                            character(0)
                          })
                          
                          delta_path_ids <- union(delta_path_ids, delta_path)
                        }
                      }
                      
                      # If both nodes are on delta path, count as delta node
                      if (from_id %in% delta_path_ids && to_id %in% delta_path_ids) {
                        delta_node_count <- delta_node_count + 1
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # Provide progress updates
    if (post_index %% 10 == 0) {
      message(paste0("Processed ", post_index, " of ", length(posts), " posts..."))
    }
  }
  
  # Set colors for each component
  comp_colors <- c(
    claims = "#1f77b4",
    premises = "#ff7f0e",
    rebuttals = "#2ca02c",
    concessions = "#d62728",
    evidence = "#9467bd"
  )
  
  # Only plot if there is at least one nonzero transition
  if (sum(global_flow_mat) == 0) {
    plot.new()
    title("No argument flow data found across discussions", cex.main = 1.2)
    return(invisible())
  }
  
  # Filter to only include component types that have some data
  active_types <- character(0)
  for (i in 1:nrow(global_flow_mat)) {
    for (j in 1:ncol(global_flow_mat)) {
      if (global_flow_mat[i, j] > 0) {
        active_types <- union(active_types, rownames(global_flow_mat)[i])
        active_types <- union(active_types, colnames(global_flow_mat)[j])
      }
    }
  }
  
  # Only continue if we have active types
  if (length(active_types) == 0) {
    plot.new()
    title("No argument flow transitions found", cex.main = 1.2)
    return(invisible())
  }
  
  # Filter matrix to only include active types
  global_flow_mat <- global_flow_mat[active_types, active_types]
  
  # Ensure all active types have a color
  legend_colors <- comp_colors[names(comp_colors) %in% active_types]
  
  # Add colors for any missing types
  missing_types <- setdiff(active_types, names(legend_colors))
  if (length(missing_types) > 0) {
    extra_colors <- setNames(rep("gray", length(missing_types)), missing_types)
    legend_colors <- c(legend_colors, extra_colors)
  }
  
  # Ensure legend_colors has the same order as active_types
  legend_colors <- legend_colors[match(active_types, names(legend_colors))]
  
  # Reset plotting area and margins to fit container
  op <- par(mar = c(1, 1, 7, 10)) # Extra space for title and legend
  plot.new()
  
  # Create chord diagram
  circlize::chordDiagram(
    global_flow_mat,
    grid.col = legend_colors,
    transparency = 0.2,
    annotationTrack = c("grid"),
    preAllocateTracks = 1
  )
  
  # Main title
  mtext("Global Flow Between Argument Components", side = 3, line = 5, cex = 1.3, font = 2)
  
  # Subtitle with statistics
  mtext(paste0("Across ", post_count, " discussions (", delta_post_count, " with deltas)"), 
        side = 3, line = 3.5, cex = 0.9)
  mtext(paste0("Analyzed ", node_count, " nodes with ", sum(global_flow_mat), " transitions"), 
        side = 3, line = 2.5, cex = 0.9)
  
  # Only add legend if we have active types
  if (length(legend_colors) > 0) {
    # Create nicer legend labels by capitalizing
    legend_labels <- tools::toTitleCase(names(legend_colors))
    
    # Draw the legend
    legend("right",
           legend = legend_labels,
           fill = legend_colors,
           border = "gray40",
           bty = "n",
           title = "Component",
           cex = 0.9,
           inset = c(-0.05, 0))
  }
  
  par(op)
  
  # Return invisibly to allow saving
  return(invisible(global_flow_mat))
}


# # ───────────────────────────────────────────────
# # Argument Components Radar
# # ───────────────────────────────────────────────


argument_components_radar <- function() {
  # Get the flattened argument data
  arg_data <- argument_structure_data()
  if (is.null(arg_data) || is.null(arg_data$flat)) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "Insufficient data for analysis", cex = 1.5)
    return()
  }
  flat <- arg_data$flat

  # Get network data to identify delta paths
  net <- networkData()
  if (is.null(net)) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No network data available", cex = 1.5)
    return()
  }

  # Build graph to find paths to deltas
  g <- graph_from_data_frame(d = net$edges, vertices = net$nodes, directed = TRUE)
  delta_nodes <- net$nodes$id[net$nodes$shape == "triangle"]
  root_nodes <- net$nodes$id[!(net$nodes$id %in% net$edges$to)]
  if (length(root_nodes) == 0) root_nodes <- net$nodes$id[1]

  # Find all nodes on paths to delta
  delta_path_ids <- character(0)
  for (root in root_nodes) {
    for (delta in delta_nodes) {
      try({
        path <- shortest_paths(g, from = root, to = delta)$vpath[[1]]
        delta_path_ids <- union(delta_path_ids, as_ids(path))
      }, silent = TRUE)
    }
  }

  # Add delta path flag to flat data
  flat$on_delta_path <- flat$node_id %in% delta_path_ids

  # Calculate average component values for delta and non-delta paths
  component_cols <- c("claims", "premises", "rebuttals", "concessions", "evidence")
  component_names <- c("Claims", "Premises", "Rebuttals", "Concessions", "Evidence")

  delta_paths <- flat[flat$on_delta_path == TRUE, ]
  non_delta_paths <- flat[flat$on_delta_path == FALSE, ]

  delta_means <- if (nrow(delta_paths) > 0) {
    colMeans(delta_paths[, component_cols, drop = FALSE], na.rm = TRUE)
  } else {
    rep(0, length(component_cols))
  }
  non_delta_means <- if (nrow(non_delta_paths) > 0) {
    colMeans(non_delta_paths[, component_cols, drop = FALSE], na.rm = TRUE)
  } else {
    rep(0, length(component_cols))
  }

  # Radar chart with explicit lines for both groups
  if (requireNamespace("fmsb", quietly = TRUE)) {
    max_val <- max(c(delta_means, non_delta_means, 1), na.rm = TRUE)
    min_val <- 0
    radar_data <- rbind(
      setNames(rep(max_val, length(component_cols)), component_names),
      setNames(rep(min_val, length(component_cols)), component_names),
      setNames(as.numeric(delta_means), component_names),
      setNames(as.numeric(non_delta_means), component_names)
    )
    radar_data <- as.data.frame(radar_data)
    rownames(radar_data) <- c("Max", "Min", "Delta Path", "Non-Delta Path")
    op <- par(mar = c(2, 2, 4, 2))
    # Only set plwd and plty for the actual lines (not for the max/min rows)
    fmsb::radarchart(
      radar_data,
      axistype = 1,
      pcol = c("#0063A6", "#E4003A"),
      pfcol = c(scales::alpha("#0063A6", 0.15), scales::alpha("#E4003A", 0.15)),
      plwd = c(3, 3),
      plty = c(1, 1),
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey40",
      vlcex = 1.1,
      title = "Argument Components: Delta vs Non-Delta Paths"
    )
    legend("topright", legend = c("Delta Path", "Non-Delta Path"),
           col = c("#0063A6", "#E4003A"), lwd = 3, bty = "n")
    par(op)
  } else {
    # Fallback: grouped bar chart
    plot_data <- data.frame(
      Component = rep(component_names, 2),
      Value = c(delta_means, non_delta_means),
      Type = rep(c("Delta Path", "Non-Delta Path"), each = length(component_cols)),
      stringsAsFactors = FALSE
    )
    plot_data$Component <- factor(plot_data$Component, levels = component_names)
    print(
      ggplot(plot_data, aes(x = Component, y = Value, fill = Type)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
        scale_fill_manual(values = c("Delta Path" = "#0063A6", "Non-Delta Path" = "#E4003A")) +
        labs(
          title = "Argument Components: Delta vs Non-Delta Paths",
          x = "",
          y = "Average Score",
          fill = "Path Type"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "top"
        )
    )
  }
}

global_argument_components_radar <- function() {
  # Check if precomputed BERT data exists
  bert_data_path <- "data/bert/reddit_bert_joined_analysis.rds"
  if (!file.exists(bert_data_path)) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No BERT data available. Run preprocessing script first.", cex = 1.2)
    return(NULL)
  }
  
  # Load precomputed BERT data
  bert_data <- tryCatch({
    readRDS(bert_data_path)
  }, error = function(e) {
    message(paste0("Error loading BERT data: ", e$message))
    return(NULL)
  })
  
  # Check for argument components data
  if (is.null(bert_data) || is.null(bert_data$arg_components) || nrow(bert_data$arg_components) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "BERT data file contains no argument components data", cex = 1.2)
    return(NULL)
  }
  
  # Get all posts data to analyze delta paths
  posts <- data_raw()
  if (is.null(posts) || length(posts) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No posts data available", cex = 1.5)
    return(NULL)
  }
  
  # Initialize data to track delta paths
  all_delta_ids <- character(0)
  post_count <- 0
  delta_post_count <- 0
  
  # Process each post to identify delta paths
  message("Identifying delta paths across posts...")
  for (post_index in seq_along(posts)) {
    post <- posts[[post_index]]
    post_count <- post_count + 1
    
    # Check if post contains a delta
    has_delta <- contains_delta(post, original_author = post$author)
    if (has_delta) {
      delta_post_count <- delta_post_count + 1
      
      # Extract network structure
      all_nodes <- get_nodes(post, is_root = TRUE)
      all_edges <- get_edges(post)
      
      if (length(all_nodes) > 0 && length(all_edges) > 0) {
        nodes_dt <- unique(data.table::rbindlist(lapply(all_nodes, as.data.table), fill = TRUE))
        edges_dt <- data.table::rbindlist(lapply(all_edges, as.data.table), fill = TRUE)
        
        # Only process if we have valid network data
        if (nrow(nodes_dt) > 0 && nrow(edges_dt) > 0) {
          # Find delta nodes
          delta_nodes <- nodes_dt$id[nodes_dt$shape == "triangle"]
          
          if (length(delta_nodes) > 0) {
            # Build graph
            g <- tryCatch({
              graph_from_data_frame(d = edges_dt, vertices = nodes_dt, directed = TRUE)
            }, error = function(e) NULL)
            
            if (!is.null(g)) {
              # Find root nodes
              root_nodes <- nodes_dt$id[!(nodes_dt$id %in% edges_dt$to)]
              if (length(root_nodes) == 0) root_nodes <- nodes_dt$id[1]
              
              # Find all nodes on paths to delta
              for (root in root_nodes) {
                for (delta in delta_nodes) {
                  # Use try to handle cases where no path exists
                  try({
                    path <- shortest_paths(g, from = root, to = delta)$vpath[[1]]
                    all_delta_ids <- union(all_delta_ids, as_ids(path))
                  }, silent = TRUE)
                }
              }
            }
          }
        }
      }
    }
  }
  
  message(paste0("Found ", length(all_delta_ids), " nodes on delta paths across ", 
                 delta_post_count, " posts with deltas"))
  
  # Add delta path flag to argument components data
  arg_data <- bert_data$arg_components
  arg_data$on_delta_path <- arg_data$id %in% all_delta_ids
  
  # Define component columns
  component_cols <- c("claims", "premises", "rebuttals", "concessions", "evidence")
  component_names <- c("Claims", "Premises", "Rebuttals", "Concessions", "Evidence")
  
  # Check if we have the required columns
  if (!all(component_cols %in% colnames(arg_data))) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "BERT data doesn't contain required argument component scores", cex = 1.2)
    return(NULL)
  }
  
  # Filter for complete cases to avoid NA issues
  complete_rows <- complete.cases(arg_data[, component_cols])
  if (sum(complete_rows) == 0) {
    plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
    text(0, 0, "No complete argument component data found", cex = 1.2)
    return(NULL)
  }
  arg_data <- arg_data[complete_rows, ]
  
  # Calculate average component values for delta and non-delta paths
  delta_paths <- arg_data[arg_data$on_delta_path == TRUE, ]
  non_delta_paths <- arg_data[arg_data$on_delta_path == FALSE, ]
  
  delta_means <- if (nrow(delta_paths) > 0) {
    colMeans(delta_paths[, component_cols, drop = FALSE], na.rm = TRUE)
  } else {
    rep(0, length(component_cols))
  }
  non_delta_means <- if (nrow(non_delta_paths) > 0) {
    colMeans(non_delta_paths[, component_cols, drop = FALSE], na.rm = TRUE)
  } else {
    rep(0, length(component_cols))
  }
  
  # Count delta and non-delta nodes
  delta_count <- nrow(delta_paths)
  non_delta_count <- nrow(non_delta_paths)
  
  # Radar chart with explicit lines for both groups
  if (requireNamespace("fmsb", quietly = TRUE)) {
    max_val <- max(c(delta_means, non_delta_means, 1), na.rm = TRUE)
    min_val <- 0
    radar_data <- rbind(
      setNames(rep(max_val, length(component_cols)), component_names),
      setNames(rep(min_val, length(component_cols)), component_names),
      setNames(as.numeric(delta_means), component_names),
      setNames(as.numeric(non_delta_means), component_names)
    )
    radar_data <- as.data.frame(radar_data)
    rownames(radar_data) <- c("Max", "Min", "Delta Path", "Non-Delta Path")
    op <- par(mar = c(2, 2, 4, 2))
    
    # Only set plwd and plty for the actual lines (not for the max/min rows)
    fmsb::radarchart(
      radar_data,
      axistype = 1,
      pcol = c("#0063A6", "#E4003A"),
      pfcol = c(scales::alpha("#0063A6", 0.15), scales::alpha("#E4003A", 0.15)),
      plwd = c(3, 3),
      plty = c(1, 1),
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey40",
      vlcex = 1.1,
      title = paste0("Global Argument Components: Delta vs Non-Delta Paths\n",
                     "Across ", post_count, " discussions (", delta_post_count, " with deltas)")
    )
    
    # Add subtitle with counts
    mtext(paste0("Delta paths: ", delta_count, " nodes | Non-delta paths: ", non_delta_count, " nodes"), 
          side = 3, line = -1.5, cex = 0.9)
    
    # Add legend
    legend("topright", legend = c("Delta Path", "Non-Delta Path"),
           col = c("#0063A6", "#E4003A"), lwd = 3, bty = "n")
    
    par(op)
  } else {
    # Fallback: grouped bar chart
    plot_data <- data.frame(
      Component = rep(component_names, 2),
      Value = c(delta_means, non_delta_means),
      Type = rep(c("Delta Path", "Non-Delta Path"), each = length(component_cols)),
      stringsAsFactors = FALSE
    )
    plot_data$Component <- factor(plot_data$Component, levels = component_names)
    
    print(
      ggplot(plot_data, aes(x = Component, y = Value, fill = Type)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
        scale_fill_manual(values = c("Delta Path" = "#0063A6", "Non-Delta Path" = "#E4003A")) +
        labs(
          title = "Global Argument Components: Delta vs Non-Delta Paths",
          subtitle = paste0(
            "Across ", post_count, " discussions (", delta_post_count, " with deltas)\n",
            "Delta paths: ", delta_count, " nodes | Non-delta paths: ", non_delta_count, " nodes"
          ),
          x = "",
          y = "Average Score",
          fill = "Path Type"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.position = "top"
        )
    )
  }
}

# ───────────────────────────────────────────────
# Temporal evolution
# ───────────────────────────────────────────────

argument_temporal_evolution <- function() {
  # 1) Pull in the flattened argument data
  arg_data <- argument_structure_data()
  flat     <- arg_data$flat
  if (is.null(flat)) {
    plot.new(); title("Insufficient data for analysis"); return(NULL)
  }
  
  # 2) Get the canonical network with seq_idx
  net <- networkData()
  if (is.null(net)) {
    plot.new(); title("No network data available"); return(NULL)
  }
  
  # 3) Map flat$node_id → the shared seq_idx
  flat$seq_idx <- net$nodes$seq_idx[match(flat$node_id, net$nodes$id)]
  
  # 4) Prepare the long‐format data for plotting
  components   <- c("claims","premises","rebuttals","concessions","evidence")
  present_cols <- intersect(components, colnames(flat))
  plot_data    <- tidyr::pivot_longer(
    flat,
    cols      = all_of(present_cols),
    names_to  = "component",
    values_to = "score"
  )
  
  # 5) Summarize mean score per seq_idx & component
  summary_data <- plot_data %>%
    dplyr::group_by(seq_idx, component) %>%
    dplyr::summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop")
  
  # 6) Find *only* the award positions (triangles)
  delta_positions <- sort(
    net$nodes$seq_idx[ net$nodes$shape == "triangle" ]
  )
  
  # 7) Plot
  ggplot(summary_data, aes(x = seq_idx, y = mean_score, color = component)) +
    geom_point(size = 1.5, alpha = 0.7) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2, size = 1.2) +
    # only at the actual award points:
    geom_vline(
      xintercept = delta_positions,
      linetype   = "dashed",
      color      = "red",
      size       = 0.8
    ) +
    scale_x_continuous(
      breaks = scales::pretty_breaks(10),
      expand = expansion(add = c(0, 0))
    ) +
    labs(
      title    = "Temporal Evolution of Argument Components",
      subtitle = paste0("Delta awarded at positions: ", 
                        paste(delta_positions, collapse = ", ")),
      x        = "Reply Index",
      y        = "Average Component Score",
      color    = "Component"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}


global_argument_temporal_evolution <- function() {
  # 1) Load & check BERT data
  bert_data_path <- "data/bert/reddit_bert_joined_analysis.rds"
  if (!file.exists(bert_data_path)) {
    plot.new(); title("No BERT data available. Run preprocessing script first."); return(NULL)
  }
  bert_data <- tryCatch(readRDS(bert_data_path), 
                        error = function(e) { plot.new(); title("Error loading BERT data"); return(NULL) })
  if (is.null(bert_data$arg_components) || nrow(bert_data$arg_components)==0) {
    plot.new(); title("BERT data contains no argument components"); return(NULL)
  }
  
  # 2) Get all posts and prepare per‐discussion data
  posts <- data_raw()
  if (is.null(posts) || length(posts)==0) {
    plot.new(); title("No posts data available"); return(NULL)
  }
  
  # which component columns we care about
  component_cols <- c("claims","premises","rebuttals","concessions","evidence")
  present_cols   <- intersect(component_cols, colnames(bert_data$arg_components))
  if (length(present_cols)<2) {
    plot.new(); title("Not enough argument component data in BERT analysis"); return(NULL)
  }
  
  all_discussion_data <- list()
  post_count       <- 0
  delta_post_count <- 0
  
  for (i in seq_along(posts)) {
    post_count <- post_count + 1
    post       <- posts[[i]]
    
    # does this discussion have any delta awards?
    if (contains_delta(post, original_author = post$author)) delta_post_count <- delta_post_count + 1
    
    # extract raw nodes & edges
    nodes    <- get_nodes(post, is_root = TRUE)
    edges    <- get_edges(post)
    if (length(nodes)==0 || length(edges)==0) next
    
    nodes_dt <- tryCatch(data.table::rbindlist(lapply(nodes, as.data.table), fill=TRUE),
                         error = function(e) data.table::data.table())[ , unique(.SD)]
    edges_dt <- tryCatch(data.table::rbindlist(lapply(edges, as.data.table), fill=TRUE),
                         error = function(e) data.table::data.table())
    if (nrow(nodes_dt)==0 || nrow(edges_dt)==0) next
    
    # build graph & find triangle‐nodes
    g         <- tryCatch(igraph::graph_from_data_frame(edges_dt, vertices=nodes_dt, directed=TRUE),
                          error = function(e) NULL)
    delta_ids <- nodes_dt$id[nodes_dt$shape=="triangle"]
    
    # assign a seq_idx by timestamp (or BFS fallback)
    if ("timestamp" %in% colnames(nodes_dt)) {
      nodes_dt[, seq_idx := frank(timestamp, ties.method="first")]
    } else {
      roots     <- setdiff(nodes_dt$id, edges_dt$to)
      if (length(roots)==0) roots <- nodes_dt$id[1]
      bfs_ord   <- unlist(lapply(roots, function(r) bfs(g, root=r, unreachable=FALSE)$order))
      nodes_dt[, seq_idx := match(id, names(bfs_ord))]
    }
    nodes_dt[, norm_seq_idx := (seq_idx - 1) / max(seq_idx - 1, 1)]
    nodes_dt[, on_delta_path := id %in% delta_ids]
    
    # pull BERT components for this discussion
    disc_arg <- bert_data$arg_components[bert_data$arg_components$id %in% nodes_dt$id, ]
    if (nrow(disc_arg)==0) next
    
    # merge in seq info & delta flag
    tmp <- merge(
      disc_arg,
      nodes_dt[, .(id, seq_idx, norm_seq_idx, on_delta_path)],
      by = "id", all.x = TRUE
    )
    # convert to data.table and subset only the cols we need
    tmp <- data.table::as.data.table(tmp)
    cols <- c("id","seq_idx","norm_seq_idx","on_delta_path", present_cols)
    all_discussion_data[[length(all_discussion_data)+1]] <- tmp[, ..cols]
  }
  
  # combine everything
  if (length(all_discussion_data)==0) {
    plot.new(); title("No valid temporal data could be extracted"); return(NULL)
  }
  combined <- data.table::rbindlist(all_discussion_data, fill=TRUE)
  
  # bin into relative steps
  bin_count <- 20
  combined[, seq_bin := as.numeric(cut(
    norm_seq_idx,
    breaks = seq(0,1,length.out = bin_count+1),
    labels = seq_len(bin_count),
    include.lowest = TRUE
  ))]
  
  # where do deltas land, in relative‐bin space?
  delta_norms <- combined$norm_seq_idx[combined$on_delta_path==TRUE]
  if (length(delta_norms)>0) {
    # use quartiles; change probs if you want thirds
    q_norms <- quantile(delta_norms, probs=c(0.25,0.5,0.75), na.rm=TRUE)
    q_bins  <- as.numeric(q_norms*(bin_count-1) + 1)
  } else {
    q_bins <- numeric(0)
  }
  
  # pivot & summarize
  plot_data <- tidyr::pivot_longer(
    combined,
    cols      = all_of(present_cols),
    names_to  = "component",
    values_to = "score"
  )
  summary_data <- plot_data %>%
    dplyr::group_by(seq_bin, component) %>%
    dplyr::summarise(mean_score = mean(score, na.rm=TRUE), .groups="drop")
  
  # build plot
  p <- ggplot(summary_data, aes(x=seq_bin, y=mean_score, color=component)) +
    geom_point(size=1.2, alpha=0.6) +
    geom_smooth(method="loess", se=TRUE, alpha=0.2, size=1) +
    scale_x_continuous(
      labels = function(x) paste0(round((x-1)/(bin_count-1)*100), "%"),
      breaks = seq(1, bin_count, length.out=5)
    ) +
    labs(
      title    = "Global Temporal Evolution of Argument Components",
      subtitle = paste0(
        "Across ", post_count, " discussions (", delta_post_count, " with deltas)\n",
        "Delta quartiles at: ", 
        if (length(q_bins)>0) paste0(round((q_bins-1)/(bin_count-1)*100), "%", collapse=", ")
        else "none"
      ),
      x     = "Discussion Progress",
      y     = "Average Component Score",
      color = "Component"
    ) +
    theme_minimal() +
    theme(legend.position="right")
  
  # add the red dashed lines
  if (length(q_bins)>0) {
    p <- p + geom_vline(
      xintercept = q_bins,
      linetype   = "dashed",
      color      = "red",
      size       = 0.8
    )
  }
  
  print(p)
  invisible(summary_data)
}