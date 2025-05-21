# ───────────────────────────────────────────────
# 🌐 USER INTERACTION NETWORK mit Delta-Färbung
# ───────────────────────────────────────────────

# Declare reactive functions first, but wrap them in existence check for input
if (exists("input")) {
  all_posts <- reactive({
    # Check if networkData exists
    if (!exists("networkData") || is.null(networkData())) {
      return(NULL)
    }
    
    net <- networkData()
    req(net)
    
    # Pull out your node table, ensure it's a true data.frame
    nodes_df <- as.data.frame(net$nodes)
    
    # Pre-process all posts' persuasion analysis when in Time Perspective
    if (input$viewPerspective == "time") {
      # Save the current warning counter
      old_miss_counter <- miss_counter
      # Process all texts in batch mode
      if (nrow(nodes_df) > 0) {
        message("Pre-processing persuasion analysis for Time Perspective view...")
        invisible(batch_analyze_persuasion(nodes_df$rawText))
      }
    }
    
    # order by timestamp, then add seq_idx and a single sentiment score
    nodes_df %>%
      arrange(timestamp) %>%
      mutate(
        seq_idx   = row_number(),
        sentiment = syuzhet::get_sentiment(rawText)
      )
  })
  
  # Keep other reactive functions that depend on input
  observe({
    df <- all_posts()
    req(df)
    updateSliderInput(
      session, "timeSlider",
      max   = nrow(df),
      value = nrow(df)  # start fully grown
    )
  })
  
  # Add an observer to pre-cache when switching to Time Perspective tab
  observeEvent(input$viewPerspective, {
    if (input$viewPerspective == "time") {
      # Trigger all_posts reactive to pre-cache
      df <- all_posts()
    }
  })
  
  timeFilteredGraph <- reactive({
    df_nodes <- all_posts()
    req(df_nodes)
    cutoff <- input$timeSlider
    
    # Keep nodes with seq_idx <= cutoff
    keep_ids <- df_nodes$id[df_nodes$seq_idx <= cutoff]
    
    # Get network data
    net <- networkData()
    req(net)
    
    all_n <- net$nodes
    all_e <- net$edges
    
    nodes_f <- all_n[all_n$id %in% keep_ids, ]
    edges_f <- all_e[all_e$from %in% keep_ids & all_e$to %in% keep_ids, ]
    
    list(nodes = nodes_f, edges = edges_f)
  })
}

# Wrap all output references in condition check
if (exists("output")) {

  output$user_interaction_network <- renderVisNetwork({
    posts <- data_raw()
    if (is.null(posts)) return(NULL)
    
    selected_index <- as.numeric(input$selected_post)
    if (is.na(selected_index) || selected_index > length(posts)) return(NULL)
    
    selected_post <- posts[[selected_index]]
    op_user <- selected_post$author
    
    extract_user_edges <- function(node, parent_author = NULL) {
      edges <- list()
      users <- c()
      
      # First ensure current_author is properly handled
      current_author <- if (!is.null(node$author) && !is.na(node$author) && node$author != "") {
        node$author
      } else {
        NA_character_
      }
      
      # Skip bots and NA authors
      if (is.na(current_author) || tolower(current_author) %in% c("deltabot", "automoderator")) {
        return(list(edges = list(), users = character(0)))
      }
      
      users <- c(users, current_author)
      
      # Only create edge if both authors are valid
      if (!is.null(parent_author) && !is.na(parent_author) && parent_author != "" &&
          !tolower(parent_author) %in% c("deltabot", "automoderator")) {
        edges[[length(edges) + 1]] <- data.frame(from = current_author, to = parent_author, stringsAsFactors = FALSE)
      }
      
      children <- NULL
      if (!is.null(node$comments) && length(node$comments) > 0) {
        children <- node$comments
      } else if (!is.null(node$replies) && length(node$replies) > 0) {
        children <- node$replies
      }
      
      if (!is.null(children)) {
        for (child in children) {
          res <- extract_user_edges(child, current_author)
          edges <- c(edges, res$edges)
          users <- c(users, res$users)
        }
      }
      
      return(list(edges = edges, users = users))
    }
    
    res <- extract_user_edges(selected_post)
    user_edges <- res$edges
    all_users <- unique(res$users)
    
    # Filter out any NA or empty users
    all_users <- all_users[!is.na(all_users) & all_users != ""]
    
    if (length(user_edges) == 0 && length(all_users) == 0) return(NULL)
    
    edge_df <- if (length(user_edges) > 0) do.call(rbind, user_edges) else data.frame(from = character(0), to = character(0))
    
    # Additional filtering to remove any NA edges
    if (nrow(edge_df) > 0) {
      edge_df <- edge_df[!is.na(edge_df$from) & !is.na(edge_df$to), ]
    }
    
    # Remove OP from edge list if requested
    if (isTRUE(input$remove_op) && !is.null(op_user)) {
      edge_df <- edge_df %>%
        filter(from != op_user & to != op_user)
      all_users <- setdiff(all_users, op_user)
    }
    
    # Aggregate weights
    edge_counts <- edge_df %>%
      group_by(from, to) %>%
      summarise(weight = n(), .groups = "drop")
    
    # Ensure all_users is a valid vector
    if (length(all_users) == 0) return(NULL)
    
    # Build graph and include all users (even isolated ones)
    g <- graph_from_data_frame(edge_counts, directed = TRUE, 
                               vertices = data.frame(name = all_users, stringsAsFactors = FALSE))
    
    # Convert input value to numeric with proper error handling
    min_deg <- tryCatch({
      as.numeric(input$min_component_size)
    }, error = function(e) {
      0  # Default value if conversion fails
    })
    
    # Ensure we have a valid numeric value
    if (is.na(min_deg) || is.null(min_deg)) {
      min_deg <- 0
    }
    
    # Fixed degree calculation for all vertices
    degs <- sapply(V(g), function(v) {
      length(c(neighbors(g, v, mode = "in"), neighbors(g, v, mode = "out")))
    })
    keep_users <- names(degs[degs >= min_deg])
    
    g_filtered <- induced_subgraph(g, vids = keep_users)
    
    if (vcount(g_filtered) == 0) return(NULL)
    
    # Prepare visNetwork nodes and edges
    vis_nodes <- data.frame(
      id = V(g_filtered)$name,
      label = V(g_filtered)$name,
      title = paste0("User: ", V(g_filtered)$name),
      stringsAsFactors = FALSE
    )
    
    # Add colors based on delta participation
    stats <- delta_participation_stats()  # Get delta stats computed elsewhere in your app
    vis_nodes$color <- sapply(vis_nodes$id, function(user) {
      delta_user_color(user, stats)
    })
    
    ig_edges <- igraph::as_data_frame(g_filtered, what = "edges")
    vis_edges <- if (nrow(ig_edges) > 0) {
      data.frame(
        from = ig_edges$from,
        to = ig_edges$to,
        value = ig_edges$weight,
        title = paste0("Interactions: ", ig_edges$weight),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(from = character(0), to = character(0), value = numeric(0), title = character(0))
    }
    
    visNetwork(vis_nodes, vis_edges) %>%
      visEdges(smooth = FALSE, arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  # Fixed version of nw_metrics
  output$nw_metrics <- renderUI({
    net <- networkData()
    if (is.null(net)) {
      return(HTML("<p>Select a discussion to view metrics.</p>"))
    }
    
    # Define the extract_user_edges function locally
    extract_user_edges <- function(node, parent_author = NULL) {
      edges <- list()
      users <- character(0)
      current_author <- node$author
      
      if (is.null(current_author) || current_author == "" ||
          tolower(current_author) %in% c("deltabot", "automoderator")) {
        return(list(edges = list(), users = character(0)))
      }
      
      users <- current_author
      
      if (!is.null(parent_author) && parent_author != "" &&
          !tolower(parent_author) %in% c("deltabot", "automoderator")) {
        edges[[length(edges) + 1]] <- data.frame(
          from = current_author, to = parent_author, stringsAsFactors = FALSE
        )
      }
      
      children <- list()
      if (!is.null(node$comments) && length(node$comments) > 0) {
        children <- c(children, node$comments)
      }
      if (!is.null(node$replies) && length(node$replies) > 0) {
        children <- c(children, node$replies)
      }
      
      for (child in children) {
        res <- extract_user_edges(child, current_author)
        edges <- c(edges, res$edges)
        users <- c(users, res$users)
      }
      
      list(edges = edges, users = users)
    }
    
    # Build & filter the interaction graph
    res <- extract_user_edges(net$selected_post)
    users <- unique(res$users)
    users <- users[!is.na(users) & users != ""]
    edges_df <- if (length(res$edges) > 0) do.call(rbind, res$edges) else
      data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    
    if (isTRUE(input$remove_op)) {
      op <- net$selected_post$author
      edges_df <- edges_df[edges_df$from != op & edges_df$to != op, ]
      users <- setdiff(users, op)
    }
    
    # Aggregate edge weights
    ec <- edges_df %>%
      group_by(from, to) %>%
      summarise(weight = n(), .groups = "drop")
    
    # If no users, show placeholder
    if (length(users) == 0) {
      return(HTML("<p>No users in this network.</p>"))
    }
    
    # Build igraph and apply min-degree filter
    g <- graph_from_data_frame(d = ec, vertices = data.frame(name = users), directed = TRUE)
    
    # Fixed degree calculation for all nodes with explicit conversion
    degs <- sapply(V(g), function(v) {
      # Calculate the degree for this vertex by counting in and out neighbors
      n_in <- length(neighbors(g, v, mode = "in"))
      n_out <- length(neighbors(g, v, mode = "out"))
      total_degree <- n_in + n_out
      return(as.numeric(total_degree))  # Ensure numeric conversion
    })
    
    # Convert input to numeric with error handling
    min_deg <- tryCatch({
      as.numeric(input$min_component_size)
    }, error = function(e) {
      0  # Default value if conversion fails
    })
    
    if (is.na(min_deg) || is.null(min_deg)) {
      min_deg <- 0
    }
    
    keep <- names(degs[degs >= min_deg])
    g2 <- induced_subgraph(g, vids = keep)
    
    if (vcount(g2) == 0) {
      return(HTML("<p>No users meet the interaction criteria.</p>"))
    }
    
    # Calculate centrality scores for filtered graph
    central_scores <- sapply(V(g2), function(v) {
      n_in <- length(neighbors(g2, v, mode = "in"))
      n_out <- length(neighbors(g2, v, mode = "out"))
      as.numeric(n_in + n_out)  # Ensure numeric conversion
    })
    
    # Assign vertex names
    names(central_scores) <- V(g2)$name
    # Identify most central user
    most_central <- names(central_scores)[which.max(central_scores)]
    
    total_users <- vcount(g2)
    total_interactions <- ecount(g2)
    
    # Calculate average degree with explicit numeric conversion
    avg_deg <- tryCatch({
      if (ecount(g2) > 0) {
        mean(sapply(V(g2), function(v) {
          n_in <- length(neighbors(g2, v, mode = "in"))
          n_out <- length(neighbors(g2, v, mode = "out"))
          as.numeric(n_in + n_out)
        }))
      } else {
        0
      }
    }, error = function(e) {
      0  # Default in case of error
    })
    
    # Round to 2 decimal places
    avg_deg <- round(as.numeric(avg_deg), 2)
    
    # Render only the four divs
    div(class = "metrics-grid",
        div(class = "metrics-item",
            div(class = "metrics-value", most_central),
            div(class = "metrics-label", "Most Central User")
        ),
        div(class = "metrics-item",
            div(class = "metrics-value", total_users),
            div(class = "metrics-label", "Network Users")
        ),
        div(class = "metrics-item",
            div(class = "metrics-value", total_interactions),
            div(class = "metrics-label", "Total Interactions")
        ),
        div(class = "metrics-item",
            div(class = "metrics-value", avg_deg),
            div(class = "metrics-label", "Avg. Connections")
        )
    )
  })
  
  # ───────────────────────────────────────────────
  # 🟠 UI: Delta Participation Liste (User Perspective)
  # ───────────────────────────────────────────────
  output$user_delta_highlights <- renderUI({
    stats <- delta_participation_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(HTML("<p>No delta participation found.</p>"))
    }
    
    op_author <- networkData()$selected_post$author
    stats <- stats[author != op_author]
    stats <- stats[delta_count > 0]
    if (nrow(stats) == 0) return(HTML("<p>No delta participation (excluding OP).</p>"))
    
    # Dynamic User List
    list_items <- lapply(seq_len(nrow(stats)), function(i) {
      row <- stats[i, ]
      tags$div(
        style = "margin-bottom: 10px; padding: 5px; border: 1px solid #ccc; border-radius: 4px;",
        tags$span(style = sprintf(
          "display:inline-block; width:20px; height:20px; background-color:%s; border-radius:50%%; margin-right:10px;",
          delta_user_color(row$author, stats)
        )),
        tags$span(sprintf("%s — Delta Rate: %.2f (Δ %s / %s)",
                          row$author, row$delta_rate, row$delta_count, row$total))
      )
    })
    
    tags$div(list_items)
  })
  
  output$discussion_metrics <- renderUI({
    metrics <- user_discussion_metrics()
    if (is.null(metrics)) return(HTML("<p>Select a discussion to view metrics.</p>"))
    
    # Calculate average connections directly from network data
    net <- networkData()
    avg_connections <- 0
    
    if (!is.null(net) && !is.null(net$edges) && nrow(net$edges) > 0 && !is.null(net$nodes) && nrow(net$nodes) > 0) {
      # Debug information
      message("Network data check:")
      message("Number of nodes: ", nrow(net$nodes))
      message("Number of edges: ", nrow(net$edges))
      
      # Ensure the edges have proper 'from' and 'to' columns
      if (!all(c("from", "to") %in% colnames(net$edges))) {
        message("Error: Network edges missing 'from' or 'to' columns")
        return(HTML("<p>Error in network data structure</p>"))
      }
      
      # Create adjacency list manually
      connections_per_node <- list()
      
      # Count connections for each node
      for (i in 1:nrow(net$edges)) {
        from_node <- net$edges$from[i]
        to_node <- net$edges$to[i]
        
        # Initialize if not exists
        if (is.null(connections_per_node[[from_node]])) {
          connections_per_node[[from_node]] <- character(0)
        }
        if (is.null(connections_per_node[[to_node]])) {
          connections_per_node[[to_node]] <- character(0)
        }
        
        # Add connections (undirected - both ways)
        connections_per_node[[from_node]] <- c(connections_per_node[[from_node]], to_node)
        connections_per_node[[to_node]] <- c(connections_per_node[[to_node]], from_node)
      }
      
      # Ensure all nodes from the nodes table are included
      for (node_id in net$nodes$id) {
        if (is.null(connections_per_node[[node_id]])) {
          connections_per_node[[node_id]] <- character(0)
        }
      }
      
      # Calculate degrees
      degrees <- sapply(connections_per_node, function(neighbors) {
        length(unique(neighbors))
      })
      
      # Calculate average
      avg_connections <- if (length(degrees) > 0) {
        mean(degrees)
      } else {
        0
      }
      
      # Debug output
      message("Manual calculation results:")
      message("Total nodes counted: ", length(degrees))
      message("Average connections: ", avg_connections)
    }
    
    # Format numbers nicely
    format_num <- function(x, digits = 2) {
      if (is.na(x)) return("N/A")
      if (is.null(x)) return("N/A")
      if (is.numeric(x)) return(round(x, digits))
      return(x)
    }
    
    # Create metrics grid with two columns
    div(class = "metrics-grid",
        # Column 1
        div(class = "metrics-item",
            div(class = "metrics-value", format_num(metrics$unique_users, 0)),
            div(class = "metrics-label", "Unique Users")
        ),
        div(class = "metrics-item",
            div(class = "metrics-value", format_num(metrics$avg_comments_per_user)),
            div(class = "metrics-label", "Avg. Comments/User")
        ),
        div(class = "metrics-item",
            div(class = "metrics-value", format_num(avg_connections)),
            div(class = "metrics-label", "Avg. Connections")
        ),
        div(class = "metrics-item",
            div(class = "metrics-value", format_num(metrics$response_density * 100)),
            div(class = "metrics-label", "Response Density %")
        ),
        # Column 2
        div(class = "metrics-item",
            div(class = "metrics-value", format_num(metrics$delta_count, 0)),
            div(class = "metrics-label", "Total Deltas")
        ),
        div(class = "metrics-item",
            div(class = "metrics-value", format_num(metrics$avg_delta_path_length)),
            div(class = "metrics-label", "Avg. Path to Delta")
        )
    )
  })
  
  # Thread Statistics visualization - User Distribution Chart
  output$thread_statistics <- renderPlot({
    net <- networkData()
    if (is.null(net)) return(NULL)
    
    nodes <- as.data.frame(net$nodes)
    
    # If too few nodes, show a message
    if (nrow(nodes) < 2) {
      # Create an empty plot with a message
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
           xlim = c(0, 1), ylim = c(0, 1))
      text(0.5, 0.5, "Not enough data for visualization", cex = 1.5)
      return()
    }
    
    # 1. Calculate user comment distribution
    user_comment_counts <- as.data.frame(table(nodes$author))
    colnames(user_comment_counts) <- c("Author", "Count")
    
    # Sort by count and take top 10
    user_comment_counts <- user_comment_counts[order(-user_comment_counts$Count), ]
    top_users <- head(user_comment_counts, 10)
    
    # 2. Reorder factors for plotting
    top_users$Author <- factor(top_users$Author, levels = top_users$Author[order(top_users$Count)])
    
    # 3. Create the plot
    ggplot(top_users, aes(x = Author, y = Count, fill = Count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "#E6F2F9", high = "#0063A6") +
      labs(title = "Top 10 Users by Comment Count",
           x = "",
           y = "Number of Comments") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 16, face = "bold"),
        panel.grid.major.y = element_blank()
      )
  })
  
  # Create an alternative thread statistics visualization that shows conversation structure
  output$thread_structure_analysis <- renderPlot({
    net <- networkData()
    if (is.null(net)) return(NULL)
    
    nodes <- as.data.frame(net$nodes)
    edges <- as.data.frame(net$edges)
    
    # If no edges or too few nodes, show a message
    if (nrow(edges) == 0 || nrow(nodes) < 3) {
      # Create an empty plot with a message
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
           xlim = c(0, 1), ylim = c(0, 1))
      text(0.5, 0.5, "Not enough data for structure analysis", cex = 1.5)
      return()
    }
    
    # Create igraph object
    g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
    
    # Find root node(s)
    root_nodes <- nodes$id[!(nodes$id %in% edges$to)]
    if (length(root_nodes) == 0) {
      # If no clear root, use the first node
      root_nodes <- nodes$id[1]
    }
    
    # Initialize depth vector with maximum possible depth as default
    max_possible_depth <- vcount(g)
    node_depths <- rep(max_possible_depth, length(V(g)))
    names(node_depths) <- V(g)$name
    
    # Assign depth 0 to root nodes
    node_depths[root_nodes] <- 0
    
    # Use breadth-first search to assign depths
    visited <- rep(FALSE, length(V(g)))
    names(visited) <- V(g)$name
    visited[root_nodes] <- TRUE
    
    current_frontier <- root_nodes
    current_depth <- 1
    
    while (length(current_frontier) > 0) {
      next_frontier <- c()
      
      for (node in current_frontier) {
        # Get neighbors (outgoing edges from this node)
        neighbors_idx <- incident(g, node, mode = "out")
        if (length(neighbors_idx) > 0) {
          for (e_idx in neighbors_idx) {
            e <- E(g)[e_idx]
            target_node <- ends(g, e)[2]
            target_id <- V(g)$name[target_node]
            
            if (!visited[target_id]) {
              visited[target_id] <- TRUE
              node_depths[target_id] <- current_depth
              next_frontier <- c(next_frontier, target_id)
            }
          }
        }
      }
      
      current_frontier <- next_frontier
      current_depth <- current_depth + 1
      
      # Safety check for potential cycles
      if (current_depth > max_possible_depth) break
    }
    
    # Handle disconnected components by setting unvisited nodes to NA
    node_depths[!visited] <- NA
    
    # Create data frame with node depths (excluding NA values)
    depth_df <- data.frame(
      node_id = names(node_depths),
      depth = node_depths,
      stringsAsFactors = FALSE
    )
    depth_df <- depth_df[!is.na(depth_df$depth), ]
    
    # Calculate branching at each depth
    branching_df <- depth_df %>%
      group_by(depth) %>%
      summarise(count = n(), .groups = "drop")
    
    # Create visualization
    if (nrow(branching_df) > 0) {
      ggplot(branching_df, aes(x = depth, y = count, fill = count)) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_fill_gradient(low = "#E6F2F9", high = "#0063A6") +
        labs(title = "Conversation Depth Analysis",
             x = "Thread Depth",
             y = "Number of Comments") +
        scale_x_continuous(breaks = unique(branching_df$depth)) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text = element_text(size = 14),
          title = element_text(size = 16, face = "bold")
        )
    } else {
      # Create an empty plot with a message
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
           xlim = c(0, 1), ylim = c(0, 1))
      text(0, 0, "No valid depth information available", cex = 1.5)
    }
  })
  
  # Add a tab panel toggle for switching between different thread statistics views
  output$thread_statistics_tabs <- renderUI({
    tabsetPanel(
      id = "thread_stats_tabs",
      tabPanel("User Activity", 
               div(class = "plot-container",
                   plotOutput("thread_statistics", height = "100%"))),
      tabPanel("Thread Structure", 
               div(class = "plot-container",
                   plotOutput("thread_structure_analysis", height = "100%")))
    )
  })
  
  # ───────────────────────────────────────────────
  # 🕓 Timeline Plot für User Interaktionen
  # ───────────────────────────────────────────────
  output$user_timeline <- renderPlot({
    net <- networkData()
    if (is.null(net)) return(NULL)
    
    df_nodes <- as.data.frame(net$nodes)
    df_nodes <- df_nodes[order(df_nodes$timestamp, na.last = TRUE), ]
    df_nodes$rel_time <- seq_len(nrow(df_nodes))
    df_nodes$DeltaFlag <- ifelse(df_nodes$shape == "triangle", "Delta", "Regular")
    
    ggplot(df_nodes, aes(x = rel_time, y = 0, color = DeltaFlag)) +
      geom_point(aes(size = ifelse(DeltaFlag == "Delta", 6, 4))) +
      scale_size_identity() +
      labs(x = "Relative Interaction Order", y = "", title = "User Interaction Timeline") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
  })
  
  output$time_argument_network <- renderVisNetwork({
    g <- timeFilteredGraph()
    req(g)
    
    # Check if we have enough data
    if (nrow(g$nodes) == 0 || nrow(g$edges) == 0) {
      return(visNetwork(
        data.frame(id = 1, label = "No arguments at this time point", stringsAsFactors = FALSE),
        data.frame(from = c(), to = c(), stringsAsFactors = FALSE)
      ))
    }
    
    visNetwork(g$nodes, g$edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  output$time_user_network <- renderVisNetwork({
    g <- timeFilteredGraph()
    req(g)
    
    # Check if we have enough data
    if (nrow(g$nodes) == 0 || nrow(g$edges) == 0) {
      return(visNetwork(
        data.frame(id = 1, label = "No user interactions at this time point", stringsAsFactors = FALSE),
        data.frame(from = c(), to = c(), stringsAsFactors = FALSE)
      ))
    }
    
    # Build user network
    user_net <- buildUserNetwork(g$nodes, g$edges)
    
    # Check if user network has nodes
    if (nrow(user_net$nodes) == 0) {
      return(visNetwork(
        data.frame(id = 1, label = "No user interactions at this time point", stringsAsFactors = FALSE),
        data.frame(from = c(), to = c(), stringsAsFactors = FALSE)
      ))
    }
    
    visNetwork(user_net$nodes, user_net$edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 42)
  })

} # End of output existence check

# Keep functions and reactives outside the output check
# since they're needed by other parts of the app

# ───────────────────────────────────────────────
# 📊 User Discussion Metrics Calculations
# ───────────────────────────────────────────────

calculate_post_metrics <- function(nodes, edges, g) {
  # 1. Basic metrics
  unique_users <- length(unique(nodes$author))
  total_comments <- nrow(nodes)
  
  # 2. Engagement metrics
  comments_per_user <- table(nodes$author)
  avg_comments_per_user <- mean(comments_per_user)
  max_comments_user <- names(which.max(comments_per_user))
  max_comments_count <- max(comments_per_user)
  
  # Calculate response density - ratio of actual responses to possible interactions
  possible_interactions <- unique_users * (unique_users - 1)
  actual_interactions <- ecount(g)
  response_density <- if (possible_interactions > 0) actual_interactions / possible_interactions else 0
  
  # 3. Delta-related metrics 
  delta_nodes <- nodes[nodes$shape == "triangle", ]
  delta_count <- nrow(delta_nodes)
  delta_rate <- delta_count / total_comments
  
  # Find paths to deltas if any exist
  delta_paths <- list()
  if (delta_count > 0) {
    root_ids <- nodes$id[!(nodes$id %in% edges$to)]
    if (length(root_ids) == 0) root_ids <- nodes$id[1]
    for (delta_id in delta_nodes$id) {
      for (root_id in root_ids) {
        path_exists <- FALSE
        tryCatch({
          path_exists <- are_adjacent(g, root_id, delta_id) ||
            length(all_simple_paths(g, from = root_id, to = delta_id)) > 0
        }, error = function(e) { path_exists <- FALSE })
        if (path_exists) {
          tryCatch({
            path <- shortest_paths(g, from = root_id, to = delta_id)$vpath[[1]]
            if (length(path) > 0) {
              delta_paths[[length(delta_paths) + 1]] <- path
              break
            }
          }, error = function(e) {})
        }
      }
    }
  }
  
  avg_delta_path_length <- if (length(delta_paths) > 0) {
    mean(sapply(delta_paths, function(path) length(path) - 1))
  } else {
    NA
  }
  
  # 4. Network structure metrics - FIXED VERSION
  is_dir <- igraph::is_directed(g)
  
  # Calculate degrees for all vertices
  degs <- tryCatch({
    if (is_dir) {
      # For directed graphs, consider all connections
      degree(g, mode = "all")
    } else {
      # For undirected graphs
      degree(g)
    }
  }, error = function(e) {
    # Fallback if degree() fails
    message("Falling back to simple degree calculation")
    sapply(V(g), function(v) length(neighbors(g, v)))
  })
  
  # Calculate average degree - explicit error handling for empty graphs
  avg_degree <- tryCatch({
    if (length(degs) > 0) {
      mean(degs)
    } else {
      0
    }
  }, error = function(e) {
    message("Error calculating average degree: ", e$message)
    0
  })
  
  # Make sure we log the calculated avg_degree for debugging
  message("Calculated avg_degree: ", avg_degree)
  
  centrality_scores <- tryCatch({
    if (is_dir) centr_degree(g, mode = "all")$res else centr_degree(g)$res
  }, error = function(e) rep(0, vcount(g)))
  
  most_central_index <- which.max(centrality_scores)
  most_central_user <- if (length(most_central_index) > 0 && most_central_index <= length(V(g))) {
    nodes$author[most_central_index]
  } else {
    "Unknown"
  }
  
  reciprocity <- tryCatch({
    if (is_dir) reciprocity(g) else NA
  }, error = function(e) NA)
  
  list(
    unique_users = unique_users,
    total_comments = total_comments,
    avg_comments_per_user = avg_comments_per_user,
    max_comments_user = max_comments_user,
    max_comments_count = max_comments_count,
    response_density = response_density,
    delta_count = delta_count,
    delta_rate = delta_rate,
    avg_delta_path_length = avg_delta_path_length,
    avg_degree = avg_degree,
    most_central_user = most_central_user,
    reciprocity = reciprocity
  )
}

# Replace the existing user_discussion_metrics reactive with this improved version
user_discussion_metrics <- reactive({
  net <- networkData()
  if (is.null(net)) return(NULL)
  
  # Extract nodes and edges data
  nodes <- as.data.frame(net$nodes)
  edges <- as.data.frame(net$edges)
  
  # Check if we have valid data for building a graph
  if (nrow(nodes) < 2 || nrow(edges) < 1 || 
      !all(c("from", "to") %in% colnames(edges))) {
    # Return default values if not enough data
    return(list(
      unique_users = length(unique(nodes$author)),
      total_comments = nrow(nodes),
      avg_comments_per_user = ifelse(length(unique(nodes$author)) > 0, 
                                     nrow(nodes) / length(unique(nodes$author)), 0),
      max_comments_user = "N/A",
      max_comments_count = 0,
      response_density = 0,
      delta_count = sum(nodes$shape == "triangle", na.rm = TRUE),
      delta_rate = ifelse(nrow(nodes) > 0, 
                          sum(nodes$shape == "triangle", na.rm = TRUE) / nrow(nodes), 0),
      avg_delta_path_length = NA,
      avg_degree = 0,
      most_central_user = "N/A",
      reciprocity = 0
    ))
  }
  
  # For a single post, calculate metrics as before
  # Building the graph from edges
  g <- tryCatch({
    graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(g)) {
    return(list(
      unique_users = length(unique(nodes$author)),
      total_comments = nrow(nodes),
      avg_comments_per_user = ifelse(length(unique(nodes$author)) > 0, 
                                     nrow(nodes) / length(unique(nodes$author)), 0),
      max_comments_user = "N/A",
      max_comments_count = 0,
      response_density = 0,
      delta_count = sum(nodes$shape == "triangle", na.rm = TRUE),
      delta_rate = ifelse(nrow(nodes) > 0, 
                          sum(nodes$shape == "triangle", na.rm = TRUE) / nrow(nodes), 0),
      avg_delta_path_length = NA,
      avg_degree = 0,
      most_central_user = "N/A",
      reciprocity = 0
    ))
  }
  
  return(calculate_post_metrics(nodes, edges, g))
})

all_posts <- reactive({
  net <- networkData()
  req(net)
  
  # Pull out your node table, ensure it's a true data.frame
  nodes_df <- as.data.frame(net$nodes)
  
  # Pre-process all posts' persuasion analysis when in Time Perspective
  if (input$viewPerspective == "time") {
    # Save the current warning counter
    old_miss_counter <- miss_counter
    # Process all texts in batch mode
    if (nrow(nodes_df) > 0) {
      message("Pre-processing persuasion analysis for Time Perspective view...")
      invisible(batch_analyze_persuasion(nodes_df$rawText))
    }
  }
  
  # order by timestamp, then add seq_idx and a single sentiment score
  nodes_df %>%
    arrange(timestamp) %>%
    mutate(
      seq_idx   = row_number(),
      sentiment = syuzhet::get_sentiment(rawText)
    )
})

# Update slider max when a new discussion is selected
observe({
  df <- all_posts()
  req(df)
  updateSliderInput(
    session, "timeSlider",
    max   = nrow(df),
    value = nrow(df)  # start fully grown
  )
})

# Add an observer to pre-cache when switching to Time Perspective tab
observeEvent(input$viewPerspective, {
  if (input$viewPerspective == "time") {
    # Trigger all_posts reactive to pre-cache
    df <- all_posts()
  }
})

timeFilteredGraph <- reactive({
  df_nodes <- all_posts()
  req(df_nodes)
  cutoff <- input$timeSlider
  
  # Keep nodes with seq_idx <= cutoff
  keep_ids <- df_nodes$id[df_nodes$seq_idx <= cutoff]
  
  # Get network data
  net <- networkData()
  req(net)
  
  all_n <- net$nodes
  all_e <- net$edges
  
  nodes_f <- all_n[all_n$id %in% keep_ids, ]
  edges_f <- all_e[all_e$from %in% keep_ids & all_e$to %in% keep_ids, ]
  
  list(nodes = nodes_f, edges = edges_f)
})