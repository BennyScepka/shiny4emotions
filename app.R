# ───────────────────────────────────────────────
# 📦 Load Required Packages
# ───────────────────────────────────────────────
library(shiny)
library(shinyBS)
library(bslib)
library(bsicons)  
library(jsonlite)
library(data.table)
library(visNetwork)
library(wordcloud2)
library(stringr)
library(syuzhet)
library(igraph)
library(udpipe)
library(text2vec)
library(tidyr)
library(ggplot2)
library(DT)
library(dplyr)
library(tibble)
library(radarchart)
library(fmsb)
library(ggraph)
library(tidygraph)
library(purrr)
library(stringdist)
# Commented out Python dependencies since we're using precomputed BERT data
# library(reticulate) # to choose python path


# Make sure radarchart is available
if (!requireNamespace("radarchart", quietly = TRUE)) {
  install.packages("radarchart", repos = "http://cran.us.r-project.org")
  if (!requireNamespace("radarchart", quietly = TRUE)) {
    # Fallback if installation fails
    message("Could not install radarchart package. Using alternative visualization.")
  }
}
if (requireNamespace("radarchart", quietly = TRUE)) {
  library(radarchart)
}

# Add circlize for chord diagram
if (!requireNamespace("circlize", quietly = TRUE)) {
  install.packages("circlize")
}
library(circlize)

# Make sure to load digest package
if (!requireNamespace("digest", quietly = TRUE)) {
  install.packages("digest")
}
library(digest)

# ───────────────────────────────────────────────
# Load Helpers
# ───────────────────────────────────────────────
source("helper.R")

# ───────────────────────────────────────────────
# Load UI Definition
# ───────────────────────────────────────────────
source("ui.R")

# ───────────────────────────────────────────────
# Server Logic
# ───────────────────────────────────────────────
server <- function(input, output, session) {
  
  
  # ───────────────────────────────────────────────
  # 🎨 Emotion-to-Color Mapping
  # ───────────────────────────────────────────────
  emotion_colors <- list(
    anger = "red",
    anticipation = "orange",
    disgust = "darkgreen",
    fear = "purple",
    joy = "yellow",
    sadness = "blue",
    surprise = "pink",
    trust = "lightblue"
  )
  
  # ───────────────────────────────────────────────
  # 🎨 Sentimentfarbe eines Textes ermitteln (für Knotenfarbe)
  # ───────────────────────────────────────────────
  get_sentiment_color <- function(text) {
    sentiment_scores <- get_nrc_sentiment(text)
    emotions <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
    emotion_scores <- sentiment_scores[emotions]
    
    if (all(emotion_scores == 0)) {
      return("gray")
    } else {
      max_emotion <- names(emotion_scores)[which.max(emotion_scores)]
      return(ifelse(!is.null(emotion_colors[[max_emotion]]), emotion_colors[[max_emotion]], "gray"))
    }
  }
  
  
  # ───────────────────────────────────────────────
  # 📦 Load UDpipe Model (Once)
  # ───────────────────────────────────────────────
  model_file <- "english-ewt-ud-2.5-191206.udpipe"
  udmodel <- reactive({
    udpipe_load_model(model_file)
  })
  
  # ───────────────────────────────────────────────
  # 📂 Load and Process JSON Data
  # ───────────────────────────────────────────────
  data_raw <- reactive({
    fromJSON("data/reddit_data_anonymized.json", simplifyVector = FALSE)
  })
  
  # Initialize classifier as NULL since we're using precomputed data
  zs_classifier <- NULL

  # ───────────────────────────────────────────────
  # 🧩 Check if Node (or Descendants) Contains a Delta
  # ───────────────────────────────────────────────
  
  contains_delta <- function(node, original_author) {
    # Only return TRUE if the node's author is the original author 
    # and its text contains a delta marker.
    if (node$author == original_author && grepl("Delta|delta|Δ|∆", node$text, ignore.case = TRUE)) {
      return(TRUE)
    }
    
    # Check recursively in any nested comments or replies.
    if (!is.null(node$comments)) {
      for (child in node$comments) {
        if (contains_delta(child, original_author)) return(TRUE)
      }
    }
    if (!is.null(node$replies)) {
      for (child in node$replies) {
        if (contains_delta(child, original_author)) return(TRUE)
      }
    }
    
    return(FALSE)
  }
  
  
  observe({
    # Initialize global analysis data when app starts
    if (!is.null(bert_data())) {
      # Pre-calculate global metrics to speed up rendering
      global_metrics_cache <- global_metrics()
      global_model_cache <- global_persuasion_model()
    }
  })
  
  # ───────────────────────────────────────────────
  # 🧠 Argument Structure Analysis Functions
  # ───────────────────────────────────────────────
  
  argument_structure_data <- eventReactive({ list(input$refresh, input$selected_post) }, {
    posts <- data_raw()
    if (is.null(posts)) return(NULL)
    selected_index <- as.numeric(input$selected_post)
    if (is.na(selected_index) || selected_index > length(posts)) return(NULL)
    selected_post <- posts[[selected_index]]
    arg_structure <- process_discussion_arguments(selected_post, is_root = TRUE)
    flat_args <- flatten_argument_structure(arg_structure)
    if (!is.null(flat_args)) {
      flat_args <- patch_persuasion_data(flat_args)
    }
    arg_flow <- calculate_argument_flow(flat_args)
    list(
      structure = arg_structure,
      flat = flat_args,
      flow = arg_flow,
      selected_post = selected_post
    )
  })
  
  output$argument_components_radar <- renderPlot({
    # Get the flattened argument data
    flat <- argument_structure_data()$flat
    if (is.null(flat)) return(NULL)
    
    # Get network data to identify delta paths
    net <- networkData()
    if (is.null(net)) return(NULL)
    
    # Build graph to find paths to deltas
    g <- graph_from_data_frame(d = net$edges, vertices = net$nodes, directed = TRUE)
    delta_nodes <- net$nodes$id[net$nodes$shape == "triangle"]
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
    
    # Add delta path flag to flat data
    flat$on_delta_path <- flat$node_id %in% delta_path_ids
    
    # Calculate average component values for delta and non-delta paths
    component_cols <- c("claims", "premises", "rebuttals", "concessions", "evidence")
    
    # Filter for delta paths and calculate means
    delta_paths <- flat[flat$on_delta_path == TRUE, ]
    non_delta_paths <- flat[flat$on_delta_path == FALSE, ]
    
    # Calculate means (with error handling for empty dataframes)
    delta_means <- if (nrow(delta_paths) > 0) {
      colMeans(delta_paths[, component_cols], na.rm = TRUE)
    } else {
      rep(0, length(component_cols))
    }
    
    non_delta_means <- if (nrow(non_delta_paths) > 0) {
      colMeans(non_delta_paths[, component_cols], na.rm = TRUE)
    } else {
      rep(0, length(component_cols))
    }
    
    # Create a simple bar chart instead of radar chart
    # Reshape data for plotting
    component_names <- c("Claims", "Premises", "Rebuttals", "Concessions", "Evidence")
    plot_data <- data.frame(
      Component = rep(component_names, 2),
      Value = c(delta_means, non_delta_means),
      Type = rep(c("Delta Path", "Non-Delta Path"), each = length(component_cols)),
      stringsAsFactors = FALSE
    )
    
    # Set Component as factor with specific order
    plot_data$Component <- factor(plot_data$Component, levels = component_names)
    
    # Create the comparison bar chart
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
  })
  
  output$argument_structure_table <- renderDT({
    arg_data <- argument_structure_data()
    if (is.null(arg_data) || is.null(arg_data$flat)) return(NULL)
    all_cols <- colnames(arg_data$flat)
    wanted_cols <- c("author", "is_root", "argument_type", "claims", "premises", 
                     "evidence", "dominant_appeal", "complexity_score", "persuasiveness_index")
    display_cols <- intersect(wanted_cols, all_cols)
    display_data <- arg_data$flat[, display_cols, drop = FALSE]
    if ("is_root" %in% display_cols) {
      display_data$is_root <- ifelse(display_data$is_root, "Yes", "No")
    }
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$persuasion_factors_plot <- renderPlot({
    arg_data <- argument_structure_data()
    if (is.null(arg_data) || is.null(arg_data$flat)) return(NULL)
    # Identify posts that lead to deltas (nodes with triangle shape or their ancestors)
    net <- networkData()
    if (is.null(net)) return(NULL)
    g <- graph_from_data_frame(d = net$edges, vertices = net$nodes, directed = TRUE)
    delta_ids <- net$nodes$id[net$nodes$shape == "triangle"]
    root <- net$nodes$id[!(net$nodes$id %in% net$edges$to)]
    if (length(root) == 0) root <- net$nodes$id[1]
    delta_path_ids <- character(0)
    for (r in root) {
      for (t in delta_ids) {
        sp <- shortest_paths(g, from = r, to = t)$vpath[[1]]
        delta_path_ids <- union(delta_path_ids, as_ids(sp))
      }
    }
    arg_data$flat$on_delta_path <- arg_data$flat$node_id %in% delta_path_ids
    persuasion_factors <- arg_data$flat %>%
      select(on_delta_path, all_of(c("evidence_to_claim_ratio", "rebuttal_presence", 
                                     "concession_presence", "complexity_score", "persuasiveness_index"))) %>%
      pivot_longer(cols = -on_delta_path, names_to = "factor", values_to = "value")
    ggplot(persuasion_factors, aes(x = factor, y = value, fill = on_delta_path)) +
      geom_boxplot() +
      scale_fill_manual(values = c("FALSE" = "lightgray", "TRUE" = "darkgreen"),
                        labels = c("FALSE" = "Non-Delta Path", "TRUE" = "Delta Path")) +
      labs(x = "Factor", y = "Value", title = "Persuasion Factors: Delta vs. Non-Delta Paths") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$delta_comparison_plot <- renderPlot({
    arg_data <- argument_structure_data()
    if (is.null(arg_data) || is.null(arg_data$flat)) return(NULL)
    net <- networkData()
    if (is.null(net)) return(NULL)
    g <- graph_from_data_frame(d = net$edges, vertices = net$nodes, directed = TRUE)
    delta_ids <- net$nodes$id[net$nodes$shape == "triangle"]
    root <- net$nodes$id[!(net$nodes$id %in% net$edges$to)]
    if (length(root) == 0) root <- net$nodes$id[1]
    delta_path_ids <- character(0)
    for (r in root) {
      for (t in delta_ids) {
        sp <- shortest_paths(g, from = r, to = t)$vpath[[1]]
        delta_path_ids <- union(delta_path_ids, as_ids(sp))
      }
    }
    arg_data$flat$on_delta_path <- arg_data$flat$node_id %in% delta_path_ids
    arg_type_counts <- arg_data$flat %>%
      group_by(argument_type, on_delta_path) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(path_type = ifelse(on_delta_path, "Delta Path", "Non-Delta Path"))
    arg_type_props <- arg_type_counts %>%
      group_by(path_type) %>%
      mutate(proportion = count / sum(count)) %>%
      ungroup()
    ggplot(arg_type_props, aes(x = argument_type, y = proportion, fill = path_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(x = "Argument Type", y = "Proportion", 
           title = "Argument Types: Delta vs. Non-Delta Paths") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$persuasiveness_by_user <- renderPlot({
    arg_data <- argument_structure_data()
    if (is.null(arg_data) || is.null(arg_data$flat)) return(NULL)
    user_persuasiveness <- arg_data$flat %>%
      group_by(author) %>%
      summarise(
        avg_persuasiveness = mean(persuasiveness_index, na.rm = TRUE),
        post_count = n(),
        .groups = "drop"
      ) %>%
      filter(post_count >= 2)  # Filter users with at least 2 posts
    ggplot(user_persuasiveness, aes(x = reorder(author, avg_persuasiveness), 
                                    y = avg_persuasiveness, fill = post_count)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      coord_flip() +
      theme_minimal() +
      labs(x = "User", y = "Average Persuasiveness Index", 
           title = "Persuasiveness by User")
  })
  
  output$fallacy_detection_plot <- renderPlot({
    arg_data <- argument_structure_data()
    if (is.null(arg_data) || is.null(arg_data$flat)) return(NULL)
    if (!("ad_hominem" %in% colnames(arg_data$flat))) return(NULL)
    fallacy_cols <- c("ad_hominem", "straw_man", "false_dichotomy", 
                      "slippery_slope", "appeal_to_popularity")
    fallacy_data <- arg_data$flat[, fallacy_cols] %>%
      rownames_to_column("post_id") %>%
      pivot_longer(cols = all_of(fallacy_cols), names_to = "fallacy_type", values_to = "count")
    fallacy_sums <- fallacy_data %>%
      group_by(fallacy_type) %>%
      summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
    ggplot(fallacy_sums, aes(x = fallacy_type, y = total, fill = fallacy_type)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Fallacy Type", y = "Total Count", title = "Detected Fallacies") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  
  # ───────────────────────────────────────────────
  # 💬 SVO Extraction from Posts
  # ───────────────────────────────────────────────
  extract_svo_post <- function(text, doc_id = "doc1", udpipe_model) {
    anno <- udpipe_annotate(udpipe_model, x = text, doc_id = doc_id)
    df <- as.data.frame(anno)
    subj <- df[df$dep_rel == "nsubj", c("doc_id", "sentence_id", "token_id", "token", "head_token_id")]
    verb <- df[(df$upos == "VERB" | df$dep_rel == "root"), c("doc_id", "sentence_id", "token_id", "token", "head_token_id")]
    obj  <- df[df$dep_rel %in% c("obj", "dobj"), c("doc_id", "sentence_id", "token_id", "token", "head_token_id")]
    merged_s <- merge(subj, verb, by.x = c("doc_id", "sentence_id", "head_token_id"),
                      by.y = c("doc_id", "sentence_id", "token_id"),
                      suffixes = c("_subj", "_verb"))
    merged_so <- merge(merged_s, obj, by.x = c("doc_id", "sentence_id", "head_token_id"),
                       by.y = c("doc_id", "sentence_id", "head_token_id"),
                       suffixes = c("", "_obj"))
    res <- data.frame(
      doc_id  = merged_so$doc_id,
      subject = merged_so$token_subj,
      verb    = merged_so$token_verb,
      object  = merged_so$token,
      stringsAsFactors = FALSE
    )
    unique(res)
  }
  
  data_svo <- reactive({
    posts <- data_raw()
    if (is.null(posts)) return(NULL)
    m <- udmodel()
    out <- list()
    for (i in seq_along(posts)) {
      p <- posts[[i]]
      post_text <- p$text
      post_id   <- p$id
      svos <- extract_svo_post(post_text, doc_id = post_id, udpipe_model = m)
      if (nrow(svos) == 0) {
        out[[i]] <- data.frame(
          post_index = i,
          post_id    = post_id,
          author     = p$author,
          text       = post_text,
          subject    = NA_character_,
          verb       = NA_character_,
          object     = NA_character_,
          stringsAsFactors = FALSE
        )
      } else {
        tmp <- svos
        tmp$post_index <- i
        tmp$post_id    <- post_id
        tmp$author     <- p$author
        tmp$text       <- post_text
        out[[i]] <- tmp[, c("post_index", "post_id", "author", "text", "subject", "verb", "object")]
      }
    }
    do.call(rbind, out)
  })
  
  observe({
    posts <- data_raw()
    if (is.null(posts)) return(NULL)
    valid_indices <- if (isTRUE(input$filter_delta)) {
      which(sapply(posts, function(p) contains_delta(p, original_author = p$author)))
    } else {
      seq_along(posts)
    }
    svo_data <- data_svo()
    post_choices <- sapply(posts[valid_indices], function(p) {
      combo <- "No SVO"
      if (!is.null(svo_data)) {
        subset_svo <- svo_data[svo_data$post_id == p$id, ]
        if (nrow(subset_svo) > 0) {
          combo <- paste0(
            ifelse(is.na(subset_svo$subject[1]), "?", subset_svo$subject[1]), "  ",
            ifelse(is.na(subset_svo$verb[1]), "?", subset_svo$verb[1]), "  ",
            ifelse(is.na(subset_svo$object[1]), "?", subset_svo$object[1])
          )
        }
      }
      combo
    })
    choices <- setNames(valid_indices, post_choices)
    updateSelectInput(session, "selected_post", choices = choices)
  })
  
  # ───────────────────────────────────────────────
  # 🔧 Hilfsfunktionen: Netzwerk-Knoten & -Kanten extrahieren
  # ───────────────────────────────────────────────
  get_nodes <- function(node, is_root = FALSE, op_author = NULL) {
    if (is_root && is.null(op_author)) {
      op_author <- node$author
    }
    node_author <- if (is.null(node$author) || length(node$author) == 0) "" else node$author
    if (tolower(node_author) %in% c("deltabot", "automoderator")) return(list())
    node_shape <- if (node_author == op_author && grepl("Delta|delta|Δ|∆", node$text, ignore.case = TRUE)) {
      "triangle"
    } else {
      "dot"
    }
    current_node <- list(
      id = node$id,
      author = node_author,
      label = "",
      title = paste0("<b>Author:</b> ", node_author, "<br><b>Text:</b> ", node$text),
      color = if (is_root) "black" else get_sentiment_color(node$text),
      shape = node_shape,
      rawText = node$text,
      timestamp = if (!is.null(node$created_utc)) as.POSIXct(node$created_utc, origin = "1970-01-01", tz = "UTC") else NA
    )
    nodes <- list(current_node)
    nested_comments <- if (!is.null(node$comments) && length(node$comments) > 0) {
      node$comments
    } else if (!is.null(node$replies) && length(node$replies) > 0) {
      node$replies
    } else {
      NULL
    }
    if (!is.null(nested_comments)) {
      for (comment in nested_comments) {
        nodes <- c(nodes, get_nodes(comment, is_root = FALSE, op_author = op_author))
      }
    }
    return(nodes)
  }
  
  get_edges <- function(node, parent_id = NA_character_) {
    node_author <- if (is.null(node$author) || length(node$author) == 0) "" else node$author
    if (tolower(node_author) %in% c("deltabot", "automoderator")) return(list())
    edges <- list()
    if (!is.na(parent_id)) {
      edges[[length(edges) + 1]] <- list(from = parent_id, to = node$id)
    }
    nested_comments <- if (!is.null(node$comments) && length(node$comments) > 0) {
      node$comments
    } else if (!is.null(node$replies) && length(node$replies) > 0) {
      node$replies
    } else NULL
    if (!is.null(nested_comments)) {
      for (comment in nested_comments) {
        edges <- c(edges, get_edges(comment, node$id))
      }
    }
    return(edges)
  }
  
  # ───────────────────────────────────────────────
  # 🔁 Netzwerkdaten für ausgewählte Diskussion erstellen
  # ───────────────────────────────────────────────
  # networkData <- eventReactive({ list(input$refresh, input$selected_post) }, {
  #   posts <- data_raw()
  #   if (is.null(posts)) return(NULL)
  #   selected_index <- as.numeric(input$selected_post)
  #   if (is.na(selected_index) || selected_index > length(posts)) return(NULL)
  #   selected_post <- posts[[selected_index]]
  #   all_nodes <- get_nodes(selected_post, is_root = TRUE)
  #   all_edges <- get_edges(selected_post)
  #   nodes_dt <- unique(rbindlist(lapply(all_nodes, as.data.table), fill = TRUE))
  #   edges_dt <- rbindlist(lapply(all_edges, as.data.table), fill = TRUE)
  #   list(nodes = nodes_dt, edges = edges_dt, selected_post = selected_post)
  # })
  
  
  networkData <- eventReactive({ list(input$refresh, input$selected_post) }, {
    posts <- data_raw()
    if (is.null(posts)) return(NULL)
    selected_index <- as.numeric(input$selected_post)
    if (is.na(selected_index) || selected_index > length(posts)) return(NULL)
    selected_post <- posts[[selected_index]]
    
    # Extract nodes & edges
    all_nodes <- get_nodes(selected_post, is_root = TRUE)
    all_edges <- get_edges(selected_post)
    nodes_dt  <- unique(data.table::rbindlist(lapply(all_nodes, as.data.table), fill = TRUE))
    edges_dt  <- data.table::rbindlist(lapply(all_edges, as.data.table), fill = TRUE)
    
    # ───────────────────────────────────────────────
    # Assign a canonical sequence index based on timestamp
    # (so every view uses the same ordering)
    # ───────────────────────────────────────────────
    # Ensure 'timestamp' is POSIXct in nodes_dt
    # Then rank by timestamp (earlier posts get lower seq_idx)
    nodes_dt[, seq_idx := data.table::frank(timestamp, ties.method = "first")]
    
    list(
      nodes = nodes_dt,
      edges = edges_dt,
      selected_post = selected_post
    )
  })
  
  # ───────────────────────────────────────────────
  # 🌐 Hauptnetzwerk-Visualisierung (Main Controls)
  # ───────────────────────────────────────────────
  output$network <- renderVisNetwork({
    net <- networkData()
    if (is.null(net)) {
      nodes <- data.frame(
        id = 1,
        label = "No data available",
        shape = "text",
        font.size = 20,
        stringsAsFactors = FALSE
      )
      edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
      return(visNetwork(nodes, edges) %>% 
               visLayout(randomSeed = 123))
    }
    if (is.null(net$nodes) || is.null(net$edges) || 
        nrow(net$nodes) == 0 || !all(c("from", "to") %in% colnames(net$edges))) {
      nodes <- data.frame(
        id = 1,
        label = "Invalid network data",
        shape = "text",
        font.size = 20,
        stringsAsFactors = FALSE
      )
      edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
      return(visNetwork(nodes, edges) %>% 
               visLayout(randomSeed = 123))
    }
    if (isTRUE(input$highlight_path)) {
      if (nrow(net$edges) > 0) {
        g <- tryCatch({
          graph_from_data_frame(d = net$edges, vertices = net$nodes, directed = TRUE)
        }, error = function(e) {
          NULL
        })
        if (!is.null(g)) {
          delta_ids <- net$nodes$id[net$nodes$shape == "triangle"]
          on_path_ids <- c()
          for (delta in delta_ids) {
            tryCatch({
              anc <- subcomponent(g, v = delta, mode = "in")
              on_path_ids <- union(on_path_ids, V(g)$name[anc])
            }, error = function(e) {
              # Continue if error occurs
            })
          }
          if (length(on_path_ids) > 0) {
            net$nodes$color <- ifelse(net$nodes$id %in% on_path_ids, net$nodes$color, "grey")
            net$edges$color <- ifelse(net$edges$from %in% on_path_ids & net$edges$to %in% on_path_ids, "red", "grey")
            net$edges$width <- ifelse(net$edges$from %in% on_path_ids & net$edges$to %in% on_path_ids, 3, 1)
          }
        }
      }
    }
    visNetwork(net$nodes, net$edges) %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
  
  # ───────────────────────────────────────────────
  # 🗨️ Originaler Post-Text
  # ───────────────────────────────────────────────
  output$original_post_text <- renderPrint({
    net <- networkData()
    if (is.null(net)) return("No post selected.")
    cat(net$selected_post$text)
  })
  
  # ───────────────────────────────────────────────
  # 🧵 Ausgewählter Knoten-Text (bei Klick)
  # ───────────────────────────────────────────────
  output$selected_node_text <- renderPrint({
    net <- networkData()
    if (is.null(net)) return("")
    selected_id <- input$network_selected
    if (is.null(selected_id) || selected_id == "") return("")
    sel_node <- net$nodes[net$nodes$id == selected_id, ]
    if (nrow(sel_node) == 0) return("")
    cat(sel_node$rawText)
  })
  
  # ───────────────────────────────────────────────
  # 🎨 Farbzuweisung basierend auf Delta-Teilnahme
  # ───────────────────────────────────────────────
  delta_user_color <- function(user, stats_df) {
    row <- stats_df[author == user]
    if (nrow(row) == 0) return("lightgray")
    if (row$delta_count >= 3) return("darkorange")
    else if (row$delta_count == 2) return("gold")
    else if (row$delta_count == 1) return("skyblue")
    else return("lightgray")
  }
  
  # ───────────────────────────────────────────────
  # 📊 Gemeinsame Reactive-Funktion: Delta Stats pro User
  # ───────────────────────────────────────────────
  delta_participation_stats <- reactive({
    net <- networkData()
    if (is.null(net)) return(NULL)
    g <- graph_from_data_frame(d = net$edges, vertices = net$nodes, directed = TRUE)
    delta_ids <- net$nodes$id[net$nodes$shape == "triangle"]
    root <- net$nodes$id[!(net$nodes$id %in% net$edges$to)]
    if (length(root)==0) root <- net$nodes$id[1]
    on_delta <- c()
    for(r in root) for(t in delta_ids) {
      sp <- shortest_paths(g, from = r, to = t)$vpath[[1]]
      on_delta <- union(on_delta, as_ids(sp))
    }
    dt <- data.table(author = net$nodes$author, on_delta = net$nodes$id %in% on_delta)
    user_stats <- dt[, .(total = .N, delta_count = sum(on_delta)), by = author]
    user_stats[, delta_rate := delta_count / total]
    user_stats
  })
  
  # ───────────────────────────────────────────────
  # UI FOR CARDS SELECTION USER PARTICIPATION TAB
  # ───────────────────────────────────────────────
  
  # Card descriptions for User Perspective tab
  user_card_descriptions <- list(
    "user_timeline" = "Timeline visualization showing the sequence of interactions between users, with delta awards highlighted.",
    "user_delta" = "List of users who have participated in delta-awarded discussions, showing their delta rate and contribution statistics.",
    "nw_metrics" = "Network analysis metrics for the current discussion, including centrality measures and interaction patterns."
  )
  
  # Render descriptions for each card
  output$card1_desc <- renderUI({
    HTML(user_card_descriptions[[input$right_graph1]])
  })
  
  output$card2_desc <- renderUI({
    HTML(user_card_descriptions[[input$right_graph2]])
  })
  
  # Card 1:
  output$card1_ui <- renderUI({
    switch(input$right_graph1,
           user_timeline      = plotOutput("user_timeline", height="100%"),
           user_delta         = uiOutput("user_delta_highlights"),
           nw_metrics         = uiOutput("nw_metrics")
    )
  })
  
  # Card 2:
  output$card2_ui <- renderUI({
    switch(input$right_graph2,
           user_timeline      = plotOutput("user_timeline", height="100%"),
           user_delta         = uiOutput("user_delta_highlights"),
           nw_metrics         = uiOutput("nw_metrics")
    )
  })
  
  # ───────────────────────────────────────────────
  # 🌐 Network Metrics
  # ───────────────────────────────────────────────
  extract_user_edges <- function(node, parent_author = NULL) {
    edges <- list(); users <- character(0)
    current_author <- node$author
    if (is.null(current_author) || current_author == "" ||
        tolower(current_author) %in% c("deltabot","automoderator")) {
      return(list(edges = list(), users = character(0)))
    }
    users <- current_author
    if (!is.null(parent_author) && parent_author != "" &&
        !tolower(parent_author) %in% c("deltabot","automoderator")) {
      edges[[ length(edges) + 1 ]] <- data.frame(
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
  
  output$nw_metrics <- renderUI({
    net <- networkData()
    if (is.null(net)) {
      return(HTML("<p>Select a discussion to view metrics.</p>"))
    }
    res       <- extract_user_edges(net$selected_post)
    users     <- unique(res$users)
    edges_df  <- if (length(res$edges) > 0) do.call(rbind, res$edges) else
      data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    op <- net$selected_post$author
    if (isTRUE(input$remove_op)) {
      edges_df <- edges_df[edges_df$from != op & edges_df$to != op, ]
      users    <- setdiff(users, op)
    }
    ec <- edges_df %>% 
      group_by(from, to) %>% 
      summarise(weight = n(), .groups = "drop")
    if (length(users) == 0) {
      return(HTML("<p>No users in this network.</p>"))
    }
    g   <- graph_from_data_frame(d = ec, vertices = data.frame(name = users), directed = TRUE)
    tryCatch({
      degs <- degree(g, mode = "all")
    }, error = function(e) {
      degs <- degree(g)
    })
    keep <- names(degs[degs >= input$min_component_size])
    g2  <- induced_subgraph(g, vids = keep)
    if (vcount(g2) == 0) {
      return(HTML("<p>No users meet the interaction criteria.</p>"))
    }
    tryCatch({
      avg_deg <- if (ecount(g2) > 0) mean(degree(g2, mode = "all")) else 0
    }, error = function(e) {
      avg_deg <- if (ecount(g2) > 0) mean(degree(g2)) else 0
    })
    central_scores <- tryCatch({
      centr_degree(g2, mode = "all")$res
    }, error = function(e) {
      centr_degree(g2)$res
    })
    names(central_scores) <- V(g2)$name
    most_central <- names(central_scores)[which.max(central_scores)]
    total_interactions <- ecount(g2)
    total_users        <- vcount(g2)
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
  
  observeEvent(input$viewPerspective, {
    if (input$viewPerspective == "global") {
      session$sendCustomMessage(type = "fix_global_plots", message = list())
      
      shinyjs::runjs("
        setTimeout(function() {
          $('.medium-plot-container, .small-plot-container').each(function() {
            $(this).css('height', '400px');
            $(this).css('max-height', '400px');
            $(this).css('overflow-y', 'auto');
          });
          
          $('.shiny-plot-output').css('max-height', '400px');
          $('.shiny-plot-output').parent().css('max-height', '400px');
          
          $(window).trigger('resize');
        }, 300);
      ")
    }
  })
  
  # Dataset overview metrics for the Home tab
  dataset_metrics <- reactive({
    posts <- data_raw()
    
    if (is.null(posts) || length(posts) == 0) {
      return(list(
        total_discussions = 0,
        unique_users = 0,
        total_messages = 0,
        earliest_date = "N/A",
        latest_date = "N/A",
        timeframe = "No data available",
        delta_count = 0,
        top_topics = c()
      ))
    }
    
    # Initialize counters
    all_users <- character(0)
    message_count <- 0
    delta_count <- 0
    all_timestamps <- c()
    all_text <- ""
    
    # Process each post
    for (post in posts) {
      # Extract all nodes
      nodes <- get_nodes(post, is_root = TRUE)
      
      # Count messages
      message_count <- message_count + length(nodes)
      
      # Extract users
      users_in_post <- sapply(nodes, function(node) node$author)
      all_users <- c(all_users, users_in_post)
      
      # Extract text for topic analysis
      post_text <- sapply(nodes, function(node) node$rawText)
      all_text <- paste(all_text, paste(post_text, collapse = " "), sep = " ")
      
      # Extract timestamps
      if (length(nodes) > 0) {
        timestamps <- sapply(nodes, function(node) {
          if (!is.null(node$timestamp)) {
            # Convert timestamp to Date object if it's not already
            if (is.character(node$timestamp)) {
              return(as.POSIXct(node$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
            } else {
              return(node$timestamp)
            }
          } else {
            return(NA)
          }
        })
        all_timestamps <- c(all_timestamps, timestamps[!is.na(timestamps)])
      }
      
      # Check if post contains delta
      if (contains_delta(post, post$author)) {
        delta_count <- delta_count + 1
      }
    }
    
    # Process the collected data
    unique_users <- length(unique(all_users))
    total_discussions <- length(posts)
    
    # Calculate timeframe
    earliest_date <- "Unknown"
    latest_date <- "Unknown"
    timeframe <- "Unknown"
    
    if (length(all_timestamps) >= 2) {
      clean_timestamps <- all_timestamps[!is.na(all_timestamps)]
      if (length(clean_timestamps) > 0) {
        min_time <- min(clean_timestamps, na.rm = TRUE)
        max_time <- max(clean_timestamps, na.rm = TRUE)
        
        earliest_date <- format(min_time, "%b %d, %Y")
        latest_date <- format(max_time, "%b %d, %Y")
        
        timeframe <- paste(earliest_date, "to", latest_date)
      }
    }
    
    # Extract main topics (simple keyword extraction)
    # In a real implementation, you might want to use a more sophisticated topic model
    top_topics <- c("Politics", "Ethics", "Social Issues", "Technology", "Philosophy", "Science")
    
    # Return the metrics
    list(
      total_discussions = total_discussions,
      unique_users = unique_users,
      total_messages = message_count,
      earliest_date = earliest_date,
      latest_date = latest_date,
      timeframe = timeframe,
      delta_count = delta_count,
      delta_rate = delta_count / total_discussions,
      top_topics = top_topics
    )
  })
  
  # # Render dataset overview for the Home tab
  # output$dataset_overview <- renderUI({
  #   metrics <- dataset_metrics()
    
  #   div(class = "dataset-overview",
  #       div(class = "overview-header",
  #           h2("Change My View Dataset Overview"),
  #           p("This dataset contains Reddit posts from r/ChangeMyView, where users post opinions for others to challenge."),
  #           p("When an original poster (OP) changes their view based on responses, they award a delta (Δ) to acknowledge this.")
  #       ),
        
  #       div(class = "overview-metrics",
  #           div(class = "metric-row",
  #               div(class = "metric-card",
  #                   div(class = "metric-value", metrics$total_discussions),
  #                   div(class = "metric-label", "Total Discussions")
  #               ),
  #               div(class = "metric-card",
  #                   div(class = "metric-value", metrics$unique_users),
  #                   div(class = "metric-label", "Unique Users")
  #               ),
  #               div(class = "metric-card",
  #                   div(class = "metric-value", metrics$total_messages),
  #                   div(class = "metric-label", "Total Messages")
  #               ),
  #               div(class = "metric-card",
  #                   div(class = "metric-value", metrics$delta_count),
  #                   div(class = "metric-label", "Deltas Awarded"),
  #                   div(class = "metric-sublabel", sprintf("%.1f%% success rate", metrics$delta_rate * 100))
  #               )
  #           )
  #       ),
        
  #       div(class = "overview-details",
  #           div(class = "details-row",
  #               div(class = "details-card timeframe-card",
  #                   h3("Dataset Timeframe"),
  #                   p(metrics$timeframe),
  #                   div(class = "date-range",
  #                       div(class = "date-item",
  #                           span(class = "date-label", "Earliest:"),
  #                           span(class = "date-value", metrics$earliest_date)
  #                       ),
  #                       div(class = "date-item",
  #                           span(class = "date-label", "Latest:"),
  #                           span(class = "date-value", metrics$latest_date)
  #                       )
  #                   )
  #               ),
  #               div(class = "details-card topics-card",
  #                   h3("Main Discussion Topics"),
  #                   div(class = "topics-grid",
  #                       lapply(metrics$top_topics, function(topic) {
  #                           div(class = "topic-tag", topic)
  #                       })
  #                   )
  #               )
  #           ),
            
  #           div(class = "details-row",
  #               div(class = "details-card delta-explainer",
  #                   h3("Understanding Deltas (Δ)"),
  #                   p("A delta (Δ) is awarded by the original poster (OP) when someone successfully changes their view."),
  #                   p("In the Change My View community, this system helps track effective persuasion techniques."),
  #                   p("Key indicators of successful persuasion in this dataset include:"),
  #                   tags$ul(
  #                       tags$li("Evidence-based arguments with reliable sources"),
  #                       tags$li("Addressing the core of the OP's reasoning"),
  #                       tags$li("Acknowledging partial agreements while challenging key assumptions"),
  #                       tags$li("Using a combination of logical and emotional appeals")
  #                   )
  #               ),
  #               div(class = "details-card app-guide",
  #                   h3("Using This Application"),
  #                   p("This dashboard lets you explore persuasion patterns in online discussions."),
  #                   tags$ol(
  #                       tags$li("Use the Network view to visualize discussion structures"),
  #                       tags$li("Timeline shows how conversations evolve over time"),
  #                       tags$li("Arguments tab reveals logical structures in persuasive discussions"),
  #                       tags$li("Switch between individual discussions and the global view to compare patterns")
  #                   ),
  #                   p("Pay special attention to delta (Δ) markers, which indicate successful persuasion.")
  #               )
  #           )
  #       )
  # })
  
  source("user_perspective.R", local=TRUE)
  source("temporal_perspective.R", local=TRUE)
  source("global_perspective.R", local=TRUE)
  source("arguments_tab.R", local=TRUE)
  source("persuasion_tab.R", local=TRUE)
}

shinyApp(ui = ui, server = server)

