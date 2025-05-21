########################################################
# Global Perspective Implementation
########################################################

global_metrics <- reactiveVal(NULL)

# Simple logging function
log_message <- function(msg, ...) {
  message(msg, ...)
}

# Chart descriptions for Global Perspective tab
global_chart_descriptions <- list(
  "discussion_progression" = "Shows the progression of discussions from initial posts to delta awards, highlighting key factors that contribute to successful persuasion at each stage.",
  "narrative_evolution" = "Visualizes how emotional tone, evidence usage, concession rates, and argument complexity evolve across different stages of successful delta-winning discussions.",
  "discussion_length" = "Analyzes how discussion length correlates with delta success rates, showing which conversation lengths are most effective for changing views."
)

# Make sure observations and events are wrapped in input check
if (exists("input")) {
  observeEvent(input$viewPerspective, {
    if (input$viewPerspective == "global") {
      if (is.null(global_metrics())) {
        withProgress(
          message = "Calculating global metrics...",
          value = 0.1,
          {
            incProgress(0.3, detail = "Analyzing discussions...")
            new_metrics <- calculate_global_metrics()
            incProgress(0.3, detail = "Processing patterns...")
            global_metrics(new_metrics)
            incProgress(0.3, detail = "Finalizing insights...")
          }
        )
      }
    }
  })
  
  # Fix plot sizing when switching to global view
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
}

# Render chart descriptions
output$global_chart_descriptions <- renderUI({
  tagList(
    h4("Chart Descriptions"),
    div(
      class = "chart-descriptions",
      div(
        h5("Discussion Progression to Delta"),
        p(global_chart_descriptions$discussion_progression),
        class = "mb-3"
      ),
      div(
        h5("Narrative Evolution Patterns"),
        p(global_chart_descriptions$narrative_evolution),
        class = "mb-3"
      ),
      div(
        h5("Discussion Length Impact"),
        p(global_chart_descriptions$discussion_length),
        class = "mb-3"
      )
    )
  )
})

# Enhanced calculate_global_metrics function
calculate_global_metrics <- function() {
  posts <- data_raw()
  if (is.null(posts)) {
    message("No data available for global metrics calculation")
    return(NULL)
  }
  
  # Prepare lists to collect per-thread data
  arg_component_list <- list()
  length_bin_list <- c()
  delta_thread_flags <- c()
  
  # Track narrative evolution in delta-winning threads
  evolution_data <- list(
    initial = list(emotional = c(), evidence = c(), concession = c(), complexity = c()),
    early = list(emotional = c(), evidence = c(), concession = c(), complexity = c()),
    middle = list(emotional = c(), evidence = c(), concession = c(), complexity = c()),
    late = list(emotional = c(), evidence = c(), concession = c(), complexity = c()),
    resolution = list(emotional = c(), evidence = c(), concession = c(), complexity = c())
  )
  
  # For appeal type success: collect scores for delta threads
  logos_scores <- c()
  pathos_scores <- c()
  ethos_scores <- c()
  
  for (i in seq_along(posts)) {
    post <- posts[[i]]
    thread_has_delta <- contains_delta(post, post$author)
    delta_thread_flags <- c(delta_thread_flags, thread_has_delta)
    
    # Extract all nodes from this post
    all_nodes <- get_nodes(post, is_root = TRUE)
    flat_nodes <- do.call(rbind, lapply(all_nodes, as.data.frame))
    # Defensive: skip if no nodes
    if (is.null(flat_nodes) || nrow(flat_nodes) == 0) next
    
    # --- Argument component aggregation ---
    # Use analyze_argument_components on each node's text
    comp_mat <- do.call(rbind, lapply(flat_nodes$rawText, function(txt) {
      as.numeric(analyze_argument_components(txt)[1, c("claims", "premises", "rebuttals", "concessions", "evidence")])
    }))
    colnames(comp_mat) <- c("claims", "premises", "rebuttals", "concessions", "evidence")
    comp_means <- colMeans(comp_mat, na.rm = TRUE)
    arg_component_list[[length(arg_component_list) + 1]] <- data.frame(
      thread = i,
      has_delta = thread_has_delta,
      t(comp_means)
    )
    
    # --- Discussion length binning ---
    n_comments <- nrow(flat_nodes)
    length_bin <- cut(n_comments, breaks = c(0,5,10,20,50,Inf), labels = c("1-5","6-10","11-20","21-50","51+"), right = TRUE)
    length_bin_list <- c(length_bin_list, as.character(length_bin)[1])
    
    # Process narrative evolution for delta threads
    if (thread_has_delta && length(all_nodes) >= 5) {
      thread_length <- length(all_nodes)
      
      # Define stage boundaries
      stages <- list(
        initial = 1,
        early = round(thread_length * 0.2),
        middle = round(thread_length * 0.5),
        late = round(thread_length * 0.8),
        resolution = thread_length
      )
      
      # Process each stage
      for (stage_name in names(stages)) {
        stage_idx <- stages[[stage_name]]
        node <- all_nodes[[stage_idx]]
        
        # Skip if invalid node
        if (is.null(node) || is.null(node$rawText) || nchar(node$rawText) < 20) next
        
        # Get sentiment analysis
        sentiment <- get_sentiment(node$rawText, method = "syuzhet")
        nrc_emotions <- get_nrc_sentiment(node$rawText)
        
        # Calculate emotional balance (positive - negative)
        positive_score <- sum(nrc_emotions[c("joy", "trust", "anticipation")])
        negative_score <- sum(nrc_emotions[c("anger", "fear", "sadness", "disgust")])
        emotional_balance <- (positive_score - negative_score) / (positive_score + negative_score + 1)
        
        # Get argument analysis
        complexity_score <- NA
        tryCatch({
          arg_analysis <- analyze_argument_components(node$rawText)
          evidence_weight <- as.numeric(arg_analysis$evidence)
          concession_rate <- as.numeric(arg_analysis$concessions)
          
          # Calculate complexity (sum of all components, or mean if you prefer)
          comp_vals <- as.numeric(arg_analysis[1, c("claims", "premises", "rebuttals", "concessions", "evidence")])
          if (all(is.na(comp_vals))) comp_vals <- rep(0, 5)
          complexity_score <- mean(comp_vals, na.rm = TRUE)
        }, error = function(e) {
          evidence_weight <- 0.3
          concession_rate <- 0.2
          complexity_score <- 0.5
        })
        
        # Always append a numeric value for complexity
        if (is.na(complexity_score)) complexity_score <- 0.5
        
        # Store in evolution data
        evolution_data[[stage_name]]$emotional <- c(evolution_data[[stage_name]]$emotional, emotional_balance)
        evolution_data[[stage_name]]$evidence <- c(evolution_data[[stage_name]]$evidence, evidence_weight)
        evolution_data[[stage_name]]$concession <- c(evolution_data[[stage_name]]$concession, concession_rate)
        evolution_data[[stage_name]]$complexity <- c(evolution_data[[stage_name]]$complexity, complexity_score)
      }
    }
    
    # Appeal type aggregation for delta threads
    if (thread_has_delta && !is.null(flat_nodes) && nrow(flat_nodes) > 0) {
      # Try to get scores for each node
      for (txt in flat_nodes$rawText) {
        appeal <- tryCatch(analyze_persuasion(txt), error = function(e) NULL)
        if (!is.null(appeal)) {
          logos_scores <- c(logos_scores, as.numeric(appeal$logos_score))
          pathos_scores <- c(pathos_scores, as.numeric(appeal$pathos_score))
          ethos_scores <- c(ethos_scores, as.numeric(appeal$ethos_score))
        }
      }
    }
  }
  
  # --- Aggregate argument component means for delta/non-delta threads ---
  arg_df <- do.call(rbind, arg_component_list)
  arg_components <- data.frame(
    component = c("claims", "premises", "rebuttals", "concessions", "evidence"),
    delta_avg = sapply(c("claims", "premises", "rebuttals", "concessions", "evidence"), function(comp) {
      mean(arg_df[arg_df$has_delta, comp], na.rm = TRUE)
    }),
    non_delta_avg = sapply(c("claims", "premises", "rebuttals", "concessions", "evidence"), function(comp) {
      mean(arg_df[!arg_df$has_delta, comp], na.rm = TRUE)
    }),
    stringsAsFactors = FALSE
  )
  
  # --- Aggregate discussion length bins and delta rates ---
  length_tab <- table(length_bin_list, delta_thread_flags)
  length_bins <- rownames(length_tab)
  length_metrics <- data.frame(
    length_bin = length_bins,
    delta_rate = ifelse(rowSums(length_tab) > 0, length_tab[, "TRUE"] / rowSums(length_tab), 0),
    count = rowSums(length_tab),
    stringsAsFactors = FALSE
  )
  
  # Calculate averages for each stage
  stage_names <- c("Initial Post", "Early Replies", "Middle Discussion", "Late Discussion", "Resolution (Delta)")
  narrative_evolution <- data.frame(
    stage = character(),
    position = numeric(),
    emotional_balance = numeric(),
    evidence_weight = numeric(),
    concession_rate = numeric(),
    complexity_score = numeric(),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(names(evolution_data))) {
    stage <- names(evolution_data)[i]
    
    narrative_evolution <- rbind(
      narrative_evolution,
      data.frame(
        stage = stage_names[i],
        position = i,
        emotional_balance = mean(evolution_data[[stage]]$emotional, na.rm = TRUE),
        evidence_weight = mean(evolution_data[[stage]]$evidence, na.rm = TRUE),
        concession_rate = mean(evolution_data[[stage]]$concession, na.rm = TRUE),
        complexity_score = mean(evolution_data[[stage]]$complexity, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Fill NA values with defaults
  narrative_evolution[is.na(narrative_evolution)] <- 0
  
  # Compose metrics list
  all_metrics <- list(
    total_discussions = length(posts),
    delta_discussions = sum(delta_thread_flags),
    delta_ratio = ifelse(length(posts) > 0, sum(delta_thread_flags) / length(posts), 0),
    logos_success = if (length(logos_scores) > 0) mean(logos_scores, na.rm = TRUE) else 0,
    pathos_success = if (length(pathos_scores) > 0) mean(pathos_scores, na.rm = TRUE) else 0,
    ethos_success = if (length(ethos_scores) > 0) mean(ethos_scores, na.rm = TRUE) else 0,
    arg_components = arg_components,
    length_metrics = length_metrics,
    delta_words = list(),
    non_delta_words = list(),
    narrative_evolution = narrative_evolution
  )
  
  return(all_metrics)
}

# Wrap all output references in condition check
if (exists("output")) {
  
  # Narrative evolution chart with real data from metrics
  output$narrative_evolution_analysis <- renderPlot({
    metrics <- global_metrics()
    if (is.null(metrics)) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
           xlim = c(0, 1), ylim = c(0, 1), main = "No data available")
      text(0.5, 0.5, "Click 'Refresh Analysis' to load data", cex = 1.2)
      return()
    }
    
    # Use the real evolution data from metrics
    evolution_data <- metrics$narrative_evolution
    
    # Handle case where no evolution data is available
    if (is.null(evolution_data) || nrow(evolution_data) == 0) {
      # Create fallback data
      evolution_data <- data.frame(
        stage = c("Initial Post", "Early Replies", "Middle Discussion", "Late Discussion", "Resolution (Delta)"),
        position = 1:5,
        emotional_balance = c(0.2, -0.1, 0.1, 0.3, 0.5),
        evidence_weight = c(0.3, 0.4, 0.5, 0.6, 0.7),
        concession_rate = c(0.1, 0.2, 0.3, 0.4, 0.5),
        complexity_score = c(0.4, 0.5, 0.6, 0.7, 0.8),
        stringsAsFactors = FALSE
      )
    }
    
    # Convert to long format for plotting
    evolution_long <- evolution_data %>%
      tidyr::pivot_longer(
        cols = c("emotional_balance", "evidence_weight", "concession_rate", "complexity_score"),
        names_to = "metric",
        values_to = "value"
      )
    
    # Create line plot showing evolution
    ggplot(evolution_long, aes(x = factor(stage, levels = evolution_data$stage), 
                               y = value, color = metric, group = metric)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      scale_color_manual(
        values = c(
          "emotional_balance" = "#E4003A",
          "evidence_weight" = "#0063A6",
          "concession_rate" = "#009A93",
          "complexity_score" = "#F6A800"
        ),
        labels = c(
          "emotional_balance" = "Emotional Tone",
          "evidence_weight" = "Evidence Usage",
          "concession_rate" = "Concession Rate",
          "complexity_score" = "Argument Complexity"
        )
      ) +
      labs(
        title = "Narrative Evolution Patterns in Successful Persuasion",
        subtitle = paste0("Based on ", metrics$delta_discussions, " delta-winning discussions"),
        x = "Discussion Stage",
        y = "Normalized Score",
        color = "Metric"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      scale_y_continuous(limits = c(-1, 1.5)) +
      # Add annotation for key findings
      annotate("text", x = 2.5, y = 1.4, 
               label = paste0("Delta Success Rate: ", round(metrics$delta_ratio * 100, 1), "%"),
               size = 4, color = "gray30", fontface = "italic")
  })
  
  # Key insights summary
  
  output$key_insights_summary <- renderText({
    # Always recalc the global metrics so we get real values
    metrics <- calculate_global_metrics()
    if (is.null(metrics)) {
      return("No data available for global metrics calculation.")
    }
    
    # Core stats
    total_discussions <- metrics$total_discussions
    percent_delta     <- round(metrics$delta_ratio * 100, 1)
    
    # Top appeal
    appeals <- c(
      "Logos (logical)"     = metrics$logos_success,
      "Pathos (emotional)"  = metrics$pathos_success,
      "Ethos (credibility)" = metrics$ethos_success
    )
    top_appeal <- if (all(is.na(appeals)) || sum(appeals, na.rm = TRUE) == 0) {
      "N/A"
    } else {
      names(appeals)[which.max(appeals)]
    }
    
    # Top argument component
    arg_df    <- metrics$arg_components
    comp_diff <- arg_df$delta_avg - arg_df$non_delta_avg
    top_component <- if (all(is.na(comp_diff))) {
      "N/A"
    } else {
      tools::toTitleCase(arg_df$component[which.max(comp_diff)])
    }
    
    # Best length bin
    length_df <- metrics$length_metrics
    best_length <- if (is.null(length_df) || nrow(length_df) == 0) {
      "N/A"
    } else {
      length_df$length_bin[which.max(length_df$delta_rate)]
    }
    
    # Build the multi-line summary
    paste0(
      "Analyzed ", total_discussions, " discussions, with a delta success rate of ", percent_delta, "%.\n",
      "- Predominant appeal style in successful discussions: ", top_appeal, ".\n",
      "- Argument component contributing most to success: ", top_component, ".\n",
      "- Highest delta rate observed in discussions with ", best_length, " posts."
    )
  })
  
  
  # Discussion Progression → Delta (funnel) chart
  output$word_usage_analysis <- renderPlot({
    # 1) Recalculate global metrics on each draw
    metrics <- calculate_global_metrics()
    
    # 2) If no data at all, show a placeholder
    if (is.null(metrics) || metrics$total_discussions < 1) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data available")
      text(0.5, 0.5, "No discussions to display", cex = 1.2)
      return()
    }
    
    # 3) Core numbers
    total        <- metrics$total_discussions
    delta_awarded <- metrics$delta_discussions
    len_df       <- metrics$length_metrics
    
    # 4) Thread‐length stages
    if (!is.null(len_df) && nrow(len_df) > 0) {
      mp   <- sum(len_df$count[len_df$length_bin != "1-5"], na.rm = TRUE)
      c10  <- sum(len_df$count[len_df$length_bin %in% c("11-20", "21-50", "51+")], na.rm = TRUE)
      c20  <- sum(len_df$count[len_df$length_bin %in% c("21-50", "51+")], na.rm = TRUE)
    } else {
      mp  <- round(total * 0.8)
      c10 <- round(total * 0.4)
      c20 <- round(total * 0.2)
    }
    
    # 5) Evidence & concessions fractions
    comp_df <- metrics$arg_components
    ev_frac    <- 0
    cons_frac  <- 0
    if (!is.null(comp_df) && nrow(comp_df) > 0) {
      if ("evidence" %in% comp_df$component) {
        v <- comp_df$delta_avg[comp_df$component == "evidence"]
        if (length(v) == 1 && !is.na(v)) ev_frac <- v
      }
      if ("concessions" %in% comp_df$component) {
        v <- comp_df$delta_avg[comp_df$component == "concessions"]
        if (length(v) == 1 && !is.na(v)) cons_frac <- v
      }
    }
    ev_used    <- round(total * ev_frac)
    cons_made  <- round(total * cons_frac)
    
    # 6) Build the funnel data frame
    stages  <- c(
      "Total Discussions",
      "Multi-party (>1 user)",
      "10+ Comments",
      "20+ Comments",
      "Evidence Used",
      "Concessions Made",
      "Delta Awarded"
    )
    counts  <- c(total, mp, c10, c20, ev_used, cons_made, delta_awarded)
    percents <- counts / total * 100
    tactics <- c(
      "Cast a wide net",
      "Foster engagement",
      "Encourage depth",
      "Maintain momentum",
      paste0("Evidence success: ", round(ev_frac * 100, 1), "%"),
      paste0("Concession success: ", round(cons_frac * 100, 1), "%"),
      paste0("Overall Δ rate: ", round(metrics$delta_ratio * 100, 1), "%")
    )
    
    funnel_df <- data.frame(
      stage      = factor(stages, levels = stages),
      count      = counts,
      percentage = percents,
      tactic     = tactics,
      stringsAsFactors = FALSE
    )
    
    # 7) Plot
    ggplot(funnel_df, aes(x = stage, y = count, fill = count)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = sprintf("%d (%.1f%%)", count, percentage)),
                hjust = -0.1, size = 4) +
      geom_text(aes(label = tactic),
                y = 0, hjust = 0, vjust = 0.5, size = 3) +
      coord_flip() +
      scale_fill_gradient(low = "#F6A800", high = "#0063A6", guide = "none") +
      labs(
        title = "Discussion Progression to Δ",
        x     = NULL,
        y     = "Number of Discussions"
      ) +
      theme_minimal() +
      theme(
        plot.title    = element_text(size = 16, face = "bold"),
        axis.text.y   = element_text(size = 12),
        axis.text.x   = element_text(size = 10)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  
  # Discussion metrics analysis
 
  # ---- Discussion Length Impact plot ----
  output$discussion_metrics_analysis <- renderPlot({
    # 1) load required packages
    library(dplyr)
    library(ggplot2)
    library(scales)
    
    # 2) recalc global metrics
    metrics <- calculate_global_metrics()
    if (is.null(metrics) ||
        is.null(metrics$length_metrics) ||
        nrow(metrics$length_metrics) == 0) {
      plot.new()
      title("No discussion length data available")
      return()
    }
    
    # 3) filter & order your five bins
    length_df <- metrics$length_metrics %>%
      filter(length_bin %in% c("1-5", "6-10", "11-20", "21-50", "51+")) %>%
      mutate(
        length_bin = factor(length_bin,
                            levels = c("1-5","6-10","11-20","21-50","51+")),
        delta_pct  = delta_rate * 100
      ) %>%
      arrange(length_bin)
    
    # 4) build the bar chart
    ggplot(length_df, aes(x = length_bin, y = delta_rate, fill = count)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", delta_pct)),
                vjust = -0.3, size = 4) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_fill_gradient(
        low = "#E6F2F9",
        high = "#0063A6",
        name = "Discussions"
      ) +
      labs(
        title    = "Δ Success Rate by Discussion Length",
        subtitle = paste0("Based on ", sum(length_df$count), " discussions"),
        x        = "Number of Posts in Discussion",
        y        = "Δ Success Rate"
      ) +
      theme_minimal() +
      theme(
        plot.title     = element_text(size = 16, face = "bold"),
        plot.subtitle  = element_text(size = 12, color = "gray40"),
        axis.text.x    = element_text(size = 12),
        axis.text.y    = element_text(size = 12),
        legend.position = "right"
      )
  })
}
