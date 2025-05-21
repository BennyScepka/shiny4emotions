# ───────────────────────────────────────────────
# 🕓 Time Perspective Tab 
# ───────────────────────────────────────────────

# Wrap all references to input in existence check
if (exists("input")) {
  # Make sure all_posts function exists and is properly defined before using it
  if (!exists("all_posts")) {
    all_posts <- reactive({
      # Check if networkData exists
      if (!exists("networkData") || is.null(networkData())) {
        return(NULL)
      }
      
      net <- networkData()
      req(net)
      
      # Processing logic for all_posts
      nodes_df <- as.data.frame(net$nodes)
      nodes_df %>%
        arrange(timestamp) %>%
        mutate(
          seq_idx = row_number(),
          sentiment = syuzhet::get_sentiment(rawText)
        )
    })
  }
}

# Wrap all output references in condition check
if (exists("output")) {

  output$post_timeline <- renderPlot({
    req(input$timeSlider)
    df <- all_posts()
    req(df)
    
    cutoff      <- input$timeSlider
    filtered_df <- df[df$seq_idx <= cutoff, ]
    
    # define DeltaFlag here
    filtered_df$DeltaFlag <- ifelse(filtered_df$shape == "triangle", "Delta", "Regular")
    
    ggplot(filtered_df, aes(x = seq_idx, y = 0, color = DeltaFlag)) +
      geom_point(aes(size = ifelse(DeltaFlag == "Delta", 6, 4))) +
      scale_size_identity() +
      labs(
        x     = "Relative Interaction Order",
        y     = "",
        title = "User Interaction Timeline"
      ) +
      theme_minimal() +
      theme(
        axis.text.y        = element_blank(),
        axis.ticks.y       = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })


  output$current_post_preview <- renderUI({
    req(input$timeSlider)      
    df <- all_posts()
    req(df)
    
    current_idx <- input$timeSlider
    if (current_idx <= nrow(df)) {
      current_post <- df[current_idx, ]
      div(
        h4(paste("Post #", current_idx, "by", current_post$author)),
        div(class = "verbatim-text", style = "max-height: 150px;", 
            tags$pre(current_post$rawText))
      )
    }
  })


  output$enhanced_time_slider <- renderUI({
    df <- all_posts()
    req(df)
    
    total_posts <- nrow(df)
    delta_positions <- which(df$shape == "triangle")
    delta_markers <- if(length(delta_positions) > 0) {
      paste("Deltas at positions:", paste(delta_positions, collapse = ", "))
    } else {
      "No deltas in this discussion"
    }
    
    tagList(
      sliderInput("timeSlider", "Show up to reply index:",
                  min = 1, max = total_posts, value = total_posts, step = 1,
                  animate = animationOptions(interval = 800, loop = FALSE),
                  width = "100%"),
      div(style = "display: flex; justify-content: space-between;",
          actionButton("prev_post", "< Previous", class = "btn-sm"),
          span(textOutput("current_position_info")),
          actionButton("next_post", "Next >", class = "btn-sm")
      ),
      div(style = "margin-top: 10px;", delta_markers)
    )
  })


  output$current_position_info <- renderText({
    req(input$timeSlider)
    df <- all_posts()
    req(df)
    current_idx <- input$timeSlider
    max_idx <- nrow(df)
    paste("Position", current_idx, "of", max_idx)
  })


  observeEvent(input$prev_post, {
    current <- input$timeSlider
    if(current > 1) {
      updateSliderInput(session, "timeSlider", value = current - 1)
    }
  })

  observeEvent(input$next_post, {
    current <- input$timeSlider
    max_val <- as.numeric(isolate(all_posts() %>% nrow()))
    if(current < max_val) {
      updateSliderInput(session, "timeSlider", value = current + 1)
    }
  })


  output$comparative_metrics <- renderUI({
    req(input$timeSlider)  
    
    df <- all_posts()
    req(df)
    
    cutoff <- input$timeSlider
    
    current_authors <- length(unique(df$author[df$seq_idx <= cutoff]))
    current_deltas  <- sum(df$shape[df$seq_idx <= cutoff] == "triangle")
    
    final_authors <- length(unique(df$author))
    final_deltas  <- sum(df$shape == "triangle")
    
    author_progress <- round(current_authors / final_authors * 100)
    delta_progress  <- if (final_deltas > 0) round(current_deltas / final_deltas * 100) else 0
    
    div(
      h4("Discussion Progress"),
      div(class = "metrics-grid",
          div(class = "metrics-item",
              div(class = "metrics-value", paste0(author_progress, "%")),
              div(class = "metrics-label", "Users Joined")
          ),
          div(class = "metrics-item",
              div(class = "metrics-value", current_authors),
              div(class = "metrics-label", paste0("of ", final_authors, " Users"))
          ),
          div(class = "metrics-item",
              div(class = "metrics-value", paste0(delta_progress, "%")),
              div(class = "metrics-label", "Deltas Awarded")
          ),
          div(class = "metrics-item",
              div(class = "metrics-value", current_deltas),
              div(class = "metrics-label", paste0("of ", final_deltas, " Deltas"))
          )
      )
    )
  })



  output$time_sentiment_trajectory <- renderPlot({
    req(input$timeSlider)           
    
    df <- all_posts()
    req(df)
    if (nrow(df) == 0) {
      plot.new(); title("No data available")
      return()
    }
    
    cutoff <- input$timeSlider
    nrc <- syuzhet::get_nrc_sentiment(df$rawText)
    nrc$seq_idx <- df$seq_idx
    nrc_long <- pivot_longer(
      nrc,
      cols      = c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust"),
      names_to  = "emotion",
      values_to = "score"
    )
    past   <- filter(nrc_long, seq_idx <= cutoff)
    future <- filter(nrc_long, seq_idx  > cutoff)
    deltas <- df$seq_idx[df$shape == "triangle"]
    
    ggplot() +
      geom_line(data = past,   aes(seq_idx, score, color = emotion), linewidth = 1) +
      geom_line(data = future, aes(seq_idx, score, color = emotion), linewidth = 0.5, alpha = 0.3) +
      geom_point(data = filter(past, seq_idx %in% deltas),
                 aes(seq_idx, score), color="red", size=3) +
      geom_vline(xintercept = cutoff, lty="dashed", alpha = 0.5) +
      labs(x="Reply index", y="Emotion count", title="Emotion trajectories over time") +
      theme_minimal() +
      theme(legend.position="bottom")
  })



  output$key_events <- renderUI({
    req(input$timeSlider)       # ← guard here
    
    df <- all_posts()
    req(df)
    
    cutoff       <- input$timeSlider
    filtered_df  <- df[df$seq_idx <= cutoff, ]
    delta_events <- filtered_df[filtered_df$shape == "triangle", ]
    
    sent_shifts <- data.frame(pos = integer(), author = character(), shift = numeric())
    if (nrow(filtered_df) > 1) {
      for (i in 2:nrow(filtered_df)) {
        shift_val <- filtered_df$sentiment[i] - filtered_df$sentiment[i - 1]
        if (abs(shift_val) > 5) {
          sent_shifts <- rbind(sent_shifts, data.frame(
            pos    = i,
            author = filtered_df$author[i],
            shift  = shift_val
          ))
        }
      }
    }
    
    tagList(
      h4("Key Events"),
      if (nrow(delta_events) > 0) {
        div(
          h5("Deltas Awarded:"),
          tags$ul(
            lapply(seq_len(nrow(delta_events)), function(i) {
              tags$li(sprintf("At position %d by %s",
                              delta_events$seq_idx[i],
                              delta_events$author[i]))
            })
          )
        )
      } else {
        div("No deltas awarded yet.")
      },
      
      if (nrow(sent_shifts) > 0) {
        div(
          h5("Significant Sentiment Shifts:"),
          tags$ul(
            lapply(seq_len(nrow(sent_shifts)), function(i) {
              shift_type <- if (sent_shifts$shift[i] > 0) "positive" else "negative"
              tags$li(sprintf("At position %d: %s shift by %s",
                              sent_shifts$pos[i],
                              shift_type,
                              sent_shifts$author[i]))
            })
          )
        )
      } else {
        div("No significant sentiment shifts detected.")
      }
    )
  })


  output$argument_tactics_heatmap <- renderPlot({
    arg_data <- argument_structure_data()
    if (is.null(arg_data) || is.null(arg_data$flat)) return(NULL)
    
    df <- all_posts()
    req(df)
    cutoff <- input$timeSlider
    
    filtered_ids <- df$id[df$seq_idx <= cutoff]
    
    filtered_args <- arg_data$flat[arg_data$flat$node_id %in% filtered_ids, ]
    
    filtered_args$seq_idx <- match(filtered_args$node_id, df$id)
    filtered_args <- filtered_args[order(filtered_args$seq_idx), ]
    
    components <- filtered_args[, c("seq_idx", "claims", "premises", "rebuttals", "concessions", "evidence")]
    
    plot_data <- components %>%
      tidyr::pivot_longer(cols = all_of(c("claims", "premises", "rebuttals", "concessions", "evidence")),
                          names_to = "component", values_to = "count")
    
    ggplot(plot_data, aes(x = seq_idx, y = component, fill = count)) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_gradient(low = "#E6F2F9", high = "#0063A6") +
      theme_minimal() +
      labs(x = "Reply Sequence", y = "Argument Component", 
           title = "Argument Tactics Used Over Time",
           fill = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "right")
  })

  # ───────────────────────────────────────────────
  # Dynamic Chart Title and Content
  # ───────────────────────────────────────────────

  # Dynamic chart title based on selection
  output$time_chart_title <- renderText({
    switch(input$time_chart_type,
           "emotion_trajectory" = "Emotion Trajectory",
           "argument_tactics" = "Argument Tactics Over Time",
           "No chart selected")
  })

  # Render the selected chart based on user choice
  output$time_selected_chart <- renderUI({
    switch(input$time_chart_type,
           "emotion_trajectory" = plotOutput("time_sentiment_trajectory", height = "100%", width = "100%"),
           "argument_tactics" = plotOutput("argument_tactics_heatmap", height = "100%", width = "100%"),
           # Fallback
           div("Please select a chart type from the sidebar"))
  })

  # Only show descriptions for Emotion Trajectory and Argument tactics
  time_chart_descriptions <- list(
    "emotion_trajectory" = "Shows the emotional progression of the discussion over time, tracking how different emotions (anger, joy, trust, etc.) fluctuate throughout the conversation.",
    "argument_tactics" = "Displays the rhetorical strategies used across the discussion timeline, highlighting shifts in argumentation approaches and persuasion techniques."
  )

  output$time_chart_description <- renderUI({
    chart <- input$time_chart_type
    # Defensive: Only proceed if chart is a non-empty string and a valid key
    if (is.null(chart) || chart == "" || !(chart %in% names(time_chart_descriptions))) return(NULL)
    desc <- time_chart_descriptions[[chart]]
    tags$div(
      style = "margin-bottom: 0.5rem;",
      tags$p(desc)
    )
  })
}