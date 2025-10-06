# app.R - Peter Cebo - Johhns Hopkins Data Science Capstone - Next Word Predictor NLP Model
library(shiny)
library(stringr)
library(shinythemes)

# Load model
model <- readRDS("model_70pct_5gram_accurate.rds")
MODEL_SIZE_MB <- 14.8

# Stopwords
STOPWORDS <- c("the", "a", "an", "of", "to", "in", "for", "on", "at", "with",
               "and", "or", "but", "is", "are", "was", "were", "be", "been",
               "have", "has", "had", "do", "does", "did", "will", "would",
               "could", "should", "may", "might", "must", "can", "my", "your",
               "his", "her", "its", "our", "their", "this", "that", "these",
               "those", "i", "you", "he", "she", "it", "we", "they")

# Prediction functions
predict_base <- function(text, n = 3) {
  text <- text %>% tolower() %>%
    str_replace_all("[''`´]", "'") %>%
    str_replace_all("[——]", "-")
  
  tokens <- str_extract_all(text, "[a-z]+(['-][a-z]+)*")[[1]]
  count <- length(tokens)
  
  if (count >= 4 && !is.null(model$fivegram_hash)) {
    key <- paste(tail(tokens, 4), collapse = "_")
    if (!is.null(model$fivegram_hash[[key]])) 
      return(head(model$fivegram_hash[[key]], n))
  }
  if (count >= 3 && !is.null(model$fourgram_hash)) {
    key <- paste(tail(tokens, 3), collapse = "_")
    if (!is.null(model$fourgram_hash[[key]])) 
      return(head(model$fourgram_hash[[key]], n))
  }
  if (count >= 2 && !is.null(model$trigram_hash)) {
    key <- paste(tail(tokens, 2), collapse = "_")
    if (!is.null(model$trigram_hash[[key]])) 
      return(head(model$trigram_hash[[key]], n))
  }
  if (count >= 1 && !is.null(model$bigram_hash)) {
    key <- tail(tokens, 1)
    if (!is.null(model$bigram_hash[[key]])) 
      return(head(model$bigram_hash[[key]], n))
  }
  return(head(model$unigrams$word, n))
}

predict_with_chaining <- function(text, n = 3) {
  base <- predict_base(text, n * 2)
  if (length(base) == 0) return(character(0))
  
  first_n <- head(base, n)
  max_chains <- if (all(first_n %in% STOPWORDS)) n else 2
  
  result <- character(0)
  chains <- 0
  
  for (word in base) {
    # Always add the single word first
    result <- c(result, word)
    
    # Then add two-word chain if it's a stopword
    if (word %in% STOPWORDS && chains < max_chains) {
      next_word <- predict_base(paste(text, word), 1)
      if (length(next_word) > 0) {
        result <- c(result, paste(word, next_word[1]))
        chains <- chains + 1
      }
    }
    
    if (length(result) >= n) break
  }
  
  # Format for display
  result <- gsub("\\bi\\b", "I", result)
  result <- gsub("^i ", "I ", result)
  return(head(result, n))
}

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel(
    windowTitle = "Peter Cebo - NLP Model - Next Word Predictor",
    div(
      h3("Peter Cebo"),
      h3("Johns Hopkins Data Science Capstone Project - Next Word Predictor"),
      p(style = "color: #7f8c8d; font-size: 14px;", 
        br(),
        "A 5-gram NLP model with smart two-word chaining | ", a("Documentation / README (opens in new tab)", href = "https://rpubs.com/pcebo/CapstoneREADME", target = "_blank"),
      br(),
      br(),)
      
    )
  ),
  
  fluidRow(
    column(8, offset = 2,
           wellPanel(
             textInput("user_input", "Enter your text and hit the \"Predict Next Word\" button ... :", 
                       value = "Welcome!",
                       width = "100%",
                       placeholder = "Type a phrase..."),
             actionButton("predict", "Predict Next Word", 
                          class = "btn-primary btn-lg btn-block"),
             br(),
             h5("... OR try the pre-filled examples below!", style = "font-weight: bold;"),
             fluidRow(
               column(4, actionButton("ex1", "I went to the", class = "btn-sm btn-block")),
               column(4, actionButton("ex2", "Thanks for the", class = "btn-sm btn-block")),
               column(4, actionButton("ex3", "How are you", class = "btn-sm btn-block"))
             ),
             h5("Please click the button TWICE (or click a preset and then click the Predict button)", style = "font-weight: bold;")
           ),
           
           wellPanel(
             h3("Predictions:"),
             verbatimTextOutput("output"),
             uiOutput("stats")
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Auto-predict when example buttons clicked
  observeEvent(input$ex1, { 
    updateTextInput(session, "user_input", value = "I went to the")
  })
  observeEvent(input$ex2, { 
    updateTextInput(session, "user_input", value = "Thanks for the")
  })
  observeEvent(input$ex3, { 
    updateTextInput(session, "user_input", value = "How are you")
  })
  
  # Make predictions
  results <- eventReactive(list(input$predict, input$ex1, input$ex2, input$ex3), {
    if (nchar(input$user_input) > 0) {
      start <- Sys.time()
      preds <- predict_with_chaining(input$user_input, 3)
      time_ms <- as.numeric(difftime(Sys.time(), start, units = "secs")) * 1000
      list(predictions = preds, time = time_ms)
    }
  })
  
  output$output <- renderText({
    res <- results()
    if (is.null(res) || length(res$predictions) == 0) {
      "Type a phrase and click predict, or try an example."
    } else {
      paste(1:length(res$predictions), ". ", res$predictions, collapse = "\n")
    }
  })
  
  output$stats <- renderUI({
    res <- results()
    if (!is.null(res) && length(res$predictions) > 0) {
      tags$div(
        style = "color: #7f8c8d; font-size: 0.9em; margin-top: 10px;",
        sprintf("⚡ %.1f ms | 💾 %.1f MB model", res$time, MODEL_SIZE_MB)
      )
    }
  })
}

shinyApp(ui, server)