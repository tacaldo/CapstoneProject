# app.R — Anthony Acaldo — A+ Capstone Final App
library(shiny)
library(shinythemes)
library(stringr)

# Load model
trigrams <- readRDS("trigram.rds")
bigrams  <- readRDS("bigram.rds")
unigrams <- readRDS("unigram.rds")

predict_next_word <- function(text, top_n = 5) {
  start_time <- Sys.time()
  words <- tolower(text) %>%
    gsub("[^a-z' ]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws() %>%
    strsplit(" ") %>% unlist()
  words <- words[words != ""]
  
  result <- list(
    words = rep("the", top_n),
    source = "No input",
    confidence = "None",
    time_ms = 0
  )
  
  if (length(words) == 0) {
    result$time_ms <- round((Sys.time() - start_time) * 1000, 1)
    return(result)
  }
  
  # Try trigram
  if (length(words) >= 2) {
    ctx <- paste(tail(words, 2), collapse = " ")
    hit <- trigrams[trigrams$prefix == ctx, ]
    if (nrow(hit) > 0) {
      hit <- hit[order(-hit$n), ]
      result$words <- head(hit$word, top_n)
      result$source <- paste("Trigram match")
      result$confidence <- "High"
      result$time_ms <- round((Sys.time() - start_time) * 1000, 1)
      return(result)
    }
  }
  
  # Try bigram
  if (length(words) >= 1) {
    ctx <- tail(words, 1)
    hit <- bigrams[bigrams$prefix == ctx, ]
    if (nrow(hit) > 0) {
      hit <- hit[order(-hit$n), ]
      result$words <- head(hit$word, top_n)
      result$source <- "Bigram match"
      result$confidence <- "Medium"
      result$time_ms <- round((Sys.time() - start_time) * 1000, 1)
      return(result)
    }
  }
  
  # Unigram fallback
  result$words <- head(unigrams$word, top_n)
  result$source <- "Unigram fallback"
  result$confidence <- "Low"
  result$time_ms <- round((Sys.time() - start_time) * 1000, 1)
  return(result)
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Next Word Predictor — Anthony Acaldo"),
  
  sidebarLayout(
    sidebarPanel(
      width = 5,
      textInput("input", "Type your sentence:", value = "", width = "100%"),
      radioButtons("mode", "Show:", 
                   choices = c("Top 5 Predictions" = 5, "Best 1 Prediction" = 1), 
                   selected = 5, inline = TRUE),
      fluidRow(
        column(6, actionButton("go", "Predict", class = "btn-primary btn-lg", width = "100%")),
        column(6, actionButton("clear", "Clear", class = "btn-danger btn-lg", width = "100%"))
      ),
      br(), br(),
      tags$h4("Predictions"),
      br(),
      uiOutput("prediction_buttons")
    ),
    
    mainPanel(
      width = 7,
      tags$div(
        style = "background:#f8f9fa; padding:30px; border-radius:12px; font-family:Arial;",
        tags$h3("Model Insights", style = "color:#2c3e50; margin-top:0;"),
        br(),
        uiOutput("metrics")
      )
    )
  )
)

server <- function(input, output, session) {
  
  prediction_result <- eventReactive(input$go, {
    predict_next_word(input$input, as.numeric(input$mode))
  })
  
  # Always show metrics (even on empty/clear)
  current_result <- reactive({
    if (input$input == "" && input$go == 0) {
      list(confidence = "None", source = "No input", time_ms = 0)
    } else {
      prediction_result()
    }
  })
  
  output$metrics <- renderUI({
    res <- current_result()
    
    conf_color <- switch(res$confidence,
                         "High" = "#27ae60",
                         "Medium" = "#f39c12", 
                         "Low" = "#e74c3c",
                         "None" = "#95a5a6")
    
    tags$div(
      tags$div(
        style = "font-size:24px; font-weight:bold;",
        "Confidence: ",
        tags$span(style = paste0("color:", conf_color, ";"), res$confidence),
        tags$div(
          style = paste0("background:", conf_color, "; height:12px; width:",
                         ifelse(res$confidence=="High", "100%",
                                ifelse(res$confidence=="Medium", "65%",
                                       ifelse(res$confidence=="Low", "35%", "0%"))), 
                         "; border-radius:6px; margin-top:8px;")
        )
      ),
      br(),
      tags$div(style = "font-size:18px;", "Source: ", res$source),
      br(),
      tags$div(style = "font-size:18px;", "Speed: ", res$time_ms, " ms")
    )
  })
  
  output$prediction_buttons <- renderUI({
    preds <- if (input$input == "" && input$go == 0) 
      rep("the", as.numeric(input$mode)) 
    else prediction_result()$words
    
    btns <- lapply(seq_along(preds), function(i) {
      word <- preds[i]
      is_bad <- is.na(word) || word == "" || word == "NA"
      
      actionButton(
        inputId = paste0("btn", i),
        label = tags$span(word, style = "font-size:22px; font-weight:bold;"),
        class = if (is_bad) "btn-default" else "btn-success",
        style = paste0("width:100%; margin:8px 0; height:70px;",
                       if (is_bad) "opacity:0.4; cursor:not-allowed;" else ""),
        disabled = if (is_bad) "" else NULL,
        onclick = if (!is_bad) paste0("Shiny.setInputValue('selected_word', '", word, "', {priority: 'event'});") else NULL
      )
    })
    do.call(tagList, btns)
  })
  
  observeEvent(input$selected_word, {
    if (!is.null(input$selected_word) && !is.na(input$selected_word) && input$selected_word != "") {
      current <- trimws(input$input)
      new_text <- if (current == "" || endsWith(current, " ")) {
        paste(current, input$selected_word)
      } else {
        paste(current, input$selected_word)
      }
      updateTextInput(session, "input", value = new_text)
    }
  })
  
  observeEvent(input$clear, {
    updateTextInput(session, "input", value = "")
  })
}

shinyApp(ui = ui, server = server)
