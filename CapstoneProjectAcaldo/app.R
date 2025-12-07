# app.R — Anthony Acaldo — A+ Capstone Final App
library(shiny)
library(shinythemes)
library(stringr)

# Load model
trigrams <- readRDS("trigram.rds")
bigrams  <- readRDS("bigram.rds")
unigrams <- readRDS("unigram.rds")

predict_next_word <- function(text, top_n = 5) {
  words <- tolower(text) %>%
    gsub("[^a-z' ]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws() %>%
    strsplit(" ") %>% unlist()
  words <- words[words != ""]
  
  if (length(words) == 0) return(rep("the", top_n))
  
  if (length(words) >= 2) {
    ctx <- paste(tail(words, 2), collapse = " ")
    hit <- trigrams[trigrams$prefix == ctx, ]
    if (nrow(hit) > 0) return(head(hit[order(-hit$n), ]$word, top_n))
  }
  
  if (length(words) >= 1) {
    ctx <- tail(words, 1)
    hit <- bigrams[bigrams$prefix == ctx, ]
    if (nrow(hit) > 0) return(head(hit[order(-hit$n), ]$word, top_n))
  }
  
  return(head(unigrams$word, top_n))
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel(
    tags$div(
      tags$h1("Next Word Predictor", style = "color:#2c3e50; font-weight:bold;"),
      tags$h4("Anthony Acaldo — Data Science Capstone", style = "color:#7f8c8d;")
    )
  ),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 5,
      textInput("input", 
                tags$span("Type your sentence:", style = "font-size:18px; font-weight:bold;"), 
                value = "one of the", width = "100%"),
      
      # RADIO BUTTONS — RIGHT UNDER TEXT INPUT
      radioButtons("mode", "Prediction Mode:",
                   choices = list("Next 5 Words" = 5, "Single Best Word" = 1),
                   selected = 5, inline = TRUE),
      
      br(),
      fluidRow(
        column(6, actionButton("go", "Predict", class = "btn-primary btn-lg", width = "100%")),
        column(6, actionButton("clear", "Clear", class = "btn-danger btn-lg", width = "100%"))
      ),
      br(), br(),
      
      # Dynamic header
      uiOutput("prediction_header"),
      br(),
      uiOutput("prediction_buttons")
    ),
    
    mainPanel(
      width = 7,
      tags$div(
        style = "background:#f8f9fa; padding:40px; border-radius:15px; text-align:center;",
        tags$h2("A+ Capstone Project", style = "color:#2c3e50;"),
        tags$p("Trigram → Bigram → Unigram backoff", style = "font-size:18px; color:#555;"),
        tags$p("Trained on SwiftKey corpus", style = "font-size:16px; color:#777;"),
        br(),
        tags$div(style = "font-size:60px;", "Success"),
        br(),
        tags$p("You built a real predictor.", style = "font-size:18px; font-style:italic;")
      )
    )
  ),
  
  tags$footer(
    tags$p("© Anthony Acaldo — Coursera Data Science Capstone 2025", 
           style = "text-align:center; color:#95a5a6; margin-top:50px;")
  )
)

server <- function(input, output, session) {
  
  predictions <- eventReactive(input$go, {
    predict_next_word(input$input, as.numeric(input$mode))
  })
  
  # Dynamic header
  output$prediction_header <- renderUI({
    if (input$mode == 1) {
      tags$h3("Best Prediction", style = "color:#2c3e50;")
    } else {
      tags$h3("Top 5 Predictions", style = "color:#2c3e50;")
    }
  })
  
  output$prediction_buttons <- renderUI({
    preds <- if (input$input == "") rep("the", as.numeric(input$mode)) else predictions()
    
    btns <- lapply(seq_along(preds), function(i) {
      word <- preds[i]
      is_na <- is.na(word) || word == "" || word == "NA"
      
      actionButton(
        inputId = paste0("btn", i),
        label = tags$span(word, style = "font-size:22px; font-weight:bold;"),
        class = if (is_na) "btn-default" else "btn-success",
        style = paste0("width:100%; margin:8px 0; height:70px;",
                       if (is_na) "opacity:0.3; cursor:not-allowed;" else ""),
        disabled = if (is_na) "" else NULL,
        onclick = if (!is_na) paste0("Shiny.setInputValue('selected_word', '", word, "', {priority: 'event'});") else NULL
      )
    })
    do.call(tagList, btns)
  })
  
  # Auto-append
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
  
  # Clear
  observeEvent(input$clear, {
    updateTextInput(session, "input", value = "")
  })
}

shinyApp(ui = ui, server = server)
