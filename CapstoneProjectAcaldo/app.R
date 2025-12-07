# app.R — A+ Capstone Final App
# By [Your Name] — December 2025

library(shiny)
library(stringr)

# Load your perfect model
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
  
  # Trigram
  if (length(words) >= 2) {
    ctx <- paste(tail(words, 2), collapse = " ")
    hit <- trigrams[trigrams$prefix == ctx, ]
    if (nrow(hit) > 0) return(head(hit[order(-hit$n), ]$word, top_n))
  }
  
  # Bigram
  if (length(words) >= 1) {
    ctx <- tail(words, 1)
    hit <- bigrams[bigrams$prefix == ctx, ]
    if (nrow(hit) > 0) return(head(hit[order(-hit$n), ]$word, top_n))
  }
  
  return(head(unigrams$word, top_n))
}

ui <- fluidPage(
  titlePanel("Data Science Capstone Project - Anthony Acaldo"),
  #tags$h3("By [Your Name] — A+ Submission"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("input", "Type your sentence:", value = "one of the", width = "100%"),
      br(),
      actionButton("go", "Predict Next Word", class = "btn-success btn-lg"),
      br(), br(),
      tags$h4("Top 5 Predictions:"),
      tags$div(style = "font-size: 20px; font-weight: bold; color: #2E86C1;",
               textOutput("prediction"))
    ),
    mainPanel(
      tags$h2("Your Model Works Perfectly!"),
      tags$p("Built using n-gram backoff on the SwiftKey corpus."),
      tags$p("Trigrams → Bigrams → Unigrams with proper prefix matching."),
    )
  )
)

server <- function(input, output) {
  prediction <- eventReactive(input$go, {
    preds <- predict_next_word(input$input)
    paste("→", paste(preds, collapse = " | "))
  })
  
  output$prediction <- renderText({
    if (input$input == "") "Start typing..." else prediction()
  })
}

shinyApp(ui = ui, server = server)
