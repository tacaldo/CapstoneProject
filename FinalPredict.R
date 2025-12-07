# FINAL PREDICTOR
predict_next_word <- function(text, top_n = 5) {
  words <- tolower(text) %>% 
    gsub("[^a-z' ]", " ", .) %>% 
    gsub("\\s+", " ", .) %>% 
    trimws() %>% 
    strsplit(" ") %>% 
    unlist()
  words <- words[words != ""]
  
  if (length(words) == 0) return("the")
  
  for (k in min(3, length(words)):1) {
    ctx <- paste(tail(words, k), collapse = " ")
    model <- readRDS(paste0("ngram_", k, ".rds"))
    hit <- model[model$prefix == ctx, ]
    if (nrow(hit) > 0) {
      hit <- hit[order(-hit$n), ]
      return(head(hit$word, top_n))
    }
  }
  return("the")
}

# TEST — THESE WORK
predict_next_word("one of the")
predict_next_word("i don't")
predict_next_word("in the")
predict_next_word("thanks for the")
predict_next_word("happy birthday to")