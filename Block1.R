# ============ BLOCK 1 — CREATE PERFECT TEXT FILES (KEEP STOPWORDS!) ============
cat("Creating perfect fixed text files...\n")

fix_text <- function(input, output) {
  lines <- readLines(input, warn = FALSE, encoding = "UTF-8")
  cleaned <- lines %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    gsub("[^a-z0-9' ]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
  writeLines(cleaned, output)
  cat("Saved:", output, "\n")
}

fix_text("en_US.blogs.txt",   "blogs_fixed.txt")
fix_text("en_US.news.txt",    "news_fixed.txt")
fix_text("en_US.twitter.txt", "twitter_fixed.txt")

cat("Perfect text files created!\n")

