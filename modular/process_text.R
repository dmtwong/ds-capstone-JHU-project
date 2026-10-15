gc()
rm(list = ls())

library(quanteda)
library(tidytext)
# library(ngram)
library(tidyverse)
library(data.table)
library(tictoc)
library(DBI)
library(RPostgres)
library(dotenv)

setwd('data\\en_US')

badWords <- dget('swear_words.txt')
badWords <- paste("\\b", badWords, "\\b", sep="", collapse = "|")

?tic

process_text <- function(file_path, n_level = 2, min_count = 2){ # , top_k = 5) {
  print(str_c("Starting ", n_level, "-grams..."))
  # corpus_tbl <- tibble(file = file_path) %>%
  #   mutate(text = map(file, ~ read_lines(.x, progress = FALSE))) %>%
  #   unnest(text) %>%
  #   filter(text != "") %>%
  #   filter(!str_detect(text, badWords)) # Filter bad words
  tic()
  raw <- unlist(map(file_path, ~ read_lines(.x, progress = FALSE)))
  toc()
  # tokenize each text as named list of word subject to removal rules that set to be T
  tic()
  tk <- tokens(raw, remove_punct = TRUE,
                    remove_symbols = TRUE,
                    remove_numbers = TRUE,
                    remove_url = TRUE,
                    remove_separators = TRUE) %>% 
    tokens_remove(pattern = badWords) %>% 
    tokens_tolower()
  toc()
  # glue it back as one in a tibble where each row repr 1 text and the column name 
  # is text and thenshaping
  # corp <- tibble(text = sapply(tk, paste, collapse = " ")) 
  # print(nrow(corp)) # 2360148
  # corp <- filter(corp, text != "")
  # print(nrow(corp)) # 2360089
  # corp <- filter(corp, !str_detect(text, badWords))
  tic()
  corp <- tibble(text = sapply(tk, paste, collapse = " ")) %>% 
    filter(text != "") # ensure no empty corpus
  toc()
  
  print(str_c("Extracting ", n_level, "-grams...")) # a tibble of ngram (1 row, n ngram)
  tic()
  ng <- corp %>%
    unnest_tokens(output = ngram, input = text, token = "ngrams", n = n_level) %>%
    drop_na(ngram)
  toc()
  
  # create a tibble of 4 columns with count as freq
  print(str_c("Filtering ", n_level, "-grams..."))
  
  strict_eng <- "^[a-z]+('[a-z]+)?$" # strict regex filter that drops non-ASCII / non-English characters.
  pattern <- "^(.*)\\s+([^\\s]+)$" # split the last set of white space characters into 2 parts.
  tic()
  clean_df <- ng %>% count(ngram, name = "freq") %>% filter(freq >= min_count) %>%
    mutate(words_list = str_split(ngram, "\\s+")) %>%
    filter(map_lgl(words_list, ~ all(str_detect(.x, strict_eng)))) %>%
    select(-words_list) %>% 
    mutate(
      prefix = str_replace(ngram, pattern, "\\1") %>% str_replace_all("\\s+", "_"),
      prediction = str_replace(ngram, pattern, "\\2") #,
      # n = as.integer(n_level)
    ) %>% select(prefix, prediction, freq) #n, prefix, prediction, count)
  toc()
  # Turn it into data.table
  print(str_c("Indexing for ", n_level, "-grams..."))
  tic()
  dt <- as.data.table(clean_df)
  dt <- dt[order(-freq), by = .(prefix)] # , head(.SD, top_k), by = .(prefix)]
  # setkey(dt, prefix)
  toc()
  return(dt)
}

fit_schema <- function(ngram_dt, n_level = 2) {
  # tic()
  # setnames(dt, "count", "freq")
  if (n_level == 2) {
    db_df <- ngram_dt %>%
      mutate(word_1 = prefix, word_2 = prediction) %>%
      select(word_1, word_2, freq)
  } else if (n_level == 3) {
    db_df <- ngram_dt %>% 
      separate(prefix, into = c("word_1", "word_2"), sep = "_") %>%
      mutate(word_3 = prediction) %>% select(word_1, word_2, word_3, freq)
  } else if (n_level == 4) {
    db_df <- ngram_dt %>%
      separate(prefix, into = c("word_1", "word_2", "word_3"), sep = "_") %>%
      mutate(word_4 = prediction) %>%
      select(word_1, word_2, word_3, word_4, freq)
  }
  # toc()
  return(db_df)
}

write_db <- function(ngram_df, n_level){
  if (n_level == 2) {
    table_name <- "bigrams"
  } else if (n_level == 3){
    table_name <- "trigrams"
  } else if (n_level == 4){
    table_name <- "quadgrams"
  }
  
  con <- dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("DB_HOST"),
    port     = as.integer(Sys.getenv("DB_PORT")),
    dbname   = Sys.getenv("DB_NAME"),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  
  tic()
  dbWriteTable(con, table_name, ngram_df, append = TRUE, row.names = FALSE)
  toc()
  
  dbDisconnect(con)
}

corpus_files <- c("en_US.twitter.txt", "en_US.blogs.txt", "en_US.news.txt")
dt_2gram <- process_text(corpus_files, n_level = 2, min_count = 2) # 1299.14 sec elapsed
# rm("dt_2gram"); rm("dt_2gram_fit")
dt_3gram <- process_text(corpus_files, n_level = 3, min_count = 2) # 1299.14 sec elapsed
dt_4gram <- process_text(corpus_files, n_level = 4, min_count = 2) #

str(dt_2gram)
dt_2gram_fit <- fit_schema(dt_2gram)
str(dt_2gram_fit)
write_db(dt_2gram_fit, 2)
rm("dt_2gram"); rm("dt_2gram_fit")

str(dt_3gram)
dt_3gram_fit <- fit_schema(dt_3gram, 3)
str(dt_3gram_fit)
write_db(dt_3gram_fit, 3)
rm("dt_3gram"); rm("dt_3gram_fit")

str(dt_3gram)
dt_3gram_fit <- fit_schema(dt_3gram, 3)
str(dt_3gram_fit)
write_db(dt_3gram_fit, 3)
rm("dt_3gram"); rm("dt_3gram_fit")

str(dt_4gram)
dt_4gram_fit <- fit_schema(dt_4gram, 4)
str(dt_4gram_fit)
write_db(dt_4gram_fit, 4)
rm("dt_4gram"); rm("dt_4gram_fit")

# corpus_files <- c("en_US.twitter_tilshutthe.txt")

# dt_2gram <- process_text(corpus_files, n_level = 2, min_count = 1) # , top_k = 3)

# str(dt_2gram)
# dt_2gram["text172"] # Tokens consisting of 1 document. text172 : [1] "yeah"   "l"      "could"  "be"     "the"    "bigger" "person" "or"     "you"    "could"  "just"   "shut"  


# }
# dt_2gram <- process_text(corpus_files, n_level = 2, min_count = 2, top_k = 3)
# nrow(dt_2gram) # 2285287, fruther reduce after text including swear words removed  
# # sum(sapply(dt_2gram, function(x) str_detect(x, badWords)))
# 
# 
#   
#   
#   
# }
# # tmp <- tibble(file = corpus_files)
# # tmp
# 
# predict_next_word <- function(dt_model, input_text, top_n = 5) {
#   words <- input_text %>% str_to_lower() %>% str_extract_all("\\b[a-z']+\\b") %>% pluck(1)
#   len <- length(words)
#   search_prefixes <- 4:2
#   
#   temp <- list()
#   for (n_val in search_prefixes) {
#     len_prefix <- n_val - 1
#     if (len > len_prefix) {
#       prefix_key <- str_c(tail(words, len_prefix), collapse = "_")
#       if (!is.null(prefix_key)) {
#         res <- dt_model[.(prefix_key)][n == n_val]
#         res <- res[!is.na(prediction)]
#       if (nrow(res) > 0) {
#         temp[[length(temp) + 1 ]] <- res
#         # print(temp)
#         }
#         }
#     } # else {
#       # search_prefixes[[as.character(n_val)]] <- NULL
#     # }
#   # return(temp)
#   }
#   if (length(temp) >  0){
#     # print('1')
#     temp <- rbindlist(temp)
#     result <- temp[order(-n, count)] %>% unique(by = "prediction") %>%
#       head(top_n)
#     return(head(result, top_n))
#   } 
#   print('no records found')
# }
# 
# corpus_files <- c("en_US.twitter.txt", "en_US.blogs.txt", "en_US.news.txt")
# # corpus_files <- c("en_US.twitter.txt")
# # process_text <- function(file_path, n_level, min_count = 2, top_k = 5) {
# # tic("Total Processing Time")
# dt_2gram <- process_text(corpus_files, n_level = 2, min_count = 2) #, top_k = 5)  
# 
# model_dt <- rbindlist(list(dt_2gram))
# dt_3gram <- process_text(corpus_files, n_level = 3, min_count = 2, top_k = 5)  
# model_dt <- rbindlist(list(dt_3gram))
# dt_4gram <- process_text(corpus_files, n_level = 4, min_count = 2, top_k = 5)  
# model_dt <- rbindlist(list(dt_4gram))
# 
# dt_2gram <- process_text(corpus_files, n_level = 2, min_count = 2, top_k = 5)  
# plot(model_dt$count)
# library(ggplot2)
# 
# ggplot(model_dt, aes(x = reorder(prediction, count), y = count, fill = factor(n))) +
#   geom_col(show.legend = TRUE) +
#   coord_flip() + # Flips to a horizontal bar chart for easy reading of words
#   labs(
#     title = "Top Word Predictions",
#     subtitle = "Aggregated across N-gram models",
#     x = "Predicted Word",
#     y = "freq / Score",
#     fill = "N-gram Level"
#   ) +
#   theme_minimal() +
#   scale_fill_viridis_d(option = "plasma") # Professional color palette
# 
# library(ggplot2)
# 
# # Assuming 'model_dt' is your data.table containing the 'count' column
# ggplot(model_dt, aes(x = count)) +
#   geom_histogram(bins = 50, fill = "steelblue", color = "white") +
#   scale_x_log10() + # Essential for text corpora due to power-law distribution
#   labs(
#     title = "Distribution of N-Gram Frequencies",
#     subtitle = "Log-scaled x-axis to account for Zipf's law",
#     x = "Count (Log Scale)",
#     y = "freq of Occurrence"
#   ) +
#   theme_minimal()
# 
# custom_breaks <- c(2:10, 50)
# 
# library(scales) # Required for nice axis label formatting
# ggplot(model_dt, aes(x = count)) +
#   # after_stat(count / sum(count) * 100) calculates the exact percentage per bin
#   geom_histogram(aes(y = after_stat(count / sum(count)) * 100), 
#                  bins = 60, 
#                  color = "white", 
#                  linewidth = 0.2) +
#   scale_x_log10(
#     breaks = custom_breaks,
#     labels = scales::comma_format()
#   ) +
#   scale_y_continuous(
#     labels = scales::percent_format(scale = 1) # Formats Y-axis labels with a '%' sign
#   ) +
#   labs(
#     title = "Percentage Distribution of N-Gram Frequencies",
#     subtitle = "Proportion of N-grams falling within each freq interval",
#     x = "N-Gram Count Interval (Log Scale)",
#     y = "Percentage of Total N-Grams (%)"
#   ) +
#   theme_minimal(base_size = 8) +
#   theme(
#     panel.grid.minor = element_blank(),
#     axis.text.x = element_text(angle = 90, hjust = 1)
#   )
# setkey(model_dt, prefix)
# 
# dt_2gram <- process_text(corpus_files, n_level = 2, min_count = 3, top_k = 5)
# dt_3gram <- process_text(corpus_files, n_level = 3, min_count = 2, top_k = 5)
# dt_4gram <- process_text(corpus_files, n_level = 4, min_count = 2, top_k = 5)
# # model_dt <- rbindlist(list(dt_2gram))
# model_dt <- rbindlist(list(dt_4gram, dt_3gram, dt_2gram))
# setkey(model_dt, prefix)
# # str(model_dt)
# str(model_dt)
# filter(model_dt, n == 4)
# filter(model_dt, prefix == "at_the")
# filter(model_dt, prefix == "date_at_the")
# filter(model_dt, prefix == "date_at")
# filter(model_dt, prefix == "romantic")
# filter(model_dt, prefix == "romantic_date")
# filter(model_dt, prefix == "a_romantic_date")
# 
# predict_next_word(model_dt, "Go on a romantic date at the", 15)
# head(model_dt$prefix)
# tail(model_dt$prefix)
# str(model_dt)
# model_dt$prefix
# 
# # predict_next_word(model_dt, "Very early observations on the Bills game: Offense still struggling but the", 10) == "defense?"
# # predict_next_word(model_dt, "Go on a romantic date at the", 10) == "beach?"
# # predict_next_word(model_dt, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", 10" == fingers?"
# # predict_next_word(model_dt, "Be grateful for the good times and keep the faith during the", 10) == "bad?"
# # predict_next_word(model_dt, "If this isn't the cutest thing you've ever seen, then you must be", 10) == "insensitive?"
# 
# "beer" %in% predict_next_word(model_dt, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", 10)
# "world" %in% predict_next_word(model_dt, "You're the reason why I smile everyday. Can you follow me please? It would mean the", 10)$prediction
# "happiest" %in% predict_next_word(model_dt, "Hey sunshine, can you follow me and make me the", 10)
# "defensive" %in% predict_next_word(model_dt, "Very early observations on the Bills game: Offense still struggling but the", 10)$prediction
# "beach" %in% predict_next_word(model_dt, "Go on a romantic date at the", 10)$prediction
# "way" %in% predict_next_word(model_dt, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", 10)$prediction
# "time" %in% predict_next_word(model_dt, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", 10)$prediction
# "fingers" %in% predict_next_word(model_dt, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", 10)$prediction
# "bad" %in% predict_next_word(model_dt, "Be grateful for the good times and keep the faith during the", 10)$prediction
# predict_next_word(model_dt, "If this isn't the cutest thing you've ever seen, then you must be", 10)
# 
# predict_next_word(model_dt, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
# 
# predict_next_word(model_dt, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", 10)
# predict_next_word(model_dt, "You're the reason why I smile everyday. Can you follow me please? It would mean the", 10)
# predict_next_word(model_dt, "Hey sunshine, can you follow me and make me the", 10)
# predict_next_word(model_dt, "Very early observations on the Bills game: Offense still struggling but the", 10)
# predict_next_word(model_dt, "Go on a romantic date at the", 15)
# predict_next_word(model_dt, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", 10)
# predict_next_word(model_dt, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", 10)
# predict_next_word(model_dt, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", 10)
# # predict_next_word(model_dt, "Be grateful for the good times and keep the faith during the", 15)
# 

# process_text <- function(file_path, n_level, min_count = 2, top_k = 5) {
#   print(str_c("Starting ", n_level, "-grams..."))
#   corpus_tbl <- tibble(file = file_path) %>%
#     mutate(text = map(file, ~ read_lines(.x, progress = FALSE))) %>%
#     unnest(text) %>%
#     filter(text != "") %>%
#     filter(!str_detect(text, badWords))
# }
# abc <- process_text(corpus_files, n_level = 2, min_count = 2, top_k = 3)
# abc
# ix <- str_detect(abc$text, badWords)
# sum(ix) / nrow(abc)
# filter(abc, !ix)