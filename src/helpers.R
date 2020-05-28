library(tidyverse)
library(tesseract)
library(magick)
library(pdftools)
library(stringr)

####
## Import helpers
####

readTransformCollectivist <- function(article_pages, author) {
  # TODO include png already, unest in the end
  free_lib <- 'docs/free_market/'
  eng <- tesseract("eng") # initialize english enginge
  book_name <- 'Collectivist_Economic_Planning2.pdf'
  article_filename <-  paste0(free_lib, 'png/', author, '_')

  page_vector <- c()
  for (page in article_pages) {
    filename_png <- paste0(article_filename, page, ".png")
    page_png <- pdf_convert(paste0(free_lib, book_name), pages = page, 
                               dpi = 600, filenames = filename_png)
    page <- ocr(page_png, eng = eng)
    # delete first line
    page <- str_replace(page, "^(.*\n){1}", "")
    # delete last line
    page <-str_replace(page, "\\n.+\\n$", "")
    #fix syllabicifcation
    page <- fix_syllabification(page)
    # remove newlines
    page <- str_replace_all(page, "\\n", " ")
    # everything to lower
    page <- str_to_lower(page, locale = "en")
    page_vector <- c(page_vector, page)
  }
  
  # write character vector containing one page per element into df
  text_df <- dplyr::data_frame(page = seq(1:length(page_vector)), text = page_vector)
  # only keep digits if year is indicated
  # text_df <- text_df %>% 
  #   filter(
  #   !str_detect(word, "[a-z']+[:digit:]+" ), 
  #   !str_detect(word, "[:digit:]+[a-z']+"),
  #   str_detect(word, "[a-z']+|[:digit:]{4}"))
  
  return(text_df)
}

readTransformNeurath <- function(article_pages, author, png_already = FALSE) {
  socialist_lib <- 'docs/socialist/'
  eng <- tesseract("eng") # initialize english enginge
  book_name <- 'empiricism_and_sociology.pdf'
  article_filename <-  paste0(socialist_lib, 'png/', author, '_')
  
  page_vector <- c()
  for (page in article_pages) {
    
    filename_png <- paste0(article_filename, page, ".png")
    if (png_already == FALSE) {
      page_png <- pdf_convert(paste0(socialist_lib, book_name), pages = page, 
                              dpi = 600, filenames = filename_png)
    } else {
      page_png <- filename_png
    }
    
    page <- ocr(page_png, eng = eng)
    # delete first line
    page <- str_replace(page, "^(.*\n){1}", "")
    #fix syllabicifcation
    page <- fix_syllabification(page)
    # remove newlines
    page <- str_replace_all(page, "\\n", " ")
    # everything to lower
    page <- str_to_lower(page, locale = "en")
    page_vector <- c(page_vector, page)
  }
  
  # write character vector containing one page per element into df
  text_df <- dplyr::data_frame(page = seq(1:length(page_vector)), text = page_vector)
  
  # text_df <- text_df %>%
  #   unnest_tokens(word, text, to_lower = TRUE) %>% 
  #   anti_join(stop_words)  
  # only keep digits if year is indicated
  
  # text_df <- text_df %>%
  #   filter(
  #     !str_detect(text, "[a-z']+[:digit:]+" ),
  #     !str_detect(text, "[:digit:]+[a-z']+"),
  #     str_detect(text, "[a-z']+|[:digit:]{4}"))

  return(text_df)
}

readTransformEconomicReview <- function(article_pages, author, book_name, png_already = FALSE) {
  socialist_lib <- 'docs/socialist/'
  eng <- tesseract("eng") # initialize english enginge
  
  article_filename <-  paste0(socialist_lib, 'png/', author, '_')
  
  page_vector <- c()
  for (page in article_pages) {
    
    filename_png <- paste0(article_filename, page, ".png")
    if (png_already == FALSE) {
      page_png <- pdf_convert(paste0(socialist_lib, book_name), pages = page, 
                              dpi = 600, filenames = filename_png)
    } else {
      page_png <- filename_png
    }
    
    # read in png to string
    page <- ocr(page_png, eng = eng)
    # delete first line
    page <- str_replace(page, "^(.*\n){1}", "")
    #fix syllabicifcation
    page <- fix_syllabification(page)
    # remove last two lines (indicating jstor source)
    page <- str_replace(page, "\\n.+\\n.+\\n$", "")
    # remove newlines
    page <- str_replace_all(page, "\\n", " ")
    # everything to lower
    page <- str_to_lower(page, locale = "en")
    page_vector <- c(page_vector, page)
  }
  
  # write character vector containing one page per element into df
  text_df <- dplyr::data_frame(page = seq(1:length(page_vector)), text = page_vector)
  
  # text_df <- text_df %>%
  #   unnest_tokens(word, text, to_lower = TRUE) %>%
  #   anti_join(stop_words)
  # only keep digits if year is indicated
  
  # text_df <- text_df %>%
  #   filter(
  #     !str_detect(text, "[a-z']+[:digit:]+" ),
  #     !str_detect(text, "[:digit:]+[a-z']+"),
  #     str_detect(text, "[a-z']+|[:digit:]{4}"))
  
  return(text_df)
}


# concat syllabication 
fix_syllabification <- function(text) {
  # use only first element in list since every page is a big chunk of string
  words_to_fix <- str_extract_all(text, "[a-zA-Z]+-\\n[a-zA-Z]+")[[1]]
  print(words_to_fix)
  # fix words
  words_corrected <- str_replace_all(words_to_fix, c("-\n" = ""))
  print(words_corrected)
  # correct words in text
  counter <- 1
  for (element  in words_to_fix) {
    text <- gsub(element, words_corrected[counter], text)
    counter <- counter + 1
  }
  return(text)
}


####
## Analysis helpers
####

proportionDF <- function(df) {
  
    df <- 
      df %>%
      unnest_tokens(word, text, to_lower = TRUE) %>% 
      anti_join(stop_words) %>% 
      count(word, sort = TRUE) %>% 
      mutate(proportion = n / sum(n)) %>% 
      arrange(-proportion) %>% 
      mutate(rank = row_number())

  return(df)
}

# different because importer already unnests pages into tables by page
# TODO fix this: fix proportionDF so it works same as proportionDF_socialist
proportionDF_socialist <- function(df) {
  df <- 
    df %>%
    count(word, sort = TRUE) %>% 
    mutate(proportion = n / sum(n)) %>% 
    arrange(-proportion) %>% 
    mutate(rank = row_number())
  
  return(df)
}

no_mention_words <- function(author_name, df) {
  # TODO Hayek harcdoded
  res <- bind_rows(
    df %>% 
      mutate(na_sum = is.na(`Hayek: debate`) + is.na(proportion)) %>% 
      filter(author == author_name, na_sum == 1) %>% 
      arrange(-`Hayek: debate`) %>% 
      head(10),
    df %>% 
      mutate(na_sum = is.na(`Hayek: debate`) + is.na(proportion)) %>% 
      filter(author == author_name, na_sum == 1) %>% 
      arrange(-`proportion`) %>% 
      head(10))
  
  return(res)
}

# sentiment help func
# find words up to 20 words before/after trigger word
sliding_range <- function(num) {
  res <- (num-20) : (num + 20)
  res <- res[!res == num] # remove trigger word
  res <- res[res > 0] # remove negative indexes
  return(res)
}

# sentiment help func
# find how many word range for each trigger 
id_length <- function(idx_vec) {
  return(length(idx_vec))
}

# sentiment help func
# generate id for sentiment_df 
gen_id <- function(id_length_vec) {
  
  id_res <- c()
  counter <- 1
  
  for (element in id_length_vec) {
    id_res <- c(id_res, rep(counter, element))
    
    counter <- counter + 1
  }
  
  return(id_res)
}

# return dataframe ready to be sentiment analysed with sentimentr
sentiment_df  <- function(df, author) {
  
  # define which words to track
  trigger_words <- c("social", "socialist", "socialism")
  
  # get words within range of 
  # unnest tokens but leave stopwords
  df <- df %>%
    unnest_tokens(word, text, to_lower = TRUE)
  
  # find trigger words
  social_rows <- df %>%
    mutate(row_idx = row_number()) %>% 
    filter(word %in% trigger_words) 
  
  social_rows <- social_rows$row_idx
  
  
  # get indexes in unnested df
  sliding_idx <- lapply(social_rows, sliding_range)
  
  # find how many triggers found
  trigger_cnt <- length(sliding_idx)
  
  # this is for when not 40 words returned for a trigger word
  trigger_range <- sapply(sliding_idx, id_length)
  sliding_id_vec <- gen_id(trigger_range)
  
  # unlist to get indices
  sliding_idx <- unlist(sliding_idx)
  
  df_sliding <- df[sliding_idx, ]
  df_sliding$trigger_id <- sliding_id_vec
  
  # wide format
  df_sliding <- df_sliding %>% 
    group_by(trigger_id) %>% 
    summarize(text = str_c(word, collapse = " ")) %>%
    ungroup()
  
  df_sliding$author <- c(author)
  
  return(df_sliding)
}



####
## Viz helpers
## credits: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
####
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

# flipped bar chart with pooled axis on most commom words
top10_pooled_plot <- function(df) {
  
  p1 <- df %>% filter(rank < 11) %>%  
          ggplot(aes(x = reorder(word, - proportion), y = proportion)) +
          geom_bar(stat = 'identity') +
          coord_flip() +
          facet_grid(~author) +
          scale_y_continuous(labels = scales::percent) 
  
  return(p1)
}

top10_plot <- function(df) {
  
  p1 <- df %>% filter(rank < 11) %>%  
          ggplot(aes(reorder_within(word, proportion, author), proportion)) +
          geom_col() +
          scale_x_reordered() +
          coord_flip() +
          facet_wrap(~ author, scales = "free_y") +
          labs(x = "word")
  
  return(p1)
  }


# look for authors name in texts
cross_refer <- function(df, author) {
  authors <- c("pierson", "mises", "hayek", "neurath", "lange", "lerner", "dickinson")
  res <- df %>% 
    filter(word %in% authors) %>% 
    select(word, proportion)
  res$author <- c(author)
  
  return(res)
}
