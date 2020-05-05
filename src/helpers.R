library(tidyverse)
library(tesseract)
library(magick)
library(pdftools)
library(stringr)

####
## Import helpers
####

readTransformCollectivist <- function(article_pages, author) {
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