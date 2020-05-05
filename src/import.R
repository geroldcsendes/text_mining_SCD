library(tidyverse)
library(tesseract)
library(magick)
library(pdftools)
library(stringr)
library(tidytext)

source("src/helpers.R")

socialist_lib <- 'docs/socialist/'
free_lib <- 'docs/free_market/'
eng <- tesseract("eng") # initialize english enginge
#pierson_book_name <- 'Collectivist_Economic_Planning2.pdf'

# read in pearson article
pierson_pages <- seq(46, 90)
pierson_df <- readTransformCollectivist(article_pages = pierson_pages, author = "pierson")
saveRDS(pierson_df, 'data/pierson.RDS')

# read in mises article
mises_pages <- seq(92, 135)
mises_df <- readTransformCollectivist(article_pages = mises_pages, author = "mises")
saveRDS(mises_df, 'data/mises.RDS')

# read in Hayek Nature and History..
hayek_nature_pages <- seq(6, 45)
hayek_nature_df <- readTransformCollectivist(article_pages = hayek_nature_pages, author = "hayek_nature")
saveRDS(hayek_nature_df, 'data/hayek_nature.RDS')

# read in Hayek Present State of the debate
hayek_present_state_pages <- seq(206, 248)
hayek_present_df <- readTransformCollectivist(article_pages = hayek_present_state_pages, author = "hayek_present_state")
saveRDS(hayek_present_df, 'data/hayek_present_state.RDS')

# basic stats
text_df <- dplyr::data_frame(page = seq(1:length(pierson_text)), text = pierson_text)
View(text_df)

text_df %>%
  unnest_tokens(word, text, to_lower = TRUE) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head(30)

text_df %>%
  count(word, sort = TRUE) 


