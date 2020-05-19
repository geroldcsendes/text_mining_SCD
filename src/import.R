library(tidyverse)
library(tesseract)
library(magick)
library(pdftools)
library(stringr)
library(tidytext)

source("src/helpers.R")

socialist_lib <- 'docs/socialist/'
free_lib <- 'docs/free_market/'


## free marketers
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


## socialists
# Neurath
neurath_pages <- seq(140,170)
neurath <- readTransformNeurath(article_pages = neurath_pages, author = 'neurath', png_already = TRUE)
saveRDS(neurath, 'data/neurath.RDS')

# Oskar Lange 1
lange_part1_pages <- seq(2,20)
lange_part1_bookname <- 'lange_on_the_economic_theory_of_socialist_part_one.pdf'
lange_part1 <- readTransformEconomicReview(article_pages = lange_part1_pages, author = 'lange_part1', 
                                   book_name =  lange_part1_bookname, png_already = TRUE )
saveRDS(lange_part1, 'data/lange1.RDS')

# Oskar Lange 2
lange_part2_pages <- seq(2,21)
lange_part2_bookname <- 'lange_on_the_economic_theory_of_socialist_part_two.pdf'
lange_part2 <- readTransformEconomicReview(article_pages = lange_part2_pages, author = 'lange_part2',
                                           book_name =  lange_part2_bookname , png_already = TRUE)
saveRDS(lange_part2, 'data/lange2.RDS')

# Lerner
lerner_pages <- seq(2,12)
lerner_bookname <- 'lerner_economic_theory_and_socialist_economy.pdf'
lerner <- readTransformEconomicReview(article_pages = lerner_pages, author = 'lerner',
                                      book_name =  lerner_bookname , png_already = TRUE)
saveRDS(lerner, 'data/lerner.RDS')

# Dickinson
dickinson_pages <- seq(2,15)
dickinson_bookname <- 'dickinson_Price_Formation_in_a_Socialist_Community.pdf'
dickinson <- readTransformEconomicReview(article_pages = dickinson_pages, author = 'dickinson', 
                                         book_name = dickinson_bookname, png_already = TRUE)
saveRDS(dickinson, 'data/dickinson.RDS')

                                         