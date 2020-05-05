library(tidyverse)
library(tidytext)
library(stringr)
library(plotly)
library(ggrepel)

source('src/helpers.R')

# read in free market data
pierson <- readRDS('data/pierson.RDS')
mises <- readRDS('data/mises.RDS')
hayek_nature <- readRDS('data/hayek_nature.RDS')
hayek_debate <- readRDS('data/hayek_present_state.RDS')

# read in socialist data
# TODO


# meta data for texts
text_meta <- data.frame(
  author = c('N. G. Pierson', 'Ludwig von Mises', 'F. A. Hayek', 'F. A. Hayek'),
  alias = c('Pierson', 'von Mises', 'Hayek', 'Hayek'),
  year = c(1902, 1920, 1935, 1935),
  pages = c(45, 44, 43, 40),
  socialist = c(0, 0, 0, 0)
)

# quick analysis
# get df with word frequencies in decending order
pierson_top <- proportionDF(pierson)
pierson_top  

mises_top <- proportionDF(mises)
mises_top

hayek_nature_top <- proportionDF(hayek_nature)
hayek_nature_top

hayek_debate_top <- proportionDF(hayek_debate)
hayek_debate_top

# create dataframe for free marketers
free_marketers <- bind_rows(mutate(pierson_top, author = 'Pierson'),
                            mutate(mises_top, author = 'von Mises'),
                            mutate(hayek_nature_top, author = 'Hayek: nature'),
                            mutate(hayek_debate_top, author = 'Hayek: debate'))


# plot with "pooled axis"
free_marketers %>% filter(rank < 10) %>%  
  ggplot(aes(x = reorder(word, - proportion), y = proportion)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  facet_grid(~author) +
  scale_y_continuous(labels = scales::percent) 
  #theme(axis.text=element_text(size=12))
ggsave('report/free_market_word_pooled_y.png', width = 12, height = 6)


free_marketers %>% filter(rank < 11) %>%  
  ggplot(aes(reorder_within(word, proportion, author), proportion)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ author, scales = "free_y")
ggsave('report/free_market_word_free_y.png', width = 12, height = 6)


# create scatterplot
scatter_df <- free_marketers %>% 
  filter(author != 'Hayek: debate',
         !str_detect(word, "[a-z']+[:digit:]+" ), 
         !str_detect(word, "[:digit:]+[a-z']+"),
         str_detect(word, "[a-z']+|[:digit:]{4}")) %>% 
  select(-n, -rank) %>% 
  spread(author, proportion) %>%
  gather(author, proportion, `Pierson`:`von Mises`) 

# scatter_df$`Hayek: nature`[is.na(scatter_df$`Hayek: nature`)] <- 0.00001
# scatter_df$proportion[is.na(scatter_df$proportion)] <- 0.00001

p1 <- ggplot(scatter_df, aes(x = proportion, y = `Hayek: nature`, color = abs(`Hayek: nature` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, ) +
  #geom_text_repel(aes(label = word)) +
  scale_x_log10(labels = scales::percent) +
  scale_y_log10(labels = scales::percent) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Hayek: nature", x = NULL)

ggplotly(p1)
