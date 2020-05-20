library(tidyverse)
library(tidytext)
library(stringr)
library(ggthemr)

ggthemr('dust')
source('src/helpers.R')

# read in free market data
pierson <- readRDS('data/pierson.RDS')
mises <- readRDS('data/mises.RDS')
hayek_nature <- readRDS('data/hayek_nature.RDS')
hayek_debate <- readRDS('data/hayek_present_state.RDS')

# read in socialist data
dickinson <- readRDS('data/dickinson.RDS')
lange1 <- readRDS('data/lange1.RDS')
lange2 <- readRDS('data/lange2.RDS')
# concat lange into one df -> treat it as one article
lange <- rbind(lange1, lange2)
lerner <- readRDS('data/lerner.RDS')
neurath <- readRDS('data/neurath.RDS')

# meta data for texts
free_meta <- data.frame(
  author = c('N. G. Pierson', 'Ludwig von Mises', 'F. A. Hayek', 'F. A. Hayek'),
  alias = c('Pierson', 'von Mises', 'Hayek', 'Hayek'),
  year = c(1902, 1920, 1935, 1935),
  pages = c(45, 44, 43, 40),
  socialist = c(0, 0, 0, 0)
)

# get df with word frequencies in decending order
# free marketers
pierson_top <- proportionDF(pierson)
pierson_top  

mises_top <- proportionDF(mises)
mises_top

hayek_nature_top <- proportionDF(hayek_nature)
hayek_nature_top

hayek_debate_top <- proportionDF(hayek_debate)
hayek_debate_top

# socialists
dickinson_top <- proportionDF_socialist(dickinson)
lange_top <- proportionDF_socialist(lange)
lerner_top <- proportionDF_socialist(lerner)
neurath_top <- proportionDF_socialist(neurath)


# create dataframe for free marketers
free_marketers <- bind_rows(mutate(pierson_top, author = 'Pierson'),
                            mutate(mises_top, author = 'von Mises'),
                            mutate(hayek_nature_top, author = 'Hayek: nature'),
                            mutate(hayek_debate_top, author = 'Hayek: debate'))
free_marketers$author <- factor(free_marketers$author, levels = c("Pierson", "von Mises", "Hayek: nature", "Hayek: debate"))

# create dataframe for socialists
socialists <- bind_rows(mutate(dickinson_top, author = 'Dickinson'),
                            mutate(lange_top, author = 'Lange'),
                            mutate(lerner_top, author = 'Lerner'),
                            mutate(neurath_top, author = 'Neurath'))
socialists$author <- factor(socialists$author, levels = c("Neurath", "Dickinson", "Lerner", "Lange"))

######
## Free-market plots
######
top10_pooled_plot(free_marketers)

top10_plot(free_marketers)
# ggsave('report/free_market_word_free_y.png', width = 12, height = 6)

# create scatterplot
market_scatter <- free_marketers %>% 
  filter(author != 'Hayek: nature',
         !str_detect(word, "[a-z']+[:digit:]+" ), 
         !str_detect(word, "[:digit:]+[a-z']+"),
         str_detect(word, "[a-z']+|[:digit:]{4}")) %>% 
  select(-n, -rank) %>% 
  spread(author, proportion) %>%
  gather(author, proportion, `Pierson`:`von Mises`) 

scatter_p <- ggplot(market_scatter, aes(x = proportion, y = `Hayek: debate`, color = abs(`Hayek: debate` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_text(aes(label = word),position=position_jitter(width=0.2,height=0.2) , check_overlap = TRUE) +
  scale_x_log10(labels = scales::percent) +
  scale_y_log10(labels = scales::percent) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Hayek: debate", x = NULL)
# ggsave('report/free_market_scatter.png', scatter_p, width = 12, height = 6)


# get an idea of differences in the plot what these mean in numbers
viz_nums <- filter(free_marketers, word %in% c('system', 'social', 'society', 'economic', 'exchange', 'discussion', 
                                               'planning', 'rate', 'lenin')) %>% 
  arrange(word)


# words that one of the two authors dont mention
hayek_pierson <- no_mention_words(author = 'Pierson', df = market_scatter)
hayek_mises <- no_mention_words(author = 'von Mises', df = market_scatter)

# munge data so it can be plotted nicely with facets
# hayek vs pierson
hayek_pierson <- hayek_pierson %>% 
  select(-na_sum, -author) %>% 
  rename(Pierson = proportion) %>% 
  gather(key = "author", value = "proportion", `Hayek: debate`, Pierson, -word) %>% 
  replace_na(list(proportion = 0)) %>% 
  mutate()

hayek_pierson$proportion <- with(hayek_pierson, ifelse(author == "Pierson", -proportion, proportion))
hayek_pierson$desc <- c("Hayek vs. Pierson")

# Hayek vs Mises
hayek_mises <- hayek_mises %>% 
  select(-na_sum, -author) %>% 
  rename(Mises = proportion) %>% 
  gather(key = "author", value = "proportion", `Hayek: debate`, Mises, -word) %>% 
  replace_na(list(proportion = 0)) %>% 
  mutate()

hayek_mises$proportion <- with(hayek_mises, ifelse(author == "Mises", -proportion, proportion))
hayek_mises$desc <- c("Hayek vs. Mises")

binded_df <- rbind(myres, hayek_mises)

ggplot(binded_df, aes(x = reorder(word, proportion), y = proportion, fill = author)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_grid(~desc) + 
  labs(x = "words", y = "proportion")
# ggsave('report/nomention_free_market.png',width = 12, height = 6)


######
## Socialist plots
######

pooled_socalists <- top10_pooled_plot(socialists)
#ggsave('report/socialist_word_pooled_y.png', pooled_socalists, width = 12, height = 6)

nopooled_socialists <- top10_plot(socialists)
# ggsave('report/socialist_word_free_y.png', nopooled_socialists, width = 12, height = 6)

# create scatterplot
socialist_scatter <- socialists %>% 
  filter(author != 'Dickinson', # sorry Dickinson you are aout
         !str_detect(word, "[a-z']+[:digit:]+" ), 
         !str_detect(word, "[:digit:]+[a-z']+"),
         str_detect(word, "[a-z']+|[:digit:]{4}")) %>% 
  select(-n, -rank) %>% 
  spread(author, proportion) %>%
  gather(author, proportion, `Lerner`:`Neurath`) 

scatter_socialist <- ggplot(socialist_scatter, aes(x = proportion, y = `Lange`, color = abs(`Lange` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_text(aes(label = word),position=position_jitter(width=0.2,height=0.2) , check_overlap = TRUE) +
  scale_x_log10(labels = scales::percent) +
  scale_y_log10(labels = scales::percent) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Lange", x = NULL)
ggsave('report/socialist_scatter.png', scatter_socialist, width = 12, height = 6)
