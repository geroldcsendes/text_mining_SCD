library(tidyverse)
library(tidytext)
library(stringr)
library(ggthemr)
library(sentimentr)
library(visNetwork)
library(htmlwidgets)

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
# new page def
lange$page <- 1:nrow(lange)

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
mises_top <- proportionDF(mises)
hayek_nature_top <- proportionDF(hayek_nature)
hayek_debate_top <- proportionDF(hayek_debate)

# socialists
dickinson_top <- proportionDF(dickinson)
lange_top <- proportionDF(lange)
lerner_top <- proportionDF(lerner)
neurath_top <- proportionDF(neurath)


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
# ggsave('report/socialist_word_pooled_y.png', pooled_socalists, width = 12, height = 6)

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


######
# topics analysis
######

# early debate
early_topic <- rbind(
                  pierson_top %>% 
                    mutate(author = c("Pierson")),
                  neurath_top %>% 
                    mutate(author = c("Neurath")),
                  mises_top %>% 
                    mutate(author = c("Mises"))
                  ) %>% 
                    arrange(-n)

early_topic <- early_topic %>%
  bind_tf_idf(word, author, n) %>% 
  arrange(-tf_idf)

# put authors into factors to fix chart order
early_topic$author <- factor(early_topic$author, levels = c("Pierson", "Neurath", "Mises"))
  
  
early_tf_idf <- early_topic %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(author) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, scales = "free") +
  coord_flip()
# ggsave('report/early_tf_idf.png', early_tf_idf, width = 12, height = 6 )


# later alternative
# late debate
late_topic <- rbind(
  rbind(
    hayek_debate_top,
    hayek_nature_top) %>% 
      mutate(side = "Free"),
  rbind(
    dickinson_top,
    lange_top,
    lerner_top
  ) %>% 
    mutate(side = "Socialists")
)

late_topic <- late_topic %>% 
  group_by(side, word) %>% 
  summarise(n = sum(n)) %>% 
  arrange(-n)
  
late_topic <- late_topic %>%
  bind_tf_idf(word, side, n) %>% 
  arrange(-tf_idf) %>% 
  filter(word != 1)


late_tf_idf <- late_topic %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(side) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(x=reorder_within(word, tf_idf, side), y=tf_idf, fill=side)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~side, scales = "free") +
  coord_flip()
ggsave('report/late_tf_idf.png', late_tf_idf, width = 12, height = 6 )


# network viz
words_by_authors <- rbind(
  pierson_top %>% 
    mutate(author = c("Pierson")),
  neurath_top %>% 
    mutate(author = c("Neurath")),
  mises_top %>% 
    mutate(author = c("Mises")),
  hayek_debate_top %>% 
    mutate(author = c("Hayek: debate")),
  hayek_nature_top %>% 
    mutate(author = c("Hayek: nature")),
  dickinson_top %>% 
    mutate(author = c("Dickinson")),
  lange_top %>% 
    mutate(author = c("Lange")),
  lerner_top %>% 
    mutate(author = c("Lerner"))
) %>% 
  arrange(-n)

library(widyr)
authors_cors <- words_by_authors %>%
  pairwise_cor(author, word, n, sort = TRUE)

library(ggraph)
library(igraph)
set.seed(2017)
authors_cors <- authors_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation), edge_color = "royalblue") +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# ggsave("report/authors_cors.png", authors_cors, , width = 12, height = 6 )



## Sentiment analysis


# socialist sentiment
neurath_sentiment <- sentiment_df(neurath, author = "neurath")
lange_sentiment <- sentiment_df(lange, author = "lange")
lerner_sentiment <- sentiment_df(lerner, author = "lerner")
dickinson_sentiment <- sentiment_df(dickinson, author = "dickinson")

# free market sentiment
pierson_sentiment <- sentiment_df(pierson, author = "pierson")
mises_sentiment <- sentiment_df(lerner, author = "mises")
hayek_nature_sentiment <- sentiment_df(hayek_nature, author = "hayek_nature")
hayek_debate_sentiment <- sentiment_df(hayek_debate, author = "hayek_debate")

sentiment_df <- rbind(
  neurath_sentiment,
  lange_sentiment,
  lerner_sentiment,
  dickinson_sentiment,
  pierson_sentiment,
  mises_sentiment,
  hayek_nature_sentiment,
  hayek_debate_sentiment
  
)

(sentiment_score <- with(
  sentiment_df, 
  sentiment_by(
    get_sentences(text), 
    list(author),
    averaging.function = sentimentr::average_mean
  )
))

plot(sentiment_score)


# see individual sentiments
(sentiment_neurath <- with(
  neurath_sentiment, 
  sentiment(
    get_sentences(text)
  )
))

(sentiment_hayek_debate <- with(
  hayek_debate_sentiment, 
  sentiment(
    get_sentences(text)
  )
))

happy_sentiment <- as.character(neurath_sentiment[26, "text"])

hayek_neutral_sentiment <- as.character(hayek_debate_sentiment[45, "text"])


sentiment("Socialism is the most beautiful thing in the world, people will be happy and rich")
sentiment("Socialism is plague to all of us")



# show texts highlighted
# library(magrittr)
# set.seed(2)
# 
# sentiment_df %>%
#   mutate(review = get_sentences(text)) %$%
#   sentiment_by(review, author) %>%
#   highlight()
# 
# 


# cross references

# free market
pierson_cross <- cross_refer(pierson_top, "pierson")
mises_cross <- cross_refer(mises_top, "mises")
hayek_nature_cross <- cross_refer(hayek_nature_top, "hayek")
hayek_debate_cross <- cross_refer(hayek_debate_top, "hayek")

# socialist
# neurath_cross <- cross_refer(neurath_top, "neurath") # no one mentioned
lerner_cross <- cross_refer(lerner_top, "lerner")
dickinson_cross <- cross_refer(dickinson_top, "dickinson")
lange_cross <- cross_refer(lange_top, "lange")


cross_df <- rbind(
  pierson_cross,
  mises_cross,
  hayek_nature_cross,
  hayek_debate_cross,
  lerner_cross,
  dickinson_cross,
  lange_cross
)

# summarise hayek two article
cross_df <- cross_df %>% 
  group_by(author, word) %>% 
  summarise(proportion = mean(proportion))

# munge for graph setup
cross_df <- cross_df %>% 
  select(from = author,
         to = word,
         value = proportion) 
cross_df$length <- c(300)

# change replace hayek_nature and debate
#cross_df$from[cross_df$from == "hayek_"] <- 0

nodes <- data.frame(id = c("pierson", "mises", "hayek",
                           "neurath", "lange", "lerner", "dickinson"),
                    color = c(rep("blue", 3), rep("red", 4)),
                    group = c(rep("opponent", 3), rep("socialist",4)),
                    label = c("pierson", "mises", "hayek",
                              "neurath", "lange", "lerner", "dickinson"))

edges <- cross_df

cross_network <- visNetwork(nodes, edges, height = "600px", width = "600px") %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") %>%
  visEdges(arrows = "to") %>%
  visLayout(randomSeed = 123)

# save network vis
withr::with_dir('report', htmlwidgets::saveWidget(cross_network, file="cross_network.html"))


# all words
all_words <- rbind(
  pierson_top,
  mises_top,
  hayek_nature_top,
  hayek_debate_top,
  dickinson_top ,
  lange_top ,
  lerner_top,
  neurath_top)

all_words <- rbind(
  free_marketers,
  socialists
)

all_words %>% 
  filter(word %in% c("austria", "austrian"))

hayek_austria <- hayek_nature %>% 
  filter(str_detect(string = text, pattern = "austria|austrian"))

str_view_all(hayek_austria$text, pattern="austria|austrian")
