# using only a few hundred lines from the dataset

if (!require(tidytext)) install.packages("tidytext")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(wordcloud)) install.packages("wordcloud")
if (!require(reshape2)) install.packages("reshape2")
if (!require(Rmpfr)) install.packages("Rmpfr") # sudo apt-get install libgmp3-dev, 
if (!require(topicmodels)) install.packages("topicmodels") # sudo apt-get install gsl-bin
if (!require(ldatuning)) install.packages("ldatuning")
library(tidytext)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(ldatuning)

# setwd("~/R/amazon.finefoods")
setwd("C:/Users/e/Documents/R/amazon.finefoods")

y <- readLines("data/foods.txt",n=5000)

y.1 <- y %>% 
  data_frame(chars = trimws(y)) %>%
  mutate(variable_num = cumsum(grepl(":", chars)) ) %>%
  group_by(variable_num) %>%
  summarise(chars = paste0(chars, collapse = " ")) %>%
  separate(chars, into = c("variable", "value"), sep = ": ") %>%
  select(-variable_num) %>% 
  mutate(variable = sub(".*/", "", variable),
         record_num = cumsum(variable == "productId")) %>% 
  spread(variable, value, convert = T) %>% 
  select(text)

y.2 <- y.1 %>% 
  unnest_tokens(word,text)

data("stop_words")
y.3 <- y.2 %>% 
  anti_join(stop_words) %>% 
  filter(word != "br") # remove 'br'

y.3 %>% 
  count(word, sort=TRUE)

y.3 %>% 
  count(word, sort=TRUE) %>%
  filter(n>60) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n))+
  geom_col() + 
  coord_flip()

sent.afinn <- get_sentiments("afinn")
sent.bing <- get_sentiments("bing")
sent.nrc <- get_sentiments("nrc")

y.4 <- y.3 %>% 
  inner_join(sent.bing) %>% 
  count(word,sentiment,sort=TRUE)

bing.pos <- sent.bing %>% 
  filter(sentiment=="positive")

y.3 %>% 
  semi_join(bing.pos) %>% 
  count(word, sort=T)

y.3 %>% 
  inner_join(sent.bing) %>% 
  count(word, sentiment) %>% 
  spread(sentiment,n,fill=0) %>% 
  mutate(sentiment=positive-negative)
  
y.4 <- y.3 %>% 
  inner_join(sent.bing) %>% 
  count(word, sentiment) %>% 
  spread(sentiment,n,fill=0) %>% 
  mutate(sentiment=positive-negative)

y.3 %>%
  count(word) %>% 
  with(wordcloud(word,n,max.words=100))

y.3 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 80,
                   scale=c(3,.2))

y.3 %>% 
  inner_join(sent.bing) %>% 
  count(word, sentiment, sort=TRUE) %>% 
  filter(n>10) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

## LDA tuning?
y.lda <- y %>% 
  data_frame(chars = trimws(y)) %>%
  mutate(variable_num = cumsum(grepl(":", chars)) ) %>%
  group_by(variable_num) %>%
  summarise(chars = paste0(chars, collapse = " ")) %>%
  separate(chars, into = c("variable", "value"), sep = ": ") %>%
  select(-variable_num) %>% 
  mutate(variable = sub(".*/", "", variable),
         record_num = cumsum(variable == "productId")) %>% 
  spread(variable, value, convert = T) %>% 
  select(profileName,score,text) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  filter(word != "br") # remove 'br'

y.lda <- y.lda %>% 
  count(profileName, word,sort=TRUE)

y.lda <- y.lda %>% 
  cast_dtm(profileName,word,n)

inspect(y.lda[1:10,1:10])

lda.tops <- LDA(y.lda,k=9,control=list(seed = 1234))
lda.tops
y.topics <- tidy(lda.tops,matrix="beta")

top_terms <- y.topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms
# Plot the 20 top terms per topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

result <- FindTopicsNumber(
  y.lda,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)
FindTopicsNumber_plot(result)


## Data import from stackoverflow postn
# Stepwise to figure out what the hell this does
# https://stackoverflow.com/questions/53165140/reading-file-with-one-column-with-rows-as-variable-names/53165451#53165451

# reviews <- readLines("data/foods.txt")
# # assuming your data is in file called reviews.txt
# reviews   <- readLines("data/foods.txt",n=809) #
# # "chars" = name of tibble, trim white space
# # trimws e.g. "   ..    " => ".."
# reviews.1 <- data_frame(chars = trimws(reviews))
# # add number variable to column for every :
# reviews.2 <- reviews.1 %>% 
#   mutate(variable_num = cumsum(grepl(":", reviews.1$chars)))
# reviews.3 <- reviews.2 %>% 
#   group_by(variable_num) %>% 
#   summarise(chars = paste0(chars, collapse=" "))
# reviews.4 <- reviews.3 %>% 
#   separate(chars, into = c("variable","value"), sep=": ")
# reviews.5 <- reviews.4 %>% 
#   select(-variable_num) %>% 
#   mutate(variable = sub(".*/","",variable),
#          record_num = cumsum(variable == "productId"))
# reviews.6 <- reviews.5 %>% 
#   spread(variable,value,convert=T)
# Warning messages:
# 1: In grepl(":", chars) : input string 809 is invalid in this locale
# 2: In grepl(":", chars) : input string 1857 is invalid in this locale
# 3: In grepl(":", chars) : input string 4760 is invalid in this locale
# 4: In grepl(":", chars) : input string 5147 is invalid in this locale
# 5: In grepl(":", chars) : input string 5705 is invalid in this locale
# 6: Expected 2 pieces. Additional pieces discarded in 29725 rows [48, 576, 719, 903, 943, 1175, 1271, 1495, 1575, 1649, 1774, 1854, 1926, 2110, 2118, 2366, 2542, 3158, 3318, 3389, ...]. 

## Alternative way to import data from slack person:

#library('tidyverse')
# data <- read_delim(gzfile("data/foods.txt"), 
#                    delim = '\n', 
#                    col_names = FALSE,
#                    n_max=100)
# data_clean <- data %>%
#   separate(col = 'X1',
#            into = c('type', 'value'), 
#            sep = ':', 
#            fill = 'right', 
#            extra = 'merge')
