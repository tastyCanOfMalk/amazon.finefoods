if (!require(tidytext)) install.packages("tidytext")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(wordcloud)) install.packages("wordcloud")
if (!require(reshape2)) install.packages("reshape2")
if (!require(Rmpfr)) install.packages("Rmpfr")
if (!require(topicmodels)) install.packages("topicmodels")
if (!require(ldatuning)) install.packages("ldatuning")
library(tidytext)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(ldatuning)

setwd("~/R/amazon.finefoods")

x <- readLines("data/foods.txt", n=10000)

# put variables into columns
x.tidy <- x %>%
  data_frame(chars = trimws(x)) %>%
  mutate(variable_num = cumsum(grepl(":", chars)) ) %>%
  group_by(variable_num) %>%
  summarise(chars = paste0(chars, collapse = " ")) %>%
  separate(chars, into = c("variable", "value"), sep = ": ") %>%
  select(-variable_num) %>%
  mutate(variable = sub(".*/", "", variable),
  record_num = cumsum(variable == "productId")) %>%
  spread(variable, value, convert = T) 

data("stop_words")

# unnest words, remove stop words, remove 'br'
x.tidy <- x.tidy %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  filter(word != "br") 

## Generally, what are the most common words used?
x.tidy %>%
  count(word, sort = T)

## Wordcloud of the above common words
pal = brewer.pal(9,"Blues")
x.tidy %>%
  count(word) %>% 
  with(wordcloud(word,n,
                 max.words=100,
                 min.freq = 100,
                 random.order = F,
                 random.color = F,
                 colors = pal,
                 scale=c(3,.3)))

## Comparison wordcloud of all text with sentiments
x.tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 80,
                   scale=c(3,.2))

x.tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort=TRUE) %>% 
  filter(n>100) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

## Frequency graph comparing 5-star to 1-4-star ratings
frequency <- x.tidy %>%
  count(score,word,sort=T) %>% 
  group_by(score) %>% 
  mutate(proportion=n/sum(n)) %>% 
  select(-n) %>% 
  spread(score, proportion) %>%
  gather(score, proportion, `1`,`2`,`3`,'4')

ggplot(frequency, aes(x = proportion, y = `5`, color = abs(`5` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~score, ncol = 5) +
  theme(legend.position="none") +
  labs(y = "5", x = NULL)

## Have proportions of sentiment in each review value
# need to figure out how to visualize
  # filter by score, wordclouds of each 1-5
x.tidy %>% 
  select(score,word) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(score,sentiment,sort=T) %>% 
  spread(score,n)

## Contribution of top 10 positive/negative words to sentiment
x.tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort=T) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


## LDA tuning probably doesn't make sense for amazon reviews?
# convert to dtm
x.dtm <- x.tidy %>% 
  count(productId,word,sort=T) %>% 
  ungroup() %>% 
  cast_dtm(productId,word,n)

# perform LDA using 4 topics
product.lda <- LDA(x.dtm,k=4,control=list(seed=1234))

# viewable LDA matrix with beta
product.topics <- tidy(product.lda,matrix="beta")

# top n terms in each topic k
top.terms <- product.topics %>% 
  group_by(topic) %>% 
  top_n(5,beta) %>%
  ungroup() %>% 
  arrange(topic,-beta)

# graph top terms
top.terms %>% 
  mutate(term=reorder(term,beta)) %>% 
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend=F)+
  facet_wrap(~topic,scales="free")+
  coord_flip()

# how to search for optimal number of topics? LDAtuning
tuner1 <- FindTopicsNumber(
  x.dtm,
  topics = seq(from = 5, to = 50, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)
FindTopicsNumber_plot(tuner)

# optimum topic numbers around 10-20, try again more narrowed
tuner2 <- FindTopicsNumber(
  x.dtm,
  topics = seq(from = 10, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)
FindTopicsNumber_plot(tuner)

# Optimal topics maybe 15
# perform LDA using optimal topics from above
product.lda <- LDA(x.dtm,k=15,control=list(seed=1234))
product.topics <- tidy(product.lda,matrix="beta")

# top n terms in each topic k
product.topics %>% 
  group_by(topic) %>% 
  top_n(5,beta) %>%
  ungroup() %>% 
  arrange(topic,-beta) %>% 
  mutate(term=reorder(term,beta)) %>% 
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend=F)+
  facet_wrap(~topic,scales="free")+
  coord_flip()
