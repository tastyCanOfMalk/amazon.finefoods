if (!require(tidytext)) install.packages("tidytext")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(wordcloud)) install.packages("wordcloud")
if (!require(reshape2)) install.packages("reshape2")
library(tidytext)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(reshape2)

setwd("~/R/amazon.finefoods")

x <- readLines("data/foods.txt")

x.cols <- x %>%
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

x.cols <- x.cols %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

x.1 <- x %>% s
  count(word, sort=TRUE)

x.1 <- x.1[-1,]  # remove "br"

x.1 %>% 
  filter(n>50000) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n))+
  geom_col() + 
  coord_flip()
