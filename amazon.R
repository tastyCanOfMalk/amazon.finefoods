if (!require(tidytext)) install.packages("tidytext")
if (!require(tidyverse)) install.packages("tidyverse")
library(tidytext)
library(tidyverse)

setwd("~/R/amazon.finefoods")

# start with 100 lines

y <- readLines("data/foods.txt",n=100)

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
  anti_join(stop_words)


# x <- readLines("data/foods.txt")
# x.1 <- x %>% 
#   data_frame(chars = trimws(x)) %>%
#   mutate(variable_num = cumsum(grepl(":", chars)) ) %>%
#   group_by(variable_num) %>%
#   summarise(chars = paste0(chars, collapse = " ")) %>%
#   separate(chars, into = c("variable", "value"), sep = ": ") %>%
#   select(-variable_num) %>% 
#   mutate(variable = sub(".*/", "", variable),
#          record_num = cumsum(variable == "productId")) %>% 
#   spread(variable, value, convert = T)

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
