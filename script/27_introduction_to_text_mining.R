library(tidyverse)
library(tidytext)
library(gutenbergr)

# Reading in the content of the 4 books 

titles <- c("The War of the Worlds",
            "The Time Machine",
            "Twenty Thousand Leagues under the Sea",
            "The Invisible Man: A Grotesque Romance")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

str(books)

head(books, n = 15)

books %>% 
  distinct(title)

books$text[31:40]

# Unnesting the words

all_text <- books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

all_text

# Top 10 words in The Time Machine and The War of the Worlds

all_text %>%
  filter(title == "The Time Machine") %>%
  count(word, sort = TRUE) %>%
  top_n(10)

all_text %>%
  filter(title == "The War of the Worlds") %>%
  count(word, sort = TRUE) %>%
  top_n(10)

all_text %>%
  filter(title == "The Time Machine") %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(x = "Word", 
       y = "Count", 
       title = "Top 10 most commonly occuring words in The Time Machine") +
  theme_minimal()

ggsave("Top_10_Time_Machine_Words.jpg", width = 7, height = 3.6)

all_text %>%
  filter(title == "The War of the Worlds") %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(x = "Word", 
       y = "Count",
       title = "Top 10 most commonly occuring words in The War of the Worlds") +
  theme_minimal()
         
ggsave("Top_10_War_Worlds_Words.jpg", width = 7, height = 3.6)

get_sentiments("bing")

all_text_sentiments <- all_text %>%
  inner_join(get_sentiments("bing"))

head(all_text_sentiments)

all_text_sentiments %>%
  filter(title == "The War of the Worlds") %>%
  count(word, sentiment, sort = TRUE) %>%
  top_n(25) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sentiment Analysis of Top 25 Words in The War of the Worlds", 
       x = "Word",
       y = "Count") 

ggsave("Top_25_War_Worlds_Sentiments.jpg", width = 7, height = 3.6)

# Examining the proportion of useage of each word in each book

book_words <- all_text %>% 
  group_by(title) %>% 
  count(title, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(title) %>% 
  summarise(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words %>%
  mutate(proportion = n/total) %>%
  group_by(title) %>%
  arrange(desc(title, proportion)) %>%
  top_n(3) %>%
  select(-n, -total)

book_words %>%
  ggplot(aes(x = n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free")

ggsave("Zipf's Law.jpg", width = 7, height = 3.6)

# Term Frequency and Inverse Document Frequency

book_words_tf_idf <- book_words %>%
  bind_tf_idf(word, title, n)

book_words_tf_idf %>%
  top_n(15, tf_idf) %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf_idf") +
  facet_wrap(~ title, ncol = 2, scales = "free") +
  coord_flip()
