library(tidyverse)
library(tidytext)
library(gutenbergr)

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

all_text <- books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

all_text

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


