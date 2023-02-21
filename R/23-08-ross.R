library(dplyr)
library(tidyr)
library(ggplot2)
# library(glue)
library(tidytext)
library(stringr)
library(wordcloud)
library(wordcloud2)
library(arules)
library(arulesViz)

tuesdata <- tidytuesdayR::tt_load(x = '2023-02-21')
bob_ross <- tuesdata$bob_ross

# frequency table
bob_ross |> 
  select(painting_title) |> 
  unnest_tokens(word, painting_title) |> 
  group_by(word) |> 
  summarize(n = n()) |> 
  arrange(desc(n))

# frequency plot
bob_ross |> 
  select(painting_title) |> 
  unnest_tokens(word, painting_title) |> 
  group_by(word) |> 
  summarize(n = n()) |> 
  ggplot(aes(x = n)) +
  geom_histogram()

# frequency of non-stop words
bob_words <- bob_ross |> 
  select(painting_title) |> 
  unnest_tokens(word, painting_title) |> 
  group_by(word) |> 
  filter(!is.element(word, stop_words$word)) |> 
  summarize(freq = n())

# link I'm following :)
# https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a

# word cloud of non-stop words
cloud <- wordcloud(
  words = bob_words$word, 
  freq = bob_words$freq, 
  min.freq = 3,
  colors=brewer.pal(8, 'Dark2'),
  scale=c(3.5,0.25)
) 

# set color palette
autumn_colors <- c('#ffd200', '#9c5708', '#f47b20', '#f79762', '#f05133')
mountain_colors <- c('#31304f', '#3c517e', '#75424b', '#586e97', '#555361')
cold_colors <- c('#003366', '#968d99', '#000f89', '#123524', '#003153')

# unique colors
bob_color_vector <- bob_ross |> 
  summarize(
    col_hexes = str_remove_all(
      color_hex, '\\[|\\]|\''
    ) |> str_split(', ')
  ) |> 
  unnest(col_hexes) |> 
  distinct() |> 
  as.vector()

# make color vector for plot
color_vector <- rep(bob_color_vector$col_hexes, length.out=nrow(bob_words))

# trying wordcloud2
wordcloud2(
  bob_words, 
  size = 1.5,
  shape = 'circle',
  color = color_vector,
  backgroundColor = 'grey'
)

# table of all colors by descending frequency
bob_ross |> 
  summarize(
    col_names = str_remove_all(
      colors, '\\[|\\]|\\\\r|\\\\n|\''
    ) |> 
      str_split(', '),
    col_hexes = str_remove_all(
      color_hex, '\\[|\\]'
    ) |> str_split(', ')
  ) |> 
  unnest(c(col_names, col_hexes)) |> 
  group_by(col_names, col_hexes) |> 
  summarize(
    n = n()
  ) |> 
  arrange(desc(n))

# market basket analysis rules viz
bob_ross |> 
  select(10:27) |> 
  as('transactions') |> 
  apriori(
    parameter = list(
      maxlen = 18,
      target = 'rules'
    )
  ) |> 
  head(
    n = 10, # more? fewer?
    by = 'confidence'
  ) |> 
  plot(method = 'paracoord')
  
