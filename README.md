# Sentiment-Analysis-Gutenberg
A comparison between "The Railway Children and Great Expectation" of the Gutenberg Collection. Analysing the frequency of sentiment of the books on R markdown
---
title: 'Sentiment Analysis: A Gutenbergs books comparison'
author: 'Liz'
date: "20/12/2021"
output:
  word_document:![common and negative](https://user-images.githubusercontent.com/67712050/148384375-94a360fd-5957-4ef3-b924-6618e0ee59e1.png)
![correlation](https://user-images.githubusercontent.com/67712050/148384378-f86cdb94-8dd3-4f56-92c9-08a29b3dfbb1.png)
![Railway Children Freq](https://user-images.githubusercontent.com/67712050/148384382-5cc2a2b1-35b2-44c7-a0d2-c74b7d1b7ab1.png)

    toc: yes
  html_document:
    toc: yes
    fig_width: 4
    fig_height: 3.5
    fig_caption: yes
    number_sections: yes
  pdf_document:
    toc: yes
---

```{r setup, include=FALSE}
library(tigerstats)
library(knitr)
opts_chunk$set(echo = FALSE)
```
# Introduction
Sentiment Analysis is the process of analysing text to determine if it's positive, negative or neutral by exploring the sentiment packages such as 'nrc,bing,and afinn'. In this report, I explored and compared two books from the _Gutenbergs collections_. The first from child list is ID 1874,	**'The Railway Children'** by	Nesbit, E. (Edith) and from the Adult list is ID 1400	**'Great Expectations'** by	Dickens, Charles. This report focused on the sentiment analysis between these books.
# Methodology
Certain libraries are required repeatedly after installation for the analysis.
```{r}
library(gutenbergr)   # a library of many texts
library(dplyr)        # for data manipulation
library(tidyr)        # to tidy data
library(stringr)      # to make working with strings easy
library(tidytext)     # switching between tidy format and text
library(tm)           # for text mining
library(NLP)          # Natural Language Process
```
### The Railway Children
We need to download the first book using the Gutenbergs ID 1874.
```{r}
RailChildren <- gutenberg_download(c(1874))   # downloaded by number
original_RailChildren <- RailChildren %>%     # transform the book to show additional columns
  mutate(linenumber = row_number(),           # add columns with line and chapter
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]", # to find where all chapters are
                                           ignore_case = TRUE))))

original_RailChildren
table(original_RailChildren$chapter) # tables no of lines per chapter and book with 0 to 14 being the chapters
```
## Tokenization
The output is 7,873 observation under text in the form of sentences because it is a book. However, to analyze the text to get the sentiment out of it, we need to break every sentence into a singular text. This process is known as *tokenization*; breaking sentences into tokens. # to reduce the sentences to single word per row compared to the untidy data. The punctuation have been removed and data is now in lower-case and the observation increased to 60,195. For example, lets take a look at the books;
```{r}
# We restructured the original_RailChildren to one-token-per-row format.
RailChildren_tidy <- original_RailChildren %>%    # convert to a tidy format
  unnest_tokens(word, text)                       # one-token-per-row
head(RailChildren_tidy) 
```
## Stop Words
Generally, there are some commonly used `words` which basically provides no depth to the purpose of a sentiment because they appear too much and hence becomes less useful. Examples as we can see in these books are "a","the","is","of" and etc. We will remove the existing stopwords using the _(anti_join)_ function. 
``` {r}
data(stop_words)                                        # view stopwords
RailChildren_tidy <- original_RailChildren %>%          # unnest i.e. convert to tidy format 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)                                 # removal of stop words here
RailChildren_tidy %>%count(word, sort = TRUE)           #to find the most frequent word
```
Lets visualize the above
```{r fig.cap="We have 5 words which exist more. These words represent people, they are names. Which makes the assumption that the book is about 'peter, phyllis, bobbie, mother and children',the main characters"}
RailChildren_tidy %>%
  count(word, sort = TRUE) %>%                          # count words in RailChildren_tidy
  filter(n > 100) %>%                                   # filter words that appeared 100 times more
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +                                # x- axis labelled as n
  geom_col(fill = "blue") +                             # fill bars with colour blue
  labs(y = "word") +
  ggtitle("Main Characters")                          # plot title
```
### Great Expectation
We repeat the same process as we did The Railway Children for my second choice 'Great Expectations'. Here we will see that the main characters goes by the names "Joe, Wemmick, Pip, Herbert, Estella, Havisham"
```{r}
GreatExp <- gutenberg_download(c(1400))             # We have 20,397 observations of 2 variables
original_GreatExp <- GreatExp %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE))))

table(original_GreatExp$chapter)                    # tables no of lines per chapter and book

GreatExp_tidy <- original_GreatExp %>%              # unnest i.e. convert to tidy format 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)                             # removal of stop words here
GreatExp_tidy%>%count(word, sort = TRUE)            # words in desc order
```
## Basic Sentiment Analysis
The _tidytext_ package is contained of three lexicons which contains many English words and are classified to indicate whether a word is positive or negative. The lexicons are;
* AFINN 
* bing
* nrc 
To compare the books, I combined them as shown below;
```{r}
Gut_books <- gutenberg_download(c(1874,1400)) %>%
  group_by(gutenberg_id) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
```
Combining the books makes it possible to compare results simultaneously. The 'nrc' sentiment is examined below. It compares 'nrc' sentiment in both books and we see that 'Great Expectation' has more negative words compared to the 'Railway children', however we cant conclude because there are more words in 'Great Expectation'
```{r}
Gut_books %>%
        right_join(get_sentiments("nrc")) %>%
        filter(!is.na(sentiment)) %>%
        count(sentiment, sort = TRUE)
```

```{r}
Gut_sentiment <- Gut_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(gutenberg_id, index = linenumber %/% 100, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)
```
```{r fig.cap= "Figure 2.2:This shows the different levvels of sentiment in each book. Both books has more of negative presence than positive"}
  
ggplot(Gut_sentiment, aes(index, sentiment, fill = gutenberg_id)) + 
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) + 
  ggtitle("Sentiment Representation") + 
  facet_wrap(~ gutenberg_id, ncol = 2, scales = "free_x")
```
## Sentiment Comparison
Here, we want to see how the three sentiment lexicons differ
``` {r}
afinn <- Gut_books %>%
        group_by(gutenberg_id) %>% 
        mutate(word_count = 1:n(),
               index = word_count %/% 300 + 1) %>% 
        inner_join(get_sentiments("afinn")) %>%
        group_by(gutenberg_id, index) %>%
        summarise(sentiment = sum(value)) %>%
        mutate(method = "AFINN")

bing_and_nrc <- bind_rows(Gut_books %>%
                  group_by(gutenberg_id) %>% 
                  mutate(word_count = 1:n(),
                         index = word_count %/% 300 + 1) %>% 
                  inner_join(get_sentiments("bing")) %>%
                  mutate(method = "Bing"),
          Gut_books %>%
                  group_by(gutenberg_id) %>% 
                  mutate(word_count = 1:n(),
                         index = word_count %/% 300 + 1) %>%
                  inner_join(get_sentiments("nrc") %>%
                                     filter(sentiment %in% c("positive", "negative"))) %>%
                  mutate(method = "NRC")) %>%
        count(gutenberg_id, method, index = index , sentiment) %>%
        ungroup() %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative) %>%
        select(gutenberg_id, index, method, sentiment)
```

```{r fig.cap= "Figure 2.3: Lexicon sentiment comparison in both books"}
bind_rows(afinn, 
          bing_and_nrc) %>%
        ungroup() %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_grid(gutenberg_id ~ method)
```
The AFINN lexicon gives the largest values with its peak at positive values. Bing gives the opposite of AFINN and the NRC detects more negative values compared to positive. In conclusion we find that the AFINN sentiment is high, the NRC sentiment has more variance and Bing sentiment has more longer stretches of similar text. This trajectories isn't absolute when we put into consideration the ratio of negative words to positive words in each of these lexicons. We find that the ratio of negative word to positive words is higher in Bing compared to NRC. 

### Common and Uncommon words
``` {r}
bing_word_counts <- Gut_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts
```
```{r fig.cap= "Figure 2.4: Positive and Negative sentiment trends in The Railway Children and Great Expectation"}
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL) +
  ggtitle("Most Common and Uncommon Trend")
```
### Custom Stop_words
In Figure 2.4, the word which appears as the most negative sentiment "miss" is a title for a young, unmarried woman in Great Expectations, hence not negative word. So we make it a custom stop word and bind it to the existing stop words as shown below.
```{r}
custom_stop_words <- bind_rows(tibble(word = c("miss"), 
                                      lexicon = c("custom")), 
                               stop_words)
```
## Wordclouds
```{r}
library(wordcloud)
library(reshape2)

Gut_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
```

```{r}
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
```

```{r}
library(stringr)
library(ggplot2)
library(igraph)
library(ggraph)
gutbook <- gutenberg_download(c(1400,1874))
gut_bigrams <- gutbook %>%
  count_bigrams()

# filter out rare combinations, as well as digits
gut_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()
```

```{r}
gutbook %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
```

```{r}
gut_section_words <- gutbook %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

gut_section_words
```
```{r}
library(widyr)
# count words co-occuring within sections
word_pairs <- gut_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs
```
```{r}
# we need to filter for at least relatively common words first
word_cors <- gut_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors
```

```{r}
word_cors %>%
  filter(item1 %in% c("pip", "havisham", "samaritan", "bobbie")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
```

```{r}
set.seed(2016)

word_cors %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
```
