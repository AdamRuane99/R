##Imagine 3 books having been ripped apart and having no way to stick back each chapter together, will use LDA to piece it back ##

titles <- c("Twenty Thousand Leagues under the Sea",
            "Pride and Prejudice", 
            "Great Expectations")

library("gutenbergr") ## for the text of these books 


books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")


## -- I divide these into chapters, use tidytext’s unnest_tokens() to separate them into words, then remove stop_words. ##

library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^chapter ", ignore_case = TRUE)
  ))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()


## -- data frame word_counts is in a tidy form, with one-term-per-document-per-row, but the topicmodels package requires a DocumentTermMatrix##
## Use cast_dm ## 

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)


## -- Looking for 3 books ##

chapters_lda <- LDA(chapters_dtm, k = 3)


## -- To test this ##


chapter_topics <- tidy(chapters_lda, matrix = "beta") %>%
  mutate(percent_beta = beta * 100)

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)


library(ggplot2)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)


chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))

## looks  little rocky in Great expectations but mostly a good prediction ##


chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  slice_max(gamma) %>%
  ungroup()

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)
