library(topicmodels)

data("AssociatedPress")
AssociatedPress

# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

#> A LDA_VEM topic model with 2 topics.

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")

## To Find the most common term within each df... ##

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


## The visulaisation is good to easily suggest one is about polictics and the other is likely to do iwth a business... ##


##  To constrain it to a set of especially relevant words, we can filter for relatively common words, such as those that have a 
##β
##greater than 1/1000 in at least one topic.##
library(tidyr)

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>% ##add number to "Topic" as seperate column ##
  pivot_wider(names_from = topic, values_from = beta) %>%  ##Pivot values ##
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

##Besides estimating each topic as a mixture of words,
##LDA also models each document as a mixture of topics. 
##We can examine the per-document-per-topic probabilities, called (“gamma”), 


ap_documents <- tidy(ap_lda, matrix = "gamma")


##check

## I can see that many of these documents were drawn from a mix of the two topics, 
##but that document 6 was drawn almost entirely from topic 2, having a  γ
##from topic 1 close to zero. To check this answer, we could tidy() the document-term matrix 
##and check what the most common words in that document were.

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))


## algorithim was right ##

###########################################
