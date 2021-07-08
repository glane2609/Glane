library(readtext)
library(tm)
library(tidytext)
library(tidyverse)
library(topicmodels)
library(SnowballC)
library(car)

filenames <- read.table(path="C:/Users/glane/Downloads/Reviews/",
                        pattern = ".dat", header=T ,sep = ",\t")
temp <- lapply(filenames, fread, sep=",\t")
data <- rbindlist(temp)
class(as.data.frame(data))
View(data)
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(data)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  number_of_topics = 4
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  topics
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
top_terms

# create a document term matrix to clean
reviewsCorpus <- Corpus(VectorSource(reviews$text)) 
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)

# convert the document term matrix to a tidytext corpus
reviewsDTM_tidy <- tidy(reviewsDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in hotel reviews
custom_stop_words <- tibble(word = c("hotel", "room"))

# remove stopwords
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# check out what the clean documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)
