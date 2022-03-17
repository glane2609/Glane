library(readtext)
data_dir <- "C:/Users/glane/Downloads"
train_pos <- readtext(paste0(data_dir, "/aclImdb_v1/aclImdb/train/pos/*"))
View(train_pos)
class(train_pos)

train_neg <- readtext(paste0(data_dir, "/aclImdb_v1/aclImdb/train/neg/*"))
test_pos <- readtext(paste0(data_dir, "/aclImdb_v1/aclImdb/test/pos/*"))
test_neg <- readtext(paste0(data_dir, "/aclImdb_v1/aclImdb/test/neg/*"))

#Since the process take a quite long time to run, we will use parallel computing to get the job done using the furrr package.
cleansing_text <- function(x) x %>% 
 tolower() %>% 
 str_replace_all(pattern = "\\@.*? |\\@.*?[:punct:]", replacement = " ") %>% 
 str_replace_all("\\?", " questionmark") %>% 
 str_replace_all("\\!", " exclamationmark") %>% 
 str_replace_all("[:punct:]", " ") %>% 
 str_replace_all("[:digit:]", " ") %>% 
 str_trim() %>% 
 str_squish()

library(furrr) 
plan(multisession, workers = 4) # Using 4 CPU cores

train_pos_clean <- train_pos %>% 
 mutate(
  text_clean = text %>% 
   future_map_chr(cleansing_text)
 ) 

head(train_pos_clean)
View(train_pos_clean)
train_pos_clean$tag <- "pos"

train_neg_clean <- train_neg %>% 
 mutate(
  text_clean = text %>% 
   future_map_chr(cleansing_text)
 ) 

head(train_neg_clean)
train_neg_clean$tag <- "neg"

train1 <- rbind(train_pos_clean,train_neg_clean)
View(train1)


test_pos_clean <- test_pos %>% 
 mutate(
  text_clean = text %>% 
   future_map_chr(cleansing_text)
 ) 

head(test_pos_clean)
test_pos_clean$tag <- "pos"


test_neg_clean <- test_neg %>% 
 mutate(
  text_clean = text %>% 
   future_map_chr(cleansing_text)
 ) 

head(test_neg_clean)
test_neg_clean$tag <- "neg"


test1 <- rbind(test_pos_clean,test_neg_clean)
View(test1)


# Tokenization
#We will check how many unique words that we have in our corpus.
#train
paste(train1$text_clean, collapse = " ") %>% 
 str_split(" ") %>% 
 unlist() %>% 
 n_distinct()

#test
paste(test1$text_clean, collapse = " ") %>% 
 str_split(" ") %>% 
 unlist() %>% 
 n_distinct()


library(keras)
library(tensorflow)
num_words <- 10000
max_length <- 50
text_vectorization <- layer_text_vectorization(
 max_tokens = num_words, 
 output_sequence_length = max_length, 
)

text_vectorization %>% 
 adapt(train1$text_clean)

input <- layer_input(shape = c(1), dtype = "string")

output <- input %>% 
 text_vectorization() %>% 
 layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
 layer_global_average_pooling_1d() %>%
 layer_dense(units = 16, activation = "relu") %>%
 layer_dropout(0.5) %>% 
 layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(input, output)

model %>% compile(
 optimizer = 'adam',
 loss = 'binary_crossentropy',
 metrics = list('accuracy')
)


history <- model %>% fit(
 train1$text_clean,
 as.numeric(train1$tag == "pos"),
 epochs = 10,
 batch_size = 512,
 validation_split = 0.2,
 verbose=2
)

results <- model %>% evaluate(test1$text_clean, as.numeric(test1$tag == "pos"), verbose = 0)
results

plot(history)



#Negative words
history_neg <- model %>% fit(
 train1$text_clean,
 as.numeric(train1$tag == "neg"),
 epochs = 10,
 batch_size = 512,
 validation_split = 0.2,
 verbose=2
)

results_neg <- model %>% evaluate(test1$text_clean, as.numeric(test1$tag == "neg"), verbose = 0)
results_neg

plot(history_neg)