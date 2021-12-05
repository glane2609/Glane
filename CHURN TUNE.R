set.seed(1)
library(keras)
FLAGS1 <- flags(
 flag_numeric("nodes1", 128),
 flag_numeric("batch_size", 100),
 flag_string("activation", "relu"),
 flag_numeric("learning_rate", 0.001),
 flag_numeric("epochs", 30),
 flag_numeric("nodes2", 250)
)

model =keras_model_sequential()

model %>%
 layer_dense(units = FLAGS1$nodes1, activation =
              FLAGS1$activation, input_shape = dim(churn_train_final)[2]) %>% layer_dropout(0.5) %>% 
 layer_dense(units = FLAGS1$nodes2, activation = FLAGS1$activation) %>% layer_dropout(0.5) %>%
 layer_dense(units = 1,activation = FLAGS1$activation)

model %>% compile(
 loss = 'binary_crossentropy',
 optimizer = optimizer_adam(lr=0.01),
 metrics = c('accuracy'))

model %>% fit(
 churn_train_final , churn_train_final_lab, epochs = FLAGS1$epochs
 , batch_size= FLAGS1$batch_size,
 validation_data=list(churn_val_final, churn_val_final_lab))