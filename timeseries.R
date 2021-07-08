library(lubridate)
library(dplyr)
library(ggplot2)
library(outliers)
library(prophet)
library(corrplot)
library(caret)
library(plyr)
library(fBasics)
library(OutlierDetection)
library(leaflet)
library(party)
library(randomForest)

solar<-readRDS("C:/Users/glane/Downloads/solar_dataset.RData")
stations_info <- read.csv("C:/Users/glane/Downloads/station_info.csv")

stations_info1 <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = stations_info$elon , lat= stations_info$nlat, popup= stations_info$stid)
stations_info1 # Print the map

#Selecting the first 99 columns
solar1<- select(solar,c(1:99))
View(solar1)


#Selecting the columns from 2-99 to find mean and sd
solar2<- select(solar1,c(2:99))
solar2 <- na.omit(solar2)

Mean <- colMeans(solar2)

sd <- colStdevs(solar2)


#Correlation
corr<- cor(solar2)

#Visualisation of correlation
corrplot(corr, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))



#PCA
pca_out <- prcomp(solar2,scale. = T)
pca_out

#Detecting outliers
pca_out$x
theme_set(bigstatsr::theme_bigstatsr(0.8))
qplot(pca_out$x[, 1], pca_out$x[, 2]) + coord_equal()


boxplot(pca_out$x)$out

Q <- quantile(pca_out$x, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(pca_out$x)
eliminated<- subset(pca_out, pca_out$x > (Q[1] - 1.5*iqr) & pca_out$x < (Q[2]+1.5*iqr))
eliminated

solar2_pca <- pca_out$x
solar2_pca

summary(pca_out)
plot(pca_out)

#Biplot
par(mar=c(4,4,2,2))
biplot(pca_out, cex =  0.5 , cex.axis = 0.5)

#Since the points are closed to each other ( red colored indicate the columns) they are correlated


#Linear Regression

## 75% of the sample size
solar1<- na.omit(solar1)

smp_size <- floor(0.75 * nrow(solar1))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(solar1)), size = smp_size)

train <- solar1[train_ind, ]
test <- solar1[-train_ind, ]

lm(solar1, data=train)-> model1
summary(model1)
predict(model1, newdata=test)->predicted_values


train$Date <- as.factor(train$Date)
set.seed(51)
# Training using 'random forest' algorithm
model <- train(Date~.,
               data = train, # Use the train data frame as the training data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 5)) # Use 5 folds for cross-validation
#Decision tree
library("rpart") ## recursive partitioning
m <- rpart(Date ~ ., data = solar1,
           method = "class")


p <- predict(m, solar1, type = "class")
p




myFolds <- createFolds(solar1$Date, k = 5)
str(myFolds)

myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProb = TRUE,
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds
)


knn_model <- train(Date ~ .,
                   train,
                   metric = "ROC",
                   method = "knn",
                   tuneLength = 20,
                   trControl = myControl)
print(knn_model)