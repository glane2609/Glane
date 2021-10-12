train <- read.csv("C:/Users/glane/Downloads/train.csv")
test <- read.csv("C:/Users/glane/Downloads/test.csv")
train<-janitor::clean_names(train)
View(train)
train[is.na(train)]<-0
train<-subset(train,select = -c(x,id))
test<-janitor::clean_names(test)
View(test)
test[is.na(test)]<-0
test<-subset(test,select = -c(x,id))


#EDA
library(ggplot2)
library(plotly)

#Satisfaction 
a<-ggplot(train,aes(satisfaction))+geom_bar()+ggtitle("Plot of people that represent their satisfaction ")
ggplotly(a)

#Satisfaction vs customer_type
b<-ggplot(train,aes(satisfaction,fill=customer_type))+geom_bar(position = "dodge")+ggtitle("Different types of customers that are satisfied or dissatisfied/neutral  ")
ggplotly(b)

#Satisfaction vs class
c<-ggplot(train,aes(satisfaction,fill=class))+geom_bar(position = "dodge")+ggtitle("Different types of customers that are satisfied or dissatisfied/neutral based on class ")
ggplotly(c)

#class
d<-ggplot(train,aes(class))+geom_bar(position = "dodge")+ggtitle("Different types of class ")
ggplotly(d)

#Travle_type
e<-ggplot(train,aes(type_of_travel))+geom_bar(position = "dodge")+ggtitle("Different types of travel ")
ggplotly(e)

#Satisfaction vs cleanliness
f<-ggplot(train,aes(cleanliness,fill=satisfaction))+geom_histogram()+ggtitle("Cleanliness vs Satisfaction ")
ggplotly(f)

#Gender
#Satisfaction vs cleanliness
g<-ggplot(train,aes(gender))+geom_bar(position = "dodge")+ggtitle("Female vs Male passenger count on plane ")
ggplotly(g)

#decision tree
str(train)
#Converting character variables to factor
library(dplyr)
train_tree <- train %>% mutate_if(is.character,as.factor)
str(train_tree)

#Building model
library(rpart)
library(rpart.plot)
fit <- rpart(satisfaction~., data = train_tree, method = 'class')
rpart.plot(fit, extra = 100)

#Predicting test data
test_tree <- test %>% mutate_if(is.character,as.factor)
str(test_tree)

predict_tree <-predict(fit, test, type = 'class')

table_mat_tree <- table(test$satisfaction, predict_tree)
table_mat_tree


#Accuracy
accuracy_Test_tree <- sum(diag(table_mat_tree)) / sum(table_mat_tree)
#sum(diag(table_mat_tree)) - sum of diagonal
#sum(table_mat_tree) - sum of matrix

print(paste('Accuracy for test for tree is', accuracy_Test_tree))

#KNN
str(train)
summary(train)
#Normalise data
##Generate a random number that is 90% of the total number of rows in train dataset
ran <- sample(1:nrow(train), 0.9 * nrow(train))
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on coulumns of dataset which are the predictors
train_norm <- as.data.frame(lapply(train[,c(3,6:22)], nor))
test_norm <- as.data.frame(lapply(test[,c(3,6:22)], nor))

summary(train_norm)
train_new <- train_norm[ran,]
test_new <- test_norm[-ran,]



##extract 23rd column (satisfaction) of train dataset because it will be used as 'cl' argument in knn function.
train_target_category <- train[ran,23]
##extract 23rd column if test dataset to measure the accuracy
test_category <- test[-ran,23]


library(class)

k <- knn(train_new,test_new,cl=train_target_category,k=13)

##create confusion matrix
tab <- table(k,test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy_Test_knn <- sum(diag(tab)) / sum(tab)
print(paste('Accuracy for test for knn is', accuracy_Test_knn))
