library(caTools)
library(ggplot2)
library(dplyr)
#Loading the data
data1 <- read.csv("C:/Users/glane/Downloads/baseball/baseball.csv", header=T)
data2 <- read.csv("C:/Users/glane/Downloads/baseball/weather.csv", header=T)

View(data1)
View(data2)

#Merging baseball and weather
data <- merge(data1 , data2)
View(data)
#To remove NA's (missing values denoted with NA) from the data
data <- na.omit(data)

data_new <- subset(data,season <= 2014, date <= "31-12-2014" )
data_new <- na.omit(data_new)
View(data_new)
sample.split(data_new$attendance,SplitRatio = 0.65)-> split_tag
subset(data_new, split_tag==T)->train
subset(data_new, split_tag==F)->test
nrow(train)
nrow(test)


#Running multiple linear regression model on trained data
lm(attendance~season+opponent+event_id+event_date+event_time+win+tickets_sold+tickets_resale+opponent_payroll+opponent_rank+opponent_win_pcnt+braves_payroll+braves_rank+braves_win_pcnt+mean_temp+weather,data=train)-> model
summary(model)

#Predict the model based on test data
predict(model, newdata=test)->predicted_multi_linear
head(predicted_multi_linear)
View(predicted_multi_linear)
#You'll get 5 random predicted values of attendance via head function 
#You can also use View function to view the whole predicted values

#combine the actual values of attendance with the predicted ones
cbind(Actual=test$attendance, Predicted=predicted_multi_linear)-> final_data
as.data.frame(final_data)->final_data
class(final_data)
head(final_data)

#This helps us in getting the error ( difference)
final_data$Actual - final_data$Predicted ->error
View(error)

#Graph for a model with tickets_sold
ggplot(data= final_data, aes(x=Predicted, y=Actual)) + geom_point()
ggplot(data= final_data, aes(x=Predicted, y=error)) + geom_point()

#Building a model without tickets_sold variable
lm(attendance~season+opponent+event_id+event_date+event_time+win+tickets_resale+opponent_payroll+opponent_rank+opponent_win_pcnt+braves_payroll+braves_rank+braves_win_pcnt+mean_temp+weather,data=train)-> model1
summary(model1)

#Predict the model based on test data
predict(model1, newdata=test)->predicted_multi_linear1
head(predicted_multi_linear1)
View(predicted_multi_linear1)
#You'll get 5 random predicted values of attendance via head function 
#You can also use View function to view the whole predicted values

#combine the actual values of attendance with the predicted ones
cbind(Actual1=test$attendance, Predicted1=predicted_multi_linear1)-> final_data1
as.data.frame(final_data1)->final_data1
class(final_data1)
head(final_data1)

#This helps us in getting the error ( difference)
final_data1$Actual1 - final_data1$Predicted1 ->error1
View(error1)

#Graph for a model without tickets_sold
ggplot(data= final_data1, aes(x=Predicted1, y=Actual1)) + geom_point()
ggplot(data= final_data1, aes(x=Predicted1, y=error1)) + geom_point()



#For 2015 2016
data_new1 <- subset(data,season > 2014)
View(data_new1)
data_new1 <- na.omit(data_new1)
sample.split(data_new1$attendance,SplitRatio = 0.70)-> split_tag
subset(data_new1, split_tag==T)->train1
subset(data_new1, split_tag==F)->test1
nrow(train1)
nrow(test1)

#Running multiple linear regression model on trained data
lm(attendance~season+opponent+event_id+event_date+event_time+win+tickets_sold+tickets_resale+opponent_payroll+opponent_rank+opponent_win_pcnt+braves_payroll+braves_rank+braves_win_pcnt+mean_temp+weather,data=train1)-> model2
summary(model2)

#Predict the model based on test data
predict(model2, newdata=test1)->predicted_multi_linear2
head(predicted_multi_linear2)
View(predicted_multi_linear2)
#You'll get 5 random predicted values of attendance via head function 
#You can also use View function to view the whole predicted values

#combine the actual values of attendance with the predicted ones
cbind(Actual2=test1$attendance, Predicted2=predicted_multi_linear2)-> final_data2
as.data.frame(final_data2)->final_data2
class(final_data2)
head(final_data2)

#This helps us in getting the error ( difference)
final_data2$Actual2 - final_data2$Predicted2 ->error2
View(error2)

#Graph for a model with tickets_sold (2015- 2016)
ggplot(data= final_data2, aes(x=Predicted2, y=Actual2)) + geom_point()
ggplot(data= final_data2, aes(x=Predicted2, y=error2)) + geom_point()

#Building the model for 2015-2016 without ticket sold
lm(attendance~season+opponent+event_id+event_date+event_time+win+tickets_resale+opponent_payroll+opponent_rank+opponent_win_pcnt+braves_payroll+braves_rank+braves_win_pcnt+mean_temp+weather,data=train1)-> model3
summary(model3)

#Predict the model based on test data
predict(model3, newdata=test1)->predicted_multi_linear3
head(predicted_multi_linear3)
View(predicted_multi_linear3)
#You'll get 5 random predicted values of attendance via head function 
#You can also use View function to view the whole predicted values

#combine the actual values of attendance with the predicted ones
cbind(Actual3=test1$attendance, Predicted3=predicted_multi_linear3)-> final_data3
as.data.frame(final_data3)->final_data3
class(final_data3)
head(final_data3)

#This helps us in getting the error ( difference)
final_data3$Actual3 - final_data3$Predicted3 ->error3
View(error3)

#Graph for a model without tickets_sold (2015- 2016)
ggplot(data= final_data3, aes(x=Predicted3, y=Actual3)) + geom_point()
ggplot(data= final_data3, aes(x=Predicted3, y=error3)) + geom_point()


#Conclusion
#We have come to a conclusion that without variable ticket_sold has no adverse effect on the model 
#and that gives the same graph (a straight line) on all 4 models built .
# 2011- 2014 ( with and without tickets_sold) and 2015-2016 ( with and without tickets_sold)
# Mathermatical linear regression equation  : yi = b0 + b1xi1 + b2xi2 + ... bpxip
#The graph has no outliers