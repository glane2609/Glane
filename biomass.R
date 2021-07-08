library(xlsx)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
data<-read.xlsx("C:/Users/glane/Downloads/Pilot Experiment.xlsx",2)
View(data)
data<-data[-c(40:1005),-c(14:25)]
data <- na.omit(data)


mod <- aov(Day ~ ., data = data)

summary(mod)
str(data)

#As you can see through summary Diets named CF_70% have p value more than significance level of 0.05.
#This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

#For CF 80% the significance value is less than 0.05 which means it is statistically significant. It indicates strong evidence against the null hypothesis, as there is less than a 5% probability the null is correct (and the results are random). 
#Therefore, we reject the null hypothesis, and accept the alternative hypothesis.

data_new<-data %>% group_by(Day)%>%summarise(across(Mix:Meat2,mean))
View(data_new)

data_new %>% 
  rownames_to_column() %>% 
  mutate(Day = factor(Day)) %>% 
  gather(key = Diet, value = value, Mix:Meat2) %>% 
  ggplot(aes(as.numeric(Day), value, color = Diet)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(name = "Day") +
  theme_bw()+ylab("Weight of larva (gm)")


#Day and CF 80

mod_CF_80 <- aov(Day ~ CF_80, data = data)

summary(mod_CF_80)

#Day and CF 70
mod_CF_70 <- aov(Day ~ CF_70, data = data)

summary(mod_CF_70)

#Checking p value which is less than 0.05 ,both CF_80 and CF_70 are statistically significant. CF_70 is the most significant factor variable. These results would lead us to believe that changing diet methods CF_80 or CF_70, will impact significantly the Days.