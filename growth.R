library(xlsx)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
data<-read.xlsx("Pilot Experiment.xlsx",1)
View(data)
data<-data[-c(70:1035),]
data <- na.omit(data)

data <- filter(data, COH != "na" & Meat1 != "na")
data$Meat1 <- as.numeric(data$Meat1)
data$COH <- as.numeric(data$COH)

mod <- aov(Day ~ ., data = data)

summary(mod)
str(data)

#As you can see through summary Diets named OKA,RIB,SBM and CF_70% have p value more than significance level of 0.05.
#This means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

data_new<-data %>% group_by(Day)%>%summarise(across(Meat2:CF_70,mean))
View(data_new)

data_new %>% 
  rownames_to_column() %>% 
  mutate(Day = factor(Day)) %>% 
  gather(key = Diet, value = value, Meat2:CF_70) %>% 
  ggplot(aes(as.numeric(Day), value, color = Diet)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(name = "Day") +
  theme_bw()+ylab("Length of larva (cm)")


