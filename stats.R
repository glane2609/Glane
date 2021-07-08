library(readxl)
library(tidyverse)


data <- read_xlsx("C:/Users/glane/Downloads/NRE May 31st.xlsx",sheet = 3)
View(data)

data <- subset(data,select = c("Race","Worst Crime Display" ,"ILD"))

data[is.na(x = data)] <- "No ILD"

# Convert ILD as a factor and recode the levels
# as "1" for ILD and "2" for Non ILD

data$ILD <- factor(data$ILD, 
                   levels = c("ILD", "No ILD"),
                       labels = c(1, 2))
head(data)



# Convert Race as a factor and recode the levels
# as "1" for Black, "2" for White , "3" for Hispanic  , "4" for Asian , "5" for Dont know

data$Race <- factor(data$Race, 
                   levels = c("Black", "White", "Hispanic", "Asian" ,"Don't Know","Native American","Other"),
                   labels = c(1, 2,3,4,5,6,7))
head(data)

data_murder <- data %>% filter(`Worst Crime Display`=="Murder")
View(data_murder)


data_attempted_murder <- data %>% filter(`Worst Crime Display`=="Attempted Murder")
View(data_attempted_murder)

data_robbery <- data %>% filter(`Worst Crime Display`=="Robbery")
View(data_robbery)

data_drugs <- data %>% filter(`Worst Crime Display`=="Drug Possession or Sale")
View(data_drugs)

data_Assault <- data %>% filter(`Worst Crime Display`=="Assault")
View(data_Assault)

data_weapon <- data %>% filter(`Worst Crime Display`=="Weapon Possession or Sale")
View(data_weapon )

data_sexual_assault <- data %>% filter(`Worst Crime Display`=="Child Sex Abuse" | `Worst Crime Display`=="Sexual Assault" | `Worst Crime Display`=="Sex Offender Registration")
View(data_sexual_assault)





#Murder 
data_murder$Race <- as.numeric(as.character(data_murder$Race))
data_murder$ILD <- as.numeric(as.character(data_murder$ILD))
chisq.test(data_murder$Race,data_murder$ILD)

#data_attempted_murder
data_attempted_murder$Race <- as.numeric(as.character(data_attempted_murder$Race))
data_attempted_murder$ILD <- as.numeric(as.character(data_attempted_murder$ILD))
chisq.test(data_attempted_murder$Race,data_attempted_murder$ILD)

#data_drugs
data_drugs$Race <- as.numeric(as.character(data_drugs$Race))
data_drugs$ILD <- as.numeric(as.character(data_drugs$ILD))
chisq.test(data_drugs$Race,data_drugs$ILD)

#data_robbery
data_robbery$Race <- as.numeric(as.character(data_robbery$Race))
data_robbery$ILD <- as.numeric(as.character(data_robbery$ILD))
chisq.test(data_robbery$Race,data_robbery$ILD)

#data_Assault
data_Assault$Race <- as.numeric(as.character(data_Assault$Race))
data_Assault$ILD <- as.numeric(as.character(data_Assault$ILD))
chisq.test(data_Assault$Race,data_Assault$ILD)

#data_weapon
data_weapon$Race <- as.numeric(as.character(data_weapon$Race))
data_weapon$ILD <- as.numeric(as.character(data_weapon$ILD))
chisq.test(data_weapon$Race,data_weapon$ILD)

#data_sexual_assault
data_sexual_assault$Race <- as.numeric(as.character(data_sexual_assault$Race))
data_sexual_assault$ILD <- as.numeric(as.character(data_sexual_assault$ILD))
chisq.test(data_sexual_assault$Race,data_sexual_assault$ILD)
