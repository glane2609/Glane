#docker run -d -p 4445:4444 selenium/standalone-chrome ------> run it in terminal after starting docker application
#docker ps  -----> to check if it has started

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",port = 4445L,browserName = "chrome")
remDr$open()
remDr$navigate("https://fbref.com/en/comps/22/passing/Major-League-Soccer-Stats")
library(tidyverse)
library(rvest)
mls <- xml2::read_html(remDr$getPageSource()[[1]]) %>% rvest::html_nodes("#div_stats_squads_passing_for")%>% html_table()  

mls <- as.data.frame(mls)
colnames(mls) <- mls[1,]
mls <- mls[-1, ] 
View(mls)
mls <- mls[,-c(5:25)]
mls <- subset(mls,select = c("Squad","90s","Cmp"))
colnames(mls)[3] <- "Total_passes_completed"
colnames(mls)[2] <- "Games_Played"

str(mls)
mls$Games_Played <- as.numeric(mls$Games_Played)
mls$Total_passes_completed <- as.numeric(mls$Total_passes_completed)

options(repr.plot.width=8, repr.plot.height=3)
ggplot(mls, aes(x = Squad, y = Total_passes_completed, main="Pass completion")) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_y_continuous(name="Passes completed : min 18 matches") +
  scale_x_discrete(name="Teams") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))+geom_text(aes(label=Total_passes_completed),hjust = - 0.4)

