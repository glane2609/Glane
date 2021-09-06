remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",port = 4445L,browserName = "chrome")
remDr$open()
remDr$navigate("https://www.premierleague.com/players?se=418&cl=11")
library(tidyverse)
library(rvest)
W1 <- xml2::read_html(remDr$getPageSource()[[1]]) %>% rvest::html_nodes(".hide-s:nth-child(2) , .playerName")%>% html_text()  
W1 <- W[2:49]

W1<-gsub(" " , "__", W1 , fixed = TRUE)
W1 <- data.frame(W1)

W1_even<-W1 %>% filter(row_number() %% 2 == 0) ## Select even rows
W1_odd<-W1 %>% filter(row_number() %% 2 == 1) ## Select odd rows
view(W1_even)
view(W1_odd)
colnames(W1_even) <- "Position"
colnames(W1_odd) <- "Name"

final_W1 <- cbind(W1_odd,W1_even)

write.xlsx(final_arsenal, file="FPL.xlsx", sheetName="Arsenal", row.names=FALSE)
write.xlsx(final_AstonVilla, file="FPL.xlsx", sheetName="Aston Villa",append=TRUE, row.names=FALSE)
write.xlsx(final_Brentford, file="FPL.xlsx", sheetName="Brentford",append=TRUE, row.names=FALSE)
write.xlsx(final_Brighton, file="FPL.xlsx", sheetName="Brighton",append=TRUE, row.names=FALSE)
write.xlsx(final_Burnley, file="FPL.xlsx", sheetName="Burnley",append=TRUE, row.names=FALSE)
write.xlsx(final_Chelsea, file="FPL.xlsx", sheetName="Chelsea",append=TRUE, row.names=FALSE)
write.xlsx(final_CP, file="FPL.xlsx", sheetName="Crystal Palace",append=TRUE, row.names=FALSE)
write.xlsx(final_EFC, file="FPL.xlsx", sheetName="Everton",append=TRUE, row.names=FALSE)
write.xlsx(final_LUFC, file="FPL.xlsx", sheetName="Leeds United",append=TRUE, row.names=FALSE)
write.xlsx(final_LCFC, file="FPL.xlsx", sheetName="Leicester City",append=TRUE, row.names=FALSE)
write.xlsx(final_LFC, file="FPL.xlsx", sheetName="Liverpool",append=TRUE, row.names=FALSE)
write.xlsx(final_W1, file="FPL.xlsx", sheetName="Man City",append=TRUE, row.names=FALSE)
write.xlsx(final_MUFC, file="FPL.xlsx", sheetName="Man United",append=TRUE, row.names=FALSE)
write.xlsx(final_NUFC, file="FPL.xlsx", sheetName="Newcastle United",append=TRUE, row.names=FALSE)
write.xlsx(final_NCFC, file="FPL.xlsx", sheetName="Norwich City",append=TRUE, row.names=FALSE)
write.xlsx(final_Soton, file="FPL.xlsx", sheetName="Southampton",append=TRUE, row.names=FALSE)
write.xlsx(final_THFC, file="FPL.xlsx", sheetName="Tottenham",append=TRUE, row.names=FALSE)
write.xlsx(final_Wat, file="FPL.xlsx", sheetName="Watford",append=TRUE, row.names=FALSE)
write.xlsx(final_Whu, file="FPL.xlsx", sheetName="West Ham",append=TRUE, row.names=FALSE)
write.xlsx(final_W, file="FPL.xlsx", sheetName="Wolves",append=TRUE, row.names=FALSE)




