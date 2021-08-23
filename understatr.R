library(understatr)
library(ggsoccer)
library(tidyverse)
library(png)
library(grid)

rm = get_team_players_stats("Real Madrid",2020)
dato = c(rm$player_id)
shotx = get_player_shots(dato[1])
Karim <- get_player_matches_stats(2370)
View(Karim)
View(shotx)



Karimstats <- Karim %>% select(goals,shots,year,match_id) %>% filter(year == "2020" & goals >  0)
View(Karimstats)

Karimshotdata = shotx %>% select(result,X,Y, year,match_id)  %>% filter(year == '2020')
View(Karimshotdata)

final <- left_join(Karimstats,Karimshotdata,by = "match_id")
final <- subset(final,select = -c(year.x,year.y))
str(final)
final$X <- 100 * final$X
final$Y <- 100 * final$Y

ABC <- final %>% filter(match_id == '15154')

Karim_img <- png::readPNG("C:/Users/glane/Downloads/karim-benzema-50.png")
ggplot(final)+
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  theme_pitch()+
  theme(panel.background = element_rect(fill = "springgreen4"))+
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112))+ annotation_custom(rasterGrob(Karim_img))+
  geom_point(aes(x=X,y=Y,size = goals, color = result == "Goal" )) 
