#!/usr/bin/Rscript

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
# library(jwutil)
library(tcltk)


appearances = read_csv("./data/appearances.csv")
characters = read_csv("./data/characters.csv")
episodes = read_csv("./data/episodes.csv")
scenes = read_csv("./data/scenes.csv")

nbFemalePerHouse <- characters %>% filter(sex == "female") %>% group_by(house, sex) %>% 
  summarize(nb = n())%>% filter(!is.na(house))
nbMalePerHouse <- characters %>% filter(sex == "male") %>% group_by(house, sex) %>% 
  summarize(nb = n())%>% filter(!is.na(house))
data <- rbind(nbFemalePerHouse,nbMalePerHouse)

X11()
ggplot(data, aes(x = house,  y = nb, fill=sex)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer("Gender",palette = "Spectral")+
  theme_minimal()+
  theme(panel.background = element_rect(fill ="lightgray" ,color=NA), plot.title = element_text(size=, color= "red", face="bold", hjust = 0.5))+
  scale_x_discrete(guide = guide_axis(angle = 30))+
  scale_y_continuous(breaks = seq(0,13,by = 100))+
  labs(title = "Gender distribution per houses", x="", y="")


numberSeasons = c(1, 2, 3, 5, 6, 8)
  
# nbCharacters = countNotNumeric(characters$name)
nbCharacters = characters %>% filter(!is.na(name)) %>% nrow()
nbSurv <- scenes %>% group_by(episodeId) %>% summarize(nb = (nbCharacters - sum(nbdeath))) %>% left_join(episodes)
survivalPerSeason <- nbSurv %>% filter(seasonNum %in% numberSeasons)
survivals <- cbind.data.frame("Season" = survivalPerSeason$seasonNum,"Survivals" = survivalPerSeason$nb)
dfs <- survivals %>% group_by(Season) %>% summarise(Survivals = sum(Survivals))
propSurvivals <- dfs %>% mutate(Prop = Survivals/nbCharacters)

X11()
ggplot(propSurvivals, aes(x="", y=Prop, fill=factor(Season))) + geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(Prop), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#B833FF", "#FF3333"))+ 
  labs(x = NULL, y = NULL, fill = NULL, title = "Survivals distribution per season")+
  theme_classic() + theme(axis.line = element_blank(),
                            axis.text = element_blank(),
                            axis.ticks = element_blank(),
                            plot.title = element_text(hjust = 0.5, color = "#666666"))

capture <- tk_messageBox(message="")



