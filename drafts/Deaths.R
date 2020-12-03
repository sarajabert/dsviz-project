library(sf)
library(dplyr)
library(ggplot2)
library("RColorBrewer")

continents <- st_read("data/GoTRelease/Continents.shp", crs=4326)

islands <- st_read("data/GoTRelease/Islands.shp", crs=4326)

lakes <- st_read("data/GoTRelease/Lakes.shp", crs=4326)

land <- st_read("data/GoTRelease/Land.shp", crs=4326)

landscape <- st_read("data/GoTRelease/Landscape.shp", crs=4326)

locations <- st_read("data/GoTRelease/Locations.shp", crs=4326)

kingdoms <- st_read("data/GoTRelease/Political.shp", crs=4326)

regions <- st_read("data/GoTRelease/Regions.shp", crs=4326)

rivers <- st_read("data/GoTRelease/Rivers.shp", crs=4326)

roads <- st_read("data/GoTRelease/Roads.shp", crs=4326)

scenesLocations <- st_read("data/GoTRelease/ScenesLocations.shp", crs=4326)

wall <- st_read("data/GoTRelease/Wall.shp", crs=4326)


colforest="#c0d7c2"
colriver="#7ec9dc"
collake="#87cdde"
colland="ivory"
borderland = "ivory3"

library(readr)
appearances = read_csv("./data/appearances.csv")
characters = read_csv("./data/characters.csv")
episodes = read_csv("./data/episodes.csv")
scenes = read_csv("./data/scenes.csv")

scenes_loc = st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326)

# ************************************************************************************************************
# 1st FIGURE : no parameter (or maybe the color of points)
# ************************************************************************************************************
# Display the map with the "deaths" locations
# The size of the circles corresponds to the number of deaths at this location
colorArea = "pink"
nbDeathsPerLocation <- scenes %>% group_by(location) %>% summarize(nbdeath = sum(nbdeath)) %>% filter(nbdeath != 0) %>% left_join(scenes_loc) %>% st_as_sf()

locationsOfDeath <- scenes %>% group_by(location) %>% summarize(nbdeath = sum(nbdeath)) %>%
           left_join(scenes_loc) %>% left_join(locations, by = c("location"="name")) %>% select(location, geometry.x) %>% st_as_sf()


ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1) +
  geom_sf(data=lakes,fill=collake,col=collake,size=0.1) +
  geom_sf(data=rivers,fill=colriver,col=colriver,size=0.1) +
  geom_sf(data=landscape %>% filter(type=="forest"),fill=colforest,col=colforest)+
  geom_sf(data=wall,col="black",size=1)+
  geom_sf(data=nbDeathsPerLocation, aes(size=nbdeath), color=colorArea)+
  scale_size_area("Number of deaths")+
  geom_sf_text(data=locationsOfDeath %>% filter(location != "The Dothraki Sea" & location != "The Narrow Sea"), aes(label=location),size=2.5,family="Palatino", fontface="italic", 
               nudge_y = -1)+
  theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
  theme(panel.background = element_rect(fill = colriver,color=NA), plot.title = element_text(size=22, color= "red", face="bold", hjust = 0.5))+
  labs(title = "Location of deaths", x="",y="")


# ************************************************************************************************************
# 2nd FIGURE : can be used with a parameter corresponding to the number of "best killers" we want to display
# ************************************************************************************************************
# Displays the n characters that have killed the most.
n = 3
sumKilledByChar10first <- characters %>% group_by(killedBy) %>% summarize(nbKilled = n()) %>% filter(!is.na(killedBy)) %>%
  arrange(desc(nbKilled)) %>% head(n)

sumKilledFemaleByChar <- characters %>% filter(sex == "female") %>% group_by(killedBy, sex) %>% 
  summarize(nbKilled = n()) %>% filter(!is.na(killedBy))
sumKilledMaleByChar <- characters %>% filter(sex == "male") %>% group_by(killedBy, sex) %>% 
  summarize(nbKilled = n()) %>% filter(!is.na(killedBy))

# Binds male and female rows
sumKilledByCharSex <- rbind(sumKilledFemaleByChar,sumKilledMaleByChar)

# I select the 10 biggest killers
sumKilledByCharSex10Biggest <- subset(sumKilledByCharSex, killedBy %in% sumKilledByChar10first$killedBy)


# Stack bars
ggplot(sumKilledByCharSex10Biggest, aes(x = killedBy,  y = nbKilled, fill=sex)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer("Sex of victims",palette = "Spectral")+
  theme_minimal()+
  theme(panel.background = element_rect(fill ="lightgrey" ,color=NA), plot.title = element_text(size=22, color= "red", face="bold", hjust = 0.5))+
  scale_x_discrete(guide = guide_axis(angle = 30))+
  scale_y_continuous(breaks = seq(0,13,by = 2))+
  labs(title = "Best killers", x="", y="Number of victims")

# ************************************************************************************************************
# 3rd FIGURE : can be used with a parameter corresponding to the number of seasons we want to display
#              and a parameter for the palette
# ************************************************************************************************************
# Displays the number of deaths by episode of season

numberSeasons = c(1, 2, 3, 5, 6, 8)
enrichedSeasonNum = c("1" = "Season 1",
                      "2" = "Season 2",
                      "3" = "Season 3",
                      "4" = "Season 4",
                      "5" = "Season 5",
                      "6" = "Season 6",
                      "7" = "Season 7",
                      "8" = "Season 8")
palette = brewer.pal(n = 8, name = "RdPu") # Set3 BuPu Blues Pastel2 RdPu

nbDeathsPerEpisodeOfSeasons <- scenes %>% group_by(episodeId) %>% summarize(nbdeath = sum(nbdeath)) %>% left_join(episodes)

filterBySeason <- nbDeathsPerEpisodeOfSeasons %>% filter(seasonNum %in% numberSeasons)

ggplot(filterBySeason) +
  geom_col(aes(y=nbdeath,x=episodeNum, fill=factor(seasonNum))) +
  scale_fill_manual(values=palette, name="") + guides(fill = FALSE)+
  facet_wrap(~ seasonNum, nrow=2,labeller = labeller(seasonNum = 
                                                      enrichedSeasonNum))+
  scale_x_continuous(breaks = seq(0,10,by = 1))+
  scale_y_continuous(breaks = seq(0,35,by = 10))+
  theme_minimal()+
  theme(panel.background = element_rect(fill ="lightgray" ,color=NA), plot.title = element_text(size=22, color= "red", face="bold", hjust = 0.5))+
  labs(title = "Number of deaths by episodes", x="Episode number", y="Number of deaths")
