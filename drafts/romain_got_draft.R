#!/usr/bin/Rscript

#library(readr)
library(dplyr)
library(tidyr)
library(tcltk)
library(ggplot2)
library(sf)
library(cartography)

# characters = read_csv("data/characters.csv")
# episodes = read_csv("data/episodes.csv")
# scenes = read_csv("data/scenes.csv")
# appearances = read_csv("data/appearances.csv")

locations=st_read("data/GoTRelease/Locations.shp", crs=4326)
lakes=st_read("data/GoTRelease/Lakes.shp", crs=4326)
conts=st_read("data/GoTRelease/Continents.shp", crs=4326)
land=st_read("data/GoTRelease/Land.shp", crs=4326)
wall=st_read("data/GoTRelease/Wall.shp", crs=4326)
islands=st_read("data/GoTRelease/Islands.shp", crs=4326)
kingdoms=st_read("data/GoTRelease/Political.shp", crs=4326)
landscapes=st_read("data/GoTRelease/Landscape.shp", crs=4326)
roads=st_read("data/GoTRelease/Roads.shp", crs=4326)
rivers=st_read("data/GoTRelease/Rivers.shp", crs=4326)
scenes_locations=st_read("data/GoTRelease/ScenesLocations.shp", crs=4326)

landscapes = landscapes %>%
  mutate(continent=replace(continent, type=="swamp", "Westeros"))

islands = islands %>%
  mutate(continent=replace(continent, continent=="Sothyrios", "Sothoryos")) %>%
  mutate(continent=replace(continent, is.na(continent), "Essos"))

wall = bind_cols(
  wall,
  data.frame(continent=("Westeros"))
)

geo = landscapes %>%
  mutate(area=st_area(geometry)) %>% 
  group_by(continent) %>%
  count(type,wt=as.numeric(area)) %>% 
  st_drop_geometry() %>% 
  full_join(
    bind_cols(
      data.frame(type=c("islands")),
      islands %>%
        mutate(area=st_area(geometry)) %>%
        group_by(continent) %>%
        count(wt=as.numeric(area)) %>%
        st_drop_geometry()
    )
  ) %>%
  full_join(
    bind_cols(
      data.frame(type=c("lakes")),
      lakes %>%
        mutate(area=st_area(geometry)) %>%
        group_by(continent) %>%
        count(wt=as.numeric(area)) %>%
        st_drop_geometry()
    )
  ) %>%
  full_join(
    bind_cols(
      data.frame(type=c("rivers")),
      rivers %>%
        mutate(length=st_length(geometry)) %>%
        group_by(continent) %>%
        #https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2013WR015068
        count(wt=as.numeric(length*135.4)) %>%
        st_drop_geometry()
    )
  ) %>%
  full_join(
    bind_cols(
      data.frame(type=c("roads")),
      roads %>%
        mutate(length=st_length(geometry)) %>%
        group_by(continent) %>%
        #https://fr.wikipedia.org/wiki/Profil_en_travers_(route)
        count(wt=as.numeric(length*3.5)) %>%
        st_drop_geometry()
    )
  ) %>%
  full_join(
    bind_cols(
      data.frame(type=c("wall"), continent=("Westeros")),
      wall %>%
        mutate(length=st_length(geometry)) %>%
        #https://gameofthrones.fandom.com/wiki/Wall
        count(wt=as.numeric(length*91)) %>%
        st_drop_geometry()
    )
  ) %>%
  arrange(continent, type) %>%
  filter(!is.na(continent))

#geo %>% print(n = Inf)

##GRAPH 1
X11()
ggplot(data = geo, aes(x = type, y = n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_brewer("Geography", palette = "Spectral") +
  geom_text(aes(label = paste(signif(n, 2), "m2")), hjust = -0.25, size=4) +
  facet_grid(continent ~ ., scales = "free_y") +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.border = element_rect(fill = NA, colour = "black")) +
  scale_y_log10("Surface area (m2)") +
  xlab("") +
  coord_flip(ylim = c(1e07, 1e13)) +
  ggtitle("Landscapes by continents and surface area, logarithmic scale.")

###GRAPH 2
landmass = conts %>%
  mutate(area=st_area(geometry)) %>%
  count(name,wt=as.numeric(area)) %>%
  st_drop_geometry()
landmass <- landmass %>%
  arrange(desc(name)) %>%
  mutate(prop = n / sum(landmass$n) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

X11()
ggplot(landmass, aes(x="", y=prop, fill=name)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = name), color = "black", size=6) +
  scale_fill_brewer(palette="Spectral") +
  ggtitle("Total landmass in the GoT universe.")
###

colforest="#c0d7c2"
colmountain="#92817a"
colstepp="#f7d1ba"
colswamp="#776d8a"
colroad="#383e56"
colwater="#7ec9dc"
colland="ivory"
colborderland="ivory3"
colsymbol="#d35c61"


continent_names = c("Westeros", "Essos", "Sothoryos")
for(continent_name in continent_names){
  ###GRAPH 3
  X11()
  plot(st_geometry(conts %>% filter(name==continent_name)), col=colland, border=colborderland, bg=colwater)
  plot(st_geometry(landscapes %>% filter(type=="forest" & continent==continent_name)), col=colforest, border=colforest, lwd=0.5, add=T)
  plot(st_geometry(landscapes %>% filter(type=="mountain" & continent==continent_name)), col=colmountain, border=colmountain, lwd=0.5, add=T)
  plot(st_geometry(landscapes %>% filter(type=="stepp" & continent==continent_name)), col=colstepp, border=colstepp, lwd=0.5, add=T)
  plot(st_geometry(landscapes %>% filter(type=="swamp" & continent==continent_name)), col=colswamp, border=colswamp, lwd=0.5, add=T)
  plot(st_geometry(islands %>% filter(continent==continent_name)), col=colland, border=colborderland, lwd=0.5, add=T)
  plot(st_geometry(lakes %>% filter(continent==continent_name)), col=colwater, border=colwater, lwd=0.5, add=T)
  plot(st_geometry(rivers %>% filter(continent==continent_name)), col=colwater, border=colwater, lwd=0.5, add=T)
  plot(st_geometry(roads %>% filter(continent==continent_name)), col=colroad, border=colroad, lwd=1, add=T)
  plot(st_geometry(wall %>% filter(continent==continent_name)), col="black", border="black", lwd=1, add=T)
  north(pos="topleft", col=colsymbol)
  layoutLayer(title=continent_name,
              col=colsymbol, coltitle="white", tabtitle=TRUE,
              frame=TRUE, scale=NULL, north=FALSE)
  title(paste0("The continent of ",continent_name,".")) 
  ###
}

###GRAPH 4
X11()
plot(st_geometry(conts), col=colland, border=colborderland, bg=colwater)
plot(st_geometry(landscapes %>% filter(type=="forest")), col=colforest, border=colforest, lwd=0.5, add=T)
plot(st_geometry(landscapes %>% filter(type=="mountain")), col=colmountain, border=colmountain, lwd=0.5, add=T)
plot(st_geometry(landscapes %>% filter(type=="stepp")), col=colstepp, border=colstepp, lwd=0.5, add=T)
plot(st_geometry(landscapes %>% filter(type=="swamp")), col=colswamp, border=colswamp, lwd=0.5, add=T)
plot(st_geometry(islands), col=colland, border=colborderland, lwd=0.5, add=T)
plot(st_geometry(lakes), col=colwater, border=colwater, lwd=0.5, add=T)
plot(st_geometry(rivers), col=colwater, border=colwater, lwd=0.5, add=T)
plot(st_geometry(roads), col=colroad, border=colroad, lwd=1, add=T)
plot(st_geometry(wall), col="black", border="black", lwd=1, add=T)
plot(st_geometry(locations), border=NA, lwd=0.5, add=T)
propSymbolsLayer(locations, var="size", col=colsymbol, inches=0.15,
                 border="black", lwd=0.7, symbols="square",
                 legend.style="e", legend.pos="topright", 
                 legend.title.txt="Location size",
                 legend.values.rnd=0)
north(pos="topleft", col=colsymbol)
layoutLayer(title="Known locations by size.",
            col=colsymbol, coltitle="white", tabtitle=TRUE,
            frame=TRUE, scale=NULL, north=FALSE)
title("Civilization of the know world.")
###

###GRAPH 5
X11()
locations_xy = data.frame(x=st_coordinates(st_centroid(st_geometry(locations)))[,1],
                          y=st_coordinates(st_centroid(st_geometry(locations)))[,2])
plot.new()
plot(
  ggplot() + 
  geom_sf(data=land, fill=colland, col=colborderland, size=0.1) +
  geom_sf(data=landscapes %>% filter(type=="forest"), fill=colforest, col=colforest) +
  geom_sf(data=landscapes %>% filter(type=="mountain"), fill=colmountain, col=colmountain) +
  geom_sf(data=landscapes %>% filter(type=="stepp"), fill=colstepp, col=colstepp) +
  geom_sf(data=landscapes %>% filter(type=="swamp"), fill=colswamp, col=colswamp) +
  geom_sf(data=islands, fill=colland, col=colborderland) +
  geom_sf(data=lakes, col=colwater, fill=colwater) +
  geom_sf(data=rivers, col=colwater) +
  geom_sf(data=roads, col=colroad) +
  geom_sf(data=wall, col="black", size=1) + 
  geom_point(data = locations_xy, aes(x=x, y=y), color="black", size=3) +
    geom_point(data = locations_xy, aes(x=x, y=y), color=colsymbol, size=2) +
  stat_density2d(data = locations_xy, aes(x=x, y=y), color="black") +
  theme_minimal() +
  coord_sf(expand=0, ndiscr=0) + 
  xlab("") +
  ylab("") +
  theme(panel.background=element_rect(fill=colwater, color=NA)) +
  theme(panel.border = element_rect(colour = colsymbol, fill=NA, size=1)) +
  theme(plot.margin = unit(c(0.75,0.75,0.75,0.75), "in")),
  #labs(title="Population density.", caption="", x="", y=""),
  add=T)
#north(pos="topleft", col=colsymbol)
# layoutLayer(title="Known locations by size.",
#             col=colsymbol, coltitle="white", tabtitle=TRUE,
#             frame=TRUE, scale=NULL, north=FALSE)
title("Population density of the know world.")

capture <- tk_messageBox(message="")

# #####characters
# 
# ── Column specification ────────────────────────────────────────────────────────────────────────────────────
# cols(
#   name = col_character(),
#   sex = col_character(),
#   house = col_character(),
#   killedBy = col_character(),
#   image = col_character()
# )
# 
# 
# ###summary
# name               sex               house             killedBy        
# Length:587         Length:587         Length:587         Length:587        
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# image          
# Length:587        
# Class :character  
# Mode  :character  
# 
# ###str
# tibble [587 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
# $ name    : chr [1:587] "Addam Marbrand" "Adrack Humble" "Aeron Greyjoy" "Aerys Targaryen" ...
# $ sex     : chr [1:587] "male" "male" "male" "male" ...
# $ house   : chr [1:587] NA NA "Greyjoy" NA ...
# $ killedBy: chr [1:587] NA NA NA NA ...
# $ image   : chr [1:587] NA NA "https://images-na.ssl-images-amazon.com/images/M/MV5BNzI5MDg0ZDAtN2Y2ZC00MzU1LTgyYjQtNTBjYjEzODczZDVhXkEyXkFqcG"| __truncated__ NA ...
# - attr(*, "spec")=
#   .. cols(
#     ..   name = col_character(),
#     ..   sex = col_character(),
#     ..   house = col_character(),
#     ..   killedBy = col_character(),
#     ..   image = col_character()
#     .. )
# 
# 
# #####episodes
# 
# ── Column specification ────────────────────────────────────────────────────────────────────────────────────
# cols(
#   episodeTitle = col_character(),
#   episodeNum = col_double(),
#   seasonNum = col_double(),
#   episodeId = col_double(),
#   total_duration = col_double()
# )
# 
# 
# ###summary
# episodeTitle         episodeNum       seasonNum       episodeId 
# Length:73          Min.   : 1.000   Min.   :1.000   Min.   : 1  
# Class :character   1st Qu.: 3.000   1st Qu.:2.000   1st Qu.:19  
# Mode  :character   Median : 5.000   Median :4.000   Median :37  
# Mean   : 5.192   Mean   :4.205   Mean   :37  
# 3rd Qu.: 7.000   3rd Qu.:6.000   3rd Qu.:55  
# Max.   :10.000   Max.   :8.000   Max.   :73  
# total_duration
# Min.   :2696  
# 1st Qu.:2969  
# Median :3142  
# Mean   :3204  
# 3rd Qu.:3279  
# Max.   :4510  
# 
# ###str
# tibble [73 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
# $ episodeTitle  : chr [1:73] "Winter Is Coming" "The Kingsroad" "Lord Snow" "Cripples, Bastards, and Broken Things" ...
# $ episodeNum    : num [1:73] 1 2 3 4 5 6 7 8 9 10 ...
# $ seasonNum     : num [1:73] 1 1 1 1 1 1 1 1 1 1 ...
# $ episodeId     : num [1:73] 1 2 3 4 5 6 7 8 9 10 ...
# $ total_duration: num [1:73] 3509 3142 3257 3164 3073 ...
# - attr(*, "spec")=
#   .. cols(
#     ..   episodeTitle = col_character(),
#     ..   episodeNum = col_double(),
#     ..   seasonNum = col_double(),
#     ..   episodeId = col_double(),
#     ..   total_duration = col_double()
#     .. )
# 
# 
# #####scenes
# 
# ── Column specification ────────────────────────────────────────────────────────────────────────────────────
# cols(
#   sceneStart = col_time(format = ""),
#   sceneEnd = col_time(format = ""),
#   location = col_character(),
#   subLocation = col_character(),
#   episodeId = col_double(),
#   duration = col_double(),
#   nbc = col_double(),
#   sceneId = col_double(),
#   nbdeath = col_double()
# )
# 
# 
# ###summary
# sceneStart         sceneEnd          location         subLocation       
# Length:3840       Length:3840       Length:3840        Length:3840       
# Class1:hms        Class1:hms        Class :character   Class :character  
# Class2:difftime   Class2:difftime   Mode  :character   Mode  :character  
# Mode  :numeric    Mode  :numeric                                         
# 
# 
# episodeId        duration           nbc            sceneId      
# Min.   : 1.00   Min.   :  1.00   Min.   : 1.000   Min.   :   1.0  
# 1st Qu.:27.00   1st Qu.: 14.00   1st Qu.: 2.000   1st Qu.: 960.8  
# Median :46.00   Median : 36.00   Median : 2.000   Median :1920.5  
# Mean   :44.18   Mean   : 60.91   Mean   : 3.155   Mean   :1920.5  
# 3rd Qu.:65.00   3rd Qu.: 86.00   3rd Qu.: 4.000   3rd Qu.:2880.2  
# Max.   :73.00   Max.   :661.00   Max.   :24.000   Max.   :3840.0  
# nbdeath       
# Min.   :0.00000  
# 1st Qu.:0.00000  
# Median :0.00000  
# Mean   :0.09792  
# 3rd Qu.:0.00000  
# Max.   :7.00000  
# 
# ###str
# tibble [3,840 × 9] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
# $ sceneStart : 'hms' num [1:3840] 00:00:40 00:01:45 00:03:24 00:03:31 ...
# ..- attr(*, "units")= chr "secs"
# $ sceneEnd   : 'hms' num [1:3840] 00:01:45 00:03:24 00:03:31 00:03:38 ...
# ..- attr(*, "units")= chr "secs"
# $ location   : chr [1:3840] "The Wall" "North of the Wall" "North of the Wall" "North of the Wall" ...
# $ subLocation: chr [1:3840] "Castle Black" "The Haunted Forest" "The Haunted Forest" "The Haunted Forest" ...
# $ episodeId  : num [1:3840] 1 1 1 1 1 1 1 1 1 1 ...
# $ duration   : num [1:3840] 65 99 7 7 112 5 7 10 23 18 ...
# $ nbc        : num [1:3840] 3 3 2 1 3 1 2 1 3 1 ...
# $ sceneId    : num [1:3840] 1 2 3 4 5 6 7 8 9 10 ...
# $ nbdeath    : num [1:3840] 0 0 1 0 0 0 0 0 1 0 ...
# - attr(*, "spec")=
#   .. cols(
#     ..   sceneStart = col_time(format = ""),
#     ..   sceneEnd = col_time(format = ""),
#     ..   location = col_character(),
#     ..   subLocation = col_character(),
#     ..   episodeId = col_double(),
#     ..   duration = col_double(),
#     ..   nbc = col_double(),
#     ..   sceneId = col_double(),
#     ..   nbdeath = col_double()
#     .. )
# 
# 
# #####appearances
# 
# ── Column specification ────────────────────────────────────────────────────────────────────────────────────
# cols(
#   name = col_character(),
#   sceneId = col_double()
# )
# 
# 
# ###summary
# name              sceneId    
# Length:12114       Min.   :   1  
# Class :character   1st Qu.: 937  
# Mode  :character   Median :1842  
# Mean   :1859  
# 3rd Qu.:2820  
# Max.   :3840  
# 
# ###str
# tibble [12,114 × 2] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
# $ name   : chr [1:12114] "Gared" "Waymar Royce" "Will" "Gared" ...
# $ sceneId: num [1:12114] 1 1 1 2 2 2 3 3 4 5 ...
# - attr(*, "spec")=
#   .. cols(
#     ..   name = col_character(),
#     ..   sceneId = col_double()
#     .. )

# ##################### locations[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  247 obs. of  6 variables:
#   $ id       : num  1 2 3 4 5 6 7 8 9 10 ...
# $ name     : Factor w/ 238 levels "Acorn Hall","Antlers",..: 91 70 154 126 18 176 209 74 39 2 ...
# $ size     : num  5 3 3 1 1 3 4 2 4 3 ...
# $ confirmed: num  1 0 1 0 0 1 1 1 1 1 ...
# $ type     : Factor w/ 5 levels "Castle","City",..: 2 1 1 3 3 1 5 4 5 1 ...
# $ geometry :sfc_POINT of length 247; first list element:  'XY' num  19.08 3.95
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA
# ..- attr(*, "names")= chr  "id" "name" "size" "confirmed" ...
# 
# 
# ##################### lakes[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  19 obs. of  4 variables:
#   $ id       : num  1 2 3 4 5 6 7 8 9 10 ...
# $ name     : Factor w/ 6 levels "Dagger Lake",..: 2 4 NA NA NA 3 NA NA NA NA ...
# $ continent: Factor w/ 2 levels "Essos","Westeros": 2 2 2 2 2 2 2 2 2 2 ...
# $ geometry :sfc_POLYGON of length 19; first list element: List of 1
# ..$ : num [1:36, 1:2] 16.7 17.1 17.1 17.2 17.4 ...
# ..- attr(*, "class")= chr  "XY" "POLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA
# ..- attr(*, "names")= chr  "id" "name" "continent"
# 
# 
# ##################### conts[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  3 obs. of  3 variables:
#   $ id      : num  1 2 3
# $ name    : Factor w/ 3 levels "Essos","Sothoryos",..: 3 1 2
# $ geometry:sfc_POLYGON of length 3; first list element: List of 1
# ..$ : num [1:2931, 1:2] 16.4 16.5 16.4 16.5 16.6 ...
# ..- attr(*, "class")= chr  "XY" "POLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA
# ..- attr(*, "names")= chr  "id" "name"
# 
# 
# ##################### land[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  2 obs. of  3 variables:
#   $ id      : num  0 0
# $ name    : Factor w/ 0 levels: NA NA
# $ geometry:sfc_MULTIPOLYGON of length 2; first list element: List of 3
# ..$ :List of 1
# .. ..$ : num [1:2931, 1:2] 16.4 16.5 16.4 16.5 16.6 ...
# ..$ :List of 1
# .. ..$ : num [1:1740, 1:2] 66.4 66.5 66.6 66.7 66.6 ...
# ..$ :List of 1
# .. ..$ : num [1:280, 1:2] 53.5 53.6 53.7 54 54.2 ...
# ..- attr(*, "class")= chr  "XY" "MULTIPOLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA
# ..- attr(*, "names")= chr  "id" "name"
# 
# 
# ##################### wall[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  1 obs. of  3 variables:
#   $ id      : num 1
# $ name    : Factor w/ 1 level "The Wall": 1
# $ geometry:sfc_LINESTRING of length 1; first list element:  'XY' num [1:12, 1:2] 20.7 19.9 19.1 18.5 17.8 ...
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA
# ..- attr(*, "names")= chr  "id" "name"
# 
# 
# ##################### islands[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  86 obs. of  4 variables:
#   $ id       : num  1 2 3 4 5 6 7 8 9 10 ...
# $ name     : Factor w/ 37 levels "Ax Isle","Basilisk Isles",..: 3 29 NA NA 30 NA 15 4 14 24 ...
# $ continent: Factor w/ 4 levels "Essos","Sothoryos",..: 4 4 4 4 4 4 NA 4 4 4 ...
# $ geometry :sfc_MULTIPOLYGON of length 86; first list element: List of 1
# ..$ :List of 1
# .. ..$ : num [1:47, 1:2] 10.7 10.6 10.6 10.5 10.5 ...o
# ..- attr(*, "class")= chr  "XY" "MULTIPOLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA
# ..- attr(*, "names")= chr  "id" "name" "continent"
# 
# 
# ##################### kingdoms[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  12 obs. of  4 variables:
#   $ id       : num  1 2 3 4 5 6 7 8 9 10 ...
# $ name     : Factor w/ 11 levels "Bran's Gift",..: 4 5 NA 1 8 7 3 6 10 11 ...
# $ ClaimedBy: Factor w/ 11 levels "Arryn","Baratheon",..: 6 9 11 6 7 3 5 2 1 4 ...
# $ geometry :sfc_MULTIPOLYGON of length 12; first list element: List of 1
# ..$ :List of 1
# .. ..$ : num [1:41, 1:2] 15 15.5 16.1 16.9 17.7 ...
# ..- attr(*, "class")= chr  "XY" "MULTIPOLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA
# ..- attr(*, "names")= chr  "id" "name" "ClaimedBy"
# 
# 
# ##################### landscapes[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  37 obs. of  6 variables:
#   $ id       : num  1 2 3 4 5 6 7 8 9 10 ...
# $ name     : Factor w/ 10 levels "Forest of Qohor",..: 6 3 NA NA NA 9 NA NA 10 2 ...
# $ type     : Factor w/ 4 levels "forest","mountain",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ continent: Factor w/ 3 levels "Essos","Sothoryos",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ size     : num  3 3 2 1 1 1 2 2 3 2 ...
# $ geometry :sfc_MULTIPOLYGON of length 37; first list element: List of 1
# ..$ :List of 1
# .. ..$ : num [1:66, 1:2] 23.2 23 23 23.2 23 ...
# ..- attr(*, "class")= chr  "XY" "MULTIPOLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA
# ..- attr(*, "names")= chr  "id" "name" "type" "continent" ...
# 
# 
# ##################### roads[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  21 obs. of  5 variables:
#   $ id       : num  1 2 3 4 5 6 7 8 9 10 ...
# $ name     : Factor w/ 6 levels "Goldroad","High Road",..: NA 6 NA 4 NA 5 NA NA 1 2 ...
# $ size     : num  1 2 1 2 1 1 2 1 2 2 ...
# $ continent: Factor w/ 2 levels "Essos","Westeros": 2 2 2 2 2 2 2 2 2 2 ...
# $ geometry :sfc_LINESTRING of length 21; first list element:  'XY' num [1:15, 1:2] 22.7 22.9 23 22.9 22.8 ...
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA
# ..- attr(*, "names")= chr  "id" "name" "size" "continent"
# 
# 
# ##################### rivers[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  74 obs. of  5 variables:
#   $ id       : num  1 2 3 4 5 6 7 8 9 10 ...
# $ name     : Factor w/ 31 levels "Antler","Blackwater Rush",..: 2 9 9 3 19 NA 8 NA NA NA ...
# $ size     : num  2 0 0 0 0 0 0 0 0 0 ...
# $ continent: Factor w/ 3 levels "Essos","Sothoryos",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ geometry :sfc_MULTILINESTRING of length 74; first list element: List of 5
# ..$ : num [1:6, 1:2] 13.5 13.6 13.7 14 14.1 ...
# ..$ : num [1:11, 1:2] 13.5 13.7 13.7 13.8 13.9 ...
# ..$ : num [1:15, 1:2] 14.9 14.9 14.8 14.8 14.7 ...
# ..$ : num [1:30, 1:2] 15 15.1 15.1 15.2 15.3 ...
# ..$ : num [1:17, 1:2] 16.7 16.7 16.8 16.8 16.8 ...
# ..- attr(*, "class")= chr  "XY" "MULTILINESTRING" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA
# ..- attr(*, "names")= chr  "id" "name" "size" "continent"
# 
# 
# ##################### scenes_locations[1] "sf"         "data.frame"
# Classes ‘sf’ and 'data.frame':  26 obs. of  3 variables:
#   $ location: Factor w/ 26 levels "Astapor","Braavos",..: 8 12 15 21 5 4 2 20 13 22 ...
# $ type    : Factor w/ 7 levels "Castle","City",..: 3 3 3 7 4 2 1 3 3 3 ...
# $ geometry:sfc_POINT of length 26; first list element:  'XY' num  21.23 5.64
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA
# ..- attr(*, "names")= chr  "location" "type"
