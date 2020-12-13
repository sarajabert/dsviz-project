#!/usr/bin/Rscript

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(cartography)
library("RColorBrewer")

appearances = read_csv("./data/appearances.csv")
characters = read_csv("./data/characters.csv")
episodes = read_csv("./data/episodes.csv")
scenes = read_csv("./data/scenes.csv")

continents <- st_read("./data/GoTRelease/Continents.shp", crs=4326)
islands <- st_read("./data/GoTRelease/Islands.shp", crs=4326)
lakes <- st_read("./data/GoTRelease/Lakes.shp", crs=4326)
land <- st_read("./data/GoTRelease/Land.shp", crs=4326)
landscape <- st_read("./data/GoTRelease/Landscape.shp", crs=4326)
locations <- st_read("./data/GoTRelease/Locations.shp", crs=4326)
kingdoms <- st_read("./data/GoTRelease/Political.shp", crs=4326)
regions <- st_read("./data/GoTRelease/Regions.shp", crs=4326)
rivers <- st_read("./data/GoTRelease/Rivers.shp", crs=4326)
roads <- st_read("./data/GoTRelease/Roads.shp", crs=4326)
scenesLocations <- st_read("./data/GoTRelease/ScenesLocations.shp", crs=4326)
wall <- st_read("./data/GoTRelease/Wall.shp", crs=4326)

colforest="#c0d7c2"
colmountain="#92817a"
colstepp="#f7d1ba"
colswamp="#776d8a"
colroad="#383e56"
colwater="#7ec9dc"
colland="ivory"
colborderland="ivory3"
colsymbol="#d35c61"
colriver=colwater
collake=colwater
borderland=colborderland

landscapes_fix = landscape %>%
  mutate(continent=replace(continent, type=="swamp", "Westeros"))

islands_fix = islands %>%
  mutate(continent=replace(continent, continent=="Sothyrios", "Sothoryos")) %>%
  mutate(continent=replace(continent, is.na(continent), "Essos"))

wall_fix = bind_cols(
  wall,
  data.frame(continent=("Westeros"))
)

#load("./got_data.Rdata")


# Define UI for application that draws a histogram
ui <- fluidPage(
  #    theme = "bootswatch-cerulean.css",
  
  #titlePanel("Project of Alexandre, Natacha, Romain and Sara"),
  
  navbarPage("Project DS",
             navbarMenu("Deaths",
                        tabPanel("Deaths by location",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("colorArea", "Color of areas",
                                                  c("Pink"="pink", "Purple"="purple", "Blue"="blue4")
                                     ),
                                     sliderInput("rangeNbDeaths",
                                                 "Range of deaths :",
                                                 min = 1,  max = 150, value = c(10,100))
                                   ),
                                   mainPanel(
                                     plotOutput("deathsByLocationPlot", height="auto")
                                   )
                                 )
                        ),
                        tabPanel("Best killers" ,
                                 sidebarLayout(
                                   sidebarPanel(
                                     numericInput("killersNb", "Number of best killers:", 5)
                                   ) ,
                                   mainPanel(
                                     plotOutput("bestKillersPlot", height = "auto")
                                   )
                                 )
                        ),
                        
                        tabPanel("Deaths by episodes",
                                 sidebarLayout(
                                   sidebarPanel(
                                     checkboxGroupInput("checkSeasons", label = h3("Checkbox group"),
                                                        choices = list("Season 1" = 1, "Season 2" = 2, "Season 3" = 3,"Season 4" = 4, "Season 5" = 5, "Season 6" = 6, "Season 7" = 7, "Season 8" = 8),
                                                        selected = c(1,2,3)),
                                     selectInput("selectedPalette", label = h3("Select palette"),
                                                 choices = list("RdPu" = "RdPu", "Spectral" = "Spectral", "Pastel2" = "Pastel2", "Set3"="Set3"),
                                                 selected = 1)
                                   ),
                                   mainPanel(
                                     plotOutput("deathsByEpisodes", height = "auto")
                                   )
                                   
                                 )
                        )
             ),
             navbarMenu("Characters",
                        tabPanel("Characters info",
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("dataset",label="Choose a dataset",choice=c("characters"=1,
                                                                                             "episodes"=2,
                                                                                             "scenes"=3,
                                                                                             "appearances"=4), selectize=FALSE),
                                     
                                     selectInput("choiceName", "Choose a name", choice=c(appearances %>% 
                                                                                           group_by(name) %>%
                                                                                           count(name,wt=sceneId) %>%
                                                                                           arrange(desc(n)) %>%
                                                                                           pull(name)), selected  = NULL),
                                     
                                     selectInput("choiceInfo", "Info to display", choice=c("Episodes by season" = 1,
                                                                                           "Time screen" = 2), selected  = NULL)
                                     
                                     
                                   ),
                                   mainPanel(
                                     h2("Summary of the dataset"),
                                     verbatimTextOutput("sumEpisodes"),
                                     plotOutput("plot1", height = "auto")
                                   )
                                 )
                                 
                        ),
                        tabPanel("Distributions",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("distribution_type", "Distribution",
                                                  c("Gender per house"="gender_per_house", "Survival per season"="survival_per_season")
                                     )
                                   ),
                                   mainPanel(
                                     plotOutput("distributionPlot", height = "auto")
                                   )
                                 )
                                 
                        )
             ),
             navbarMenu("Geography",
                        tabPanel("Demography",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("demography_type", "Demography",
                                                  c("By location density"="location_density", "By location size"="location_size")
                                     ),
                                     sliderInput("demography_range", "Size of locations:",
                                                 min = 1,  max = 5, value = c(1,5)
                                     )
                                   ),
                                   mainPanel(
                                     plotOutput("demographyPlot", height = "auto")
                                   )
                                 )
                                 
                        ),
                        tabPanel("Topology",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("continent_name", "Continent",
                                                  c("Westeros"="Westeros", "Essos"="Essos", "Sothoryos"="Sothoryos")
                                     )
                                   ),
                                   mainPanel(
                                     plotOutput("topologyPlot", height = "auto")
                                   )
                                 )
                        ),
                        tabPanel("Landscapes",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("landscape_type", "Landscape",
                                                  c("By surface area"="surface_area", "By landmass"="landmass")
                                     )
                                   ),
                                   mainPanel(
                                     plotOutput("landscapePlot", height = "auto")
                                   )
                                 )
                                 
                        )
             )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$deathsByLocationPlot <- renderPlot({
    colorArea = "pink"
    nbDeathsPerLocation <- scenes %>% group_by(location) %>% summarize(nbdeath = sum(nbdeath)) %>% filter(nbdeath != 0 & nbdeath >= input$rangeNbDeaths[1] &  nbdeath <= input$rangeNbDeaths[2]) %>% left_join(scenesLocations) %>% st_as_sf()
    
    locationsOfDeath <- scenes %>% group_by(location) %>% summarize(nbdeath = sum(nbdeath)) %>%
      left_join(scenesLocations) %>% left_join(locations, by = c("location"="name")) %>% select(location, geometry.x) %>% st_as_sf()
    
    
    ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1) +
      geom_sf(data=lakes,fill=collake,col=collake,size=0.1) +
      geom_sf(data=rivers,fill=colriver,col=colriver,size=0.1) +
      geom_sf(data=landscape %>% filter(type=="forest"),fill=colforest,col=colforest)+
      geom_sf(data=wall,col="black",size=1)+
      geom_sf(data=nbDeathsPerLocation, aes(size=nbdeath), color=input$colorArea)+
      scale_size_area("Number of deaths")+
      geom_sf_text(data=locationsOfDeath %>% filter(location != "The Dothraki Sea" & location != "The Narrow Sea"), aes(label=location),size=2.5,family="Palatino", fontface="italic", 
                   nudge_y = -1)+
      geom_polygon(data=data.frame(x=c(3.75,2.5,5), y=c(48,44,44)), aes(x=x, y=y), fill=colsymbol, color=colsymbol) +
      geom_text(aes(label = "N", x=3.75, y=42), color=colsymbol, size=4, fontface = "bold") +
      theme_minimal()+
      coord_sf(expand=0, ndiscr=0) + 
      xlab("") + 
      ylab("") +
      facet_grid(. ~ "By range") +
      ggtitle("Location of deaths.") +
      theme(panel.background=element_rect(fill=colwater, color=NA)) +
      theme(panel.border = element_rect(colour = colsymbol, fill=NA, size=0.5)) +
      theme(plot.margin = unit(c(1,1,1,1), "cm")) +
      theme(plot.title = element_text(size=18, color= colsymbol, face="bold", hjust = 0.5)) +
      theme(strip.background = element_rect(fill=colsymbol, color=colsymbol), strip.text = element_text(size=9, colour="white", face="bold", hjust = 0))
  },
  height = function() {
    session$clientData$output_deathsByLocationPlot_width
  })
  
  output$bestKillersPlot <- renderPlot({
    
    sumKilledByChar10first <- characters %>% group_by(killedBy) %>% summarize(nbKilled = n()) %>% filter(!is.na(killedBy)) %>%
      arrange(desc(nbKilled)) %>% head(input$killersNb)
    
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
      theme(panel.background = element_rect(fill ="lightgrey" ,color=NA), plot.title = element_text(size=18, color= colsymbol, face="bold", hjust = 0.5))+
      scale_x_discrete(guide = guide_axis(angle = 30))+
      scale_y_continuous(breaks = seq(0,13,by = 2))+
      labs(title = "Best killers", x="", y="Number of victims")
  },
  height = function() {
    session$clientData$output_bestKillersPlot_width
  })
  
  
  output$deathsByEpisodes <- renderPlot({
    numberSeasons = input$checkSeasons #c(1, 2, 3, 5, 6, 8)
    enrichedSeasonNum = c("1" = "Season 1",
                          "2" = "Season 2",
                          "3" = "Season 3",
                          "4" = "Season 4",
                          "5" = "Season 5",
                          "6" = "Season 6",
                          "7" = "Season 7",
                          "8" = "Season 8")
    palette = brewer.pal(n = 8, name = input$selectedPalette) # Set3 BuPu Blues Pastel2 RdPu
    
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
      theme(panel.background = element_rect(fill ="lightgray" ,color=NA), plot.title = element_text(size=18, color=colsymbol, face="bold", hjust = 0.5))+
      labs(title = "Number of deaths by episodes", x="Episode number", y="Number of deaths")
    
  },
  height = function() {
    session$clientData$output_deathsByEpisodes_width
  })
  
  output$sumEpisodes <- renderPrint({
    if (input$dataset == 1) {
      summary(characters)
    } else if (input$dataset == 2) {
      summary(episodes)
    } else if (input$dataset == 3) {
      summary(scenes)
    } else {
      summary(appearances)
    }
  })
  
  output$plot1 <- renderPlot({
     if(input$choiceInfo == 1){
      jstime = appearances %>% filter(name==input$choiceName) %>%
        left_join(scenes) %>% select(name, episodeId) %>%
        left_join(episodes)%>% 
        select(episodeId, seasonNum) %>%
        unique %>%
        group_by(seasonNum) %>%
        summarise(NbEpisode = n())
      
      ggplot(jstime, aes(x=seasonNum, y=NbEpisode, fill=NbEpisode)) + 
        geom_bar(stat="identity", width=0.9)+
        theme_minimal()+
        theme_bw()+
        xlab("season")+ 
        scale_x_continuous(breaks = c(1:8), labels = c(1:8), limits = c(NA,9)) + 
        ggtitle("Episode appearance by seasons") 
      
      
    } else {
      jstime = appearances %>% filter(name==input$choiceName) %>% 
        left_join(scenes) %>% 
        group_by(episodeId) %>% 
        summarise(time=sum(duration))
      
      
      if (nrow(jstime) == 1) {
        ggplot(jstime) + 
          geom_point(aes(x=episodeId,y=time), size = 4)+
          theme_bw()+
          xlab("episod")+ylab("time")+
          #    expand_limits(x = 0, y = 0)+
          ggtitle("Time screen")
      } else {
        ggplot(jstime, aes(x=episodeId,y=time, color=time)) + 
          geom_line(size = 1.2)+
          theme_bw()+
          xlab("episod")+ylab("time")+
          scale_x_continuous(breaks = round(seq(min(jstime$episodeId), max(jstime$episodeId), by=5), 0))+
          ggtitle("Time screen") +
          geom_point(size = 3) +
          scale_color_gradient(low="blue", high="red")
      }
    }
    
  },
  height = function() {
    session$clientData$output_plot1_width
  })
  
  output$distributionPlot <- renderPlot({
    if(input$distribution_type == "gender_per_house"){
      nbFemalePerHouse <- characters %>% filter(sex == "female") %>% group_by(house, sex) %>% 
        summarize(nb = n())%>% filter(!is.na(house))
      nbMalePerHouse <- characters %>% filter(sex == "male") %>% group_by(house, sex) %>% 
        summarize(nb = n())%>% filter(!is.na(house))
      data <- rbind(nbFemalePerHouse,nbMalePerHouse)
      
      ggplot(data, aes(x = house,  y = nb, fill=sex)) + 
        geom_bar(position="stack", stat="identity")+
        scale_fill_brewer("Gender",palette = "Spectral")+
        theme_minimal()+
        theme(panel.background = element_rect(fill ="lightgray" ,color=NA), plot.title = element_text(size=18, color= colsymbol, face="bold", hjust = 0.5))+
        scale_x_discrete(guide = guide_axis(angle = 30))+
        scale_y_continuous(breaks = seq(0,25,by = 2))+
        labs(title = "Gender distribution per houses", x="", y="")
    }else if(input$distribution_type == "survival_per_season"){
      numberSeasons = c(1, 2, 3, 5, 6, 8)
      # nbCharacters = countNotNumeric(characters$name)
      nbCharacters = characters %>% filter(!is.na(name)) %>% nrow()
      nbSurv <- scenes %>% group_by(episodeId) %>% summarize(nb = (nbCharacters - sum(nbdeath))) %>% left_join(episodes)
      survivalPerSeason <- nbSurv %>% filter(seasonNum %in% numberSeasons)
      survivals <- cbind.data.frame("Season" = survivalPerSeason$seasonNum,"Survivals" = survivalPerSeason$nb)
      dfs <- survivals %>% group_by(Season) %>% summarise(Survivals = sum(Survivals))
      propSurvivals <- dfs %>% mutate(Prop = Survivals/nbCharacters)
      
      ggplot(propSurvivals, aes(x="", y=Prop, fill=factor(Season))) + geom_bar(stat="identity", width=1)+
        coord_polar("y", start=0) + geom_text(aes(label = paste0(round(Prop), "%")), position = position_stack(vjust = 0.5))+
        scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#B833FF", "#FF3333"))+ 
        labs(x = NULL, y = NULL, fill = "Season number", title = "Survivals distribution per season")+
        theme_classic() + theme(axis.line = element_blank(),
                                axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                plot.title = element_text(size=18, color= colsymbol, face="bold", hjust = 0.5))
    }
  },
  height = function() {
    session$clientData$output_distributionPlot_width
  })
      
  
  output$landscapePlot <- renderPlot({
    if(input$landscape_type == "surface_area"){
      
      geo = landscapes_fix %>%
        mutate(area=st_area(geometry)) %>% 
        group_by(continent) %>%
        count(type,wt=as.numeric(area)) %>% 
        st_drop_geometry() %>% 
        full_join(
          bind_cols(
            data.frame(type=c("islands")),
            islands_fix %>%
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
            wall_fix %>%
              mutate(length=st_length(geometry)) %>%
              #https://gameofthrones.fandom.com/wiki/Wall
              count(wt=as.numeric(length*91)) %>%
              st_drop_geometry()
          )
        ) %>%
        arrange(continent, type) %>%
        filter(!is.na(continent))
      
      #geo %>% print(n = Inf)
      
      ggplot(data = geo, aes(x = type, y = n, fill = type)) +
        geom_bar(stat = "identity", width = 1) +
        scale_fill_manual("Geography", values = c("#66C2A5","#F46D43",colstepp,colswamp,"#FEE08B","#3288BD",colwater,colmountain,colroad)) +
        geom_text(aes(label = paste(signif(n, 2), "m2")), hjust = -0.25, size=4) +
        facet_grid(continent ~ ., scales = "free_y") +
        theme(panel.background = element_rect(fill = NA, color = "black"), panel.border = element_rect(fill = NA, colour = "black")) +
        scale_y_log10("Surface area (m2), logarithmic scale") +
        xlab("") +
        coord_flip(ylim = c(1e07, 1e13)) +
        ggtitle("Landscapes by continents and surface area.") +
        theme(plot.title = element_text(size=18, color= colsymbol, face="bold", hjust = 0.5))
      
    }else if(input$landscape_type == "landmass"){
      
      landmass = continents %>%
        mutate(area=st_area(geometry)) %>%
        count(name,wt=as.numeric(area)) %>%
        st_drop_geometry()
      landmass <- landmass %>%
        arrange(desc(name)) %>%
        mutate(prop = n / sum(landmass$n) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop)
      
      ggplot(landmass, aes(x="", y=prop, fill=name)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() +
        theme(legend.position="none") +
        geom_text(aes(y = ypos, label = name), color = "black", size=6) +
        scale_fill_manual(values = c("#F46D43","#66C2A5","#FEE08B")) +
        ggtitle("Total landmass in the GoT universe.") +
        theme(plot.title = element_text(size=18, color= colsymbol, face="bold", hjust = 0.5))
      
    }
    
  },
  height = function() {
    session$clientData$output_landscapePlot_width
  })
  
  output$topologyPlot <- renderPlot({
    
    lanscapes_locations = landscapes_fix %>% filter(continent==input$continent_name) %>% filter(size >= 3 & !is.na(name))
    continent_locations = locations %>% select(-confirmed) %>%
      filter(id %in% unlist(st_contains(continents %>% filter(name==input$continent_name), locations))) %>%
      filter(!is.na(name)) %>%
      arrange(desc(size)) %>% 
      head(10 - nrow(lanscapes_locations)) %>%
      bind_rows(lanscapes_locations %>% select(-continent))
    
    plot(st_geometry(continents %>% filter(name==input$continent_name)), col=colland, border=colborderland, bg=colwater, xaxs="i", yaxs="i")
    plot(st_geometry(landscapes_fix %>% filter(type=="forest" & continent==input$continent_name)), col=colforest, border=colforest, lwd=0.5, add=T)
    plot(st_geometry(landscapes_fix %>% filter(type=="mountain" & continent==input$continent_name)), col=colmountain, border=colmountain, lwd=0.5, add=T)
    plot(st_geometry(landscapes_fix %>% filter(type=="stepp" & continent==input$continent_name)), col=colstepp, border=colstepp, lwd=0.5, add=T)
    plot(st_geometry(landscapes_fix %>% filter(type=="swamp" & continent==input$continent_name)), col=colswamp, border=colswamp, lwd=0.5, add=T)
    plot(st_geometry(islands_fix %>% filter(continent==input$continent_name)), col=colland, border=colborderland, lwd=0.5, add=T)
    plot(st_geometry(lakes %>% filter(continent==input$continent_name)), col=colwater, border=colwater, lwd=0.5, add=T)
    plot(st_geometry(rivers %>% filter(continent==input$continent_name)), col=colwater, border=colwater, lwd=0.5, add=T)
    plot(st_geometry(roads %>% filter(continent==input$continent_name)), col=colroad, border=colroad, lwd=1, add=T)
    plot(st_geometry(wall_fix %>% filter(continent==input$continent_name)), col="black", border="black", lwd=3, add=T)
    labelLayer(x=continent_locations, txt="name", halo=TRUE, cex = 1, col= "#000000", bg = "#FFFFFF50", overlap = FALSE)
    north(pos="topleft", col=colsymbol)
    layoutLayer(title=input$continent_name,
                col=colsymbol, coltitle="white", tabtitle=TRUE,
                frame=TRUE, scale=NULL, north=FALSE)
    title(paste0("The continent of ",input$continent_name,"."), col.main=colsymbol, cex.main=1.5)
    
  },
  height = function() {
    session$clientData$output_topologyPlot_width
  })
  
  output$demographyPlot <- renderPlot({
    if(input$demography_type == "location_size"){
      
      locations_range = locations %>% filter(size >= input$demography_range[1] & size <= input$demography_range[2])
      
      plot(st_geometry(land), col=colland, border=colborderland, bg=colwater, xaxs="i", yaxs="i")
      plot(st_geometry(landscapes_fix %>% filter(type=="forest")), col=colforest, border=colforest, lwd=0.5, add=T)
      plot(st_geometry(landscapes_fix %>% filter(type=="mountain")), col=colmountain, border=colmountain, lwd=0.5, add=T)
      plot(st_geometry(landscapes_fix %>% filter(type=="stepp")), col=colstepp, border=colstepp, lwd=0.5, add=T)
      plot(st_geometry(landscapes_fix %>% filter(type=="swamp")), col=colswamp, border=colswamp, lwd=0.5, add=T)
      plot(st_geometry(islands_fix), col=colland, border=colborderland, lwd=0.5, add=T)
      plot(st_geometry(lakes), col=colwater, border=colwater, lwd=0.5, add=T)
      plot(st_geometry(rivers), col=colwater, border=colwater, lwd=0.5, add=T)
      plot(st_geometry(roads), col=colroad, border=colroad, lwd=1, add=T)
      plot(st_geometry(wall_fix), col="black", border="black", lwd=1, add=T)
      plot(st_geometry(locations_range), border=NA, lwd=0.5, add=T)
      propSymbolsLayer(locations_range, var="size", col=colsymbol, inches=0.15,
                       border="black", lwd=0.7, symbols="square",
                       legend.style="e", legend.pos="topright", 
                       legend.title.txt="Location size",
                       legend.values.rnd=0)
      north(pos="topleft", col=colsymbol)
      layoutLayer(title="By location size",
                  col=colsymbol, coltitle="white",
                  frame=TRUE, scale=NULL, north=FALSE)
      title("Demography of the know world.", col.main=colsymbol, cex.main=1.5)
      
    }else if(input$demography_type == "location_density"){
      
      locations_range = locations %>% filter(size >= input$demography_range[1] & size <= input$demography_range[2])
      
      locations_xy = data.frame(x=st_coordinates(st_centroid(st_geometry(locations_range)))[,1],
                                y=st_coordinates(st_centroid(st_geometry(locations_range)))[,2],
                                w=locations_range %>% pull(size))
      
      density_xy = with(locations_xy, locations_xy[rep(1:nrow(locations_xy), w),])

      ggplot() + 
        geom_sf(data=land, fill=colland, col=colborderland, size=0.1) +
        geom_sf(data=landscapes_fix %>% filter(type=="forest"), fill=colforest, col=colforest) +
        geom_sf(data=landscapes_fix %>% filter(type=="mountain"), fill=colmountain, col=colmountain) +
        geom_sf(data=landscapes_fix %>% filter(type=="stepp"), fill=colstepp, col=colstepp) +
        geom_sf(data=landscapes_fix %>% filter(type=="swamp"), fill=colswamp, col=colswamp) +
        geom_sf(data=islands_fix, fill=colland, col=colborderland) +
        geom_sf(data=lakes, col=colwater, fill=colwater) +
        geom_sf(data=rivers, col=colwater) +
        geom_sf(data=roads, col=colroad) +
        geom_sf(data=wall_fix, col="black", size=1) + 
        geom_point(data = locations_xy, aes(x=x, y=y), color="black", size=3) +
        geom_point(data = locations_xy, aes(x=x, y=y), color=colsymbol, size=2) +
        stat_density2d(data = density_xy, aes(x=x, y=y), color="black") +
        geom_polygon(data=data.frame(x=c(3.75,2.5,5), y=c(48,44,44)), aes(x=x, y=y), fill=colsymbol, color=colsymbol) +
        geom_text(aes(label = "N", x=3.75, y=42), color=colsymbol, size=4, fontface = "bold") +
        theme_minimal() +
        coord_sf(expand=0, ndiscr=0) + 
        xlab("") +
        ylab("") +
        facet_grid(. ~ "By location density") +
        ggtitle("Demography of the know world.") +
        theme(panel.background=element_rect(fill=colwater, color=NA)) +
        theme(panel.border = element_rect(colour = colsymbol, fill=NA, size=0.5)) +
        theme(plot.margin = unit(c(1,1,1,1), "cm")) +
        theme(plot.title = element_text(size=18, color= colsymbol, face="bold", hjust = 0.5)) +
        theme(strip.background = element_rect(fill=colsymbol, color=colsymbol), strip.text = element_text(size=9, colour="white", face="bold", hjust = 0))
      
    }
    
  },
  height = function() {
    session$clientData$output_demographyPlot_width
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
