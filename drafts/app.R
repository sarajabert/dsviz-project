#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library("RColorBrewer")

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
colriver="#7ec9dc"
collake="#87cdde"
colland="ivory"
borderland = "ivory3"

library(readr)
appearances = read_csv("./data/appearances.csv")
characters = read_csv("./data/characters.csv")
episodes = read_csv("./data/episodes.csv")
scenes = read_csv("./data/scenes.csv")


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
                                     plotOutput("deathsByLocationPlot")
                                   )
                                 )
                        ),
                        tabPanel("Best killers" ,
                                 sidebarLayout(
                                   sidebarPanel(
                                     numericInput("killersNb", "Number of best killers:", 5)
                                   ) ,
                                   mainPanel(
                                     plotOutput("bestKillersPlot")
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
                                     plotOutput("deathsByEpisodes")
                                   )
                                   
                                 )
                        )
             ),
             navbarMenu("Characters plots",
                        tabPanel("Characters plots",
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("dataset",label="Choose a dataset",choice=c("characters"=1,
                                                                                             "episodes"=2,
                                                                                             "scenes"=3,
                                                                                             "appearances"=4), selectize=FALSE),
                                     
                                     selectInput("choiceName", "Choose a name", choice=c(appearances$name), selected  = NULL),
                                     
                                     selectInput("choiceInfo", "Info to display", choice=c("Time screen" = 1,
                                                                                           "Episodes by season" = 2), selected  = NULL)
                                     
                                     
                                   ),
                                   mainPanel(
                                     h2("Summary of the dataset"),
                                     verbatimTextOutput("sumEpisodes"),
                                     plotOutput("plot1")
                                   )
                                 )
                                 
                        )
             ),
             navbarMenu("Geographical plots",
                        tabPanel("Density"),
                        tabPanel("Type of landscapes")
             )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
      theme_minimal()+ coord_sf(expand = 0,ndiscr = 0)+
      theme(panel.background = element_rect(fill = colriver,color=NA), plot.title = element_text(size=22, color= "red", face="bold", hjust = 0.5))+
      labs(title = "Location of deaths", x="",y="")
    
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
      theme(panel.background = element_rect(fill ="lightgrey" ,color=NA), plot.title = element_text(size=22, color= "red", face="bold", hjust = 0.5))+
      scale_x_discrete(guide = guide_axis(angle = 30))+
      scale_y_continuous(breaks = seq(0,13,by = 2))+
      labs(title = "Best killers", x="", y="Number of victims")
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
      theme(panel.background = element_rect(fill ="lightgray" ,color=NA), plot.title = element_text(size=22, color= "red", face="bold", hjust = 0.5))+
      labs(title = "Number of deaths by episodes", x="Episode number", y="Number of deaths")
    
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
        left_join(scenes) %>% 
        group_by(episodeId) %>% 
        summarise(time=sum(duration))
      
      ggplot(jstime) + 
        geom_line(aes(x=episodeId,y=time))+
        theme_bw()+
        xlab("episod")+ylab("time")+
        ggtitle("Time screen")
    } else {
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
      
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
