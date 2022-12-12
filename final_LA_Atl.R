library(shiny)
library(leaflet)
library(tidyverse)
library(data.table)
library(shinythemes)
library(plotly)
library(lubridate)
library(bslib)
library(ggplot2)
set.seed(100)
crimeLA <- read.csv("crimeLA.csv")
crimeLA = na.omit(crimeLA)
crimeLA=sample_n(crimeLA,10000)

crimeLA$Date.Reported <- as.POSIXct(paste(crimeLA$Date.Reported), format = "%m/%d/%Y")
crimeLA$year = year(crimeLA$Date.Reported)
crimeLA$year = as.numeric(as.character(crimeLA$year))
crimeLA$Victim.Age = as.numeric(crimeLA$Victim.Age)

Atlanta<-read.csv("https://query.data.world/s/eiblgqhd555clewhca3cpkgpq5jnf5")
Atlanta$date <- as.POSIXct(paste(Atlanta$date), format = "%m/%d/%Y")
Atlanta$year = year(Atlanta$date)
Atlanta$year = as.numeric(as.character(Atlanta$year))

npu<-pull(Atlanta,npu)%>%
  unique()%>%
  na.omit()
crime<-pull(Atlanta,crime)%>%
  unique()%>%
  na.omit()
pal <- colorFactor(c("blue","yellow","red"), domain = Atlanta$crime)
crime_plot<-function(dt){
  sub<-dt%>%filter(selected)
  leaflet(sub) %>%
    addProviderTiles(providers$CartoDB.Positron,group = "OSM") %>%
    setView(
      lng = -84.411900,
      lat = 33.773567,
      zoom=10)%>%
    addCircleMarkers(
      lng = sub$long,
      lat = sub$lat,
      radius = 4,
      stroke = FALSE,
      fillOpacity = 1,
      popup = sub$crime,
      fillColor = ~pal(sub$crime)
      )%>%
    addLegend( pal = pal, 
              values = sub$crime,
               opacity = 1)
}

count_plot<-function(df){
  df%>%filter(selected)%>%
    group_by(year,neighborhood) %>%
    count() %>%
    ggplot()+
    geom_line(aes(year,n,group=neighborhood,col=neighborhood),size=2) +
    scale_x_discrete(labels = year) +
    theme(text = element_text(size = 14, face = "bold")) +
    labs(title = "Crime amount of neighborhood")+
    scale_x_continuous(limits = c(2009,2016), expand = c(0, 0))+
    xlab("Year") + ylab("Total Amount of Crimes")
}

#crimeLA$Victim.Descent = replace(crimeLA$Victim.Descent, c("O","","X","K","A","C","F","P","I","U","L"), "Other")
#crimeLA$Victim.Descent[crimeLA$Victim.Descent == c("O","","X","K","A","C","F","P","I","U","L")] <- "Other"

ui <- 
  navbarPage("Past Crime of Neighborhoods",
             tabPanel("LA Crime",
                      
                      leafletOutput("map",width="1100",height="900"),
                      
                      absolutePanel(class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 50, bottom = "auto",
                                    width = 400, height = "auto", 
                                    style = "margin-left: auto; 
                margin-right: auto;
                border-radius: 5pt; 
                text-align: center;",
                                    fluidRow(align="center",
                                             h2("Crime"),
                                             
                                             selectizeInput("city", "Neighborhood",unique(crimeLA$Area.Name), multiple = TRUE, options = list(maxItems = 3)),
                                             sliderInput("age", "Victim Age", min = min(crimeLA$Victim.Age), max =max(crimeLA$Victim.Age), c(10,98),sep=""),
                                             
                                             
                                             plotlyOutput("bar",width=350,height=300),
                                             plotOutput("line",width=350,height=300)))),
             
             tabPanel("Atlanta Crime",
                      theme = bs_theme(bootswatch = "simplex"),
                      tags$h1("Mapping Atlanta crime incidences from 2009 to 2016"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("NPU","Select a Neighborhood Planning Unit(NPU)",npu,multiple = T),
                          uiOutput("selection1"),
                          selectInput("crime_type","Select a crime type",crime,multiple = T)
                        ),
                        mainPanel(
                          leafletOutput("plot1"),
                          plotOutput("plot2")
                        )
                      )
                      )
             
             
  )


server <- function(input, output) {
  crime_subset<-reactive({
    crimeLA %>%
      filter((crimeLA$Area.Name %in% input$city),
             (Victim.Age >= input$age[1]),
             (Victim.Age <= input$age[2]))
    
  })
  output$selection1 <- renderUI({
    selectInput("neighborhood", "Select a neighborhood", 
                choices = Atlanta[Atlanta$npu==input$NPU,"neighborhood"],multiple = T)
  })
  
  crime_subset_Atl<-reactive({
    Atlanta%>%mutate(selected= (npu%in%input$NPU)&
                       (neighborhood%in%input$neighborhood)&
                       (crime%in%input$crime_type))
  })
  output$plot1<-renderLeaflet({
    crime_plot(crime_subset_Atl())
  })
  output$plot2<-renderPlot({
    count_plot(crime_subset_Atl())
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = -118.243683,
        lat = 34.052235 ,
        zoom = 10
      ) %>%
      addMarkers(
        data=crime_subset(),
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste("Date:",Date.Reported, "<br>", "Description:",Crime.Code.Description,"<br>", "Ethnicity:",Victim.Descent),
        clusterOptions = markerClusterOptions())
  })
  
  output$bar <- renderPlotly({
    ggplotly(crime_subset()%>%
               group_by(year,Area.Name) %>%
               count() %>%
               ggplot()+
               geom_bar(aes(year,n,fill=Area.Name),position="stack",stat="identity") +
               xlab("Year") + ylab("Total Amount of Crimes") +
               theme(legend.position='none'))%>%
      config(displayModeBar=F)
  })
  
  output$line <- renderPlot ({
    crime_subset() %>%
      group_by(year,Area.Name)%>%
      count() %>%
      ggplot()+
      geom_line(aes(year,n,col=Area.Name))+
      scale_x_continuous(limits =c(2010,2017),expand=c(0,0))+
      xlab("Year") + ylab("Total Number of Crimes")
  })
  
}


shinyApp(ui, server)

