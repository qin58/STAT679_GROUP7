library(shiny)
library(leaflet)
library(tidyverse)
library(data.table)
library(shinythemes)
library(plotly)

set.seed(100)
crimeLA <- read.csv("crimeLA.csv")
crimeLA = na.omit(crimeLA)
crimeLA=sample_n(crimeLA,10000)

crimeLA$Date.Reported <- as.POSIXct(paste(crimeLA$Date.Reported), format = "%m/%d/%Y")
crimeLA$year = year(crimeLA$Date.Reported)
crimeLA$year = as.numeric(as.character(crimeLA$year))
crimeLA$Victim.Age = as.numeric(crimeLA$Victim.Age)



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
                                             
                                             
                                             plotlyOutput("line",width=350,height=300)))),
             
             tabPanel("Atlanta Crime")
             
             
  )


server <- function(input, output) {
  crime_subset<-reactive({
    crimeLA %>%
      filter((crimeLA$Area.Name %in% input$city),
             (Victim.Age >= input$age[1]),
             (Victim.Age <= input$age[2]))
    
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
  
  output$line <- renderPlotly({
    ggplotly(crime_subset()%>%
               group_by(year,Area.Name) %>%
               count() %>%
               ggplot()+
               geom_bar(aes(year,n,fill=Area.Name),position="stack",stat="identity") +
               xlab("Year") + ylab("Total Amount of Crimes") +
               theme(legend.position='none'))%>%
      config(displayModeBar=F)
  })
  
}


shinyApp(ui, server)

