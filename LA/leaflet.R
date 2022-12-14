library(shiny)
library(leaflet)
library(tidyverse)
library(data.table)
library(shinythemes)

set.seed(100)
crimeLA <- read.csv("crimeLA.csv")
crimeLA = na.omit(crimeLA)
crimeLA=sample_n(crimeLA,10000)

crimeLA$Date.Reported <- as.POSIXct(paste(crimeLA$Date.Reported), format = "%m/%d/%Y")
crimeLA$year = year(crimeLA$Date.Reported)
crimeLA$year = as.numeric(as.character(crimeLA$year))



ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Past Crime of Neighborhoods in LA"),
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Neighborhood",unique(crimeLA$Area.Name), multiple = TRUE)
    ),
    mainPanel(
      fluidRow(
        column(10,leafletOutput("map")),
        column(10,plotOutput("line"))
      )
    )
  )
  
)


server <- function(input, output) {
  crime_subset<-reactive({
    crimeLA[crimeLA$Area.Name %in% input$city, ]
    
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = -118.243683,
        lat = 34.052235 ,
        zoom = 10
      ) %>%
      addCircleMarkers(
        data=crime_subset(),
        lng = ~longitude,
        lat = ~latitude,
        popup = ~Area.Name)
  })
  
  output$line <- renderPlot({
    crime_subset()%>%
      group_by(year,Area.Name) %>%
      count() %>%
      ggplot()+
      geom_line(aes(year,n,group=Area.Name,col=Area.Name)) +
      scale_x_continuous(limits = c(2010,2017), expand = c(0, 0))+
      xlab("Year") + ylab("Total Amount of Crimes")
  })
  
}




shinyApp(ui, server)

