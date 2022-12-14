library(shiny)
library(tidyverse)
library(lubridate)
library(bslib)
library(leaflet)
library(ggplot2)
Atlanta<-read.csv("https://query.data.world/s/eiblgqhd555clewhca3cpkgpq5jnf5")
Atlanta$date <- as.POSIXct(paste(Atlanta$date), format = "%m/%d/%Y")
Atlanta$year = year(Atlanta$date)
Atlanta$year = as.numeric(as.character(Atlanta$year))

npu<-pull(Atlanta,npu)%>%
  unique()%>%
  na.omit()

pal <- colorFactor(c("blue","yellow","red"), domain = Atlanta$crime)
crime_plot<-function(dt){
  sub<-dt%>%filter(selected)
    leaflet(sub) %>%
    addProviderTiles(providers$CartoDB.Positron,group = "OSM") %>%
    addCircleMarkers(
      lng = sub$long,
      lat = sub$lat,
      radius = 4,
      stroke = FALSE,
      fillOpacity = 1,
      popup = sub$crime,
      fillColor = ~pal(sub$crime))
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
ui<-fluidPage(
  theme = bs_theme(bootswatch = "simplex"),
  tags$h1("Mapping Atlanta crime incidences from 2010 to 2016"),
  sidebarLayout(
    sidebarPanel(
      selectInput("NPU","Select a NPU",npu,multiple = T),
      uiOutput("selection1"),
      uiOutput("selection2")
    ),
    mainPanel(
        leafletOutput("plot1"),
        plotOutput("plot2")
      )
    )
  )
server<-function(input,output){
  output$selection1 <- renderUI({
    selectInput("neighborhood", "Select a neighborhood", 
                choices = Atlanta[Atlanta$npu==input$NPU,"neighborhood"],multiple = T)
  })
  output$selection2 <- renderUI({
    selectInput("crime_type", "Select a crime type", 
                choices = Atlanta[Atlanta$npu==input$NPU & Atlanta$neighborhood==input$neighborhood,"crime"],multiple=T)
  })
  crime_subset<-reactive({
    Atlanta%>%mutate(selected= (npu%in%input$NPU)&
                       (neighborhood%in%input$neighborhood)&
                       (crime%in%input$crime_type))
  })
  output$plot1<-renderLeaflet({
    crime_plot(crime_subset())
  })
  output$plot2<-renderPlot({
    count_plot(crime_subset())
  })
}
shinyApp(ui, server)
