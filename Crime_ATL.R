library(shiny)
library(tidyverse)
library(lubridate)
library(ggmap)
library(bslib)
library(leaflet)
Atlanta<-read.csv("https://query.data.world/s/eiblgqhd555clewhca3cpkgpq5jnf5")
Atlanta<-Atlanta[which(Atlanta$lat>33.5),]
Atlanta$date <- as.POSIXct(paste(Atlanta$date), format = "%m/%d/%Y")
Atlanta$year = year(Atlanta$date)
Atlanta$year = as.numeric(as.character(Atlanta$year))

crime_type<-pull(Atlanta,crime)%>%
  unique()%>%
  na.omit()
npu<-pull(Atlanta,npu)%>%
  unique()%>%
  na.omit()



crime_plot<-function(df){
  qmplot(long, lat, data = df, maptype = "toner-lite",color=crime)+
    geom_point(data = df, aes(x = long, y = lat, colour = crime)) +
    scale_colour_manual(values = rainbow(11)) +
    theme(text = element_text(size = 14, face = "bold")) +
    labs(title = "Different crime incidences and locations",
         x = "long", y = "lat", colour = "crime")
}

count_plot<-function(df){
  df%>%
    group_by(year,npu) %>%
    count() %>%
    ggplot()+
    geom_line(aes(year,n,group=npu,col=npu),size=2) +
    scale_x_continuous(limits = c(2009,2016), expand = c(0, 0))+
    scale_x_discrete(labels = year) +
    scale_colour_manual(values = rainbow(25)) +
    theme(text = element_text(size = 14, face = "bold")) +
    labs(title = "Crime amount of npu",
         colour = "npu")+
    scale_x_continuous(limits = c(2010,2016), expand = c(0, 0))+
    xlab("Year") + ylab("Total Amount of Crimes")
  
}
ui<-fluidPage(
  theme = bs_theme(bootswatch = "simplex"),
  tags$h1("Mapping Atlanta crime incidences from 2010 to 2016"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crime_type", "Select a crime type",crime_type,multiple = T,selected = crime_type[1]),
      selectInput("NPU","Select a NPU",npu,multiple = T,selected = npu[1])
    ),
    mainPanel(
        plotOutput("plot1"),
        plotOutput("plot2")
      )
    )
  )
  

server<-function(input,output){
  crime_subset<-reactive({
    Atlanta[which((Atlanta$crime %in% input$crime_type )& (Atlanta$npu%in% input$NPU)), ]
  })
  
  
  
  output$plot1<-renderPlot({
    crime_plot(crime_subset())
  })
  output$plot2<-renderPlot({
    count_plot(crime_subset())
  })
  
}


shinyApp(ui, server)
