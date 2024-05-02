library(shiny)
library(ggplot2)
library(tidyverse)
library(jsonlite)

ui<- fluidPage(
  
  titlePanel("Veiklos kodas 452000"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Įmonė",
                     "Pasirinkite įmonę",choices=NULL)),
    
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server<-function(input, output, session) {
  data=read.csv("../data/452000.csv")
  updateSelectizeInput(session, "Įmonė", choices = data$name, server = T)
  output$plot=renderPlot(
    data%>%
      filter(name==input$Įmonė)%>%
      ggplot(aes(x=ym(month), y=avgWage))+geom_point()+geom_line()+theme_classic()+labs(x="Mėnesis", y="Vidutinis atlyginimas")
  )
}

shinyApp(ui = ui, server = server)