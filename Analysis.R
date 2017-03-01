library(data.table)
library(dplyr)
library(fgui)
library(ggplot2)
library(gWidgets)
library(gWidgetsRGtk2)
library(lubridate)
library(plyr)
library(reshape)
library(reshape2)
library(RGtk2)
library(sqldf)
library(xlsx)
library(xlsxjars)
library(shiny)
#the user inter face of the code
ui<-fluidPage(
  #the title of the app
  titlePanel("Analysis"),
  fluidRow(
    
    column(3, wellPanel(
      #drop down with the analysis type
      selectInput("input_type", "Enter the type of analysis",
                  c("Year", "Month")
      )
    )),
    
    #action button which updates the value dynamically
    actionButton(inputId="OK",label="Click Me to update"),
    
    #creates space for stats related display
    verbatimTextOutput("stats"),
    plotOutput("hist"),
    
    
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    ))
  ))
server<-function(input, output) {
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "Month" = sliderInput("Common1", "Select a month",
                                 min = 1, max = 12, value = 1),
           "Year" = radioButtons("Common1", "Select the year:",
          c("2015","2014","2013","9999"))
    )
  })
  observeEvent(input$OK,{
    d<-as.numeric(input$Common1)
    cat(d,"\n")
    e<-d/2
    output$stats<-renderPrint(
      {
        train <- read.csv("C:\Users\Seema Kanuri\Desktop\train.csv")
        if(e>6&&e<4000)
        {
          cat("This is data for year no: ",d,"\n")
          trainf<-filter(train,year(train$Date)==d)
          traing<-filter(train,month(train$Date)<=6)
          print(head(traing))
          trainh<-filter(train,month(train$Date)>=7)
          print(head(trainh))
          b<-round(sqldf("Select avg(sales) from traing"))
          b<-as.numeric(b)
          cat("Average sales from Jan-Jun is:-",b,"\n")
          c<-round(sqldf("Select avg(Customers) from traing"))
          c<-as.numeric(c)
          cat("Average customers from Jan-Jun is:-",c,"\n")
          z<-round(sqldf("Select avg(sales) from trainh"))
          z<-as.numeric(z)
          cat("Average sales from Jan-Jun is:-",z,"\n")
          x<-round(sqldf("Select avg(Customers) from trainh"))
          x<-as.numeric(x)
          cat("Average sales from Jan-Jun is:-",x,"\n")
          t<-c(b,c,z,x)
          labels <- c("1st asales=",b, "1st cus",c, "2nd asales",z, "2nd cus",x)
          pie(t,labels)
          output$hist=renderPlot(pie(t,labels))
        }
        else if(e<=6)
        {
          cat("This is data for month no: ",d,"\n")
          trainf<-filter(train,month(train$Date)==d)
          print(head(trainf))
          f<-round(sqldf("Select avg(sales) from trainf"))
          print(f)
          g<-round(sqldf("Select avg(Customers) from trainf"))
          print(g)
        }
        else{
          b<-round(sqldf("Select avg(sales) from train"))
          print(b)
          c<-round(sqldf("Select avg(Customers) from train"))
          print(c)
        }
        
      })
  })
}
shinyApp(ui=ui,server=server)