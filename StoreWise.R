#loading libraries
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
ui<-fluidPage(
  #the title of the app
  titlePanel("StoreWise"),
  fluidRow(
    
    column(3, wellPanel(
      #drop down with the analysis type
      textInput(inputId = "StoreSelection",
                label="Enter the store number:",
                value="")
    )
    )),
  textInput(inputId = "TypeSelection",
            label="Enter analysis type: 1: Year Wise\n
            2: Month Wise\n
            3: Day Wise\n
            4: Proof1\n
            5: Proof2\n
            6: Promo Analysis",
            value=""),
  
  #action button which updates the value dynamically
  actionButton(inputId="OK",label="Click Me to update"),
  plotOutput("hist"),
  #textInput(inputId = "title",
            #label="write something",
            #value="we gotta use for plots"),
  
  #creates space for stats related display
  #verbatimTextOutput("stats"),
  plotOutput("grpah"),
  
  
  
  column(3, wellPanel(
    # This outputs the dynamic UI component
    uiOutput("ui")
  ))
)
server<-function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$Common1))
      return()
  })
  observeEvent(input$OK,{
    train <- read.csv("D:/R project/Gaurav/train.csv")
    store <- read.csv("D:/R project/Gaurav/store.csv")
    store_input<-as.numeric(input$StoreSelection)
    print(store_input)
    if(store_input>=1&&store_input<=1115){
      print("Correct")
    }
    else{
      print("Incorrect")
    }
    train["year"] <- year(train$Date)
    train["month"] <- month(train$Date)
    store_data<-filter(train,train$Store==store_input)
    choice <- as.integer(input$TypeSelection)
    print(choice)
    if(choice==1){
      y_w_s <-tapply(store_data$Sales,store_data$year,mean)
      output$hist=renderPlot(barplot(y_w_s,xlab = "year",ylab="Sales",main = paste("year wise sales of Store-",store_input),col = heat.colors(length(y_w_s))))
    }
    if(choice==2){
      m_w_s <- tapply(store_data$Sales,list(store_data$year,store_data$month),mean)
      output$hist=renderPlot(barplot(m_w_s,beside = T,xlab = "month",ylab="Sales",main = paste("month wise sales of Store-",store_input,"for years 2013,14,15"),col= c("blue","green","yellow")))
    }
    if(choice==3){
      d_w_s <-tapply(store_data$Sales,list(store_data$DayOfWeek,store_data$month),mean)
      output$hist=renderPlot(barplot(d_w_s,beside = T,xlab = "day of week",ylab="Sales",main = paste("day wise sales of Store-",store_input,"for years 2013,14,15"),col = c("blue","green","yellow")))
    }
    if(choice==4){
      # sales and customer relation ship
      output$hist=renderPlot(plot(store_data$Customers,store_data$Sales,type='p',col='black'))
    }
    if(choice==5){
      store_all <- store$Store
      # empty list to store our pearson's coeffient
      list_coeff <- NULL
      
      #store_data_coeff is taking store data to find coefficient
      for (i in store_all)
      { store_data_coeff <- filter(train,Store==i)
      list_coeff <- c(list_coeff,cor(store_data_coeff$Customers,store_data_coeff$Sales))
      }
      output$hist=renderPlot(hist(list_coeff,xlab = "Pearsons coefficient of corelation",main="plot to prove that sales and customers are corelated"))
    }
    #effect of promos on sales
    if(choice==6)
    {
      if(choice==6){
        promo_data<-tapply(store_data$Sales,list(store_data$year,store_data$month,store_data$Promo),mean)
        promo_data <- as.matrix(as.data.frame(promo_data))
        promo_data[is.na(promo_data)] <- 0   
        output$hist=renderPlot(barplot(promo_data,beside = T,main = paste("Comparision of promotional data of store-",store_input," for all three years"),col = c("blue","green","yellow")))
      }
    }
    #sorting data 
    
    #month wise analysis of store sales
    #with promo
    #mws_py <- ddply(store_data_promo_yes,.(month,year),summarise,sales_py=mean(Sales),dayswithpromo=length(Date))
    
    #without promo
    #mws_pn <- ddply(store_data_promo_no,.(month,year),summarise,sales_pn=mean(Sales),dayswithnopromo=length(Date))
  
    
    })
}
shinyApp(ui=ui,server=server)