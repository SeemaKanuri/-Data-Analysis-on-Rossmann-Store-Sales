#loading libraries
library(plotly)
library(ggplot2)
library(forecast)
library(reshape)
library(RH2)
library(xts)
library(dygraphs)
library(dplyr)
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(lubridate)
library(scales)
library(splines)
library(sqldf)
library(reshape2)
library(pystr)
library(reshape)
library(psych)
library(plyr)
library(PerformanceAnalytics)
library(MASS)
library(lattice)
library(gridExtra)
library(grid)
library(graphics)
library(ggplot2)
library(datasets)
library(dplyr)
library(scales)
library(tcltk)
library(lubridate)
library(forecast)
library(reshape)
library(forecast)
library(reshape)
#the user inter face of the code
ui<-fluidPage(
  #the title of the app
  titlePanel("Analysis"),
  fluidRow(
    
    column(3, wellPanel(
      #drop down with the analysis typea  
      selectInput("input_type", "Enter the type of analysis",
                  c("Assortment_Type", "Store_Type","State_Holiday")
      )
    )),
    
    #action button which updates the value dynamically
    actionButton(inputId="OK",label="Click Me to update"),
    dygraphOutput("hist"),
    plotlyOutput("Beta"),
    plotOutput('Charlie'),
    column(4, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    ))
  ))
#server function drives the logic of all the temp mentioned in UI function
server<-function(input, output) {
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    switch(input$input_type,
           "Assortment_Type" = radioButtons("Common1", "Select your choice:",
                                            c("Effect_on_sales","Customers_and_promo","Effect_on_Assortment_with_promo")),
           "Store_Type" = radioButtons("Common1", "Select your choice:",
                                       c("Effect_sales","Customer_Sales")),
           "State_Holiday"=radioButtons("Common1", "Select your choice:",
                                        c("Total_Sales","Distribution"))
    )
  })
  observeEvent(input$OK,{
    train <- read.csv("D:\\scripting project\\train.csv")
    store <- read.csv("D:\\scripting project\\store.csv")
    train$Date <- as.Date(train$Date,'%m/%d/%Y')
    my.train <- train
    my.store <- sqldf("select Store,Assortment, StoreType from store")
    my.train <- merge(my.train,my.store)
    my.train$MonthYear <- as.factor(format(my.train$Date, "%m-%Y"))
    my.train$SchoolHoliday <- as.factor(my.train$SchoolHoliday)
    my.train$Month <- as.factor(format(as.Date(my.train$Date),"%m"))
    my.train$Day <- as.factor(format(as.Date(my.train$Date),"%d"))
    my.train$Year <- as.factor(format(as.Date(my.train$Date),"%Y"))
    my.train$MonthYear <- as.factor(format(my.train$Date, "%m-%Y"))
    Alpha<-input$Common1
    if(Alpha=="Effect_on_sales"){
      Xts_Sales_by_assortment  <- aggregate(my.train$Sales,by = list(my.train$Assortment,my.train$Date),function(x){mean(as.numeric(x))})
      names(Xts_Sales_by_assortment) <- c("AssortmentType","Date", "AverageSales")
      
      Xts_Sales_by_assortment_A <- filter(Xts_Sales_by_assortment, AssortmentType=="a")
      Xts_Sales_by_assortment_B <- filter(Xts_Sales_by_assortment, AssortmentType=="b")
      Xts_Sales_by_assortment_C <- filter(Xts_Sales_by_assortment, AssortmentType=="c")
      
      
      A_xts <- xts(Xts_Sales_by_assortment_A$AverageSales,order.by=Xts_Sales_by_assortment_A$Date,frequency=365)
      B_xts <- xts(Xts_Sales_by_assortment_B$AverageSales,order.by=Xts_Sales_by_assortment_B$Date,frequency=365)
      C_xts <- xts(Xts_Sales_by_assortment_C$AverageSales,order.by=Xts_Sales_by_assortment_C$Date,frequency=365)
      
      linking <- cbind(A_xts,B_xts,C_xts)
      
      output$hist=renderDygraph({dygraph(linking,ylab="AverageSales", 
                                         main="Average Sales based on the assortment Type") %>%
          dySeries("..1",label="A-Basic") %>%
          dySeries("..2",label="B-Extended") %>%
          dySeries("..3",label="C-Extra") %>%
          dyOptions(colors = c("green","blue", "orange")) %>%
          dyRangeSelector()})
      print("seemaaa")
    }
    if(Alpha=="Customers_and_promo"){
      my.train2 <- my.train
      my.train2$Assortment <- as.character(my.train2$Assortment)
      my.train2$Assortment[my.train2$Assortment == "a"]<-"Basic Store level-A"
      my.train2$Assortment[my.train2$Assortment == "b"]<-"Extended Store level-B"
      my.train2$Assortment[my.train2$Assortment == "c"]<-"Extra Store level-C"
      my.train2$Assortment <- as.factor(my.train2$Assortment)
      
      Promo <- aggregate(my.train2$Promo,by = list(my.train2$Assortment),function(x){mean(as.numeric(x))})
      names(Promo) <- c("Assortment","Promo")
      
      sales <- aggregate(my.train2$Sales,by = list(my.train2$Assortment),function(x){mean(as.numeric(x))})
      names(sales) <- c("Assortment","Sales")
      
      Customers <- aggregate(my.train2$Customers,by = list(my.train2$Assortment),function(x){mean(as.numeric(x))})
      names(Customers) <- c("Assortment","Customers")
      
      Store <- aggregate(my.train2$Store,by = list(my.train2$Assortment),function(x){mean(as.numeric(x))})
      names(Store) <- c("Assortment","Store")
      
      output$Beta=renderPlotly({plot_ly(Customers, labels = Customers$Assortment, values = Customers$Customers, type = "pie", domain = list(x = c(0, 0.4), y = c(0.4, 1)),
                                        name = "Customers", showlegend = F) %>% layout(title = "Customers")  %>%
          add_trace(data= sales, labels = sales$Assortment, values = sales$Sales, type = "pie", domain = list(x = c(0.6, 1), y = c(0.4, 1)),
                    name = "sales", showlegend = F) %>% layout(title = "Customers")  %>%
          add_trace(data = Promo, labels = Store$Assortment, values = Promo$Promo, type = "pie", domain = list(x = c(0.25, 0.75), y = c(0, 0.6)),
                    name = "Promo", showlegend = F) %>%
          layout(title = "Percentage of Customers, Sales & Promo in the stores grp by Assortment type")})
    }
    if(Alpha=="Effect_on_Assortment_with_promo"){
      sales_by_assortment <- aggregate(my.train$Sales,by = list(my.train$Assortment,my.train$Promo,my.train$Date),function(x){mean(as.numeric(x))})
      names(sales_by_assortment) <- c("AssortmentType","Promo","Date","AverageSales")
      sales_by_assortment$AverageSales <- format(round(sales_by_assortment$AverageSales),digits=1, nsmall = 0)
      sales_by_assortment_promo <- filter(sales_by_assortment, Promo==1)
      
      #head(sales_by_assortment_promo)
      a <- filter(sales_by_assortment_promo, sales_by_assortment_promo$AssortmentType=="a")
      b <- filter(sales_by_assortment_promo, sales_by_assortment_promo$AssortmentType=="b")
      c <- filter(sales_by_assortment_promo, sales_by_assortment_promo$AssortmentType=="c")
      AverageSales <- a$AverageSales
      b <- b$AverageSales
      c <- c$AverageSales
      CalenderYear <- x <- sales_by_assortment_promo$Date
      data <- data.frame(CalenderYear, AverageSales, b, c)
      
      output$Beta=renderPlotly({plot_ly(data, x = ~CalenderYear, y = ~AverageSales, name = 'A Basic', type = 'scatter', mode = 'lines') %>%
          add_trace(y = ~b, name = 'B Extended', mode = 'lines+markers') %>%
          add_trace(y = ~c, name = 'C Extra', mode = 'markers') %>%
          layout(title = "Effect of Promotions on Average sales of Stores grp by Assortment (type)",
                 xaxis = list(title = "Calender year"),
                 yaxis = list (title = "Average Sales of stores"))})
    }
    if(Alpha=="Effect_sales"){
      par(mfrow=c(3,3))
      Sales_by_storeType <- aggregate(my.train$Sales,by = list(my.train$MonthYear,my.train$StoreType),function(x){mean(as.numeric(x))})
      names(Sales_by_storeType) <- c("Year","Store.Type","Average.Sales")
      Sales_by_storeType$Store.Type <- as.character(Sales_by_storeType$Store.Type)
      Sales_by_storeType$Store.Type[Sales_by_storeType$Store.Type == "a"]<-"StoreType-A"
      Sales_by_storeType$Store.Type[Sales_by_storeType$Store.Type == "b"]<-"StoreType-B"
      Sales_by_storeType$Store.Type[Sales_by_storeType$Store.Type == "c"]<-"StoreType-C"
      Sales_by_storeType$Store.Type[Sales_by_storeType$Store.Type == "d"]<-"StoreType-D"
      
      Sales_by_storeType$Store.Type <- as.factor(Sales_by_storeType$Store.Type)
      output$Beta=renderPlotly({plot_ly(data = Sales_by_storeType, x = ~Year, y = ~Average.Sales, color = ~Store.Type)})
      print("gaurav") }
    if(Alpha=="Customer_Sales"){
      my.train2 <- my.train
      my.train2$StoreType <- as.character(my.train2$StoreType)
      my.train2$StoreType[my.train2$StoreType == "a"]<-"Store Type A"
      my.train2$StoreType[my.train2$StoreType == "b"]<-"StoreType-B"
      my.train2$StoreType[my.train2$StoreType == "c"]<-"StoreType-C"
      my.train2$StoreType[my.train2$StoreType == "c"]<-"StoreType-D"
      my.train2$StoreType <- as.factor(my.train2$StoreType)
      
      Promo <- aggregate(my.train2$Promo,by = list(my.train2$StoreType),function(x){mean(as.numeric(x))})
      names(Promo) <- c("StoreType","Promo")
      
      sales <- aggregate(my.train2$Sales,by = list(my.train2$StoreType),function(x){mean(as.numeric(x))})
      names(sales) <- c("StoreType","Sales")
      
      Customers <- aggregate(my.train2$Customers,by = list(my.train2$StoreType),function(x){mean(as.numeric(x))})
      names(Customers) <- c("StoreType","Customers")
      
      Store <- aggregate(my.train2$Store,by = list(my.train2$StoreType),function(x){mean(as.numeric(x))})
      names(Store) <- c("StoreType","Store")
      
      output$Beta=renderPlotly({plot_ly(Customers, labels = Customers$StoreType, values = Customers$Customers, type = "pie", domain = list(x = c(0, 0.4), y = c(0.4, 1)),
              name = "Customers", showlegend = F) %>% layout(title = "Customers")  %>%
        add_trace(data= sales, labels = sales$StoreType, values = sales$Sales, type = "pie", domain = list(x = c(0.6, 1), y = c(0.4, 1)),
                  name = "sales", showlegend = F) %>% layout(title = "Customers")  %>%
        add_trace(data = Store, labels = Store$StoreType, values = Store$Store, type = "pie", domain = list(x = c(0.25, 0.75), y = c(0, 0.6)),
                  name = "Store", showlegend = F) %>%
        layout(title = "Percentage of Customers, Sales & Promo in the stores grp by StoreType type")})
    }
    if(Alpha=="Total_Sales"){
      par(mfrow=c(2,1))
      
      # I investigate how customers are related to the State Holiday throughout the data set:
      #a = public holiday, b = Easter holiday, c = Christmas, 0 = None
      
      my.train2 <- my.train
      my.train2$StateHoliday <- as.character(my.train2$StateHoliday)
      #my.train2$StateHoliday <- as.character(sales_by_state_holiday_open$StateHoliday)
      my.train2$StateHoliday[my.train2$StateHoliday == "a"]<-"public holiday"
      my.train2$StateHoliday[my.train2$StateHoliday == "b"]<-"Easter holiday"
      my.train2$StateHoliday[my.train2$StateHoliday == "c"]<-"Christmas"
      my.train2$StateHoliday[my.train2$StateHoliday == "d"]<-"No Holiday"
      my.train2$StateHoliday <- as.factor(my.train2$StateHoliday)
      
      sales_by_state_holiday_open <- aggregate(my.train2$Sales,by = list(my.train2$StateHoliday),function(x){mean(as.numeric(x))})
      sales_by_state_holiday_open <- sales_by_state_holiday_open[-1,]
      names(sales_by_state_holiday_open) <- c("StateHoliday", "Average.Sales")
      
      ggplot(data=sales_by_state_holiday_open,aes(x=StateHoliday,y=Average.Sales,fill=StateHoliday)) + geom_bar(stat="identity") + ggtitle("Average sales by state holiday")
      
    }
    if(Alpha=="Distribution"){
      my.train2 <- my.train
      my.train2$StateHoliday <- as.character(my.train2$StateHoliday)
      #my.train2$StateHoliday <- as.character(sales_by_state_holiday_open$StateHoliday)
      my.train2$StateHoliday[my.train2$StateHoliday == "a"]<-"public holiday"
      my.train2$StateHoliday[my.train2$StateHoliday == "b"]<-"Easter holiday"
      my.train2$StateHoliday[my.train2$StateHoliday == "c"]<-"Christmas"
      my.train2$StateHoliday[my.train2$StateHoliday == "0"]<-"No Holiday"
      my.train2$StateHoliday <- as.factor(my.train2$StateHoliday)
      
      sales_by_state_holiday_open <- aggregate(my.train2$Sales,by = list(my.train2$StateHoliday,my.train2$Open),function(x){mean(as.numeric(x))})
      sales_by_state_holiday_open <- sales_by_state_holiday_open[-1,]
      names(sales_by_state_holiday_open) <- c("StateHoliday","OpenStore", "Average.Sales")
      
      
      Promo <- aggregate(my.train2$Promo,by = list(my.train2$StateHoliday),function(x){mean(as.numeric(x))})
      names(Promo) <- c("StateHoliday","Promo")
      
      sales <- aggregate(my.train2$Sales,by = list(my.train2$StateHoliday),function(x){mean(as.numeric(x))})
      names(sales) <- c("StateHoliday","Sales")
      
      Customers <- aggregate(my.train2$Customers,by = list(my.train2$StateHoliday),function(x){mean(as.numeric(x))})
      names(Customers) <- c("StateHoliday","Customers")
      
      Store <- aggregate(my.train2$Store,by = list(my.train2$StateHoliday),function(x){mean(as.numeric(x))})
      names(Store) <- c("StateHoliday","Store")
      
      output$Beta=renderPlotly({plot_ly(Customers, labels = Customers$StateHoliday, values = Customers$Customers, type = "pie", domain = list(x = c(0, 0.4), y = c(0.4, 1)),
              name = "Customers", showlegend = F) %>% layout(title = "Customers")  %>%
        add_trace(data= sales, labels = sales$StateHoliday, values = sales$Sales, type = "pie", domain = list(x = c(0.6, 1), y = c(0.4, 1)),
                  name = "sales", showlegend = F) %>% layout(title = "Customers")  %>%
        add_trace(data = Promo, labels = Promo$StateHoliday, values = Promo$Promo, type = "pie", domain = list(x = c(0.25, 0.75), y = c(0, 0.6)),
                  name = "Promo", showlegend = F) %>%
        layout(title = "Percentage of Customers, Sales & Promo in the stores by state holiday type")})
    }
  })
}
shinyApp(ui=ui,server=server)