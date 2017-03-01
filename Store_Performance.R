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

#the user inter face of the code
ui <- fluidPage(
  
  tags$h1(tags$u(tags$b("Effect of Competition on Store Performance"))),
  selectInput("CompetitionOpenSinceYear", label = h3("Select Competition Year:"), 
              choices = list("2013" = 2013, "2014" = 2014, "2015" = 2015), 
              selected = 2013),
  selectInput("Type", label = h3("Analyse over"), 
              choices = list("Sales" = "Sales", "Customers" = "Customers"), 
              selected = "Sales"),
  actionButton(inputId = "Competition", 
               label = "Analyse"),
  plotOutput("SC"),
  
  
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$h1(tags$u(tags$b("Effect of Competition and Promo on Sales"))),
  
  
  textInput(inputId="S", label=h3("Enter Store number between 1 to 1115"), value = "1", width =NULL, placeholder = NULL),
  
  #selectInput("P", label = h3("Promo"), 
              #choices = list("Promo On" = 1, "Promo Off" = 0), 
              #selected = 2013),
  selectInput("Type1", label = h3("Analyse over"), 
              choices = list("Sales", "Customers"), 
              selected = "Sales"),
 
  actionButton(inputId = "CompetitionNPromo", 
               label = "Analyse"),
  plotOutput("CC")
  
  
)

#server function drives the logic of all the temp mentioned in UI function
server <- function(input,output) {
  Rossmann<-read.csv("D:\\scripting project\\Rossmann.csv")
  
  #waits for an event from the user to proceed further
  observeEvent(input$Competition, {
  alpha=as.numeric(input$CompetitionOpenSinceYear)
  beta=as.character(input$Type)
  if(beta=="Sales")
  {
  #Filter the sales data considering sales after the competition year from Jan to June
  a<-filter(Rossmann,Rossmann$CompetitionOpenSinceYear==alpha,year(Rossmann$Date)>=alpha,month(Rossmann$Date)<=6)
  a1<-sqldf("select * from a where Promo=0")
  b<-tapply(a1$Sales,a1$Store,mean)
  b1<-as.vector(b)
  
  
  #Filter the sales data considering sales after the competition year from June to December
  p<-filter(Rossmann,Rossmann$CompetitionOpenSinceYear==alpha,year(Rossmann$Date)>=alpha,month(Rossmann$Date)>6)
  p1<-sqldf("select * from p where Promo=0")
  q<-tapply(p1$Sales,p1$Store,mean)
  q1<-as.vector(q)
  #q2<-as.data.frame(q)
  #m1<-mean(q2[[1]])
  
  
  #Finding the Store with maximum sales
  avg1<-sqldf("select store,avg(Sales) as m from a1 group by Store")
  avg2<-sqldf("select store,avg(Sales) as m from p1 group by Store")
  max1<-sqldf("select Store as Store_With_Max_Sales from avg1 where m =(select max(m) from avg1) ")
  max2<-sqldf("select Store as Store_With_Max_Sales from avg2 where m =(select max(m) from avg2) ")
  max=max(max1,max2)
  
  
  #Plotting a grouped barplot
  o<-rbind(b1,q1)
  cols<-c('green','blue')
  output$SC <- renderPlot({
  barplot(o,main=paste("Effect of Selected CompetitionOpenYear on Sales \n \n The Store with Maximum Sales is",max),font.main=3,xlab="Store Numbers->",ylab="Sales->",col=cols,beside=T)
  legend("topright", 
         legend = c("Jan-Jun", "Jun-July"),
         fill = c("Green", "Blue"))  
  })
  }
  else 
    {
      if(beta=="Customers")
  {
    #Filter the sales data considering sales after the competition year from Jan to June
    a<-filter(Rossmann,Rossmann$CompetitionOpenSinceYear==alpha,year(Rossmann$Date)>=alpha,month(Rossmann$Date)<=6)
    a1<-sqldf("select * from a where Promo=0")
    b<-tapply(a1$Customers,a1$Store,mean)
    b1<-as.vector(b)
    
    
    #Filter the sales data considering sales after the competition year from June to December
    p<-filter(Rossmann,Rossmann$CompetitionOpenSinceYear==alpha,year(Rossmann$Date)>=alpha,month(Rossmann$Date)>6)
    p1<-sqldf("select * from p where Promo=0")
    q<-tapply(p1$Customers,p1$Store,mean)
    q1<-as.vector(q)
    #q2<-as.data.frame(q)
    #m1<-mean(q2[[1]])
    
    
    #Finding the Store with maximum sales
    avg1<-sqldf("select store,avg(Customers) as m from a1 group by Store")
    avg2<-sqldf("select store,avg(Customers) as m from p1 group by Store")
    max1<-sqldf("select Store as Store_With_Max_Customers from avg1 where m =(select max(m) from avg1) ")
    max2<-sqldf("select Store as Store_With_Max_Customers from avg2 where m =(select max(m) from avg2) ")
    max=max(max1,max2)
    
    
    #Plotting a grouped barplot
    o<-rbind(b1,q1)
    cols<-c('green','blue')
    output$SC <- renderPlot({
    barplot(o,main=paste("Effect of Selected CompetitionOpenYear on Sales \n \n The Store with Maximum Customers is",max),font.main=3,xlab="Store Numbers->",ylab="Customers->",col=cols,beside=T)
    legend("topright", 
           legend = c("Jan-Jun", "Jun-July"),
           fill = c("Green", "Blue")) 
    })
      }
  }
})
  #waits for an event from the user to proceed further
  observeEvent(input$CompetitionNPromo, {
    alpha=as.numeric(input$S)
    #gamma=as.numeric(input$P)
    beta=as.character(input$Type1)
    if(beta =='Sales')
    {
    #Filter the sales data considering data after the competition year from Jan to June
    i<-filter(Rossmann,Rossmann$Store==alpha)
    a<-filter(i,year(i$Date)>=i$CompetitionOpenSinceYear,month(i$Date)<=6)
    a1<-sqldf("select * from a where Promo=0 and CompetitionOpenSinceYear IS NOT NULL")
    b<-mean(a1$Sales)
    
    
    #Filter the sales data considering data after the competition year from June to December
    k<-filter(Rossmann,Rossmann$Store==alpha)
    p<-filter(k,year(k$Date)>=k$CompetitionOpenSinceYear,month(k$Date)>6)
    p1<-sqldf("select * from p where Promo=0 and CompetitionOpenSinceYear IS NOT NULL")
    q<-mean(p1$Sales)
    
    j<-filter(Rossmann,Rossmann$Store==alpha)
    c<-filter(j,year(j$Date)>=j$CompetitionOpenSinceYear,month(j$Date)<=6)
    c1<-sqldf("select * from c where Promo=1 and CompetitionOpenSinceYear IS NOT NULL")
    d<-mean(c1$Sales)
    
    
    #Filter the sales data considering data after the competition year from June to December
    l<-filter(Rossmann,Rossmann$Store==alpha)
    r<-filter(l,year(l$Date)>=l$CompetitionOpenSinceYear,month(l$Date)>6)
    r1<-sqldf("select * from r where Promo=1 and CompetitionOpenSinceYear IS NOT NULL")
    s<-mean(r1$Sales)
    
    
    Without_Promo<-b+q
    With_Promo<-s+d
    
    cols=c("Green","blue")
    
    output$CC <- renderPlot({
    barplot(cbind(With_Promo,Without_Promo),beside=T,ylab="Sales->",main=paste("Effect of Promo and Competition on Sales of store",alpha),col=cols)
    legend("topright", 
           legend = c("With Promotion and With  Competition", "Without Promotion and with competition"), 
           fill = c("Blue", "Green"))
    })
    }
    else 
      {
        if(beta =='Customers')
    {
      #Filter the sales data considering data after the competition year from Jan to June
      i<-filter(Rossmann,Rossmann$Store==alpha)
      a<-filter(i,year(i$Date)>=i$CompetitionOpenSinceYear,month(i$Date)<=6)
      a1<-sqldf("select * from a where Promo=0 and CompetitionOpenSinceYear IS NOT NULL")
      b<-mean(a1$Customers)
      
      
      #Filter the sales data considering data after the competition year from June to December
      k<-filter(Rossmann,Rossmann$Store==alpha)
      p<-filter(k,year(k$Date)>=k$CompetitionOpenSinceYear,month(k$Date)>6)
      p1<-sqldf("select * from p where Promo=0 and CompetitionOpenSinceYear IS NOT NULL")
      q<-mean(p1$Customers)
      
      j<-filter(Rossmann,Rossmann$Store==alpha)
      c<-filter(j,year(j$Date)>=j$CompetitionOpenSinceYear,month(j$Date)<=6)
      c1<-sqldf("select * from c where Promo=1 and CompetitionOpenSinceYear IS NOT NULL")
      d<-mean(c1$Customers)
      
      
      #Filter the sales data considering data after the competition year from June to December
      l<-filter(Rossmann,Rossmann$Store==alpha)
      r<-filter(l,year(l$Date)>=l$CompetitionOpenSinceYear,month(l$Date)>6)
      r1<-sqldf("select * from r where Promo=1 and CompetitionOpenSinceYear IS NOT NULL")
      s<-mean(r1$Customers)
      
      
      
      Without_Promo<-b+q
      With_Promo<-s+d
      
      cols=c("Green","blue")
      output$CC <- renderPlot({
      barplot(cbind(With_Promo,Without_Promo),beside=T,ylab="Customer Count->",main=paste("Effect of Promo and Competition on Customer count on store",alpha),col=cols)
      legend("topright", 
             legend = c("With Promotion and with Competition", "Without Promotion and with competition"),
             fill = c("Blue", "Green"))
      })
        }
      }
   
  })
}

shinyApp(ui = ui, server = server)
