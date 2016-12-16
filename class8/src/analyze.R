library(shiny)
library(shinydashboard)
library(psych)
library(dplyr)
library(qcc)
library(ggplot2)

#flights.df <- read.csv("On_Time_On_Time_Performance_2016_8.csv",header=TRUE,stringsAsFactors = FALSE)

source('src/dbSettings.R')

flights.db <- src_postgres(dbname = dbname,
                           host = host,
                           port = port,
                           user = user,
                           password = pwd)

flights <- tbl(flights.db, "connections")

# Check if everything was insert correctly
flights %>% 
  distinct(Month) %>% 
  arrange(Month) %>% 
  collect(n= Inf)

flights.df <- flights %>%
  select(FlightDate, CRSDepTime, DepDelayMinutes, UniqueCarrier, Origin) %>%
  filter(UniqueCarrier == "UA") %>%
  collect(n=Inf)

flight.df <- flights %>%
  select(FlightDate, DepTime, DepDelayMinutes) %>%
  filter(DepDelayMinutes > 0) %>%
  collect(n = Inf)

flight.df$FlightTime <- as.POSIXct(paste(flight.df$FlightDate, flight.df$DepTime), 
                                   tz = Sys.timezone(), format="%Y-%m-%d %H%M")

test <- xts(flight.df$DepDelayMinutes, flight.df$FlightTime)

test.d <- to.daily(test)

#####################################test ############333

flights.df <- flights.df %>%
  select(FlightDate, CRSDepTime, DepDelayMinutes, UniqueCarrier, Origin) %>%
  filter(UniqueCarrier == "UA") %>%
  collect()

ui <- dashboardPage(
  dashboardHeader(title = "United Airlines Flight Performance"),
  dashboardSidebar(sidebarMenu(
    menuItem("Histogram", tabName = "hist", icon = icon("dashboard")),
    menuItem("Boxplot", tabName = "boxplot", icon = icon("th")),
    menuItem("Statistical Summary",tabName = "stats",icon = icon("th")),
    menuItem("Xbar and Range Chart",tabName = "bar",icon=icon("th")),
    menuItem("Data Table",tabName = "data",icon=icon("th")),
    menuItem("Process Capabilities",tabName = "process",icon=icon("th"))
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "hist",
              fluidRow(box(uiOutput("choose_hist"))),
              fluidRow((plotOutput("histPlot",height=450,width = 450)))
      ),
      
      # Second tab content
      tabItem(tabName = "boxplot",
              fluidRow((plotOutput("boxPlot",height=450,width = 450)))
      ),
      #statistical summary
      tabItem(tabName = "stats",
              verbatimTextOutput("summary")
      ),
      #Xbar and range chart
      tabItem(tabName = "bar",
              fluidRow((plotOutput("xbarPlot",height=450,width=700)))
      ),
      #data table
      tabItem(tabName = "data",
              fluidPage(fluidRow(column(5,dataTableOutput('table'))))
      ),
      #process
      tabItem(tabName="process",
              h2("Process Capabilities"))
    )
  )
)

############################### SERVER ###############################

server <- function(input, output) {
  
  output$choose_hist <- renderUI({
    # Get the data set with the appropriate name
    options <- unique(flights.df$Origin)
    # Create the checkboxes and select them all by default
    selectInput("columns", "Choose airport", as.list(options))
  })
  
  #histogram
  output$histPlot <- renderPlot({
    hi <- flights.df %>% filter(Origin==input$columns)
    hist(hi$DepDelayMinutes)
  })
  #boxplot
  output$boxPlot <- renderPlot({
    hi <- flights.df %>% filter(Origin==input$columns)
    boxplot(hi$DepDelayMinutes)
  })
  #range and mean values
  del.mean <- mean(flights.df$DepDelayMinutes,na.rm=TRUE)
  del.max <- max(flights.df$DepDelayMinutes,na.rm=TRUE)
  del.min <- min(flights.df$DepDelayMinutes,na.rm=TRUE)
  del.range <- del.max-del.min
  #calculate upper and lower limits
  del.ucl <- mean(del.range) + 1.5*sd(del.range)
  del.lcl <- mean(del.range) - 1.5*sd(del.range)
  mean.del.range <- mean(del.range)
  #data table
  output$table <- renderDataTable(flights.df)
  
  #xbar chart
  output$xbarPlot <- renderPlot({
    hi <- flights.df %>% filter (Origin==input$columns)
    qcc(hi$DepDelayMinutes,type="xbar.one",nsigmas = 3,ylim=c(0,600))
  })
  
  #range chart
  output$rangeChart <- renderPlot({
    hi <- flights.df %>% filter (origin==input$columns)
    
  })
  
  # descriptive stats
  output$summary <- renderPrint({
    describe(flights.df)
  })
  
}

shinyApp(ui, server)