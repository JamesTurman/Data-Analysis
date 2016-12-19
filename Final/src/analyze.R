library(shiny)
library(shinydashboard)
library(psych)
library(dplyr)
library(qcc)
library(ggplot2)

source('src/dbSettings.R')

flights.db <- src_postgres(dbname = dbname,
                           host = host,
                           port = port,
                           user = user,
                           password = pwd)

flights <- tbl(flights.db, "connections")

########## test ##################

  # Check if everything was insert correctly
  flights %>% 
    distinct(Month) %>% 
    arrange(Month) %>% 
    collect(n= Inf)
  flights.df <- flights %>%
    select(DayOfWeek, CRSDepTime, DepDelayMinutes, UniqueCarrier, Origin) %>%
    filter(UniqueCarrier == "UA") %>%
    collect(n=Inf)
 
#####################################test ####################
  flights.df <- flights.df %>%
    select(DayOfWeek, CRSDepTime, DepDelayMinutes, UniqueCarrier, Origin) %>%
    filter(UniqueCarrier == "UA") %>%
    collect()
  
# remove na values
flights.df <- flights.df[complete.cases(flights.df),]

################################## BUILD ANALYSIS DATA SET #############################
# pull in random sample for data to use on Xbar range and process capabilites
# sample for first day of week
dset <- flights.df %>%
  group_by(DayOfWeek)

set.seed(34)
fl.1 <- flights.df %>%
  select(DepDelayMinutes,DayOfWeek) %>%
  filter(DayOfWeek == 1)
fl.1 <- fl.1[sample(1:nrow(fl.1),1000,replace=FALSE),][1]
#sample for 2nd day of week
fl.2 <- flights.df %>%
  select(DepDelayMinutes,DayOfWeek) %>%
  filter(DayOfWeek == 2)
fl.2 <- fl.2[sample(1:nrow(fl.2),1000,replace=FALSE),][1]
# sample 3rd day of week
fl.3 <- flights.df %>%
  select(DepDelayMinutes,DayOfWeek) %>%
  filter(DayOfWeek == 3)
fl.3 <- fl.3[sample(1:nrow(fl.3),1000,replace=FALSE),][1]
# sample 4th day of week
fl.4 <- flights.df %>%
  select(DepDelayMinutes,DayOfWeek) %>%
  filter(DayOfWeek == 4)
fl.4 <- fl.4[sample(1:nrow(fl.4),1000,replace=FALSE),][1]
# sample 5th day of week
fl.5 <- flights.df %>%
  select(DepDelayMinutes,DayOfWeek) %>%
  filter(DayOfWeek == 5)
fl.5 <- fl.5[sample(1:nrow(fl.5),1000,replace=FALSE),][1]
# sample 6th day of week
fl.6 <- flights.df %>%
  select(DepDelayMinutes,DayOfWeek) %>%
  filter(DayOfWeek == 6)
fl.6 <- fl.6[sample(1:nrow(fl.6),1000,replace=FALSE),][1]
# sample 7th day of week
fl.7 <- flights.df %>%
  select(DepDelayMinutes,DayOfWeek) %>%
  filter(DayOfWeek == 7)
fl.7 <- fl.7[sample(1:nrow(fl.7),1000,replace=FALSE),][1]

# build data frame of all samples
fl.delays <- data.frame(fl.1,fl.2,fl.3,fl.4,fl.5,fl.6,fl.7)
# assign column names
colnames(fl.delays) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
rownames(fl.delays) <- NULL
# add x value
fl.delays$x <- c(1:1000)

# calculate range and mean values
fl.delays <- fl.delays %>%
  subset(select = 1:8) %>%
  mutate(max = do.call(pmax,(.)),
         min = do.call(pmin,(.)),
         mean = rowMeans(.),
         range = do.call(pmax,(.))-do.call(pmin,(.))
  )
fl.delaysX.ucl <- mean(fl.delays$mean) + 3*sd(fl.delays$mean)
fl.delaysX.lcl <- mean(fl.delays$mean) - 3*sd(fl.delays$mean)
fl.delays.ucl <- mean(fl.delays$range)+ 1.5 * sd(fl.delays$range)
fl.delays.lcl <- mean(fl.delays$range)- 1.5 * sd(fl.delays$range)


############### UI #####################

ui <- dashboardPage(
  dashboardHeader(title = "United Airlines Flight Performance"),
  dashboardSidebar(sidebarMenu(
    menuItem("Histogram", tabName = "hist", icon = icon("dashboard")),
    menuItem("Boxplot", tabName = "boxplot", icon = icon("th")),
    menuItem("Statistical Summary",tabName = "stats",icon = icon("th")),
    menuItem("X bar Chart",tabName = "bar",icon=icon("th")),
    menuItem("Data Table",tabName = "data",icon=icon("th")),
    menuItem("Process Capabilities",tabName = "process",icon=icon("th"))
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "hist",
              fluidRow(box(uiOutput("choose_hist"))),
              fluidRow((plotOutput("histPlot",height=450,width = 700)))
      ),
      
      # Second tab content
      tabItem(tabName = "boxplot",
              fluidRow((plotOutput("boxPlot",height=450,width = 700)))
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
              fluidPage(fluidRow(column(5,dataTableOutput("table"))))
      ),
      #process
      tabItem(tabName="process",
              fluidRow((plotOutput("procap",height = 450,width = 700)))
    ))
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
    m <- ggplot(hi, aes(x=DepDelayMinutes))+
      geom_histogram(aes(fill = ..count..))
    print(m)
  })
  #boxplot
  output$boxPlot <- renderPlot({
    hi <- flights.df %>% filter(Origin==input$columns)
    p <- ggplot(hi, aes(x=UniqueCarrier,y=DepDelayMinutes))+
      geom_boxplot(aes(fill = DepDelayMinutes, colour = "#3366FF"))
    print(p)
  })
  #data table
  output$table <- renderDataTable(flights.df)
  #xbar chart
  output$xbarPlot <- renderPlot({
    hi <- data.frame(flights.df %>% filter (Origin==input$columns))
    qx <- qcc(hi$DepDelayMinutes,type="xbar.one",nsigmas = 3)
    print(qx)
  })
  
  #process capabilities
  output$procap <- renderPlot({
    hi <- flights.df %>% filter (Origin==input$columns)
    qx <- qcc(hi$DepDelayMinutes,type="xbar.one",nsigmas = 3)
    process.capability(qx,spec.limits = c(fl.delaysX.lcl,fl.delaysX.ucl))
  })
  
  #range chart
 # output$rangeChart <- renderPlot({
  #  hi <- flights.df %>% filter (origin==input$columns)
    
#  })
  
  # descriptive stats
  output$summary <- renderPrint({
    describe(flights.df)
  })
  
}

shinyApp(ui, server)