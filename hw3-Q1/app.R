#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Global Code

if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny")}
if("shinydashboard" %in% rownames(installed.packages()) == FALSE) {install.packages("shinydashboard")}
if("leaflet" %in% rownames(installed.packages()) == FALSE) {install.packages("leaflet")}
if("questionr" %in% rownames(installed.packages()) == FALSE) {install.packages("questionr")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}


library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(questionr)
library(grid)
library(rsconnect)
library(RCurl)
library(tidyverse)

data_url <- getURL('https://raw.githubusercontent.com/santoshmanjrekar/data608/master/hw3-Q1/cleaned-cdc-mortality-1999-2010-2.csv')
mortalityData <- read_csv(data_url)
Q1Data <- subset(mortalityData, Year == 2010)

#find National Average for each year for each type of death
nationalAvg <- as.data.frame(with(mortalityData, wtd.table(ICD.Chapter, Year, weights = round(100 * Deaths/(Population), 2))))

# UI

dashHeader <- dashboardHeader(title='Data 608')
dashSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text='Home',
      tabName='HomeTab',
      icon=icon('dashboard')
    ),
    menuItem(
      text='Question1',
      tabName='Question1Tab',
      icon=icon('question')
    ),
    menuItem(
      text='Question2',
      tabName='Question2Tab',
      icon=icon('question')
    )
  )
) 
dashBody <- dashboardBody(
  shinyjs::useShinyjs(),
  tabItems(
    tabItem(
      tabName = 'HomeTab',
      h1('Project 3'),
      p('This is the home page for DATA 608 - Project 3.  Submitted by Santosh Manjrekar')
    ),
    tabItem(
      tabName = 'Question1Tab',
      h1('Question 1'),
      p('As a researcher, you frequently compare mortality rates from particular causes across
        different States. You need a visualization that will let you see (for 2010 only) the crude
        mortality rate, across all States, from one cause (for example, Neoplasms, which are
        effectively cancers). Create a visualization that allows you to rank States by crude mortality
        for each cause of death.'),
      fluidRow(
        box(
          width = 12,
          collapsible = TRUE,
          selectInput(
            inputId = 'CauseSelector',
            label= 'Select Cause of Death',
            choices=unique(mortalityData$ICD.Chapter)
          )
          
        ),
        tabBox(
          width = 12,
          tabPanel(
            title='Data Table',
            DT::dataTableOutput(outputId = 'Q1Table')
          ),
          tabPanel(
            title='Visualisation',
            plotOutput(outputId = 'Q1Plot', height=500)
            
          )
          
        )
      )
      ),
    tabItem(
      tabName = 'Question2Tab',
      h1('Question 2'),
      p('Often you are asked whether particular States are improving their mortality rates (per cause)
        faster than, or slower than, the national average. Create a visualization that lets your clients
        see this for themselves for one cause of death at the time. Keep in mind that the national
        average should be weighted by the national population.'),
      fluidRow(
        box(
          width = 6,
          collapsible = TRUE,
          selectInput(
            inputId = 'CauseSelector2',
            label= 'Select Cause of Death',
            choices=unique(mortalityData$ICD.Chapter)
          )
        ),
        box(
          width=6,
          collapsible = TRUE,
          selectInput(
            inputId = 'StateSelector',
            label = 'Select State',
            choices = unique(mortalityData$State)
          )
        ),
        tabBox(
          width = 12,
          tabPanel(
            title='Data Table',
            DT::dataTableOutput(outputId = 'Q2Table')
          ),
          tabPanel(
            title='Visualisation',
            plotOutput(outputId = 'Q2Plot')
            
          )
          
        )
      )
      )
    
  )
  
  )



ui <- dashboardPage(
  header=dashHeader,
  sidebar = dashSidebar,
  body= dashBody,
  title='Data 608 Project 3',
  skin = 'purple'
)


server <- shinyServer(function(input,output,session)
{
  mortality2010 <- reactive({
    db <- subset(Q1Data, ICD.Chapter == input$CauseSelector)
    return(db)
  })
  
  output$Q1Table <- DT::renderDataTable({
    DT::datatable(mortality2010())
  })
  
  output$Q1Plot <- renderPlot({
    ggplot(mortality2010(), aes(x=State, y=Crude.Rate)) + geom_bar(stat='identity') + coord_flip()
  })
  
  mortalityQ2 <- reactive({
    db2 <- mortalityData[mortalityData$ICD.Chapter == input$CauseSelector2 & mortalityData$State == input$StateSelector,]
    return(db2)
  })
  
  output$Q2Table <- DT::renderDataTable({
    DT::datatable(mortalityQ2())
  })
  
  ntlavg <- reactive({
    db3 <- nationalAvg[nationalAvg$Var1 == input$CauseSelector2,]
    return(db3)
  })
  
  
  
  output$Q2Plot <- renderPlot({
    p1 <- ggplot(ntlavg(), aes(x=Var2, y=Freq)) + geom_point() + ggtitle("National Average") + xlab("Year") + ylab("National Average")
    p2 <- ggplot(mortalityQ2(), aes(x=Year, y = Crude.Rate)) + geom_bar(stat="identity") + ggtitle("Mortality Rate for selected State")
    
    grid.newpage()
    grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
    
  })
  
})

shinyApp(ui=ui, server=server)

