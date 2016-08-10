library(shiny)
library(forecast,quietly=T)
library(RJDBC,quietly=T)
library(dygraphs,quietly=T)
library(zoo)

loginOracle()

processData <- function(date){
  data <- dbGetQuery(con,"select * from r_labjobs_vw")
  data <- data[data$week_of<=data,]
}

s.list <- sort(dbGetQuery(con,"select store_number from geography_analyst where lab_type<>'No Lab' and openclose_Status='Open'")[,1])

ui<-fluidPage(
  
  titlePanel(title="Volume Driver Forecast",windowTitle="Volume Driver Forecast"),
  
  fluidRow(
    
    column(4,sliderInput("store", "Select stores",min=min(s.list),max=max(s.list),value=s.list,width = '600px')),
    
    column(2,dateInput("date", "Sunday date of last full data week")),
    
    column(2,numericInput("l", "Number of forecast weeks", min=1,value=8)),
    
    column(4,textInput("dir", "Folder Path", 'C:/Users/700739/Documents/Work Documents/5 - Lab Forecasting',width = '500px'))
    
  ),
  
  fluidRow(actionButton("go", "Submit",icon("play", lib = "glyphicon"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
  
  br(),
  br(),
    
  fluidRow(
    
    column(5,DT::dataTableOutput('tbl')),
    
    column(7,dygraphOutput("dygraph"))
  )
)
  
server <- function(input, output,session){
  
  df<- eventReactive(input$go, {
    labs <- dbGetQuery(con,'select * from r_labjobs_vw')
    labs <- labs[labs$wdate<=input$date & labs$store %in% input$store,]
    labs$wdate <- as.Date(labs$wdate)
    labs
  })
    
  output$tbl <- DT::renderDataTable(
    as.data.frame(df()),
    options = list(lengthChange = FALSE),
    rownames= FALSE,
    caption = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      'Table 2: ', htmltools::em('This is a simple caption for the table.')
    )
  )
  
  output$dygraph <- renderDygraph ({
    labs <- as.data.frame(df())
    labs.z <- zoo(labs$labjobs, seq(from=min(labs$wdate),to=max(labs$wdate),by=7))
    labs.sales.z <- zoo(labs$sales, seq(from=min(labs$wdate),to=max(labs$wdate),by=7))
    dygraph(labs.z) %>% dyRangeSelector(height = 20) %>% dyAxis("y",drawGrid = FALSE)
  })
}

shinyApp(ui, server,options=list(launch.browser=TRUE))
