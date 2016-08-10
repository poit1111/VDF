library(shiny)
library(forecast,quietly=T)
library(RJDBC,quietly=T)

loginOracle <- function(server,account,password){
  drv <- JDBC("oracle.jdbc.OracleDriver",classPath="C:/Oracle11g/product/11.2.0/client_1/jdbc/lib/ojdbc5.jar", " ")
  con <<- dbConnect(drv, paste("jdbc:oracle:thin:@//",server),account,password)}

Logged = FALSE;

ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(titlePanel(title="Oralce Database Login",windowTitle="Oralce Database Login"),
                  textInput("serverName", "Server"),
                  textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2<-fluidPage(
  
  titlePanel(title="Volume Driver Forecast",windowTitle="Volume Driver Forecast"),
  
  fluidRow(
    
    column(4,sliderInput("store", "Select stores",min=1,max=7034,value=1,width = '600px')),
    
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

ui = (htmlOutput("page"))

server1 = function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(exists("con"))rm("con",pos = ".GlobalEnv")
          try(loginOracle(serverName, Username,Password),silent = T)
          ifelse(exists("con"),USER$Logged <- TRUE,USER$Logged <- FALSE)
            } 
          }
        } 
  })
  
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,ui2))
      })
      
      print(ui)
      
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
  })
  
  
}

shinyApp(ui, server1,options=list(launch.browser=TRUE))
  
