library(shiny)
library(shinythemes)
library(RPostgreSQL)
library(pool)
library(ggplot2)

# functions for server
source("shinyfunctions2.R")

# global pool connection to be referenced 
{try({
  db.pool <- db.poolfun(dbname="",
                          host="",
                          user="",
                          password="",
                          port="");
  ctable <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",dbtitle,".colonoscopies;")))
  polyps <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",dbtitle,".polyps;")))
  patients <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",dbtitle,".patients;")))
  analysistable <- c()
  
  },silent=TRUE
)}

# shiny UI
{
ui <- fluidPage(theme=shinytheme("cerulean"),
  titlePanel(title=div(div(a(img(src="glgithub.jpg", height=100,width=175,align="right"),href="https://github.com/GradyLab"),
                       HTML(paste(tags$span(style="color:black",h1("Grady Lab Database shinyR Browser"))))),
             header=h4("Directions: 1. Connect to DB >> 2. Refresh Tables >> 3. Submit a Query"))),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     helpText(h3("Database Connection")),
                     helpText("This tab shows connection details",paste("Grady Lab Database shinyR Browser")),
                     helpText(h4("Enter your connection info:")),
                     textInput("dbname", label = h5("DB Name for Connection"), value = "glcolon"),
                     textInput("hostname", label = h5("Host Name"), value = "mydb"),
                     textInput("username", label = h5("User Name"), value = "smaden"),
                     passwordInput("password", label = h5("Password"), value = "glpassword"),
                     textInput("dbtitle", label = h5("Query Database Title"), value = "gldata"),
                     numericInput("portnumber", label = h5("Port Number"), value = 32081),
                     actionButton("dbconnect", "Connect to Database")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     helpText(h3("Database Tables")),
                     helpText("This tab shows available tables from the connected database."),
                     helpText(h4("Select a table from the connected database:")),
                     actionButton("tablerefresh", "Refresh Table Options"),
                     uiOutput("tableselect"),
                     actionButton("viewtable", "View Table"),
                     downloadButton('downloadtable', 'Download Table')
    ),
    conditionalPanel(condition="input.conditionedPanels==3",
                     helpText(h3("Table Queries")),
                     helpText("This tab shows results of queries placed to the database tables."),
                     
                     # Table to query
                     uiOutput("queryselect"),
                     
                     helpText(h5("Index Filters")),
                     sliderInput("ageindex", label = h6("Age at Index"), min = 0, 
                                 max = 150, value = c(0, 150)),
                     sliderInput("npolypsindex", label = h6("Number of Polyps at Index"), min = 0, 
                                 max = 20, value = c(0, 20)),
                     uiOutput("encounterpathology"),
                     checkboxGroupInput("queryoptionscheck", label = h5("Additional Query Options"), 
                                        choices = list("Return only Index" = 1, 
                                                       "Return Recorded Polyp Sizes" = 2, 
                                                       "Return Colonoscopy Dates" = 3,
                                                       "Return Encounter Pathologies" = 4,
                                                       "Return Encounter Total Polyps" = 5,
                                                       "Return Encounter Patient Ages" = 6,
                                                       "Return Patient Date of Birth" = 7)),
                     actionButton("submitquery", "Submit Query"),
                     downloadButton('downloadquery', 'Download Query Table')
                     
    ),
    conditionalPanel(condition="input.conditionedPanels==4",
                     helpText(h3("Data Analysis")),
                     helpText("This tab provides tools to analyze and summarize tables and queries."),
                     selectInput("plottype",label="Plot Type",choices=c("histogram")),
                     uiOutput("dataselect"),
                     actionButton("refreshvar", "Refresh Variables"),
                     uiOutput("var1"),
                     uiOutput("var2"),
                     uiOutput("var3"),
                     actionButton("makeplot", "Plot")
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("1. Connection",htmlOutput("dbcon"), value=1), 
      tabPanel("2. Tables",dataTableOutput("dbtables"), value=2),
      tabPanel("3. Queries",dataTableOutput("dbquery"), value=3),
      tabPanel("4. Data Analysis",htmlOutput("analysistext1"),
               plotOutput("dataset"),
               htmlOutput("analysistext2"),
               dataTableOutput("analysistableout"), value=4)
      , id = "conditionedPanels"
    )
  )
)
}

# shiny server
server <- function(input, output) {
 
  # get connection status and properties
  connection <- observeEvent(input$dbconnect,{
    checkpool <- ""
    
    # try the database connection
    db.pool <<- db.poolfun(dbname=input$dbname,
                               host=input$hostname,
                               user=input$username,
                               password=input$password,
                               port=input$portnumber)
    
    conn <- poolCheckout(db.pool)
    conninfo <- dbGetInfo(conn)
    checkpool <- class(conn)[1]
    poolReturn(conn)
    
    
    # test for correct connection object properties, 
    # return informative html text accordingly
    if(checkpool=="PostgreSQLConnection"){
      output$dbcon <- renderUI({
        str.con1 <- paste0("Success! Connected to database: ",input$dbtitle)
        str.con2 <- paste(conninfo,collapse="<br/>")
        HTML(paste(tags$span(style="color:green",str.con1)),"<br/>Connection Info:<br/>",str.con2)
        
      })
    } else{
      output$dbcon <- renderUI({
        str.error <- paste0("Error: database connection not successful :(")
        HTML(paste(tags$span(style="color:red",str.error)))
      })  
    }
})
    
  # retrieve database tables 
  gettables <- observeEvent(input$tablerefresh,{
    tableoptions <- dbListTables(db.pool)
    output$tableselect <- renderUI({
      selectInput("tableoptions",label="Select a Table",choices=tableoptions)
    })
    output$queryselect <- renderUI({
      selectInput("queryoptions",label="Select a Table to Query",choices=tableoptions)
    })
    
    # ui ouput for encounter pathology
    ctable <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".colonoscopies;")))
    pathoptionlist <- unique(levels(as.factor(ctable$pathology_encounter)))
    output$encounterpathology <- renderUI({
      selectInput("pathoptions",label=h5("Index Encounter Pathology"),choices=c("no filter",pathoptionlist))
    })
    
    # ui ouput for plot analysis of db tables
    output$dataselect <- renderUI({
      selectInput("tabledata",label="Select a Dataset",choices=c(tableoptions,"current query"))
    })
    
    ctable <<- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".colonoscopies;")))
    polyps <<- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".polyps;")))
    patients <<- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".patients;")))
  })
  
  # view table in table tab main space
  viewtable <- observeEvent(input$viewtable,{
    tablequery <- paste0("SELECT * FROM ",input$dbtitle,".",input$tableoptions,";")
    # display tables output
    tableload <-as.data.frame(dbGetQuery(db.pool,tablequery))
    output$dbtables <- renderDataTable(
      tableload
    )
    
    # enable download for current selected table in csv format
    output$downloadtable <- downloadHandler(
      filename = function() { paste0(input$tableoptions,'.csv') },
      content = function(file) {
        write.csv(tableload, file)
      }
    )
    
  })
  
  # generate query table and view in query tab main space
  getquerytable <- observeEvent(input$submitquery,{
    currentquerytable <- queryselected(dbtitle=input$dbtitle,
                                       ageindex1=input$ageindex[1],
                                       ageindex2=input$ageindex[2],
                                       npolypsindex1=input$npolypsindex[1],
                                       npolypsindex2=input$npolypsindex[2],
                                       pathoptions=input$pathoptions,
                                       queryoptionscheck=input$queryoptionscheck,
                                       queryoptions=input$queryoptions)
    output$dbquery <- renderDataTable(
      currentquerytable
    )
    write.csv(currentquerytable,"currentquerytable.csv")
    
    output$downloadquery <- downloadHandler( 
      filename = function() {paste0(input$tableoptions,"_query",".csv") },
      content = function(file){write.csv(currentquerytable,file)}
    )
    
  })
  
  # generate variable options in data analysis tab from tables and query
  varoptions <- observeEvent(input$refreshvar,{
    if(input$tabledata=="current query"){
      analysistable <<- read.csv("currentquerytable.csv")
    }
    else{
      analysistable <<- eval(parse(text=paste0(input$tabledata)))
    }
    tablevariables <- colnames(analysistable)
    output$var1 <- renderUI({
      selectInput("tablevariables1",label="Variable 1",choices=c("none",tablevariables))
      })
    
    output$var2 <- renderUI({
      selectInput("tablevariables2",label="Variable 2",choices=c("none",tablevariables))
      })
    output$var3 <- renderUI({
      selectInput("tablevariables3",label="Variable 3",choices=c("none",tablevariables))
      })
    })
  
  #
  dataanalysis <- observeEvent(input$makeplot,{
    try({
      var1 <- analysistable[,input$tablevariables1,]
      var2 <- analysistable[,input$tablevariables2,]
      var3 <- analysistable[,input$tablevariables3,]},silent=TRUE
      )
    
    # current analysis table
    if(input$tabledata!="current query"){
      currenttable <- input$tabledata
    } else{
      currenttable <- "current query"
    }
    
    # output to data analysis tab
    output$analysistext1 <- renderUI({
      strvar1 <- paste("You are viewing a(n) ",input$plottype,"of variable '",input$tablevariables1,"' from analysis table: ",currenttable)
      HTML(paste(tags$span(style="color:green",h3(strvar1))))
      })
    
    output$dataset <- renderPlot(ggplot(analysistable,aes(x=analysistable[,input$tablevariables1,]))+geom_histogram())
    
    output$analysistext2 <- renderUI({
      HTML(paste(tags$span(style="color:green",h3(paste0("Current analysis table: ",currenttable)))))
    })
    output$analysistableout <- renderDataTable(analysistable)
  })
  
}

shinyApp(ui = ui, server = server)
