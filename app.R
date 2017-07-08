library(shiny)
library(dplyr)

#db <- FALSE

ui <- fluidPage(
  titlePanel("Grady Lab PostgreSQL Database BrowseR (DBBR)"),
  
  
  sidebarLayout(position = "left",
                sidebarPanel( 
                  # description and text in sidebar panel
                  helpText(
                    
                    p("This is a shiny app to query and access data from the Grady Lab PostgreSQL database.
                      For an overview, guidelines, and instructions to use the database, please refer to the ",
                      a("manual", 
                        href = "https://github.com/GradyLab/GL_pgsql_manual"),". To access schema details (or build a schema copy),
                      access the ",
                      a("backup files",
                        href="https://github.com/GradyLab/GradyLab_PostgreSQL")," (SQL ASCII format).")
                  ),
                  
                  # Interaction objects in the sidebar:
                  h1("Enter the Database Connection Details:"),
                  textInput("dbname", label = h5("DB Name for Connection"), value = "glcolon"),
                  textInput("hostname", label = h5("Host Name"), value = "mydb"),
                  textInput("username", label = h5("User Name"), value = ""),
                  passwordInput("password", label = h5("Password"), value = ""),
                  textInput("dbtitle", label = h5("Query Database Title"), value = "gldata"),
                  numericInput("portnumber", label = h5("Port Number"), value = ""),
                  actionButton("tablerefresh", "Refresh Table Options"),
                  h1("\nSelect Table\n"),
                  uiOutput("tableselect"),
                  downloadButton('downloadtable', 'Download Table'),
                  
                  h1("\nSelect Query"),
                  uiOutput("queryselect"),
                  checkboxGroupInput("onlyindex", label = "Only View Index Colonoscopies?", 
                                     choices = list("NO" = 1, "YES" = 2),
                                     selected = 1),
                  downloadButton('downloadquery', 'Download Query')
                  #
                  
                  ),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("DB connection", textOutput("dbcon")),
                    tabPanel("DB Tables",tableOutput("dbtables")),
                    tabPanel("Queries",tableOutput("querytable"))
                  )
                )
  )

  # content and format of main display 

  
)


server <- function(input, output) {
    
    #tablecon <- observeEvent(input$tablerefresh, {
    #  options <- src_tbls(output$dbcon)
    #})
    
    gettables <- observeEvent(input$tablerefresh,{
      
      output$dbcon <- reactive({
        
      })
      
      output$tableselect <- renderUI({
        selectInput("tableoptions",label="Select a Table",choices=src_tbls(src_postgres(dbname=input$dbname,
                                                                                host=input$hostname,
                                                                                port=input$portnumber,
                                                                                user=input$username,
                                                                                password=input$password)))
      })
      
      output$queryselect <- renderUI({
        selectInput("queryoptions",label="Select a Table to Query",choices=src_tbls(src_postgres(dbname=input$dbname,
                                                                                        host=input$hostname,
                                                                                        port=input$portnumber,
                                                                                        user=input$username,
                                                                                        password=input$password)))
      })
      
    })
    
    output$dbtables <- renderTable(as.data.frame(
      as.data.frame(tbl(src_postgres(dbname=input$dbname,
                                host=input$hostname,
                                port=input$portnumber,
                                user=input$username,
                                password=input$password),
          sql(paste0("SELECT * FROM ",input$dbtitle,".",input$tableoptions))
          ))
      )
      )
    
    output$querytable <- renderTable(as.data.frame(
      as.data.frame(tbl(src_postgres(dbname=input$dbname,
                                     host=input$hostname,
                                     port=input$portnumber,
                                     user=input$username,
                                     password=input$password),
                        sql(paste0("SELECT * FROM ",input$dbtitle,".",input$queryoptions)) # need to introduce filters!
      ))
    )
    )
    
    # download the table as csv
    output$downloadtable <- downloadHandler(
      filename = function() { paste(input$tableoptions,".csv") },
      content = function(file) {
        
        tabletowrite <- as.data.frame(tbl(src_postgres(dbname=input$dbname,
                                                       host=input$hostname,
                                                       port=input$portnumber,
                                                       user=input$username,
                                                       password=input$password),
                                          sql(paste0("SELECT * FROM ",input$dbtitle,".",input$tableoptions))
        ))
        
        write.table(tabletowrite,file,sep=",",row.names=FALSE)
      }
    )
    
    # download the query as csv
    output$downloadquery <- downloadHandler(
      filename = function() { paste(input$queryoptions,".csv") },
      content = function(file) {
        
        tabletowrite <- as.data.frame(tbl(src_postgres(dbname=input$dbname,
                                                       host=input$hostname,
                                                       port=input$portnumber,
                                                       user=input$username,
                                                       password=input$password),
                                          sql(paste0("SELECT * FROM ",input$dbtitle,".",input$queryoptions)) # need to introduce filters!
        ))
        
        write.table(tabletowrite,file,sep=",",row.names=FALSE)
      }
    )
    
    
    
}

shinyApp(ui = ui, server = server)
