library(shiny)
library(shinythemes)
library(RPostgreSQL)
library(pool)
library(ggplot2)

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
                     textInput("username", label = h5("User Name"), value = ""),
                     passwordInput("password", label = h5("Password"), value = ""),
                     textInput("dbtitle", label = h5("Query Database Title"), value = "gldata"),
                     numericInput("portnumber", label = h5("Port Number"), value = 0),
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
                     selectInput("plottype",label="Plot Type",choices=c("histogram","boxplot","scatterplot")),
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
      tabPanel("4. Data Analysis",plotOutput("dataset"), value=4)
      , id = "conditionedPanels"
    )
  )
)


server <- function(input, output) {
  
  # define the function to call the connection object
  db.pool <- reactive(dbPool(
    dbConnect(dbDriver("PostgreSQL"),
              dbname=input$dbname,
              host=input$hostname,
              user=input$username,
              password=input$password,
              port=input$portnumber)
  ))
  
  getquerytable <- function(){
    db.pool <- db.pool()
    
    # data tables
    ctable <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".colonoscopies;")))
    polyps <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".polyps;")))
    patients <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".patients;")))
    
    # get patient ages at each colonoscopy
    age = function(from, to) {
      from_lt = as.POSIXlt(from)
      to_lt = as.POSIXlt(to)
      age = to_lt$year - from_lt$year
      ifelse(to_lt$mon < from_lt$mon | (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),age - 1, age)
    }
    
    cage <- c();
    for(i in 1:nrow(ctable)){
      cage[i] <- as.numeric(age(patients[patients$patient_id==ctable[i,]$patient_id,]$dob,
                                ctable[i,]$encounter_date))
    }
    ctable$age <- cage
    
    #=================
    # apply filters
    
    # index age range extrema
    min.index.age <- as.numeric(input$ageindex[1])
    max.index.age <- as.numeric(input$ageindex[2])
    
    c1pat <- ctable[ctable$index==TRUE
                    &  min.index.age <= ctable$age
                    & ctable$age <= max.index.age,]$patient_id
    c1 <- ctable[ctable$patient_id %in% c1pat,]
    
    # npolyps criteria
    min.indexpolyp <- as.numeric(input$npolypsindex[1])
    max.indexpolyp <- as.numeric(input$npolypsindex[2])
    
    c2pat <- c1[c1$index==TRUE 
                & min.indexpolyp <= c1$total_polyps_reported
                & max.indexpolyp >= c1$total_polyps_reported,]$patient_id
    c2 <- c1[c1$patient_id %in% c2pat,]
    
    # pathology index encounter filter
    pathselect <- input$pathoptions
    
    if(!pathselect=="no filter"){
      c3pat <- c2[c2$index==TRUE 
                  & c2$pathology_encounter==pathselect,]$patient_id
      c3 <- c2[c2$patient_id %in% c3pat,]
    } else{
      c3 <- c2
    }
    
    qocheck <- input$queryoptionscheck
    
    # queryoptions check boxes for index only and polyp details
    if("1" %in% qocheck){
      c3 <- c3[c3$index==TRUE,]
    }
    if("2" %in% qocheck){
      psvar <- c()
      cpats <- c3$patient_id
      cdate <- c3$encounter_date
      
      for(i in 1:nrow(c3)){
        poli <- polyps[polyps$patient_id==cpats[i] 
                       & polyps$encounter_date==cdate[i],]
        psvar[i] <- paste(poli$polyp_size,collapse=";")
      }
      c3$polyp_sizes <- psvar
    }
    
    # query options == colonoscopies
    if(input$queryoptions=="colonoscopies"){
      if("7" %in% qocheck){
        patdob <- c()
        for(i in 1:nrow(c3)){
          patdob[i] <- as.Date(patients[patients$patient_id==c3$patient_id[i],]$dob,origin="1970-01-01")
        }
        c3$dob <- as.Date(patdob,origin="1970-01-01")
      }
      returntable <- c3
    }
    
    # query options == polyps
    if(input$queryoptions=="polyps"){
      
      if("6" %in% qocheck){
        patdage <- c()
        for(i in 1:nrow(polyps)){
          patdage[i] <- c3[which(paste0(c3$patient_id,"_",c3$encounter_date)==paste0(polyps[i,]$patient_id,"_",polyps[i,]$encounter_date)),]$age
        }
        polyps$age <- patdage
      }
      
      
      if("7" %in% qocheck){
        patdob <- c()
        for(i in 1:nrow(polyps)){
          patdob[i] <- as.Date(patients[patients$patient_id==polyps$patient_id[i],]$dob,origin="1970-01-01")
        }
        polyps$dob <- as.Date(patdob,origin="1970-01-01")
      }
      
      
      pol1 <- polyps[which(paste0(polyps$patient_id,"_",polyps$encounter_date) %in% 
                             paste0(c3$patient_id,"_",c3$encounter_date)),]
      returntable <- pol1
    }
    
    # query options == patients
    if(input$queryoptions=="patients"){
      p1 <- patients[patients$patient_id %in% c3$patient_id,]
      
      if("3" %in% qocheck){
        edvar <- c()
        ppats <- p1$patient_id
        for(i in 1:nrow(p1)){
          cpat <- c3[c3$patient_id==ppats[i],]
          edvar[i] <- paste(cpat$encounter_date,collapse=";")
        }
        
        p1$encounter_dates <- edvar
      }
      
      if("4" %in% qocheck){
        pathdvar <- c()
        ppats <- p1$patient_id
        for(i in 1:nrow(p1)){
          cpat <- c3[c3$patient_id==ppats[i],]
          pathdvar[i] <- paste(cpat$pathology_encounter,collapse=";")
        }
        
        p1$encounter_pathologies <- pathdvar
      }
      
      if("5" %in% qocheck){
        npoldvar <- c()
        ppats <- p1$patient_id
        for(i in 1:nrow(p1)){
          cpat <- c3[c3$patient_id==ppats[i],]
          npoldvar[i] <- paste(cpat$total_polyps_reported,collapse=";")
        }
        
        p1$encounter_total_polyps <- npoldvar
      }
      
      if("6" %in% qocheck){
        agedvar <- c()
        ppats <- p1$patient_id
        for(i in 1:nrow(p1)){
          cpat <- c3[c3$patient_id==ppats[i],]
          agedvar[i] <- paste(cpat$age,collapse=";")
        }
        
        p1$encounter_ages <- agedvar
      }
      
      returntable <- p1
    }
    
    return(returntable)
  }
  
  
  # get connection status and properties
  connection <- observeEvent(input$dbconnect,{
    checkpool <- ""
    
    # try the database connection
    try({db.pool <- db.pool()
    conn <- poolCheckout(db.pool)
    conninfo <- dbGetInfo(conn)
    checkpool <- class(conn)[1]
    poolReturn(conn)},
    silent=TRUE)
    
    
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
    db.pool <- db.pool()
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
    
  })
  
  varoptions <- observeEvent(input$refreshvar,{
    if(!input$tabledata=="current query"){
      db.pool <- db.pool()
      tablequery <- paste0("SELECT * FROM ",input$dbtitle,".",input$tabledata,";")
      #tablequery <- paste0("SELECT * FROM ",input$dbtitle,".","colonoscopies",";")
      selectedtable <- as.data.frame(dbGetQuery(db.pool,tablequery))
      tablevariables <- colnames(selectedtable)
      
      output$var1 <- renderUI({
        selectInput("tablevariables1",label="Variable 1",choices=c("none",tablevariables))
      })
      output$var2 <- renderUI({
        selectInput("tablevariables2",label="Variable 2",choices=c("none",tablevariables))
      })
      output$var3 <- renderUI({
        selectInput("tablevariables3",label="Variable 3",choices=c("none",tablevariables))
      }) 
    
      } else{
      
      returntable <- getquerytable()
      
      tablevariables <- colnames(returntable)
      output$var1 <- renderUI({
        selectInput("tablevariables1",label="Variable 1",choices=c("none",tablevariables))
      })
      output$var2 <- renderUI({
        selectInput("tablevariables2",label="Variable 2",choices=c("none",tablevariables))
      })
      output$var3 <- renderUI({
        selectInput("tablevariables3",label="Variable 3",choices=c("none",tablevariables))
      })
    }
    
  })
  
  viewtable <- observeEvent(input$viewtable,{
    db.pool <- db.pool()
    tablequery <- paste0("SELECT * FROM ",input$dbtitle,".",input$tableoptions,";")
    # display tables output
    output$dbtables <- renderDataTable(
      as.data.frame(dbGetQuery(db.pool,tablequery))
    )
    
    # enable download for current selected table in csv format
    output$downloadtable <- downloadHandler( 
      filename = paste(input$tableoptions,".csv"),
      content = function(filename) { # content of table to download
        tabletowrite <- as.data.frame(dbGetQuery(db.pool(),paste0("SELECT * FROM ",input$dbtitle,".",input$tableoptions,";")))
        write.table(tabletowrite,filename,sep=",",row.names=FALSE)
      }
    )
    
  })
  
  queryselected <- observeEvent(input$submitquery,{
    db.pool <- db.pool()
    
    # data tables
    ctable <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".colonoscopies;")))
    polyps <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".polyps;")))
    patients <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".patients;")))
    
    # get patient ages at each colonoscopy
    age = function(from, to) {
      from_lt = as.POSIXlt(from)
      to_lt = as.POSIXlt(to)
      age = to_lt$year - from_lt$year
      ifelse(to_lt$mon < from_lt$mon | (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),age - 1, age)
      }
    
    cage <- c();
    for(i in 1:nrow(ctable)){
      cage[i] <- as.numeric(age(patients[patients$patient_id==ctable[i,]$patient_id,]$dob,
                     ctable[i,]$encounter_date))
    }
    ctable$age <- cage
    
    #=================
    # apply filters
    
    # index age range extrema
    min.index.age <- as.numeric(input$ageindex[1])
    max.index.age <- as.numeric(input$ageindex[2])
    
    c1pat <- ctable[ctable$index==TRUE
                          &  min.index.age <= ctable$age
                          & ctable$age <= max.index.age,]$patient_id
    c1 <- ctable[ctable$patient_id %in% c1pat,]
    
    # npolyps criteria
    min.indexpolyp <- as.numeric(input$npolypsindex[1])
    max.indexpolyp <- as.numeric(input$npolypsindex[2])
    
    c2pat <- c1[c1$index==TRUE 
                & min.indexpolyp <= c1$total_polyps_reported
                & max.indexpolyp >= c1$total_polyps_reported,]$patient_id
    c2 <- c1[c1$patient_id %in% c2pat,]
    
    # pathology index encounter filter
    pathselect <- input$pathoptions
    
    if(!pathselect=="no filter"){
      c3pat <- c2[c2$index==TRUE 
                  & c2$pathology_encounter==pathselect,]$patient_id
      c3 <- c2[c2$patient_id %in% c3pat,]
    } else{
      c3 <- c2
    }
    
    qocheck <- input$queryoptionscheck
    
    # queryoptions check boxes for index only and polyp details
    if("1" %in% qocheck){
      c3 <- c3[c3$index==TRUE,]
    }
    if("2" %in% qocheck){
      psvar <- c()
      cpats <- c3$patient_id
      cdate <- c3$encounter_date
      
      for(i in 1:nrow(c3)){
        poli <- polyps[polyps$patient_id==cpats[i] 
                       & polyps$encounter_date==cdate[i],]
        psvar[i] <- paste(poli$polyp_size,collapse=";")
      }
      c3$polyp_sizes <- psvar
    }
    
    # query options == colonoscopies
    if(input$queryoptions=="colonoscopies"){
      if("7" %in% qocheck){
        patdob <- c()
        for(i in 1:nrow(c3)){
          patdob[i] <- as.Date(patients[patients$patient_id==c3$patient_id[i],]$dob,origin="1970-01-01")
        }
        c3$dob <- as.Date(patdob,origin="1970-01-01")
      }
      returntable <- c3
    }
    
    # query options == polyps
    if(input$queryoptions=="polyps"){
      
      if("6" %in% qocheck){
        patdage <- c()
        for(i in 1:nrow(polyps)){
          patdage[i] <- c3[which(paste0(c3$patient_id,"_",c3$encounter_date)==paste0(polyps[i,]$patient_id,"_",polyps[i,]$encounter_date)),]$age
        }
        polyps$age <- patdage
      }
      
      
      if("7" %in% qocheck){
        patdob <- c()
        for(i in 1:nrow(polyps)){
          patdob[i] <- as.Date(patients[patients$patient_id==polyps$patient_id[i],]$dob,origin="1970-01-01")
        }
        polyps$dob <- as.Date(patdob,origin="1970-01-01")
      }
      
      
      pol1 <- polyps[which(paste0(polyps$patient_id,"_",polyps$encounter_date) %in% 
                       paste0(c3$patient_id,"_",c3$encounter_date)),]
      returntable <- pol1
    }
    
    # query options == patients
    if(input$queryoptions=="patients"){
      p1 <- patients[patients$patient_id %in% c3$patient_id,]
      
      if("3" %in% qocheck){
        edvar <- c()
        ppats <- p1$patient_id
        for(i in 1:nrow(p1)){
          cpat <- c3[c3$patient_id==ppats[i],]
          edvar[i] <- paste(cpat$encounter_date,collapse=";")
        }
        
        p1$encounter_dates <- edvar
      }
      
      if("4" %in% qocheck){
        pathdvar <- c()
        ppats <- p1$patient_id
        for(i in 1:nrow(p1)){
          cpat <- c3[c3$patient_id==ppats[i],]
          pathdvar[i] <- paste(cpat$pathology_encounter,collapse=";")
        }
        
        p1$encounter_pathologies <- pathdvar
      }
      
      if("5" %in% qocheck){
        npoldvar <- c()
        ppats <- p1$patient_id
        for(i in 1:nrow(p1)){
          cpat <- c3[c3$patient_id==ppats[i],]
          npoldvar[i] <- paste(cpat$total_polyps_reported,collapse=";")
        }
        
        p1$encounter_total_polyps <- npoldvar
      }
      
      if("6" %in% qocheck){
        agedvar <- c()
        ppats <- p1$patient_id
        for(i in 1:nrow(p1)){
          cpat <- c3[c3$patient_id==ppats[i],]
          agedvar[i] <- paste(cpat$age,collapse=";")
        }
        
        p1$encounter_ages <- agedvar
      }
      
      returntable <- p1
    }
    
    output$dbquery <- renderDataTable(
      returntable
    )
    
    # enable download for current query result as table in csv format
    output$downloadquery <- downloadHandler( 
      filename = paste(returntable,".csv"),
      content = function(filename) { # content of query as table, to download
        querytabletowrite <- returntable
        write.table(querytabletowrite,filename,sep=",",row.names=FALSE)
      }
    )
    
    
  })
  
  dataanalysis <- observeEvent(input$makeplot,{
    plot.dat <- reactiveValues(main=NULL, layer1=NULL)
    
    
    if(!input$tabledata=="current query"){
      db.pool <- db.pool()
      selectedtable <- as.data.frame(dbGetQuery(db.pool,paste0("SELECT * FROM ",input$dbtitle,".",input$tabledata,";")))
    } else{
      selectedtable <- getquerytable()
    }
      
    if(input$tablevariables1=="none"){
      plot.dat$main <- "Plotting Error: Please select a primary variable"
    }
    else{
      if(!input$tablevariables2=="none"){
        if(!input$tablevariables3=="none"){
          if(input$plottype=="scatterplot"){
            plot.dat$main <- ggplot(data=selectedtable,aes(input$tablevariables1,input$tablevariables2,fill=input$tablevariables3))+geom_point(stat="identity")
          }
          if(input$plottype=="boxplot"){
            plot.dat$main <- ggplot(data=selectedtable,aes(input$tablevariables1,input$tablevariables2,fill=input$tablevariables3))+geom_boxplot()
          }
          else{
            plot.dat$main <- paste0("Error: invalid plot parameters")
          }
          
        } else{
          if(input$plottype=="histogram"){
            plot.dat$main <- ggplot(data=selectedtable,aes(input$tablevariables1,input$tablevariables2))+geom_histogram()
          } 
          if(input$plottype=="boxplot"){
            plot.dat$main <- ggplot(data=selectedtable,aes(input$tablevariables1,input$tablevariables2))+geom_boxplot()
          }
          if(input$plottype=="scatterplot"){
            plot.dat$main <- ggplot(data=selectedtable,aes(input$tablevariables1,input$tablevariables2))+geom_point(stat="identity")
          }
          
        }
      }
      else{
       if(input$plottype=="histogram"){
         plot.dat$main <- ggplot(data=selectedtable,aes(input$tablevariables1))+geom_histogram()
       }
        else{
          plot.dat$main <- paste0("Error: invalid plot parameters")
        }
      }
    }
    output$dataset <- renderPlot(plot.dat$main)
    
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)


