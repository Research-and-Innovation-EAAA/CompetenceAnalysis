library(DBI)
library(RMariaDB)
library(shiny)

source('credentials.R')

ui <- fluidPage(
  #####     REGION     #####
  selectInput(inputId = "regChoice",
              label = "Select Region", 
              choices = list("storkoebenhavn", "nordsjaelland", "region-sjaelland", "fyn", "region-nordjylland", "region-midtjylland", "sydjylland", "bornholm", "skaane", "groenland", "faeroeerne", "udlandet"),
              multiple = FALSE, 
              width = "400px"),
  plotOutput("regionDiagram"),
  
  #####     DATE RANGE      #####
  dateRangeInput('dateRange',
                 label = 'Date range input: yyyy-mm-dd \n Ældste dato er 2018-05-20',
                 start = "2018-05-20", end = Sys.Date()
  ),
  plotOutput("dateRangeDiagram")
)

server <- function(input, output){
  
  #####     REGION     #####
  # Plot chosen region data
  observeEvent(input$regChoice, {
    
    # Connect to my-db as defined in ~/.my.cnf
    con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
    stopifnot(is.object(con))  
  
    q1 <- 'select k.prefferredLabel, k.altLabels, a.region_id, count(k.prefferredLabel) as amount from kompetence k, annonce_kompetence ak, annonce a where k._id = ak.kompetence_id and ak.annonce_id = a._id and a.region_id = (select r.region_id from region r where r.name = "'
    q2 <- '") and grp = "Core" group by k._id'
    q3 <- paste0(q1, input$regChoice, q2)
    courses <- dbGetQuery(con, q3)
    dbDisconnect(con)
    
    # Sort data
    ds <- courses[order(courses$amount, decreasing = TRUE),]
    rownames(ds) <- NULL
     
    # Output data
    output$regionDiagram <- renderPlot(
      barplot(ds$amount, 
              main=paste0("Antal jobopslag for fag i regionen: ", input$regChoice), 
              names.arg = ds$prefferredLabel,
              las = 2
    ))
  })
  
  #####     DATE RANGE      #####
  observeEvent(input$dateRange, {
    con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
    stopifnot(is.object(con))
    
    q1 <- 'select k.prefferredLabel, k.altLabels, a.lastUpdated, a.timeStamp, count(k.prefferredLabel) as amount from kompetence k, annonce_kompetence ak, annonce a where k._id = ak.kompetence_id and ak.annonce_id = a._id and grp = "ict" and a.timeStamp between "'
    q2 <- '" and "'
    q3 <- '" group by k._id'
    q4 <- paste0(q1, format(input$dateRange[1]), q2, format(input$dateRange[2]), q3)
    
    data <- dbGetQuery(con, q4)
    dbDisconnect(con)
    
    
    # Output data
    output$dateRangeDiagram <- renderPlot(
      barplot(data$amount, 
              main=paste0("Antal jobopslag for kompetencer indenfor tidsrammen: ", format(input$dateRange[1]) , " til ", format(input$dateRange[2])), 
              names.arg = data$prefferredLabel,
              las = 2
      ))
  })
  
}

shinyApp(ui = ui, server = server)
