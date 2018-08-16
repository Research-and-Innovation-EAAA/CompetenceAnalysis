library(DBI)
library(RMariaDB)
library(shiny)

ui <- fluidPage(
  selectInput(inputId = "regChoice",
              label = "Select Region", 
              choices = list("storkoebenhavn", "nordsjaelland", "region-sjaelland", "fyn", "region-nordjylland", "region-midtjylland", "sydjylland", "bornholm", "skaane", "groenland", "faeroeerne", "udlandet"),
              multiple = FALSE, 
              width = "400px"),
  plotOutput("diagram")
)

server <- function(input, output){
  
  # Plot chosen region data
  observeEvent(input$regChoice, {
    
    # Connect to my-db as defined in ~/.my.cnf
    con <- dbConnect(RMariaDB::MariaDB(),host = "REDACTED", user = "REDACTED", password = "REDACTED", db = "REDACTED", bigint = c("numeric"))
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
    output$diagram <- renderPlot(
      barplot(ds$amount, 
              main=paste("Antal jobopslag for fag i regionen: ", input$regChoice, ""), 
              names.arg = ds$preferredLabel
      ))
  })
}

shinyApp(ui = ui, server = server)
