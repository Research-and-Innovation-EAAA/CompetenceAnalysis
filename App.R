library(DBI)
library(RMariaDB)
library(shiny)

source('credentials.R')

ui <- fluidPage(
  tabsetPanel(
    
    ##########     REGION     ##########
    tabPanel("Region", 
             selectInput(inputId = "regChoice",
                         label = "Select Region", 
                         choices = list("storkoebenhavn", "nordsjaelland", "region-sjaelland", "fyn", "region-nordjylland", "region-midtjylland", "sydjylland", "bornholm", "skaane", "groenland", "faeroeerne", "udlandet"),
                         multiple = FALSE, 
                         width = "400px"),
             plotOutput("regionDiagram")
    ),
    
  
    ##########     DATE RANGE      ##########
    tabPanel("Tidsramme", 
             dateRangeInput('dateRange',
                           label = 'Date range input: Ældste dato er 2018-05-20',
                           start = "2018-05-20", end = Sys.Date()
             ),
             plotOutput("dateRangeDiagram")
    ),
    
    
    ##########     KOMPETENCE KATEGORI      ##########
    tabPanel("Kategori",
             fluidRow(
               column(4.5, 
                      selectInput(inputId = "availableCategories",
                                  label = "Tilgængelige Kompetencer:",
                                  size = 20,
                                  selectize = FALSE,
                                  choices = list()
                      )
               ),
               column(3,
                      actionButton("addKat", "Tilføj Kategori >>"),
                      actionButton("add", "Tilføj >"),
                      actionButton("remove", "< Fjern"),
                      actionButton("removeKat", "<< Fjern Kategori")
                      
               ),
               column(4.5, 
                      selectInput(inputId = "selectedCategories",
                                  label = "Tilgængelige Kompetencer:",
                                  size = 20,
                                  selectize = FALSE,
                                  choices = list()
                      )
               )
            ),
            actionButton("categoryPlotButton", "Opdater Diagram"),
            plotOutput("categoryDiagram")
    )
  )
)

server <- function(input, output, session){
  
  ##########     REGION     ##########
  observeEvent(input$regChoice, {
    con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
    stopifnot(is.object(con))  
  
    q1 <- 'select k.prefferredLabel, a.region_id, count(ak.kompetence_id) as amount from kompetence k, annonce_kompetence ak, annonce a where k._id = ak.kompetence_id and ak.annonce_id = a._id and a.region_id = (select r.region_id from region r where r.name = "'
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
  
  ##########     DATE RANGE      ##########
  observeEvent(input$dateRange, {
    con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
    stopifnot(is.object(con))
    
    q1 <- 'select k.prefferredLabel, k.altLabels, a.lastUpdated, a.timeStamp, count(k.prefferredLabel) as amount from kompetence k, annonce_kompetence ak, annonce a where k._id = ak.kompetence_id and ak.annonce_id = a._id and grp = "ict" and a.timeStamp between "'
    q2 <- '" and "'
    q3 <- '" group by k._id'
    q4 <- paste0(q1, format(input$dateRange[1]), q2, format(input$dateRange[2]), q3)
    
    uData <- dbGetQuery(con, q4)
    dbDisconnect(con)
    
    # Sort data
    data <- uData[order(uData$amount, decreasing = TRUE),]
    rownames(data) <- NULL
    
    # Output data
    output$dateRangeDiagram <- renderPlot(
      barplot(data$amount, 
              main=paste0("Antal jobopslag for kompetencer indenfor tidsrammen: ", format(input$dateRange[1]) , " til ", format(input$dateRange[2])), 
              names.arg = data$prefferredLabel,
              las = 2
    ))
  })
  
  
  ##########     KOMPETENCE KATEGORI      ##########
  con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
  stopifnot(is.object(con))
  availableCategoryData <- dbGetQuery(con, 'select prefferredLabel, _id from kompetence where grp="ict" order by prefferredLabel asc')
  dbDisconnect(con)
  
  availableKompetencer <- as.vector(as.matrix(availableCategoryData))
  
  #kompetencer <- reactiveValues(ak = as.vector(as.matrix(availableCategoryData)), sk = list())
  kompetencer <- reactiveValues(ak = as.matrix(availableCategoryData)[,1], sk = list())
  
  updateSelectInput(session,
                    inputId = "availableCategories", 
                    choices = availableKompetencer
  )

  
  observeEvent(input$add, {
    if (!is.null(input$availableCategories)){
      
      kompetencer$sk <- c(kompetencer$sk, input$availableCategories)
      kompetencer$ak <- kompetencer$ak[kompetencer$ak != input$availableCategories]    
      
      updateSelectInput(session,
                        inputId = "selectedCategories", 
                        choices = kompetencer$sk
      )
      updateSelectInput(session,
                        inputId = "availableCategories", 
                        choices = kompetencer$ak
      )
    }
  })
  observeEvent(input$remove, {
    if (!is.null(input$selectedCategories)){
      kompetencer$ak <- c(kompetencer$ak, input$selectedCategories)
      kompetencer$sk <- kompetencer$sk[kompetencer$sk != input$selectedCategories]    
      
      updateSelectInput(session,
                        inputId = "selectedCategories", 
                        choices = kompetencer$sk
      )
      updateSelectInput(session,
                        inputId = "availableCategories", 
                        choices = kompetencer$ak
      )
    }
  })
  observeEvent(input$addKat, {
    if (!is.null(input$availableCategories)){
      done <- FALSE
      finalList <- list()
      searchList <- list(input$availableCategories)
      
      con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
      stopifnot(is.object(con))
      
      while (!done){
        subList = list()
        for (superKompetence in searchList){
          subData <- dbGetQuery(con, paste0('select dt.prefferredLabel from (select k.prefferredLabel, k.grp from kompetence k, kompetence_kategorisering kk where k.conceptUri = kk.subkompetence and kk.superkompetence = (select distinct k.conceptUri from kompetence k, kompetence_kategorisering kk where k.prefferredLabel = "', superKompetence, '" and k.conceptUri = kk.superkompetence)) dt where dt.grp = "ict"'))
          subList <- c(subList, as.vector(as.matrix(subData)))
        }
        finalList <- c(finalList, searchList)
        searchList <- subList
        if (length(subList) == 0){
          done <- TRUE
        }
      }
      dbDisconnect(con)
      
      for (kompetence in finalList){
        if (!any(kompetencer$sk==kompetence)){
          kompetencer$sk <- c(kompetencer$sk, kompetence)
          kompetencer$ak <- kompetencer$ak[kompetencer$ak != kompetence]
        }
      }
      updateSelectInput(session,
                        inputId = "selectedCategories", 
                        choices = kompetencer$sk
      )
      updateSelectInput(session,
                        inputId = "availableCategories", 
                        choices = kompetencer$ak
      )
    }
  })
  observeEvent(input$removeKat, {
    if (!is.null(input$selectedCategories)){
      done <- FALSE
      finalList <- list()
      searchList <- list(input$selectedCategories)
      
      con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
      stopifnot(is.object(con))
      
      while (!done){
        subList = list()
        for (superKompetence in searchList){
          subData <- dbGetQuery(con, paste0('select dt.prefferredLabel from (select k.prefferredLabel, k.grp from kompetence k, kompetence_kategorisering kk where k.conceptUri = kk.subkompetence and kk.superkompetence = (select distinct k.conceptUri from kompetence k, kompetence_kategorisering kk where k.prefferredLabel = "', superKompetence, '" and k.conceptUri = kk.superkompetence)) dt where dt.grp = "ict"'))
          subList <- c(subList, as.vector(as.matrix(subData)))
        }
        finalList <- c(finalList, searchList)
        searchList <- subList
        if (length(subList) == 0){
          done <- TRUE
        }
      }
      dbDisconnect(con)
      
      for (kompetence in finalList){
        if (!any(kompetencer$ak==kompetence)){
          kompetencer$ak <- c(kompetencer$ak, kompetence)
          kompetencer$sk <- kompetencer$sk[kompetencer$sk != kompetence]
        }
      }
      updateSelectInput(session,
                        inputId = "selectedCategories", 
                        choices = kompetencer$sk
      )
      updateSelectInput(session,
                        inputId = "availableCategories", 
                        choices = kompetencer$ak
      )
    }
  })
  observeEvent(input$categoryPlotButton, {
    if(length(kompetencer$sk) != 0){
      matchIndexes <- list()
      categoryMatrix <- as.matrix(availableCategoryData)
      for (kompetence in kompetencer$sk){
        matchIndexes <- c(matchIndexes, which(categoryMatrix[,1] == kompetence))
      }
      kompetenceIds <- list()
      for (index in matchIndexes){
        kompetenceIds <- c(kompetenceIds, categoryMatrix[index,2])
      }
      con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
      stopifnot(is.object(con))
      
      kompetenceData <- data.frame()
      
      for (id in kompetenceIds){
        kompetenceData <- rbind(kompetenceData, dbGetQuery(con, paste0('select k.prefferredLabel, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence ak on k._id = ak.kompetence_id where k._id = ', id, ' group by k._id')))
      }
      dbDisconnect(con)
      
      
      output$categoryDiagram <- renderPlot(
        barplot(kompetenceData$amount, 
                main="Antal jobopslag for valgte kompetencer ", 
                names.arg = kompetenceData$prefferredLabel,
                las = 2
        ))
    }
  })
}

shinyApp(ui = ui, server = server)
