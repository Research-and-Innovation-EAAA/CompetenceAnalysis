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
               column(4, 
                      selectInput(inputId = "availableCategories",
                                  label = "Tilgængelige Kompetencer:",
                                  size = 20,
                                  selectize = FALSE,
                                  choices = list()
                      )
               ),
               column(4,
                      actionButton("addKat", "Tilføj Kategori >>"),
                      actionButton("add", "Tilføj >"),
                      actionButton("remove", "< Fjern"),
                      actionButton("removeKat", "<< Fjern Kategori")
                      
               ),
               column(4, 
                      selectInput(inputId = "selectedCategories",
                                  label = "Valgte Kompetencer:",
                                  size = 20,
                                  selectize = FALSE,
                                  choices = list()
                      )
               )
            ),
            actionButton("categoryPlotButton", "Opdater Diagram"),
            plotOutput("categoryDiagram")
    ),
    ##########     COMPOSITE      ##########
    tabPanel("Composite", 
              fluidRow(
                column(6, 
                  selectInput(inputId = "compositeRegChoice",
                              label = "Select Region", 
                              choices = list("Alle Regioner", "storkoebenhavn", "nordsjaelland", "region-sjaelland", "fyn", "region-nordjylland", "region-midtjylland", "sydjylland", "bornholm", "skaane", "groenland", "faeroeerne", "udlandet"),
                              multiple = FALSE, 
                              width = "400px"),
                  dateRangeInput('compositeDateRange',
                                 label = 'Dato Interval: Ældste dato er 2018-05-20',
                                 start = "2018-05-20", end = Sys.Date()
                  ),
                  textInput(inputId = "searchField", label = "Search Field"),
                  fluidRow(
                    column(width = 5,
                           selectInput(inputId = "compositeAvailableCategories",
                                       label = "Tilgængelige Kompetencer:",
                                       size = 20,
                                       selectize = FALSE,
                                       choices = list()
                           )
                    ),
                    column(width = 2,
                           actionButton("compositeAddKat", "Tilføj Kategori >>"),
                           actionButton("compositeAdd", "Tilføj >"),
                           actionButton("compositeRemove", "< Fjern"),
                           actionButton("compositeRemoveKat", "<< Fjern Kategori")
                           
                    ),
                    column(width = 5, 
                           selectInput(inputId = "compositeSelectedCategories",
                                       label = "Valgte Kompetencer:",
                                       size = 20,
                                       selectize = FALSE,
                                       choices = list()
                           )
                    )
                  )
                ),
                column(6, 
                  actionButton("compositeCategoryPlotButton", "Opdater Diagram"),
                  plotOutput("compositeDiagram")
                )
              )
    )
  )
)

server <- function(input, output, session){
  ##########     REGION     ##########
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
  ##########     KOMPETENCE KATEGORI      ##########
  con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
  stopifnot(is.object(con))
  availableCategoryData <- dbGetQuery(con, 'select prefferredLabel, _id from kompetence where grp="ict" order by prefferredLabel asc')
  dbDisconnect(con)
  
  availableKompetencer <- as.matrix(availableCategoryData)[,1]
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
  ############### COMPOSITE ###############
  ############### COMPOSITE ###############
  #con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
  #stopifnot(is.object(con))
  #availableCategoryData <- dbGetQuery(con, 'select prefferredLabel, _id from kompetence where grp="ict" order by prefferredLabel asc')
  #dbDisconnect(con)
  
  #availableKompetencer <- as.matrix(availableCategoryData)[,1]
  compositeKompetencer <- reactiveValues(ak = as.matrix(availableCategoryData)[,1], sk = list())
  
  updateSelectInput(session,
                    inputId = "compositeAvailableCategories", 
                    choices = availableKompetencer
  )
  
  
  observeEvent(input$compositeAdd, {
    if (!is.null(input$compositeAvailableCategories)){
      
      compositeKompetencer$sk <- c(compositeKompetencer$sk, input$compositeAvailableCategories)
      compositeKompetencer$ak <- compositeKompetencer$ak[compositeKompetencer$ak != input$compositeAvailableCategories]    
      
      updateSelectInput(session,
                        inputId = "compositeSelectedCategories", 
                        choices = compositeKompetencer$sk
      )
      updateSelectInput(session,
                        inputId = "compositeAvailableCategories", 
                        choices = compositeKompetencer$ak
      )
    }
  })
  observeEvent(input$compositeRemove, {
    if (!is.null(input$compositeSelectedCategories)){
      compositeKompetencer$ak <- c(compositeKompetencer$ak, input$compositeSelectedCategories)
      compositeKompetencer$sk <- compositeKompetencer$sk[compositeKompetencer$sk != input$compositeSelectedCategories]    
      
      updateSelectInput(session,
                        inputId = "compositeSelectedCategories", 
                        choices = compositeKompetencer$sk
      )
      updateSelectInput(session,
                        inputId = "compositeAvailableCategories", 
                        choices = compositeKompetencer$ak
      )
    }
  })
  observeEvent(input$compositeAddKat, {
    if (!is.null(input$compositeAvailableCategories)){
      done <- FALSE
      finalList <- list()
      searchList <- list(input$compositeAvailableCategories)
      
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
        
        if (length(searchList) == 0){
          done <- TRUE
        }
        else
        {
          # Removes all elements from searchList that are present in the finalList, this is to prevent a loop.
          # Example that would result in a loop: 'Digital kommunikation og kollaboration'
          # It still takes several seconds as there's quite a lot of subkompetencer, over half of them belongs under it.
          searchList <- setdiff(searchList, finalList)
        }
        
      }
      dbDisconnect(con)
      
      for (kompetence in finalList){
        if (!any(compositeKompetencer$sk==kompetence)){
          compositeKompetencer$sk <- c(compositeKompetencer$sk, kompetence)
          compositeKompetencer$ak <- compositeKompetencer$ak[compositeKompetencer$ak != kompetence]
        }
      }
      updateSelectInput(session,
                        inputId = "compositeSelectedCategories", 
                        choices = compositeKompetencer$sk
      )
      updateSelectInput(session,
                        inputId = "compositeAvailableCategories", 
                        choices = compositeKompetencer$ak
      )
    }
  })
  observeEvent(input$compositeRemoveKat, {
    if (!is.null(input$compositeSelectedCategories)){
      done <- FALSE
      finalList <- list()
      searchList <- list(input$compositeSelectedCategories)
      
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
        if (!any(compositeKompetencer$ak==kompetence)){
          compositeKompetencer$ak <- c(compositeKompetencer$ak, kompetence)
          compositeKompetencer$sk <- compositeKompetencer$sk[compositeKompetencer$sk != kompetence]
        }
      }
      updateSelectInput(session,
                        inputId = "compositeSelectedCategories", 
                        choices = compositeKompetencer$sk
      )
      updateSelectInput(session,
                        inputId = "compositeAvailableCategories", 
                        choices = compositeKompetencer$ak
      )
    }
  })
  observeEvent(input$searchField, {
    if (input$searchField != ""){
      foundKompetencer <- list()
      for (kompetence in compositeKompetencer$ak){
        if (grepl(input$searchField, kompetence, fixed = TRUE)){
          foundKompetencer <- c(foundKompetencer, kompetence)
        }
      }
      updateSelectInput(session,
                        inputId = "compositeAvailableCategories",
                        choices = foundKompetencer
      )
    }
    else{
      updateSelectInput(session,
                        inputId = "compositeAvailableCategories",
                        choices = compositeKompetencer$ak
      )
    }
  })
  observeEvent(input$compositeCategoryPlotButton, {
    if(length(compositeKompetencer$sk) != 0){
      matchIndexes <- list()
      categoryMatrix <- as.matrix(availableCategoryData)
      for (kompetence in compositeKompetencer$sk){
        matchIndexes <- c(matchIndexes, which(categoryMatrix[,1] == kompetence))
      }
      kompetenceIds <- list()
      for (index in matchIndexes){
        kompetenceIds <- c(kompetenceIds, categoryMatrix[index,2])
      }
      con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
      stopifnot(is.object(con))
      
      kompetenceData <- data.frame()
      
      q1 <- 'select k.prefferredLabel, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where k._id = '
      #q2 is kompetence id, set in loop due to it being the one iterated on.
      ####REGION####
      q3 <- ' and a.region_id = (select r.region_id from region r where r.name = "'
      q4 <- input$compositeRegChoice            #region name
      q5 <- '")'
      if (q4 == "Alle Regioner"){q3=""; q4=""; q5=""} #Skærer region select ud af querien hvis regionen er 'Alle Regioner'
      ##############
      q6 <- ' and a.timeStamp between "'
      q7 <- format(input$compositeDateRange[1]) #Start date
      q8 <- '" and "'
      q9 <- format(input$compositeDateRange[2]) #End date
      q10 <- '" group by k._id'
      
      
      for (id in kompetenceIds){
        q2 <- id
        kompetenceData <- rbind(kompetenceData, dbGetQuery(con, paste0(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10)))
      }
      dbDisconnect(con)
      
      kompetenceData <- kompetenceData[order(kompetenceData$amount, decreasing = TRUE),]
      
      par(mar = c(25,6,4,2) + 0.1)
      output$compositeDiagram <- renderPlot(
        barplot(kompetenceData$amount, 
                main="Antal jobopslag for valgt region, tidsramme & kompetencer ", 
                names.arg = kompetenceData$prefferredLabel,
                las = 2,
        ))
    }
  })  
}

shinyApp(ui = ui, server = server)
