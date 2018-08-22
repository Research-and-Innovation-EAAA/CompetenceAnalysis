library(DBI)
library(RMariaDB)
library(shiny)

source('credentials.R')

ui <- fluidPage(
                fluidRow(
                  column(6,
                         fluidRow(
                           column(6,
                                  selectInput(inputId = "regChoice",
                                              label = "Select Region", 
                                              choices = list("Alle Regioner", "storkoebenhavn", "nordsjaelland", "region-sjaelland", "fyn", "region-nordjylland", "region-midtjylland", "sydjylland", "bornholm", "skaane", "groenland", "faeroeerne", "udlandet"),
                                              multiple = FALSE, 
                                              width = "400px"
                                              ),
                                  dateRangeInput('dateRange',
                                                 label = 'Dato Interval: Ældste dato er 2018-05-20',
                                                 start = "2018-05-20", end = Sys.Date()
                                                 ),
                                  textInput(inputId = "searchField", label = "Search Field")
                                 ),
                           column(6,
                                  selectInput(inputId = "groupChoice",
                                              label = "Kompetence grupper: ", 
                                              choices = list("Alle Grupper", "ict", "ict2", "Core", "language", "multimedie", "transversal", "GartnerForecast", "undefined", "_", "NULL"),
                                              multiple = FALSE, 
                                              width = "400px"
                                              )
                                 )
                         ),
                         fluidRow(
                           column(5,
                                  selectInput(inputId = "availableCategories",
                                              label = "Tilgængelige Kompetencer:",
                                              size = 20,
                                              selectize = FALSE,
                                              choices = list()
                                  )
                           ),
                           column(2,
                                  actionButton("addKat", "Tilføj Kategori >>"),
                                  actionButton("add", "Tilføj >"),
                                  actionButton("remove", "< Fjern"),
                                  actionButton("removeKat", "<< Fjern Kategori")
                                  
                           ),
                           column(5, 
                                  selectInput(inputId = "selectedCategories",
                                              label = "Valgte Kompetencer:",
                                              size = 20,
                                              selectize = FALSE,
                                              choices = list()
                                  )
                           )
                         )
                       ),
                  column(6, 
                    actionButton("categoryPlotButton", "Opdater Diagram"),
                    plotOutput("diagram", height = 700)
                  )
                )
)

server <- function(input, output, session){
  availableKompetencer <- reactiveVal()
  kompetencer <- reactiveValues(ak = NULL, sk = list())
  
  con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
  stopifnot(is.object(con))
  fullCategoryData <- dbGetQuery(con, 'select prefferredLabel, _id from kompetence order by prefferredLabel asc')
  dbDisconnect(con)
  
  observeEvent(input$groupChoice, {
    q1 <- 'select prefferredLabel, _id from kompetence where grp="'
    q2 <- input$groupChoice
    q3 <- '" order by prefferredLabel asc'
    
    if      (q2 == "_"){q2 <- ""} #Can't put empty string in selectInput, so _ is used to represent the empty string group.
    else if (q2 == "NULL"){q1 <- 'select prefferredLabel, _id from kompetence where grp is '; q3 <- ' order by prefferredLabel asc'}
    
    con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
    stopifnot(is.object(con))
    
    if (q2 == "Alle Grupper"){
      availableCategoryData <- dbGetQuery(con, 'select prefferredLabel, _id from kompetence order by prefferredLabel asc')
    }
    else{
      availableCategoryData <- dbGetQuery(con, paste0(q1, q2, q3))
    }
    dbDisconnect(con)
    availableKompetencer <- as.matrix(availableCategoryData)[,1]
    kompetencer$ak <- as.matrix(availableCategoryData)[,1]
    
    updateSelectInput(session,
                      inputId = "availableCategories",
                      choices = kompetencer$ak
    )
  })
  
  #####     ADD/REMOVE OBSERVERS     #####
  ########################################
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
          subData <- dbGetQuery(con, paste0('select k.prefferredLabel from kompetence k, kompetence_kategorisering kk where k.conceptUri = kk.subkompetence and kk.superkompetence = (select distinct k.conceptUri from kompetence k, kompetence_kategorisering kk where k.prefferredLabel = "', superKompetence, '" and k.conceptUri = kk.superkompetence)'))
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
        if (!any(kompetencer$sk == kompetence)){
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
          subData <- dbGetQuery(con, paste0('select k.prefferredLabel from kompetence k, kompetence_kategorisering kk where k.conceptUri = kk.subkompetence and kk.superkompetence = (select distinct k.conceptUri from kompetence k, kompetence_kategorisering kk where k.prefferredLabel = "', superKompetence, '" and k.conceptUri = kk.superkompetence)'))
          subList <- c(subList, as.vector(as.matrix(subData)))
        }
        finalList <- c(finalList, searchList)
        searchList <- subList
        if (length(subList) == 0){
          done <- TRUE
        }
      }
      dbDisconnect(con)
      #print(kompetencer$ak)
      for (kompetence in finalList){
       #print(kompetence)
       #print(kompetencer$ak == kompetence, max = 20000)
       #if (!any(kompetencer$ak == kompetence)){
        kompetencer$ak <- c(kompetencer$ak, kompetence)
        kompetencer$sk <- kompetencer$sk[kompetencer$sk != kompetence]
       #}
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
  #######################################
  observeEvent(input$searchField, {
    if (input$searchField != ""){
      foundKompetencer <- list()
      for (kompetence in kompetencer$ak){
        if (grepl(toupper(input$searchField), toupper(kompetence), fixed = TRUE)){
          foundKompetencer <- c(foundKompetencer, kompetence)
        }
      }
      updateSelectInput(session,
                        inputId = "availableCategories",
                        choices = foundKompetencer
      )
    }
    else{
      updateSelectInput(session,
                        inputId = "availableCategories",
                        choices = kompetencer$ak
      )
    }
  })
  
  observeEvent(input$categoryPlotButton, {
    if(length(kompetencer$sk) != 0){
      matchIndexes <- list()
      categoryMatrix <- as.matrix(fullCategoryData)
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
      
      q1 <- 'select k.prefferredLabel, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where k._id = '
      #q2 is kompetence id, set in loop due to it being the one iterated on.
      ####REGION####
      q3 <- ' and a.region_id = (select r.region_id from region r where r.name = "'
      q4 <- input$regChoice            #region name
      q5 <- '")'
      if (q4 == "Alle Regioner"){q3=""; q4=""; q5=""} #Cuts out region select if the region is 'Alle Regioner'
      ##############
      q6 <- ' and a.timeStamp between "'
      q7 <- format(input$dateRange[1]) #Start date
      q8 <- '" and "'
      q9 <- format(input$dateRange[2]) #End date
      q10 <- '" group by k._id'
      
      
      for (id in kompetenceIds){
        q2 <- id
        kompetenceData <- rbind(kompetenceData, dbGetQuery(con, paste0(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10)))
      }
      
      dbDisconnect(con)
      kompetenceData <- kompetenceData[order(kompetenceData$amount, decreasing = TRUE),]
      #print(kompetenceData) #print to get table of data
      
      op <- par(mar = c(15,5,5,2) + 0.1)
      output$diagram <- renderPlot(
        barplot(kompetenceData$amount, 
                main="Antal jobopslag for valgt region, tidsramme & kompetencer ", 
                names.arg = kompetenceData$prefferredLabel,
                las = 2
        ))
      par(op)
    }
  })  
}

shinyApp(ui = ui, server = server)
