library(DBI)
library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)

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
           actionButton("kompetencePlotButton", "Vis kompetencer i Diagram"),
           actionButton("progressionPlotButton", "Vis progression i Diagram"),
           plotOutput("diagram", height = 700)
    )
  )
                
)

server <- function(input, output, session){
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
      for (kompetence in finalList){
        kompetencer$ak <- c(kompetencer$ak, kompetence)
        kompetencer$sk <- kompetencer$sk[kompetencer$sk != kompetence]
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
  
  #####     PLOT BUTTON OBSERVERS     #####
  #########################################
  observeEvent(input$kompetencePlotButton, {
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
      
      output$diagram <- renderPlot({
        par(mar = c(15,5,5,2) + 0.1)
        barplot(kompetenceData$amount, 
                main="Antal jobopslag for valgt region, tidsramme & kompetencer ", 
                names.arg = kompetenceData$prefferredLabel,
                las = 2
        )
      })
    }
  })
  
  observeEvent(input$progressionPlotButton, {
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
      
      
      q1 <- 'select cast(a.timeStamp as date) as date, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where k._id = '
      #q2 is kompetence id, set in loop due to it being the one iterated on.
      ####REGION####
      q3 <- ' and a.region_id = (select r.region_id from region r where r.name = "'
      q4 <- input$regChoice            #region name
      q5 <- '")'
      if (q4 == "Alle Regioner"){q3=""; q4=""; q5=""} #Cuts out region where-clause if the region is 'Alle Regioner'
      ##############
      q6 <- ' and a.timeStamp between "'
      q7 <- format(input$dateRange[1]) #Start date
      q8 <- '" and "'
      q9 <- format(input$dateRange[2]) #End date
      q10 <- '" group by cast(a.timeStamp as date)'
      
      
      progressionData <- data.frame()
      for (id in kompetenceIds){
        q2 <- id
        progressionData <- rbind(progressionData, dbGetQuery(con, paste0(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10)))
      }
      dbDisconnect(con)
      
      progressionData <- progressionData %>% group_by(date) %>% summarize(amount = sum(amount))
      progressionData <- progressionData[order(progressionData$date, decreasing = FALSE),]
      
      op <- par(mar = c(15,5,5,2) + 0.1)
      
      output$diagram <- renderPlot(
        ggplot(progressionData,
               aes(x=date, y= amount)
        ) + geom_bar(stat="identity") +
          geom_smooth(method = "lm", se = FALSE) +
          labs(x = "Dato Progression", y = "Mængde af annoncer") +
          theme_bw()
      )
      
      # if (FALSE){
      #   output$diagram <- renderPlot({ #Plot code mostly taken from https://github.com/EconometricsBySimulation/OLS-demo-App/blob/master/server.R
      #     
      #     names(progressionData) <- c("x", "y")
      #     #regress <- paste(input$regression, "- 1")
      #     #print(regress)
      #     
      #     altData <- data.frame(sapply(progressionData[,1], as.numeric), progressionData[,2])
      #     print(altData)
      #     
      #     lmResults <- lm(input$regression, data=altData)
      #     #print(lmResults) # Problem here, the results are unchanged regardless of the formula provided.
      #     #print(input$regression)
      #     x <- progressionData$x
      #     y <- progressionData$y
      #     if(FALSE){
      #       xcon <- seq(min(x), max(x), 1)
      #       print(xcon)
      #       x2 <- as.numeric(xcon)^2
      #       x3 <- as.numeric(xcon)^2
      #       x4 <- as.numeric(xcon)^2
      #       x5 <- as.numeric(xcon)^2
      #       
      #       predictor <- data.frame(x=xcon,x2=x2,x3=x3,x4=x4,x5=x5)
      #       #print(predictor)
      #     }
      #     #yhat <- predict(lmResults)
      #     
      #     yline <- predict(lmResults)
      #     #print(xcon)
      #     #print(yline)
      #     
      #     
      #     plot(c(min(x), max(x)),
      #          c(min(y, yline), max(y, yline)),
      #          type = "n",
      #          xlab = "Tids progression",
      #          ylab = "Mængde af ansøgninger",
      #          main = "Progression over tid for mængden af ansøgninger for de givne kompetencer"
      #     )
      #     lines(x, yline, lwd=15, col=grey(.9))
      #     points(x,y)
      #     lines(x, yline, lwd=2, col="blue")
      #   })
      # }
      par(op)
      
    }
  })
  #########################################
  #########################################
}

shinyApp(ui = ui, server = server)
