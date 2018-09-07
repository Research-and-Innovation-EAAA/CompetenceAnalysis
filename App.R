library(DBI)
library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinycssloaders)

source('credentials.R')

ui <- fluidPage(
  fluidRow(style = "margin-top: 5px;",
    column(4, img(src = "EAAA_Logo.jpg")),
    column(8, style = "margin-top: 15px", 
           titlePanel(title = "KOMPETENCEANALYSE", windowTitle = "Annonce Analyse"),
           div(style = "margin-left: 90px; font-size: 20px;", textOutput(outputId = "annonceCountField"))
    )
  ),
  fluidRow(style = "border-bottom: 2px solid black; margin-top: 10px;"),
  fluidRow(style = "margin-top: 15px;",
    column(6,
           wellPanel(
             fluidRow(
               column(6,
                      selectInput(inputId = "regChoice",
                                  label = "Vælg region", 
                                  choices = list("Alle Regioner", "storkoebenhavn", "nordsjaelland", "region-sjaelland", "fyn", "region-nordjylland", "region-midtjylland", "sydjylland", "bornholm", "skaane", "groenland", "faeroeerne", "udlandet"),
                                  multiple = FALSE, 
                                  width = "400px"
                      )
               ),
               column(6,
                      dateRangeInput('dateRange',
                                     label = 'Periode: Ældste dato er 2018-05-20',
                                     start = "2018-05-20", end = Sys.Date()
                      )
               )
             )
           ),
           wellPanel(
             fluidRow(
               column(6,
                      textInput(inputId = "searchField", label = "Søgefelt")
               ),
               column(6,
                      selectInput(inputId = "groupChoice",
                                  label = "Kompetencegrupper: ", 
                                  choices = list("Alle Grupper", "ict", "ict2", "Core", "language", "multimedie", "transversal", "GartnerForecast", "undefined", "_", "NULL"),
                                  multiple = FALSE, 
                                  width = "400px"
                      )
               )
             ),
             wellPanel(
               fluidRow(
                 column(4,
                        selectInput(inputId = "availableCategories",
                                    label = "Tilgængelige kompetencer:",
                                    size = 20,
                                    selectize = FALSE,
                                    multiple = TRUE,
                                    choices = list(),
                                    width = "100%"
                        )
                 ),
                 column(4, align = "center", style = "margin-top: 75px;",
                        actionButton("addAll", "Tilføj alle >>>", width = 150),
                        actionButton("addKat", "Tilføj kategori >>", width = 150),
                        actionButton("add", "Tilføj >", width = 150),
                        actionButton("remove", "< Fjern", width = 150),
                        actionButton("removeKat", "<< Fjern kategori", width = 150),
                        actionButton("removeAll", "<<< Fjern alle", width = 150)
                        
                 ),
                 column(4, 
                        selectInput(inputId = "selectedCategories",
                                    label = "Valgte kompetencer:",
                                    size = 20,
                                    selectize = FALSE,
                                    multiple = TRUE,
                                    choices = list(),
                                    width = "100%"
                        )
                 )
               )
             )
           )
    ),
    column(6, 
           tabsetPanel(id = "outputPanel",
             tabPanel("Kompetencesammenligning",
                      wellPanel(
                        textOutput(outputId = "kompetenceErrorField"),
                        plotOutput("kompetenceDiagram", height = 620) %>% withSpinner(color="#0dc5c1")
                      )
                    
             ),
             tabPanel("Progression", 
                      wellPanel(
                        selectInput(inputId = "progressionDateFormat", 
                                    label = "Periodeopdeling", 
                                    choices = list("Uge", "Måned", "År")
                        ),
                        textOutput(outputId = "progressionErrorField"),
                        plotOutput("progressionDiagram", height = 540) %>% withSpinner(color="#0dc5c1")
                      )
             ),
             tabPanel("Annonceliste",
                      wellPanel(
                        fluidRow(
                          textOutput(outputId = "annonceErrorField"),
                          selectInput(inputId = "annonceList",
                                      label = "",
                                      selectize = FALSE,
                                      size = 20,
                                      choices = list(),
                                      width = "100%"
                          ),
                          textOutput(outputId = "annonceText")
                        )
                      )
             )
           )
    )
  )
)

server <- function(input, output, session){
  kompetencer <- reactiveValues(ak = NULL, sk = list())
  current <- reactiveValues(tab = 1)
  
  con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
  stopifnot(is.object(con))
  fullCategoryData <- dbGetQuery(con, 'select prefferredLabel, _id from kompetence order by prefferredLabel asc')
  annonceCount <- dbGetQuery(con, 'select count(*) from annonce')[1,1]
  output$annonceCountField <- renderText(paste0("(", annonceCount, " Annoncer)"))
  
  dbDisconnect(con)
  
  observeEvent(input$groupChoice, {
    groupUpdateEffects()
    searchFieldEffects()
  })
  
  observeEvent(input$dateRange, {
    updateCurrentTab()
  })
  
  observeEvent(input$regChoice, {
    updateCurrentTab()
  })
  
  observeEvent(input$outputPanel, {
    if (input$outputPanel == "Kompetencesammenligning"){
      current$tab <- 1
    }
    else if (input$outputPanel == "Progression"){
      current$tab <- 2
    }
    else if (input$outputPanel == "Annonceliste"){
      current$tab <- 3
    }
    updateCurrentTab()
  })
  
  ########################################
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
      searchFieldEffects()
      updateCurrentTab()
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
      searchFieldEffects()
      updateCurrentTab()
    }
  })
  observeEvent(input$addKat, {
    if (!is.null(input$availableCategories)){
      done <- FALSE
      finalList <- list()
      searchList <- c(input$availableCategories)
      
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
          # Over half the items under group ict belongs under that example.
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
      searchFieldEffects()
      updateCurrentTab()
    }
  })
  
  observeEvent(input$removeKat, {
    if (!is.null(input$selectedCategories)){
      done <- FALSE
      finalList <- list()
      searchList <- c(input$selectedCategories)
      
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
      searchFieldEffects()
      updateCurrentTab()
    }
  })
  
  observeEvent(input$addAll, {
    if (input$searchField == ""){
      kompetencer$sk <- c(kompetencer$sk, kompetencer$ak)
      
    }
    else{
      kompetencer$sk <- c(kompetencer$sk, kompetencer$ak)
    }
    
    kompetencer$ak <- setdiff(kompetencer$ak, kompetencer$sk)
    
    updateSelectInput(session,
                      inputId = "availableCategories", 
                      choices = kompetencer$ak
    )
    updateSelectInput(session,
                      inputId = "selectedCategories", 
                      choices = kompetencer$sk
    )
    updateCurrentTab()
  })
  
  observeEvent(input$removeAll, {
    kompetencer$ak <- c(kompetencer$ak, kompetencer$sk)
    kompetencer$sk <- list()
    
    updateSelectInput(session,
                      inputId = "selectedCategories", 
                      choices = kompetencer$sk
    )
    updateSelectInput(session,
                      inputId = "availableCategories", 
                      choices = kompetencer$ak
    )
    searchFieldEffects()
    updateCurrentTab()
  })
  
  #######################################
  #######################################
  observeEvent(input$searchField, {
    searchFieldEffects()
  })
  
  observeEvent(input$progressionDateFormat,{
    updateProgressionDiagram()
  })
  
  observeEvent(input$annonceList, {
    con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
    stopifnot(is.object(con))
    
    id <- unlist(strsplit(input$annonceList, " "))[1]
    
    annonceText <- dbGetQuery(con, paste0('select convert(searchable_body using utf8) as searchable_body from annonce where _id = ', id))
    
    dbDisconnect(con)
    
    output$annonceText <- renderText({annonceText[1,1]})
  })
  
  ############################################################     FUNCTIONS     ############################################################
  
  updateCurrentTab <- function(){
    if (current$tab == 1){
      updateKompetenceDiagram()
    }
    else if (current$tab == 2){
      updateProgressionDiagram()
    }
    else if (current$tab == 3){
      updateAnnonceList()
    }
  }
  
  searchFieldEffects <- function(){
    if (input$searchField != ""){
      foundKompetencer <- list()
      for (kompetence in kompetencer$ak){
        if (grepl(toupper(input$searchField), toupper(kompetence), fixed = TRUE)){
          foundKompetencer <- c(foundKompetencer, kompetence)
        }
      }
      foundKompetencer <- setdiff(foundKompetencer, kompetencer$sk)
      
      updateSelectInput(session,
                        inputId = "availableCategories",
                        choices = foundKompetencer
      )
      kompetencer$ak <- foundKompetencer
    }
    else{
      groupUpdateEffects()
    }
  }
  
  groupUpdateEffects <- function(){
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
  }
  
  updateKompetenceDiagram <- function(){
    if(length(kompetencer$sk) != 0){
      output$kompetenceErrorField <- renderText("Arbejder, Vent Venligst.")
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
      
      q1 <- 'select k.prefferredLabel, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where ('
      q2 <- '' #Kompetence id, set in loop due to it being the one iterated on.
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
      q10 <- '" group by k._id order by amount desc limit 30'
      
      for (i in 1:length(kompetenceIds)){
        if (i < length(kompetenceIds)){
          q2 <- paste0(q2, (paste0('k._id = ', kompetenceIds[i], ' or ')))
        }
        else{
          q2 <- paste0(q2, (paste0('k._id = ', kompetenceIds[i], ')')))
        }
      }
      
      kompetenceData <- dbGetQuery(con, paste0(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10))
      
      dbDisconnect(con)
      
      if (dim(kompetenceData)[1] != 0) {
        # If i order the table in the correct order in the SQL query the 'Limit 30' option cuts off the biggest instead of the smallest.
        # Which is why the table must be ordered after the query.
        kompetenceData <- kompetenceData[order(kompetenceData$amount, decreasing = FALSE),]
        
        output$kompetenceDiagram <- renderPlot({
          par(mar = c(5,15,4,2) + 0.1)
          barplot(kompetenceData$amount, 
                  main="Antal jobopslag for valgt region, tidsramme & kompetencer ", 
                  names.arg = kompetenceData$prefferredLabel,
                  las = 2,
                  horiz = TRUE
          )
        })
        output$kompetenceErrorField <- renderText("")
      }
      else{
        output$kompetenceErrorField <- renderText("Ingen annoncer fundet.")
        output$kompetenceDiagram <- NULL
      }
    }
    else{
      output$kompetenceErrorField <- renderText("")
      output$kompetenceDiagram <- NULL
    }
  }
  
  updateProgressionDiagram <- function(){
    if(length(kompetencer$sk) != 0){
      output$progressionErrorField <- renderText("Arbejder, vent venligst.")
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
      xInc <- 1
      format <- ""
      if (input$progressionDateFormat == "Uge"){
        format <- "%Y-%W"
        xInc <- 7
      }
      else if (input$progressionDateFormat == "Måned"){
        format <- "%Y-%m"
        xInc <- 30
      }
      else if (input$progressionDateFormat == "År"){
        format <- "%Y"
        xInc <- 365
      }
      
      formattedData <- progressionData %>% group_by(format(date, format)) %>% summarize(amount = sum(amount))
      colnames(formattedData)[1] <- "date"
      
      lastDate <- ""
      n <- nrow(formattedData)
      if (n > 0){ ##### Check added to prevent crash when there's no job advertisements to be found
        for (i in 1:n){
          if (lastDate != ""){
            
            year <- as.numeric(substr(formattedData$date[i], 1, 4))
            prevYear <- as.numeric(substr(lastDate, 1, 4))
            yearDif <- year - prevYear
            
            if (format != "%Y"){ #Diagram split into weeks or months
              if (yearDif == 0){
                lastPeriod <- as.numeric(strsplit(lastDate, "-")[[1]][2])
                currentPeriod <- as.numeric(strsplit(formattedData$date[i], "-")[[1]][2])
                if (lastPeriod + 1 != currentPeriod){
                  missingPeriods <- currentPeriod - lastPeriod - 1
                  
                  for (j in (lastPeriod+1):(lastPeriod+missingPeriods)){
                    if (j < 10){
                      df <- data.frame(paste0(year, "-0", j), 0)
                      names(df) <- c("date", "amount")
                      formattedData <- rbind(formattedData, df)
                    }
                    else{
                      df <- data.frame(paste0(year, "-", j), 0)
                      names(df) <- c("date", "amount")
                      formattedData <- rbind(formattedData, df)
                    }
                  }
                }
              }
              else{
                for (j in prevYear:year){
                  lastPeriod <- 1;
                  if (j == prevYear){
                    lastPeriod <- as.numeric(substr(x = lastDate, length(lastDate)-1, length(lastDate)+1))
                  }
                  if (j < year){
                    yearLength <- 52
                    if (format == "%Y-%m"){
                      yearLength <- 12
                    }
                    for (u in lastPeriod:yearLength){
                      if (u < 10){
                        df <- data.frame(paste0(j, "-0", u), 0)
                        names(df) <- c("date", "amount")
                        formattedData <- rbind(formattedData, df)
                      }
                      else{
                        df <- data.frame(paste0(j, "-", u), 0)
                        names(df) <- c("date", "amount")
                        formattedData <- rbind(formattedData, df)
                      }
                    }
                  }
                  else{
                    currentPeriod <- as.numeric(substr(x = formattedData$date[i], length(formattedData$date[i])-1, length(formattedData$date[i])+1))
                    for (u in lastPeriod:currentPeriod-1){
                      if (u < 10){
                        df <- data.frame(paste0(j, "-0", u), 0)
                        names(df) <- c("date", "amount")
                        formattedData <- rbind(formattedData, df)
                      }
                      else{
                        df <- data.frame(paste0(j, "-", u), 0)
                        names(df) <- c("date", "amount")
                        formattedData <- rbind(formattedData, df)
                      }
                    }
                  }
                }
              }
            }
            else{ # Diagram split into years
              for (j in (prevYear+1):(year-1)){
                df <- data.frame(toString(j), 0)
                names(df) <- c("date", "amount")
                formattedData <- rbind(formattedData, df)
              }
            }
          }
          lastDate <- formattedData$date[i]
        }
        
        formattedData <- formattedData[order(formattedData$date, decreasing = FALSE),]
        
        x <- 0
        y <- 0
        xy <- 0
        x2 <- 0
        y2 <- 0
        n <- nrow(formattedData)
        for (i in 1:n){
          x <- x + i * xInc
          y <- y + formattedData$amount[i]
          xy <- xy + ((i * xInc) * formattedData$amount[i])
          x2 <- x2 + (i * xInc)^2
          y2 <- y2 + formattedData$amount[i]^2
          
          lastDate <- formattedData$date[i]
        }
        
        #print(paste0("x: ", x, ", y: ", y, ", xy: ", xy, ", x2: ", x2, ", y2: ", y2, ", n: ", n ))
        
        a <- (y * x2 - x * xy) / (n * x2 - x ^ 2)
        b <- (n * xy - x * y) / (n * x2 - x ^ 2)
        
        #print (paste0("a: ", a, ", b: ", b))
        
        output$progressionDiagram <- renderPlot({
          barplot(formattedData$amount, names.arg = formattedData$date)
          if (n > 1) 
          {
            #As of writing this code there is only data from about 4 months, so less than a year.
            #If n is 1 it will result in an error when doing regression, and it is currently 1 when choosing year.
            abline(a=a, b=b, col = "red", lwd = 3)
          }
        })
        
        output$progressionErrorField <- renderText("")
      }
      else{
        output$progressionDiagram <- NULL
        output$progressionErrorField <- renderText("Ingen annoncer fundet.")
      }
    }
    else{
      output$progressionErrorField <- renderText("")
      output$progressionDiagram <- NULL
    }
  }
  
  updateAnnonceList <- function(){
    if(length(kompetencer$sk) != 0){
      output$annonceErrorField <- renderText("Arbejder, vent venligst.")
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
      
      
      q1 <- 'select a._id, a.title from annonce a join annonce_kompetence ak on a._id = ak.annonce_id join kompetence k on ak.kompetence_id = k._id where ('
      q2 <- '' #Kompetence id, set in loop due to it being the one iterated on.
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
      q10 <- '" group by a._id'
      
      for (i in 1:length(kompetenceIds)){
        if (i < length(kompetenceIds)){
          q2 <- paste0(q2, (paste0('k._id = ', kompetenceIds[i], ' or ')))
        }
        else{
          q2 <- paste0(q2, (paste0('k._id = ', kompetenceIds[i], ')')))
        }
      }
      
      annonceData <- dbGetQuery(con, paste0(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10))
      dbDisconnect(con)
      
      annonceListContents <- list()
      if (dim(annonceData)[1] != 0){
        for (i in 1:nrow(annonceData)){
          annonceListContents <- c(annonceListContents, paste0(annonceData[i,1], " || ", annonceData[i, 2]))
        }
        
        updateSelectInput(session,
                          inputId = "annonceList", 
                          choices = annonceListContents
        )
        output$annonceErrorField <- renderText("")
      }
      else{
        output$annonceErrorField <- renderText("Ingen annoncer fundet.")
        updateSelectInput(session,
                          inputId = "annonceList", 
                          choices = list()
        )
      }
    }
    else{
      output$annonceErrorField <- renderText("")
      updateSelectInput(session,
                        inputId = "annonceList", 
                        choices = list()
      )
    }
  }
  
  ############################################################################################################################################
  
}

shinyApp(ui = ui, server = server)
