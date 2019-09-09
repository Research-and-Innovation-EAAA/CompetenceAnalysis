library(DBI)
library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyTree)

source('credentials.R')

ui <- fluidPage(
  fluidRow(style = "margin-top: 5px;",
    column(6, style = "margin-top: 15px", 
           titlePanel(title = "KOMPETENCEANALYSE", windowTitle = "Annonce Analyse"),
           div(style = "font-size: 20px;", textOutput(outputId = "annonceCountField")),
           tags$span("Kilde: "),
           tags$a(href="https://www.jobindex.dk","JobIndex"),
           tags$span(" og "),
           tags$a(href="https://www.careerjet.dk","CareerJet")
    ),
    column(6, style = "margin-top: 15px; text-align: right", img(src = "EAAA_Logo.jpg"))
  ),
  fluidRow(style = "border-bottom: 2px solid black; margin-top: 10px;"),
  
  fluidRow(style = "margin-top: 15px;",
    
    column(6,
           tags$h3("Søgekriterier"),
           tabsetPanel(
             tabPanel(title = "Kompetencer",
           wellPanel(
             shinyTree("kompetenceTree", checkbox = TRUE),
             textInput(inputId = "searchField", label = "Søgefelt"),
             fluidRow(
               column(4,
                      selectInput(inputId = "availableCategories",
                                  label = "Tilgængelige:",
                                  size = 20,
                                  selectize = FALSE,
                                  multiple = TRUE,
                                  choices = list(),
                                  width = "100%"
                      )
               ),
               column(4, align = "center", style = "margin-top: 75px;",
                      actionButton("addAll", "Tilføj alle >>", width = 150),
                      actionButton("add", "Tilføj >", width = 150),
                      actionButton("remove", "< Fjern", width = 150),
                      actionButton("removeAll", "<< Fjern alle", width = 150)    
               ),
               column(4, 
                      selectInput(inputId = "selectedCategories",
                                  label = "Valgte:",
                                  size = 20,
                                  selectize = FALSE,
                                  multiple = TRUE,
                                  choices = list(),
                                  width = "100%"
                      )
               )
             ))
           ),
           tabPanel(title="Annoncer",
           wellPanel(
             fluidRow(
               column(6,
                      selectInput(inputId = "regChoice",
                                  label = "Region", 
                                  choices = list("Alle regioner", "storkoebenhavn", "nordsjaelland", "region-sjaelland", "fyn", "region-nordjylland", "region-midtjylland", "sydjylland", "bornholm", "skaane", "groenland", "faeroeerne", "udlandet"),
                                  multiple = FALSE, 
                                  width = "400px"
                      )
               ),
               column(6,
                      dateRangeInput('dateRange',
                                     label = 'Periode:',
                                     start = Sys.Date()-30, end = Sys.Date()
                      )
               ),
               column(12, textInput(inputId = "titleSearchField", label = "Søg efter jobtitel")
               ),
               column(3,align = "center", style = "margin-top: 125px;",
                      actionButton(inputId = "addTitle", label = "Tilføj", width = 150, style = "margin-top: 10px"),
                      actionButton("removeTitle", "Fjern", width = 150, style = "margin-top: 10px"),
                      actionButton("removeAllTitles", "Fjern alle", width = 150, style = "margin-top: 10px")
               ),
               column(9, 
                       selectInput(inputId = "selectedTitleSearchTerms",
                                   label = "Valgte jobtitler:",
                                   size = 20,
                                   selectize = FALSE,
                                   multiple = TRUE,
                                   choices = list(),
                                   width = "100%"
                       )
             ))
           )),
           tabPanel(title="Datafields",
                    wellPanel(
                      fluidRow(
                        column(12,
                                      selectInput(inputId = "dataFieldChoice",
                                                  label = "Vælg datafield at søge efter", 
                                                  choices = {
                                                  con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, port = credentials.port, db = credentials.db, bigint = c("numeric"))
                                                  stopifnot(is.object(con))
                                                  dataFieldNames <- dbGetQuery(con,'select name from dataField')
                                                  dbDisconnect(con) 
                                                  choices = dataFieldNames},
                                                  multiple = FALSE, 
                                                  width = "500px"
                                      ),
                               column(4,
                                      selectInput(inputId = "availableDataFields",
                                                  label = "Tilgængelige:",
                                                  size = 20,
                                                  selectize = FALSE,
                                                  multiple = TRUE,
                                                  choices = list(),
                                                  width = "100%"
                                      )
                               ),
                               column(4, align = "center", style = "margin-top: 75px;",
                                      actionButton("addDataField", "Tilføj >", width = 150),
                                      actionButton("removeDataField", "< Fjern", width = 150),
                                      actionButton("removeAllDataFields", "<< Fjern alle", width = 150)    
                               ),
                               column(4, 
                                      selectInput(inputId = "selectedDataFields",
                                                  label = "Valgte:",
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
            )
           )
    ),
    column(6,
           fluidRow(
             column(4,
                    tags$h3("Søgeresultater")
                    )
             #column(8, 
             #       selectInput(inputId = "matchChoice",
             #                   label = "Vælg matchsystem",
             #                   choices = list("Ord Match", "Machine-Learned"),
             #                   multiple = FALSE,
             #                   width = "200px"
             #                   )
             #       )
           ),
           tabsetPanel(id = "outputPanel",
             tabPanel("Kompetencesammenligning",
                      wellPanel(
                        textOutput(outputId = "kompetenceErrorField"),
                        plotOutput("kompetenceDiagram", height = 620)
                      )
             ),
             tabPanel("Progression", 
                      wellPanel(
                        selectInput(inputId = "progressionDateFormat", 
                                    label = "Periodeopdeling", 
                                    choices = list("Uge", "Måned", "År")
                        ),
                        textOutput(outputId = "progressionErrorField"),
                        plotOutput("progressionDiagram", height = 540)
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
                          tags$h5(style="font-weight:bold", "CVR-Oplysninger:"),
                          tableOutput(outputId = "annonceDataFields"),
                          tags$h5(style="font-weight:bold", "Annoncetekst:"),
                          textOutput(outputId = "annonceText"),
                          tags$style(type="text/css", "#annonceDataFields td:first-child {font-weight:bold;}") #make row names of table bold.
                        )
                      )
             )
           )
    )
  )
)

server <- function(input, output, session){
  kompetencer <- reactiveValues(ak = NULL, sk = list(), fk = list())
  datafields <- reactiveValues(titles = list(),selectedDataFields = list(), availableDataFields = list())
  current <- reactiveValues(tab = 1)
  tabUpdates <- reactiveValues(kompetence = FALSE, progression = FALSE, annonce = FALSE)
  lastShinyTree <- reactiveValues(tree = list())
  
  con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, port = credentials.port , bigint = c("numeric"))
  stopifnot(is.object(con))
  withProgress(message = "Forbereder data", expr = {
    setProgress(0)
    fullCategoryData <- dbGetQuery(con, 'select prefferredLabel, _id, grp from kompetence order by prefferredLabel asc')
    setProgress(1/3)
    annonceCount <- dbGetQuery(con, 'select count(*) from annonce')[1,1]
    output$annonceCountField <- renderText(paste0("(", annonceCount, " Annoncer)"))
    setProgress(2/3)
    treeString <- dbGetQuery(con, 'select shinyTreeJSON from global where _id = 1')[1,1]
    output$kompetenceTree <- renderEmptyTree()
    updateTree(session,"kompetenceTree", treeString)
    setProgress(1)
  })
  
  dbDisconnect(con)
  
  observeEvent(input$dateRange, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$regChoice, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$dataFieldChoice,{
    updateDataFields()
  }) 
  
  observeEvent(input$outputPanel, ignoreInit = TRUE, {
    if (input$outputPanel == "Kompetencesammenligning"){
      current$tab <- 1
      if (tabUpdates$kompetence){
        tabUpdates$kompetence <- FALSE
        updateKompetenceDiagram()
      }
    }
    else if (input$outputPanel == "Progression"){
      current$tab <- 2
      if (tabUpdates$progression){
        tabUpdates$progression <- FALSE
        updateProgressionDiagram()
      }
    }
    else if (input$outputPanel == "Annonceliste"){
      current$tab <- 3
      if (tabUpdates$annonce){
        tabUpdates$annonce <- FALSE
        updateAnnonceList()
      }
    }
  })
  
  observeEvent(input$kompetenceTree, ignoreInit = TRUE, {
    selectedList <- get_selected(input$kompetenceTree)
    if (!identical(lastShinyTree$tree, selectedList)){
      treeUpdateEffects()
      searchFieldEffects()
    }
  })
  
  
  
  ########################################
  #####     ADD/REMOVE OBSERVERS     #####
  ########################################
  observeEvent(input$add, {
    if (!is.null(input$availableCategories)){
      withProgress(message = "Tilføjer kompetence(r)", expr = {
        setProgress(0)
        kompetencer$sk <- c(kompetencer$sk, input$availableCategories)
        setProgress(1/4)
        kompetencer$ak <- setdiff(kompetencer$ak, kompetencer$sk)
        setProgress(2/4)
        updateSelectInput(session,
                          inputId = "selectedCategories", 
                          choices = kompetencer$sk
        )
        updateCurrentTab()
        updateSelectizeInput(session,
                          inputId = "availableCategories",
                          choices = kompetencer$ak
        )
        setProgress(3/4)
        searchFieldEffects()
        setProgress(1)
      })
    }
  })
  observeEvent(input$remove, {
    if (!is.null(input$selectedCategories)){
      withProgress(message = "Fjerner kompetence(r)", expr = {
        setProgress(0)
        kompetencer$ak <- c(kompetencer$ak, input$selectedCategories)
        setProgress(1/5)
        kompetencer$sk <- setdiff(kompetencer$sk, input$selectedCategories) 
        setProgress(2/5)
        updateSelectInput(session,
                          inputId = "selectedCategories", 
                          choices = kompetencer$sk
        )
        updateSelectInput(session,
                          inputId = "availableCategories", 
                          choices = kompetencer$ak
        )
        setProgress(3/5)
        searchFieldEffects()
        setProgress(4/5)
        updateCurrentTab()
        setProgress(1)
      })
    }
  })
  
  observeEvent(input$addAll, {
    withProgress(message = "Tilføjer alle på nuværende liste", expr = {
      if (input$searchField != ""){
        setProgress(0)
        kompetencer$sk <- c(kompetencer$sk, kompetencer$fk)
        setProgress(1/3)
        kompetencer$ak <- setdiff(kompetencer$ak, kompetencer$fk) 
        kompetencer$fk <- list()
        
        updateSelectInput(session,
                          inputId = "availableCategories", 
                          choices = kompetencer$ak
        )
      }
      else{
        setProgress(0)
        kompetencer$sk <- c(kompetencer$sk, kompetencer$ak)
        setProgress(1/3)
        kompetencer$ak <- list()
        
        updateSelectInput(session,
                          inputId = "availableCategories", 
                          choices = kompetencer$ak
        )
      }
      updateSelectInput(session,
                        inputId = "selectedCategories", 
                        choices = kompetencer$sk
      )
      setProgress(2/3)
      searchFieldEffects()
      updateCurrentTab()
      setProgress(1)
    })
  })
  
  observeEvent(input$removeAll, {
    withProgress(message = "Fjerner alle kompetencer", expr = {
      setProgress(0)
      kompetencer$ak <- c(kompetencer$ak, kompetencer$sk)
      setProgress(1/4)
      kompetencer$sk <- list()
      
      updateSelectInput(session,
                        inputId = "availableCategories", 
                        choices = kompetencer$ak
      )
      updateSelectInput(session,
                        inputId = "selectedCategories", 
                        choices = kompetencer$sk
      )
      setProgress(2/4)
      searchFieldEffects()
      setProgress(3/4)
      updateCurrentTab()
      setProgress(1)
    })
  })
  observeEvent(input$addDataField, {
    if (!is.null(input$availableDataFields)){
        datafields$selectedDataFields <- c(datafields$selectedDataFields, input$availableDataFields)
        updateSelectInput(session,
                          inputId = "selectedDataFields", 
                          choices = datafields$selectedDataFields
        )
        updateCurrentTab()
      
    }
  })
  
  observeEvent(input$removeDataField, {
    if (!is.null(input$selectedDataFields)){
      datafields$selectedDataFields <- datafields$selectedDataFields[!datafields$selectedDataFields %in% input$selectedDataFields]
      updateSelectInput(session,inputId = "selectedDataFields", choices = datafields$selectedDataFields)
      updateCurrentTab()
    }
  })
  
  observeEvent(input$removeAllDataFields, {
    datafields$selectedDataFields <- datafields$selectedDataFields[!datafields$selectedDataFields %in% datafields$selectedDataFields]
    updateSelectInput(session,inputId = "selectedDataFields", choices = datafields$selectedDataFields)
    updateCurrentTab()
  })
  observeEvent(input$addTitle,{
    if(input$titleSearchField != ""){
      datafields$titles <- c(datafields$titles,input$titleSearchField)
    
    
      updateSelectInput(session,inputId = "selectedTitleSearchTerms",choices = datafields$titles)
    
      updateCurrentTab()
    }
    
  })
  
  observeEvent(input$removeTitle,{
    if (!is.null(input$selectedTitleSearchTerms)){
      datafields$titles <- datafields$titles[!datafields$titles %in% input$selectedTitleSearchTerms]
      updateSelectInput(session,inputId = "selectedTitleSearchTerms", choices = datafields$titles)
      updateCurrentTab()
    }
  })
  observeEvent(input$removeAllTitles,{
    
      datafields$titles <- datafields$titles[!datafields$titles %in% datafields$titles]
      updateSelectInput(session,inputId = "selectedTitleSearchTerms", choices = datafields$titles)
      updateCurrentTab()
    
  })
  
  #######################################
  #######################################
  observeEvent(input$searchField, ignoreInit = TRUE, {
    searchFieldEffects()
  })
  
  observeEvent(input$progressionDateFormat, ignoreInit = TRUE,{
    updateProgressionDiagram()
  })
  
  observeEvent(input$annonceList, {
    withProgress(message = "Finder annoncetekst", expr = {
      setProgress(0)
      con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, port = credentials.port, db = credentials.db, bigint = c("numeric"))
      stopifnot(is.object(con))
      
      id <- unlist(strsplit(input$annonceList, " "))[1]
      
      
      annonceDataFields <- dbGetQuery(con,paste0('select dataValue, name from annonce_dataField join dataField where annonce_id = ',id,' and dataField._id = dataField_id'))
      newdf <- data.frame()
      
      
      
      
      annonceText <- dbGetQuery(con, paste0('select convert(searchable_body using utf8) as searchable_body from annonce where _id = ', id))
      
      
      if(nrow(annonceDataFields)== 0){
        annonceDataFields <- data.frame("Ingen tilgængelige cvr oplysninger")
        names(annonceDataFields) <- c("") #remove column headers
        output$annonceDataFields <- renderTable(annonceDataFields)
      } else {
      rownames(annonceDataFields) <- annonceDataFields[,2]
      annonceDataFields[2] <- NULL
      names(annonceDataFields) <- c("") #remove column headers
      output$annonceDataFields <- renderTable(annonceDataFields,include.rownames=TRUE)
      }
      
      
      setProgress(1/2)
      dbDisconnect(con)
      
      output$annonceText <- renderText({annonceText[1,1]})
      
      setProgress(1)
    })
  })
  
  ############################################################     FUNCTIONS     ############################################################
  
    updateDataFields <- function(){
    con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, port = credentials.port, db = credentials.db, bigint = c("numeric"))
    stopifnot(is.object(con))
    
    datafields <- dbGetQuery(con, paste0("select distinct dataValue from annonce_dataField where (select _id from dataField where name = '", input$dataFieldChoice, "') = dataField_id"))
    
    
    updateSelectInput(session,
                      inputId = "availableDataFields",
                      choices = datafields
    )
    
    datafields$availableDataFields <- datafields
    
    dbDisconnect(con)
    
    }
  
  updateCurrentTab <- function(){
    if (current$tab == 1){
      updateKompetenceDiagram()
      tabUpdates$progression <- TRUE
      tabUpdates$annonce <- TRUE
    }
    else if (current$tab == 2){
      updateProgressionDiagram()
      tabUpdates$kompetence <- TRUE
      tabUpdates$annonce <- TRUE
    }
    else if (current$tab == 3){
      updateAnnonceList()
      tabUpdates$kompetence <- TRUE
      tabUpdates$progression <- TRUE
    }
  }
  
  searchFieldEffects <- function(){
    if (input$searchField != ""){
      withProgress(message = "Søger efter kompetencer", expr = {
        setProgress(0)
        foundKompetencer <- list()
        for (kompetence in kompetencer$ak){
          if (grepl(toupper(input$searchField), toupper(kompetence), fixed = TRUE)){
            foundKompetencer <- c(foundKompetencer, kompetence)
          }
        }
        setProgress(1/2)
        foundKompetencer <- setdiff(foundKompetencer, kompetencer$sk)
        
        updateSelectInput(session,
                          inputId = "availableCategories",
                          choices = foundKompetencer
        )
        kompetencer$fk = foundKompetencer
        setProgress(1)
      })
    }
    else{
      updateSelectInput(session,
                        inputId = "availableCategories",
                        choices = kompetencer$ak
      )
    }
  }
  
  treeUpdateEffects <- function(){
    selectedList <- get_selected(input$kompetenceTree)
    if (length(selectedList) > 0){
      withProgress(message = "Opdaterer tilgængelig list", expr = {
        setProgress(0)
        
        q1 <- 'select prefferredLabel from kompetence where grp = '
        q2 <- ''
        for (i in 1:length(selectedList)){
          q2 <- paste0(q2, '"', selectedList[i], '"')
          if (i != length(selectedList)){
            q2 <- paste0(q2, ' or grp = ')
          }
        }
        
        searchList <- list()
        
        for (i in 1:length(selectedList)){
          searchList <- c(searchList, selectedList[[i]][1])
        }
        setProgress(1/6)
        
        i <- length(searchList)
        while(i > 0){
          if (!any(as.vector(fullCategoryData[,1]) == searchList[[i]])){
            searchList[[i]] <- NULL
          }
          i <- i - 1
        }
        
        con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, port = credentials.port, db = credentials.db, bigint = c("numeric"))
        stopifnot(is.object(con))
        
        setProgress(2/6)
        finalList <- as.vector(as.matrix(dbGetQuery(con, paste0(q1, q2))))
        
        setProgress(3/6)
        done <- FALSE
        while (!done){
          subList = c()
          for (superKompetence in searchList){
            subData <- dbGetQuery(con, paste0('select k.prefferredLabel from kompetence k, kompetence_kategorisering kk where k.conceptUri = kk.subkompetence and kk.superkompetence = (select distinct k.conceptUri from kompetence k, kompetence_kategorisering kk where k.prefferredLabel = "', superKompetence, '" and k.conceptUri = kk.superkompetence)'))
            subList <- c(subList,subData[,1])
          }
          finalList <- c(finalList, searchList)
          searchList <- subList
          if (length(searchList) == 0){
            done <- TRUE
          }
          else
          {
            searchList <- setdiff(searchList, finalList)
          }
          
        }
        dbDisconnect(con)
        
        kompetencer$ak <- c()
        
        setProgress(4/6)
        for (kompetence in finalList){
          if (!any(kompetencer$sk == kompetence)){
            kompetencer$ak <- c(kompetencer$ak, kompetence)
          }
        }
        #kompetencer$ak <- sort.list(kompetencer$ak, decreasing = FALSE)
        setProgress(5/6)
        updateSelectInput(session,
                          inputId = "availableCategories", 
                          choices = kompetencer$ak
        )
        setProgress(1)
      })
    }
    else{
      updateSelectInput(session,
                        inputId = "availableCategories", 
                        choices = list()
      )
    }
    lastShinyTree$tree <- selectedList
  }
  
  updateKompetenceDiagram <- function(){
    if(length(kompetencer$sk) != 0){
      withProgress(message = "Opdaterer Diagram", expr = {
        setProgress(0)
        matchIndexes <- list()
        categoryMatrix <- as.matrix(fullCategoryData)
        for (kompetence in kompetencer$sk){
          matchIndexes <- c(matchIndexes, which(categoryMatrix[,1] == kompetence))
        }
        kompetenceIds <- list()
        for (index in matchIndexes){
          kompetenceIds <- c(kompetenceIds, categoryMatrix[index,2])
        }
        con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, port = credentials.port, db = credentials.db, bigint = c("numeric"))
        stopifnot(is.object(con))
        
        setProgress(1/5)
        q1 <- 'select distinct k.prefferredLabel, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where '
        #if (input$matchChoice == "Machine-Learned"){
        #  q1 <- 'select distinct k.prefferredLabel, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence_machine ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where '
        #}
        q2 <- '' #Kompetence id, set in loop due to it being the one iterated on.
        ####REGION####
        q3 <- ' and a.region_id = (select r.region_id from region r where r.name = "'
        q4 <- input$regChoice            #region name
        q5 <- '")'
        if (q4 == "Alle regioner"){q3=""; q4=""; q5=""} #Cuts out region select if the region is 'Alle regioner'
        ##############
        q6 <- ' and a.timeStamp between "'
        q7 <- format(input$dateRange[1]) #Start date
        q8 <- '" and "'
        q9 <- format(input$dateRange[2]) #End date
        q10 <- '" and title regexp "'
        titleRegexp <- ""
        if(length(datafields$titles) > 0){ #check if user has entered any search terms
          for(i in 1:length(datafields$titles)){
            if(i == 1){
              titleRegexp <- datafields$titles[i]
            } else {
              titleRegexp <-paste0(titleRegexp,"|",datafields$titles[i])
            }
          }
        }
        q11 <- titleRegexp
        q12 <- '" group by ak.kompetence_id order by amount desc limit 30'
        
        q2 <- ' ak.kompetence_id IN ('
        for (i in 1:length(kompetenceIds)){
          if (i < length(kompetenceIds)){
            q2 <- paste0(q2, (paste0(kompetenceIds[i], ', ')))
          }
          else{
            q2 <- paste0(q2, kompetenceIds[i],') ')
          }
        }

        setProgress(2/5)
        
        qq <- paste0(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12)
        
       
        kompetenceData <- dbGetQuery(con, qq)
        
        dbDisconnect(con)
        
        setProgress(3/5)
        if (dim(kompetenceData)[1] != 0) {
          # If i order the table in the correct order in the SQL query the 'Limit 30' option cuts off the biggest instead of the smallest.
          # Which is why the table must be ordered after the query.
          kompetenceData <- kompetenceData[order(kompetenceData$amount, decreasing = FALSE),]
          
          setProgress(4/5)
          output$kompetenceDiagram <- renderPlot({
            par(mar = c(5,15,4,2) + 0.1)
            barplot(kompetenceData$amount, 
                    main=paste0("Kompetencer i jobopslag\n", input$regChoice, " fra ", input$dateRange[1], " til ", input$dateRange[2], "."), 
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
        setProgress(5/5)
      })
    }
    else{
      output$kompetenceErrorField <- renderText("")
      output$kompetenceDiagram <- NULL
    }
  }
  
  updateProgressionDiagram <- function(){
    if(length(kompetencer$sk) != 0){
      withProgress(message = "Opdaterer diagram", expr = {
        setProgress(0)
        matchIndexes <- list()
        categoryMatrix <- as.matrix(fullCategoryData)
        for (kompetence in kompetencer$sk){
          matchIndexes <- c(matchIndexes, which(categoryMatrix[,1] == kompetence))
        }
        kompetenceIds <- list()
        for (index in matchIndexes){
          kompetenceIds <- c(kompetenceIds, categoryMatrix[index,2])
        }
        con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, port = credentials.port, db = credentials.db, bigint = c("numeric"))
        stopifnot(is.object(con))
        
        setProgress(1/7)
        
        q1 <- 'select cast(a.timeStamp as date) as date, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where k._id = '
        #if (input$matchChoice == "Machine-Learned"){
        #  q1 <- 'select cast(a.timeStamp as date) as date, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence_machine ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where k._id = '
        #}
        #q2 is kompetence id, set in loop due to it being the one iterated on.
        ####REGION####
        q3 <- ' and a.region_id = (select r.region_id from region r where r.name = "'
        q4 <- input$regChoice            #region name
        q5 <- '")'
        if (q4 == "Alle regioner"){q3=""; q4=""; q5=""} #Cuts out region where-clause if the region is 'Alle regioner'
        ##############
        q6 <- ' and a.timeStamp between "'
        q7 <- format(input$dateRange[1]) #Start date
        q8 <- '" and "'
        q9 <- format(input$dateRange[2]) #End date
        q10 <- '" and title regexp "'
        titleRegexp <- ""
        if(length(datafields$titles) > 0){ #check if user has entered any search terms
          for(i in 1:length(datafields$titles)){
            if(i == 1){
              titleRegexp <- datafields$titles[i]
            } else {
              titleRegexp <-paste0(titleRegexp,"|",datafields$titles[i])
            }
          }
        }
        q11 <- titleRegexp
        q12 <- '" group by cast(a.timeStamp as date)'
        
        
        progressionData <- data.frame()
        for (id in kompetenceIds){
          q2 <- id
          
          progressionData <- rbind(progressionData, dbGetQuery(con, paste0(q1, q2, q3, q4, q5, q6, q7, q8, q9,q10,q11, q12)))
          
          
        }
        dbDisconnect(con)
        
        setProgress(2/7)
        progressionData <- progressionData %>% group_by(date) %>% summarize(amount = sum(amount))
        progressionData <- progressionData[order(progressionData$date, decreasing = FALSE),]
        format <- ""
        if (input$progressionDateFormat == "Uge"){
          format <- "%Y-%W"
        }
        else if (input$progressionDateFormat == "Måned"){
          format <- "%Y-%m"
        }
        else if (input$progressionDateFormat == "År"){
          format <- "%Y"
        }
        
        setProgress(3/7)
        formattedData <- progressionData %>% group_by(format(date, format)) %>% summarize(amount = sum(amount))
        colnames(formattedData)[1] <- "date"
        
        setProgress(4/7)
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
          
          setProgress(5/7)
          formattedData <- formattedData[order(formattedData$date, decreasing = FALSE),]
          
          x <- 0
          y <- 0
          xy <- 0
          x2 <- 0
          y2 <- 0
          n <- nrow(formattedData)
          for (i in 1:n){
            x <- x + i
            y <- y + formattedData$amount[i]
            xy <- xy + ((i) * formattedData$amount[i])
            x2 <- x2 + (i)^2
            y2 <- y2 + formattedData$amount[i]^2
            
            lastDate <- formattedData$date[i]
          }
          
          setProgress(6/7)
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
        setProgress(1)
      })
      
    }
    else{
      output$progressionErrorField <- renderText("")
      output$progressionDiagram <- NULL
    }
  }
  
  updateAnnonceList <- function(){
    if(length(kompetencer$sk) != 0){
      withProgress(message = "Updaterer annonceliste", expr = {
        setProgress(0)
        matchIndexes <- list()
        categoryMatrix <- as.matrix(fullCategoryData)
        for (kompetence in kompetencer$sk){
          matchIndexes <- c(matchIndexes, which(categoryMatrix[,1] == kompetence))
        }
        kompetenceIds <- list()
        for (index in matchIndexes){
          kompetenceIds <- c(kompetenceIds, categoryMatrix[index,2])
        }
        con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, port = credentials.port, db = credentials.db, bigint = c("numeric"))
        stopifnot(is.object(con))
        
        setProgress(1/3)
        
        q1 <- 'select a._id, a.title from annonce a join annonce_kompetence ak on a._id = ak.annonce_id join kompetence k on ak.kompetence_id = k._id where '
        #if (input$matchChoice == "Machine-Learned"){
        #  q1 <- 'select a._id, a.title from annonce a join annonce_kompetence_machine ak on a._id = ak.annonce_id join kompetence k on ak.kompetence_id = k._id where '
        #}
        q2 <- '' #Kompetence id, set in loop due to it being the one iterated on.
        ####REGION####
        q3 <- ' and a.region_id = (select r.region_id from region r where r.name = "'
        q4 <- input$regChoice            #region name
        q5 <- '")'
        if (q4 == "Alle regioner"){q3=""; q4=""; q5=""} #Cuts out region select if the region is 'Alle regioner'
        ##############
        q6 <- ' and a.timeStamp between "'
        q7 <- format(input$dateRange[1]) #Start date
        q8 <- '" and "'
        q9 <- format(input$dateRange[2]) #End date
        q10 <- '" and title regexp '
        titleRegexp <- "" 
        if(length(datafields$titles) > 0){ #check if user has entered any search terms
          for(i in 1:length(datafields$titles)){
            if(i == 1){
              titleRegexp <- datafields$titles[i]
            } else {
              titleRegexp <-paste0(titleRegexp,"|",datafields$titles[i])
            }
          }
        }
        q11 <- paste0("'",titleRegexp,"'")
        q12 <- ' and a._id in (select annonce_id from annonce_dataField where dataValue regexp '
        datafieldsRegexp <- ""
        if(length(datafields$selectedDataFields) > 0){ #check if user has entered any search terms
          for(i in 1:length(datafields$selectedDataFields)){
            if(i == 1){
              datafieldsRegexp <- datafields$selectedDataFields[i]
            } else {
              datafieldsRegexp <-paste0(datafieldsRegexp,"|",datafields$selectedDataFields[i])
            }
          }
        }
        q13 <- paste0("'", datafieldsRegexp, "'")
        q14 <- ')'
        print(datafieldsRegexp)
        if(datafieldsRegexp == ""){q12=""; q13=""; q14=""} #Cuts out datafield search if no datafields entered.
         
        
        q15 <- ' group by a._id'
        
        q2 <- ' ak.kompetence_id IN ('
        for (i in 1:length(kompetenceIds)){
          if (i < length(kompetenceIds)){
            q2 <- paste0(q2, (paste0(kompetenceIds[i], ', ')))
          }
          else{
            q2 <- paste0(q2, kompetenceIds[i],') ')
          }
        }
        
        query <- paste0(q1, q2, q3, q4, q5, q6, q7, q8, q9,q10,q11,q12,q13,q14,q15)
        
        print(paste0(q7, q8, q9,q10,q11,q12,q13,q14,q15))
        annonceData <- dbGetQuery(con,query)
        print(annonceData)
        
        dbDisconnect(con)
        
        setProgress(2/3)
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
        setProgress(1)
      })
      
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
