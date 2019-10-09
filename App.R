library(DBI)
library(RMariaDB)
library(shiny)
library(shiny.i18n)
library(dplyr)
library(ggplot2)
library(shinyTree)
library(devtools)

source(file = 'credentials.R')
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language(credentials.language)

ui <- fluidPage(
  fluidRow(style = "margin-top: 5px;",
    column(6, style = "margin-top: 0px; font-size: 16px;", 
           titlePanel(title = i18n$t("COMPETENCE ANALYSIS"), windowTitle = "Annonce Analyse"),
           div(style = "font-size: 20px;", textOutput(outputId = "annonceCountField")),
           tags$span("Kilde: "),
           tags$a(href="https://www.jobindex.dk","JobIndex"),
           tags$span(" og "),
           tags$a(href="https://www.careerjet.dk","CareerJet")
    ),
    column(6, style = "margin-top: 5px; text-align: right", img(src = "EAAA_Logo.jpg"))
  ),
  fluidRow(style = "border-bottom: 2px solid black; margin-top: 10px;"),
  
  fluidRow(style = "margin-top: 15px;",
    
    column(6,
           tags$h3(i18n$t("Search criterea")),
           tabsetPanel(
             tabPanel(title = i18n$t("Competences"),
           wellPanel(
             shinyTree("tree", checkbox = TRUE),
             fluidRow(style = "margin-top: 15px;"),
             fluidRow(
               column(4,
                      selectInput(inputId = "availableCategories",
                                  label = i18n$t("Available   Categories"),
                                  size = 20,
                                  selectize = FALSE,
                                  multiple = TRUE,
                                  choices = list(),
                                  width = "100%"
                      )
               ),
               column(4, align = "center", style = "margin-top: 25px;",
                      fluidRow(style = "margin-top: 15px;",
                               actionButton("addAll",label = i18n$t("Add All \u02C3\u02C3"), width = 150),
                               actionButton("add", label = i18n$t("Add \u02C3"), width = 150)
                      ),
                      fluidRow(style = "margin-top: 15px;",
                               actionButton("remove",label = i18n$t("\u02C2 Remove"), width = 150),
                               actionButton("removeAll", label = i18n$t("\u02C2\u02C2 Remove All"), width = 150)    
                      )
               ),
               column(4, 
                      selectInput(inputId = "selectedCategories",
                                  label = i18n$t("Selected Categories"),
                                  size = 20,
                                  selectize = FALSE,
                                  multiple = TRUE,
                                  choices = list(),
                                  width = "100%"
                      )
               )
             ),
             textInput(inputId = "searchField", label = "Filter"))
           ),
           tabPanel(title=i18n$t("Advertisements"),
           wellPanel(
             fluidRow(
               column(6,
                      selectInput(inputId = "regChoice",
                                  label = i18n$t("Region"), 
                                  choices = list("Alle regioner", "storkoebenhavn", "nordsjaelland", "region-sjaelland", "fyn", "region-nordjylland", "region-midtjylland", "sydjylland", "bornholm", "skaane", "groenland", "faeroeerne", "udlandet"),
                                  multiple = FALSE, 
                                  width = "400px"
                      )
               ),
               column(6,
                      dateRangeInput('dateRange',
                                     label = i18n$t("Period"),
                                     start = Sys.Date()-30, end = Sys.Date()
                      )
               ),
               column(9, textInput(inputId = "titleSearchField", label = i18n$t("Job Title"))
               ),
               column(3, align = "center", actionButton(inputId = "addTitle", label = i18n$t("Add Title \u02C3"), style = "margin-top: 25px", width = 100)
               ),
               column(9, 
                      selectInput(inputId = "selectedTitleSearchTerms",
                                  label = i18n$t("Selected Job Title"),
                                  size = 20,
                                  selectize = FALSE,
                                  multiple = TRUE,
                                  choices = list(),
                                  width = "100%"
                      )
               ),
               column(3,align = "center", style = "margin-top: 125px;",
                      actionButton("removeTitle", label = i18n$t("\u02C2 Remove"), width = 150, style = "margin-top: 10px"),
                      actionButton("removeAllTitles", label = i18n$t("\u02C2\u02C2 Remove All"), width = 150, style = "margin-top: 10px")
               )
             )
           )),
           tabPanel(title=i18n$t("Meta data"),
                    wellPanel(
                      fluidRow(
                        column(12,
                               selectInput(inputId = "dataFieldChoice",
                                           label = i18n$t("Data field"), 
                                           choices = {
                                             con <- dbConnect(RMariaDB::MariaDB(),host = credentials.host, user = credentials.user, password = credentials.password, port = credentials.port, db = credentials.db, bigint = c("numeric"))
                                             stopifnot(is.object(con))
                                             dataFieldNames <- as.matrix(dbGetQuery(con,'select name from datafield'))
                                             dbDisconnect(con) 
                                             choices = dataFieldNames
                                             },
                                           multiple = FALSE, 
                                           width = "500px"
                               )
                        ),
                        column(12,
                               textInput(inputId = "dataFieldSearchField", label = i18n$t("Filter"))
                        ),
                        column(4,
                               selectInput(inputId = "availableDataFields",
                                           label = i18n$t("Available datafields:"),
                                           size = 20,
                                           selectize = FALSE,
                                           multiple = TRUE,
                                           choices = list(),
                                           width = "100%"
                               )
                        ),
                        column(4, align = "center", style = "margin-top: 75px;",
                               actionButton("addDataField", i18n$t("Add \u02C3"), width = 150),
                               actionButton("removeDataField", i18n$t("\u02C2 Remove"), width = 150),
                               actionButton("removeAllDataFields", i18n$t("\u02C2\u02C2 Remove All"), width = 150)    
                        ),
                        column(4, 
                               selectInput(inputId = "selectedDataFields",
                                           label = i18n$t("Selected"),
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
    ),
    column(6,
           fluidRow(
             column(4,
                    tags$h3(i18n$t("Search results"))
                    )
            
           ),
           tabsetPanel(id = "outputPanel",
             tabPanel(title=i18n$t("Competence Comparison"),
                      wellPanel(
                        textOutput(outputId = "kompetenceErrorField"),
                        checkboxInput("matchAllCompetences", "Vis kun søgte kompetencer", TRUE),
                        plotOutput("kompetenceDiagram", height = 620),
                        downloadButton("downloadKompetenceData", "Download data")
                      )
             ),
             tabPanel("Progression", 
                      wellPanel(
                        selectInput(inputId = "progressionDateFormat", 
                                    label = i18n$t("Progression Period"), 
                                    choices = list("Uge", "Måned", "År")
                        ),
                        textOutput(outputId = "progressionErrorField"),
                        plotOutput("progressionDiagram", height = 540)
                      )
             ),
             tabPanel(title=i18n$t("Ad List"),
                      wellPanel(
                        fluidRow(
                          textOutput(outputId = "annonceErrorField"),
                          selectInput(inputId = "annonceList",
                                      label = i18n$t("Ad List"),
                                      selectize = FALSE,
                                      size = 20,
                                      choices = list(),
                                      width = "100%"
                          ),
                          tags$h5(style="font-weight:bold", "CVR-Oplysninger:"),
                          tableOutput(outputId = "annonceDataFields"),
                          tags$h5(style="font-weight:bold", "Annoncetekst:"),
                          textOutput(outputId = "annonceText"),
                          tags$style(type="text/css", "#annonceDataFields td:first-child {font-weight:bold;}"), #make row names of table bold.
                          downloadButton("downloadAnnonceliste", "Download data")
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
  csvData <- reactiveValues(annonceListe = list(), kompetenceListe = list())
  
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
    output$annonceCountField <- renderText(paste0(annonceCount, " jobannoncer"))
    setProgress(2/3)
    
    
    treeString <- dbGetQuery(con, 'select shinyTreeJSON from global where _id = 1')[1,1]
    output$tree <- renderEmptyTree()
    updateTree(session,"tree", treeString)
    setProgress(1)
  })
  
  output$downloadAnnonceliste <- downloadHandler(
    filename = 'annonce_liste.csv',
    content = function(file){
      write.csv(csvData$annonceListe, file, row.names = FALSE)
    }
  )
  output$downloadKompetenceData <- downloadHandler(
    filename = 'kompetence_data.csv',
    content = function(file){
      write.csv(csvData$kompetenceListe, file, row.names = FALSE)
    }
  )
  
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
  
  observeEvent(input$dataFieldSearchField, ignoreInit = TRUE, {
    updateDataFields()
  }) 
  
  observeEvent(input$matchAllCompetences, ignoreInit = TRUE,{
      updateCurrentTab()
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
  
  observeEvent(input$tree, ignoreInit = TRUE, {
    selectedList <- get_selected(input$tree)
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
      con <- dbConnect(RMariaDB::MariaDB(), port = credentials.port, host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
      stopifnot(is.object(con))
      
      id <- unlist(strsplit(input$annonceList, " "))[1]
      
      

      annonceDataFields <- dbGetQuery(con,paste0('select cvr, "CVR" as name from annonce where _id = ', id, ' union select dataValue, name from annonce_datafield join datafield where annonce_id = ',id,' and datafield._id = dataField_id'))
      annonceText <- dbGetQuery(con, paste0('select convert(searchable_body using utf8) as searchable_body from annonce where _id = ', id))
      

      
      if(nrow(annonceDataFields)== 0 || is.na(annonceDataFields)){
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
    
    if(input$dataFieldSearchField == ""){
      query <- paste0(" from annonce_datafield where (select _id from datafield where name = '", input$dataFieldChoice, "') = datafield_id and dataValue is not null")
    } else {
      query <- paste0(" from annonce_datafield where (select _id from datafield where name = '", input$dataFieldChoice, "') = datafield_id and dataValue is not null and dataValue like '%", input$dataFieldSearchField, "%'")
    }
    
    count <- paste0("select count(distinct dataValue)",query)
    
    if(dbGetQuery(con,count) > 1000){
      showNotification("Søgekriteriet er for bredt, specifikér søgningen!", type= "error")
    } else {
      updateSelectInput(session,
                        inputId = "availableDataFields",
                        choices = as.matrix(dbGetQuery(con,paste0("select distinct dataValue",query)))
      )
    }
    
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
    selectedList <- get_selected(input$tree)
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
        qq <- paste0(q1, q2)
        finalList <- as.vector(as.matrix(dbGetQuery(con, qq)))
        
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
        q1 <- 'select distinct ak.k_prefferredLabel, count(ak.kompetence_id) as amount from annonce_kompetence ak where '
        #if (input$matchChoice == "Machine-Learned"){
        #  q1 <- 'select distinct k.prefferredLabel, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence_machine ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where '
        #}
        q2 <- '' #Kompetence id, set in loop due to it being the one iterated on.
        ####REGION####
        q3 <- ' and ak.a_region_name = "'
        q4 <- input$regChoice            #region name
        q5 <- '" '
        if (q4 == "Alle regioner"){q3=""; q4=""; q5=""} #Cuts out region select if the region is 'Alle regioner'
        ##############
        q6 <- ' and ak.a_timeStamp between DATE("'
        q7 <- format(input$dateRange[1]) #Start date
        q8 <- '") and DATE("'
        q9 <- format(input$dateRange[2]) #End date
        q10 <- '") and a_title regexp '
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
        q11 <- paste0("\"",titleRegexp,"\"")
        if(titleRegexp == ""){q10 <- '")'; q11 <- ""}
        q12 <- ' and ak.annonce_id in (select annonce_id from annonce_datafield where dataValue regexp '
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
        if(datafieldsRegexp == ""){q12=""; q13=""; q14=""} #Cuts out datafield search if no datafields entered.
        q15 <- ' group by ak.kompetence_id order by amount desc limit 30'
        
        
        
        
        if(!input$matchAllCompetences){
          q1 <- 'select distinct ak.k_prefferredLabel, count(ak.kompetence_id) as amount from annonce_kompetence ak join (select distinct aki.annonce_id from kompetence k join annonce_kompetence as aki on k._id = aki.kompetence_id and aki.a_timeStamp between DATE("'
          q1 <- paste0(q1,format(input$dateRange[1]),'") and DATE("',format(input$dateRange[2]),'") where k._id in')
          q2 <- '('
          
          for (i in 1:length(kompetenceIds)){
            if (i < length(kompetenceIds)){
              q2 <- paste0(q2, (paste0(kompetenceIds[i], ', ')))
            }
            else{
              q2 <- paste0(q2, kompetenceIds[i],')) as ad_ids')
            }
          }
          q2 <- paste0(q2, ' on ak.annonce_id=ad_ids.annonce_id')
        } else {
          
          q2 <- ' ak.kompetence_id IN (select max(komp._id) from kompetence komp where komp._id in ('
          for (i in 1:length(kompetenceIds)){
            if (i < length(kompetenceIds)){
              q2 <- paste0(q2, (paste0(kompetenceIds[i], ', ')))
            }
            else{
              q2 <- paste0(q2, kompetenceIds[i],') group by komp.prefferredLabel) ')
            }
          }
        }

        setProgress(2/5)
        
        qq <- paste0(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15)
        csvDataQuery <- paste0(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14, ' group by ak.kompetence_id order by amount desc')
        #print(qq)
        kompetenceData <- dbGetQuery(con, qq)
        csvData$kompetenceListe <- dbGetQuery(con,csvDataQuery)
        #print(kompetenceData)
        
        dbDisconnect(con)
        
        setProgress(3/5)
        if (dim(kompetenceData)[1] != 0) {
          # If i order the table in the correct order in the SQL query the 'Limit 30' option cuts off the biggest instead of the smallest.
          # Which is why the table must be ordered after the query.
          kompetenceData <- kompetenceData[order(kompetenceData$amount, decreasing = FALSE),]
          
          setProgress(4/5)
          output$kompetenceDiagram <- renderPlot({
            
            par(mar = c(5,18,4,2) + 0.1)
            ylim <- c(0, 1.2*max(kompetenceData$amount))
            xx <- barplot(kompetenceData$amount, xlim = ylim, 
                    main=paste0("Kompetencer i jobopslag\n", input$regChoice, " fra ", input$dateRange[1], " til ", input$dateRange[2], "."), 
                    names.arg = kompetenceData$k_prefferredLabel,
                    las = 2,
                    horiz = TRUE
            )
            text(y = xx, x = kompetenceData$amount, label = kompetenceData$amount, pos = 4, cex = 1.2, col = "blue")
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
        
        periodQuery <- ""
        if (input$progressionDateFormat == "Uge"){
          periodQuery <- 'DATE_FORMAT(a_timeStamp,"%Y-%U")'
        }
        else if (input$progressionDateFormat == "Måned"){
          periodQuery <- 'DATE_FORMAT(a_timeStamp,"%Y-%m")'
        }
        else if (input$progressionDateFormat == "År"){
          periodQuery <- 'DATE_FORMAT(a_timeStamp,"%Y")'
        }
        
        q0 <- paste0("select ", periodQuery)
        q1 <- " period, count(DISTINCT annonce_id) amount from annonce_kompetence ak where ak.kompetence_id in"
        #if (input$matchChoice == "Machine-Learned"){
        #  q1 <- 'select cast(a.timeStamp as date) as date, count(ak.kompetence_id) as amount from kompetence k left join annonce_kompetence_machine ak on k._id = ak.kompetence_id left join annonce a on ak.annonce_id = a._id where k._id = '
        #}
        #q2 is kompetence id, set in loop due to it being the one iterated on.
        ####REGION####
        q3 <- ' and ak.a_region_name = "'
        q4 <- input$regChoice            #region name
        q5 <- '" '
        if (q4 == "Alle regioner"){q3=""; q4=""; q5=""} #Cuts out region where-clause if the region is 'Alle regioner'
        ##############
        q6 <- ' and ak.a_timeStamp between DATE("'
        q7 <- format(input$dateRange[1]) #Start date
        q8 <- '") and DATE("'
        q9 <- format(input$dateRange[2]) #End date
        q10 <- '") and a_title regexp '
        titleRegexp <- ""
        if(length(datafields$titles) > 0){ #check if user has entered any search terms
          for(i in 1:length(datafields$titles)){
            if(i == 1){
              titleRegexp <- datafields$titles[i]
            } else {
              titleRegexp <-paste0(titleRegexp,"|",datafields$titles[i])
            }
          }
        } else {
          titleRegexp <- ".*"
        }
        q11 <- paste0("\"",titleRegexp,"\"")
        q12 <- ' and ak.annonce_id in (select annonce_id from annonce_datafield where dataValue regexp '
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
        if(datafieldsRegexp == ""){q12=""; q13=""; q14=""} #Cuts out datafield search if no datafields entered.
        q15 <- ' group by cast(ak.a_timeStamp as date)'
        
        
        progressionData <- data.frame()
        q2 <- "("
        for (i in 1:length(kompetenceIds)){
          if (i < length(kompetenceIds)){
            q2 <- paste0(q2, (paste0(kompetenceIds[i], ', ')))
          }
          else{
            q2 <- paste0(q2, kompetenceIds[i],') ')
          }
        }
        
        qq <- paste0(q0, q1, q2, q3, q4, q5, q6, q7, q8, q9,q10,q11, q12,q13,q14,q15)
        #print(qq)
        formattedData <- rbind(progressionData, dbGetQuery(con, qq))

        dbDisconnect(con)
        
        setProgress(2/7)
       
        setProgress(3/7)
        
        setProgress(4/7)
        lastDate <- ""
        n <- nrow(formattedData)
        
        setProgress(5/7)
        if (n > 0){ ##### Check added to prevent crash when there's no job advertisements to be found

            n <- length(formattedData$amount)

            fda <- c(formattedData$amount)
            fdp <- c(1:n)
            
            result <- lm( formula = fda ~ fdp ) 
            a <- result$coefficients[1]
            b <- result$coefficients[2]
            v <- round(100*(b*(n+1))/abs(a), digits=0)
            
                                        # print (paste0("a: ", a, ", b: ", b, ", vækst: ", v, "%"))
            
            setProgress(6/7)

            output$progressionDiagram <- renderPlot({

                ylim <- c(0, 1.1*max(formattedData$amount))
                xx <- barplot(formattedData$amount, ylim = ylim,
                              main=paste0(v, "% vækst i perioden"),
                              ylab="Antal annoncer",
                              names.arg = formattedData$period)
                
                text(x = xx, y = formattedData$amount, label = formattedData$amount, pos = 3, cex = 1.2, col = "blue")
                
                if (n > 1) 
                {
                                        #As of writing this code there is only data from about 4 months, so less than a year.
                                        #If n is 1 it will result in an error when doing regression, and it is currently 1 when choosing year.
                                        #abline(a=a, b=b, col = "red", lwd = 3)
                    abline(result, col = "red", lwd = 3)
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
      withProgress(message = "Opdaterer annonceliste", expr = {
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
        con <- dbConnect(RMariaDB::MariaDB(), port = credentials.port, host = credentials.host, user = credentials.user, password = credentials.password, db = credentials.db, bigint = c("numeric"))
        stopifnot(is.object(con))
        
        setProgress(1/3)
        
        q1 <- 'select ak.annonce_id, ak.a_title from annonce_kompetence ak where '
        q2 <- '' #Kompetence id, set in loop due to it being the one iterated on.
        ####REGION####
        q3 <- ' and ak.a_region_name = "'
        q4 <- input$regChoice            #region name
        q5 <- '" '
        if (q4 == "Alle regioner"){q3=""; q4=""; q5=""} #Cuts out region select if the region is 'Alle regioner'
        ##############
        q6 <- ' and ak.a_timeStamp between DATE("'
        q7 <- format(input$dateRange[1]) #Start date
        q8 <- '") and DATE("'
        q9 <- format(input$dateRange[2]) #End date
        q10 <- '") and a_title regexp '
        titleRegexp <- "" 
        if(length(datafields$titles) > 0){ #check if user has entered any search terms
          for(i in 1:length(datafields$titles)){
            if(i == 1){
              titleRegexp <- datafields$titles[i]
            } else {
              titleRegexp <-paste0(titleRegexp,"|",datafields$titles[i])
            }
          }
        } else {
          titleRegexp <- ".*"
        }
        q11 <- paste0("\"",titleRegexp,"\"")
        q12 <- ' and ak.annonce_id in (select annonce_id from annonce_datafield where dataValue regexp '
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
        if(datafieldsRegexp == ""){q12=""; q13=""; q14=""} #Cuts out datafield search if no datafields entered.
         
        
        q15 <- ' group by ak.annonce_id'
        
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
        annonceData <- dbGetQuery(con,query)
        csvData$annonceListe <- annonceData
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
