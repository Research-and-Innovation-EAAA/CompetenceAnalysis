library(DBI)
library(RMariaDB)
library(shiny)
library(shinyBS)
library(shiny.i18n)
library(dplyr)
library(ggplot2)
library(shinyTree)
library(devtools)
library(jsonlite)
library(stringr)
library(wordcloud2)
library(shinyWidgets)
library(shinyjs)

source(file = 'credentials.R')
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language(credentials.language)

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    style = "margin-top: 5px;",
    column(
      6,
      style = "margin-top: 0px; font-size: 16px;",
      titlePanel(
        title = i18n$t("COMPETENCE ANALYSIS"),
        windowTitle = "Annonce Analyse"
      ),
      div(style = "font-size: 20px;", textOutput(outputId = "adCountField")),
      tags$span(paste0(i18n$t("Source:"), " ")),
      tags$a(href = "https://www.jobindex.dk", "JobIndex"),
      tags$span(paste0(" ", i18n$t("and"), " ")),
      tags$a(href = "https://www.careerjet.dk", "CareerJet")
    ),
    column(6, style = "margin-top: 5px; text-align: right", img(src = "EAAA_Logo.jpg"))
  ),
  fluidRow(style = "border-bottom: 2px solid black; margin-top: 10px;"),
  
  fluidRow(
    style = "margin-top: 15px;",
    
    column(
      6,
      tags$h3(
        i18n$t("Search criterea"),
        switchInput(
          inputId = "showSeearchCriterea",
          value = TRUE,
          onLabel = i18n$t("Show"),
          offLabel = i18n$t("Hide"),
          size = 'mini',
          inline = TRUE
        )
      ),
      tags$div(
       id="divSearchPanel",
       tabsetPanel(
        tabPanel(
          title = i18n$t("Competences"),
          wellPanel(
            fluidRow(column(8,
                            shinyTree("tree",
                                      checkbox = TRUE)),
                     column(
                       4,
                       numericInput("minimumMatches", i18n$t("Minimum occurences"), 1, min = 1)
                     )),
            fluidRow(style = "margin-top: 15px;"),
            fluidRow(
              column(
                4,
                selectInput(
                  inputId = "availableCategories",
                  label = i18n$t("Available   Categories"),
                  size = 10,
                  selectize = FALSE,
                  multiple = TRUE,
                  choices = list(),
                  width = "100%"
                )
              ),
              column(
                4,
                align = "center",
                style = "margin-top: 25px;",
                fluidRow(
                  style = "margin-top: 15px;",
                  actionButton(
                    "addAll",
                    label = i18n$t("Add All \u02C3\u02C3"),
                    width = 150
                  ),
                  actionButton("add", label = i18n$t("Add \u02C3"), width = 150)
                ),
                fluidRow(
                  style = "margin-top: 15px;",
                  actionButton(
                    "remove",
                    label = i18n$t("\u02C2 Remove"),
                    width = 150
                  ),
                  actionButton(
                    "removeAll",
                    label = i18n$t("\u02C2\u02C2 Remove All"),
                    width = 150
                  )
                )
              ),
              column(
                4,
                selectInput(
                  inputId = "selectedCategories",
                  label = i18n$t("Selected Categories"),
                  size = 10,
                  selectize = FALSE,
                  multiple = TRUE,
                  choices = list(),
                  width = "100%"
                )
              )
            ),
            textInput(inputId = "searchField", label = "Filter")
          )
        ),
        tabPanel(title = i18n$t("Advertisements"),
                 wellPanel(fluidRow(
                   column(
                     6,
                     dateRangeInput(
                       'dateRange',
                       label = i18n$t("Period"),
                       start = Sys.Date() - 30,
                       end = Sys.Date()
                     )
                   ),
                   column(
                     6,
                     selectInput(
                       inputId = "regChoice",
                       label = i18n$t("Region"),
                       choices = list(
                         "Alle regioner",
                         "storkoebenhavn",
                         "nordsjaelland",
                         "region-sjaelland",
                         "fyn",
                         "region-nordjylland",
                         "region-midtjylland",
                         "sydjylland",
                         "bornholm",
                         "skaane",
                         "groenland",
                         "faeroeerne",
                         "udlandet"
                       ),
                       multiple = FALSE,
                       width = "400px"
                     )
                   ),
                   column(12,
                          tabsetPanel(
                            tabPanel(
                              title = i18n$t("Ad id"),
                              wellPanel(
                                style = "background: white; height:400px",
                                column(
                                  9,
                                  textInput(
                                    inputId = "adIdSearchField",
                                    label = i18n$t("Ad id"),
                                    placeholder = i18n$t("Enter one or more comma separated ad ids")
                                  )
                                ),
                                column(
                                  3,
                                  align = "center",
                                  actionButton(
                                    inputId = "addAdId",
                                    label = i18n$t("Add ad id \u02C3"),
                                    style = "margin-top: 25px",
                                    width = 150
                                  )
                                ),
                                column(
                                  9,
                                  selectInput(
                                    inputId = "selectedAdIdSearchTerms",
                                    label = i18n$t("Selected Ad ids"),
                                    size = 15,
                                    selectize = FALSE,
                                    multiple = TRUE,
                                    choices = list(),
                                    width = "100%"
                                  )
                                ),
                                column(
                                  3,
                                  align = "center",
                                  style = "margin-top: 20px;",
                                  actionButton(
                                    "removeAdId",
                                    label = i18n$t("\u02C2 Remove"),
                                    width = 150,
                                    style = "margin-top: 10px"
                                  ),
                                  actionButton(
                                    "removeAllAdIds",
                                    label = i18n$t("\u02C2\u02C2 Remove All"),
                                    width = 150,
                                    style = "margin-top: 10px"
                                  )
                                )
                                
                              )
                            ),
                            tabPanel(
                              title = i18n$t("Job Title"),
                              wellPanel(
                                style = "background: white; height:400px",
                                column(
                                  9,
                                  textInput(
                                    inputId = "titleSearchField",
                                    label = i18n$t("Job Title"),
                                    placeholder = i18n$t("Enter one or more comma separated job titles")
                                  )
                                ),
                                column(
                                  3,
                                  align = "center",
                                  actionButton(
                                    inputId = "addTitle",
                                    label = i18n$t("Add Title \u02C3"),
                                    style = "margin-top: 25px",
                                    width = 150
                                  )
                                ),
                                column(
                                  9,
                                  selectInput(
                                    inputId = "selectedTitleSearchTerms",
                                    label = i18n$t("Selected Job Title"),
                                    size = 10,
                                    selectize = FALSE,
                                    multiple = TRUE,
                                    choices = list(),
                                    width = "100%"
                                  )
                                ),
                                column(
                                  3,
                                  align = "center",
                                  style = "margin-top: 20px;",
                                  actionButton(
                                    "removeTitle",
                                    label = i18n$t("\u02C2 Remove"),
                                    width = 150,
                                    style = "margin-top: 10px"
                                  ),
                                  actionButton(
                                    "removeAllTitles",
                                    label = i18n$t("\u02C2\u02C2 Remove All"),
                                    width = 150,
                                    style = "margin-top: 10px"
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = i18n$t("Job Text"),
                              wellPanel(
                                style = "background: white; height:400px",
                                column(
                                  9,
                                  textInput(
                                    inputId = "TextSearchField",
                                    label = i18n$t("Job Text"),
                                    placeholder = i18n$t("Enter one or more comma separated job texts")
                                  )
                                ),
                                column(
                                  3,
                                  align = "center",
                                  actionButton(
                                    inputId = "addText",
                                    label = i18n$t("Add Text \u02C3"),
                                    style = "margin-top: 25px",
                                    width = 150
                                  )
                                ),
                                column(
                                  9,
                                  selectInput(
                                    inputId = "selectedTextSearchTerms",
                                    label = i18n$t("Selected Job Text"),
                                    size = 10,
                                    selectize = FALSE,
                                    multiple = TRUE,
                                    choices = list(),
                                    width = "100%"
                                  )
                                ),
                                column(
                                  3,
                                  align = "center",
                                  style = "margin-top: 20px;",
                                  actionButton(
                                    "removeText",
                                    label = i18n$t("\u02C2 Remove"),
                                    width = 150,
                                    style = "margin-top: 10px"
                                  ),
                                  actionButton(
                                    "removeAllTexts",
                                    label = i18n$t("\u02C2\u02C2 Remove All"),
                                    width = 150,
                                    style = "margin-top: 10px"
                                  )
                                )
                              )
                            )
                          ))
                   
                 ))),
        tabPanel(title = i18n$t("Meta data"),
                 wellPanel(
                   fluidRow(
                     column(
                       12,
                       selectInput(
                         inputId = "dataFieldChoice",
                         label = i18n$t("Data field"),
                         choices = {
                           con <-
                             dbConnect(
                               RMariaDB::MariaDB(),
                               host = credentials.host,
                               user = credentials.user,
                               password = credentials.password,
                               port = credentials.port,
                               db = credentials.db,
                               bigint = c("numeric")
                             )
                           stopifnot(is.object(con))
                           dataFieldNames <-
                             as.matrix(dbGetQuery(con, 'select name from datafield'))
                           dbDisconnect(con)
                           choices = dataFieldNames
                         },
                         multiple = FALSE,
                         width = "500px"
                       )
                     ),
                     column(12,
                            textInput(
                              inputId = "dataFieldSearchField", label = i18n$t("Filter")
                            )),
                     column(
                       4,
                       selectInput(
                         inputId = "availableDataFields",
                         label = i18n$t("Available datafields:"),
                         size = 10,
                         selectize = FALSE,
                         multiple = TRUE,
                         choices = list(),
                         width = "100%"
                       )
                     ),
                     column(
                       4,
                       align = "center",
                       style = "margin-top: 75px;",
                       actionButton("addDataField", i18n$t("Add \u02C3"), width = 150),
                       actionButton("removeDataField", i18n$t("\u02C2 Remove"), width = 150),
                       actionButton(
                         "removeAllDataFields",
                         i18n$t("\u02C2\u02C2 Remove All"),
                         width = 150
                       )
                     ),
                     column(
                       4,
                       selectInput(
                         inputId = "selectedDataFields",
                         label = i18n$t("Selected"),
                         size = 10,
                         selectize = FALSE,
                         multiple = TRUE,
                         choices = list(),
                         width = "100%"
                       )
                     )
                   )
                 ))
       )  
      )
    ),
    column(
      6,
      fluidRow(column(12,
                      tags$h3(
                        i18n$t("Search results"),
                        textOutput(outputId = "resultCountField", inline = TRUE),
                        switchInput(
                          inputId = "showResultCriterea",
                          value = TRUE,
                          onLabel = i18n$t("Show"),
                          offLabel = i18n$t("Hide"),
                          size = 'mini',
                          inline = TRUE
                        )
                      ),
      )), 
      tags$div(
       id="divResultPanel",
       tabsetPanel(
        id = "outputPanel",
        tabPanel(title = i18n$t("Competences"),
                 wellPanel(
                   fluidRow(
                     column(
                       4,
                       numericInput(
                         inputId = "limitCountCompetences",
                         label = i18n$t("Show"),
                         value = 30,
                         min = 1,
                         max = 100000
                       ),
                       bsTooltip(
                         id = "limitCountCompetences",
                         title = i18n$t("Number of competences to show")
                       )
                     ),
                     column(
                       4,
                       radioButtons(
                         inputId = "showMostOrLess",
                         i18n$t("Requested"),
                         choiceNames = c(i18n$t("Most"),
                                         i18n$t("Less")),
                         choiceValues = c("Most",
                                          "Less"),
                         selected = "Most"
                       ),
                       bsTooltip(
                         id = "showMostOrLess",
                         title = i18n$t("Select competences to show")
                       )
                     ),
                     column(
                       4,
                       strong(i18n$t("Among")),
                       checkboxInput(
                         inputId = "showSearchedCompetences",
                         label = i18n$t("Selected"),
                         value = TRUE
                       ),
                       bsTooltip(
                         id = "showSearchedCompetences",
                         title = i18n$t("Show selected competences")
                       ),
                       checkboxInput("showOtherCompetences", i18n$t("Other"), FALSE),
                       bsTooltip(
                         id = "showOtherCompetences",
                         title = i18n$t("Show not selected competences")
                       )
                     )
                   ),
                   tabsetPanel(
                     id = "matchPanel",
                     tabPanel(
                       "Diagram",
                       wellPanel(
                         style = "background: white",
                         textOutput(outputId = "kompetenceErrorField"),
                         plotOutput("kompetenceDiagram", height = 820),
                         downloadButton("downloadKompetenceData", "Download data")
                       )
                     ),
                     tabPanel(
                       "Wordcloud",
                       wellPanel(
                         style = "background: white",
                         fluidRow(
                           column(
                             3,
                             selectInput(
                               inputId = "wordcloudColor",
                               label = i18n$t("Color"),
                               choices = list(
                                 "random-dark",
                                 "random-light",
                                 "red",
                                 "yellow",
                                 "aqua",
                                 "blue",
                                 "skyblue",
                                 "green",
                                 "navy",
                                 "teal",
                                 "olive",
                                 "lime",
                                 "orange",
                                 "fuchsia",
                                 "purple",
                                 "maroon",
                                 "black"
                               ),
                               multiple = FALSE
                             ),
                             bsTooltip(id = "wordcloudColor",
                                       title = i18n$t("Select color theme"))
                           ),
                           column(
                             3,
                             selectInput(
                               inputId = "wordcloudShape",
                               label = i18n$t("Shape"),
                               choices = list(
                                 "circle",
                                 "cardioid",
                                 "diamond",
                                 "triangle-forward",
                                 "triangle",
                                 "pentagon",
                                 "star"
                               ),
                               multiple = FALSE
                             ),
                             bsTooltip(id = "wordcloudShape",
                                       title = i18n$t("Select shape of diagram"))
                           ),
                           column(
                             3,
                             numericInput(
                               inputId = "wordcloudSize",
                               label = i18n$t("Size"),
                               value = 40,
                               min = 1,
                               max = 100,
                               step = 1
                             ),
                             bsTooltip(
                               id = "wordcloudSize",
                               title = i18n$t("Enter 1-100 for diagram size")
                             )
                           ),
                           column(
                             3,
                             numericInput(
                               inputId = "wordcloudFlatten",
                               label = i18n$t("Flatten"),
                               value = 0,
                               min = 0,
                               max = 100,
                               step = 1
                             ),
                             bsTooltip(
                               id = "wordcloudFlatten",
                               title = i18n$t("Enter 0-100 for uniform font size")
                             )
                           )
                         ),
                         wordcloud2Output("wordcloud", width = "100%", height = 820)
                       )
                     )
                   )
                 )),
        tabPanel(
          "Progression",
          wellPanel(
            selectInput(
              inputId = "progressionDateFormat",
              label = i18n$t("Progression Period"),
              choices = list("Uge", "Måned", "År")
            ),
            textOutput(outputId = "progressionErrorField"),
            plotOutput("progressionDiagram", height = 620),
            downloadButton("downloadProgressionData", "Download data")
          )
        ),
        tabPanel(title = i18n$t("Ad List"),
                 wellPanel(
                   fluidRow(
                     textOutput(outputId = "annonceErrorField"),
                     selectInput(
                       inputId = "annonceList",
                       label = i18n$t("Ad List"),
                       selectize = FALSE,
                       size = 10,
                       choices = list(),
                       width = "100%"
                     ),
                     tags$h5(style = "font-weight:bold", "CVR-Oplysninger:"),
                     tableOutput(outputId = "annonceDataFields"),
                     tags$h5(style = "font-weight:bold", "Kompetencer:"),
                     tableOutput(outputId = "annonceKompetencer"),
                     tags$h5(style = "font-weight:bold", "Annoncetekst:"),
                     textOutput(outputId = "annonceText"),
                     tags$style(
                       type = "text/css",
                       "#annonceDataFields td:first-child {font-weight:bold;}"
                     ),
                     #make row names of table bold.
                     downloadButton("downloadAnnonceliste", "Download data"),
                     downloadButton("downloadAnnonceNumre", i18n$t("Download ID list"))
                   )
                 ))
       )
      )
    )
  )
)

server <- function(input, output, session) {
  kompetencer <- reactiveValues(ak = NULL, sk = list(), fk = list())
  datafields <-
    reactiveValues(
      titles = list(),
      selectedDataFields = list(),
      availableDataFields = list()
    )
  csvData <-
    reactiveValues(
      annonceListe = list(),
      kompetenceListe = list(),
      allQuery = NULL
    )
  
  current <- reactiveValues(tab = 1)
  tabUpdates <-
    reactiveValues(
      kompetence = FALSE,
      progression = FALSE,
      annonce = FALSE,
      wordcloud = FALSE
    )
  lastShinyTree <- reactiveValues(tree = list())
  
  con <-
    dbConnect(
      RMariaDB::MariaDB(),
      host = credentials.host,
      user = credentials.user,
      password = credentials.password,
      db = credentials.db,
      port = credentials.port ,
      bigint = c("numeric")
    )
  stopifnot(is.object(con))
  withProgress(message = "Forbereder data", expr = {
    setProgress(0)
    fullCategoryData <-
      dbGetQuery(con,
                 'select prefferredLabel, _id, grp from kompetence order by prefferredLabel asc')
    setProgress(1 / 3)
    annonceCount <-
      dbGetQuery(con, 'select count(*) from annonce')[1, 1]
    output$adCountField <-
      renderText(paste0(
        format(
          annonceCount,
          big.mark = ".",
          decimal.mark = ","
        ),
        " ",
        i18n$t("job ads")
      ))
    setProgress(2 / 3)
    
    treeString <-
      dbGetQuery(con, 'select shinyTreeJSON from global where _id = 1')[1, 1]
    output$tree <- renderEmptyTree()
    updateTree(session, "tree", treeString)
    
    # generate list of all configured competence nodes
    allTreeNodes <- fromJSON(treeString, flatten = TRUE)
    allTreeNodes <-
      rapply(allTreeNodes, function(x)
        x, classes = "character", how = "unlist")
    allTreeNodes <- unname(allTreeNodes)
    qq <-
      paste0(
        'select k._id from kompetence k where k.prefferredLabel in (',
        paste(shQuote(allTreeNodes), collapse = ","),
        ')'
      )
    #print (qq)
    subData <- dbGetQuery(con, qq)
    allTreeNodes <- subData[, 1]
    
    dbDisconnect(con)
    
    setProgress(1)
  })
  
  output$downloadAnnonceliste <- downloadHandler(
    filename = 'annonce_liste.csv',
    content = function(file) {
      write.csv(csvData$annonceListe, file, row.names = FALSE)
    }
  )
  
  output$downloadAnnonceNumre <- downloadHandler(
    filename = 'annonce_numre.csv',
    content = function(file) {
      write.table(
        t(csvData$annonceNumre),
        file,
        row.names = F,
        col.names = F,
        sep = ","
      )
    }
  )
  
  output$downloadKompetenceData <- downloadHandler(
    filename = 'kompetence_data.csv',
    content = function(file) {
      con <-
        dbConnect(
          RMariaDB::MariaDB(),
          host = credentials.host,
          user = credentials.user,
          password = credentials.password,
          db = credentials.db,
          port = credentials.port ,
          bigint = c("numeric")
        )
      stopifnot(is.object(con))
      allCompetenceQuery(
        includeSearchedCompetences = TRUE,
        includeOtherCompetences = TRUE,
        returnCompetencesDescending = TRUE,
        limit = 0
      )
      kompetenceAlleListe <- dbGetQuery(con, allCompetenceQuery)
      dbDisconnect(con)
      write.csv(kompetenceAlleListe, file, row.names = FALSE)
    }
  )
  output$downloadProgressionData <- downloadHandler(
    filename = 'progression_data.csv',
    content = function(file) {
      con <-
        dbConnect(
          RMariaDB::MariaDB(),
          host = credentials.host,
          user = credentials.user,
          password = credentials.password,
          db = credentials.db,
          port = credentials.port ,
          bigint = c("numeric")
        )
      stopifnot(is.object(con))
      progressionListe <- dbGetQuery(con, csvData$progressionQuery)
      dbDisconnect(con)
      write.csv(progressionListe, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$showSeearchCriterea, ignoreInit = TRUE, {
    ## hide element
    shinyjs::toggle("divSearchPanel", anim=TRUE)
  })
  
  observeEvent(input$showResultCriterea, ignoreInit = TRUE, {
    ## hide element
    shinyjs::toggle("divResultPanel", anim=TRUE)
  })
  
  observeEvent(input$wordcloudFlatten, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$dateRange, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$minimumMatches, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$regChoice, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$dataFieldChoice, {
    updateDataFields()
  })
  
  observeEvent(input$dataFieldSearchField, ignoreInit = TRUE, {
    updateDataFields()
  })
  
  observeEvent(input$showMostOrLess, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$limitCountCompetences, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$showSearchedCompetences, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$showOtherCompetences, ignoreInit = TRUE, {
    updateCurrentTab()
  })
  
  observeEvent(input$outputPanel, ignoreInit = TRUE, {
    if (input$outputPanel == i18n$t("Competences")) {
      current$tab <- 1
      if (tabUpdates$kompetence) {
        tabUpdates$kompetence <- FALSE
        updateKompetenceDiagram()
      }
    }
    else if (input$outputPanel == i18n$t("Progression")) {
      current$tab <- 2
      if (tabUpdates$progression) {
        tabUpdates$progression <- FALSE
        updateProgressionDiagram()
      }
    }
    else if (input$outputPanel == i18n$t("Ad List")) {
      current$tab <- 3
      if (tabUpdates$annonce) {
        tabUpdates$annonce <- FALSE
        updateAnnonceList()
      }
    }
  })
  
  observeEvent(input$tree, ignoreInit = TRUE, {
    selectedList <- get_selected(input$tree)
    if (!identical(lastShinyTree$tree, selectedList)) {
      treeUpdateEffects()
      searchFieldEffects()
    }
  })
  
  
  ########################################
  #####     ADD/REMOVE OBSERVERS     #####
  ########################################
  observeEvent(input$add, {
    if (!is.null(input$availableCategories)) {
      withProgress(message = "Tilføjer kompetence(r)", expr = {
        setProgress(0)
        kompetencer$sk <-
          c(kompetencer$sk, input$availableCategories)
        setProgress(1 / 4)
        kompetencer$ak <- setdiff(kompetencer$ak, kompetencer$sk)
        setProgress(2 / 4)
        updateSelectInput(session,
                          inputId = "selectedCategories",
                          choices = kompetencer$sk)
        updateCurrentTab()
        updateSelectizeInput(session,
                             inputId = "availableCategories",
                             choices = kompetencer$ak)
        setProgress(3 / 4)
        searchFieldEffects()
        setProgress(1)
      })
    }
  })
  observeEvent(input$remove, {
    if (!is.null(input$selectedCategories)) {
      withProgress(message = "Fjerner kompetence(r)", expr = {
        setProgress(0)
        kompetencer$ak <-
          c(kompetencer$ak, input$selectedCategories)
        setProgress(1 / 5)
        kompetencer$sk <-
          setdiff(kompetencer$sk, input$selectedCategories)
        setProgress(2 / 5)
        updateSelectInput(session,
                          inputId = "selectedCategories",
                          choices = kompetencer$sk)
        updateSelectInput(session,
                          inputId = "availableCategories",
                          choices = kompetencer$ak)
        setProgress(3 / 5)
        searchFieldEffects()
        setProgress(4 / 5)
        updateCurrentTab()
        setProgress(1)
      })
    }
  })
  
  observeEvent(input$addAll, {
    withProgress(message = "Tilføjer alle på nuværende liste", expr = {
      if (input$searchField != "") {
        setProgress(0)
        kompetencer$sk <- c(kompetencer$sk, kompetencer$fk)
        setProgress(1 / 3)
        kompetencer$ak <- setdiff(kompetencer$ak, kompetencer$fk)
        kompetencer$fk <- list()
        
        updateSelectInput(session,
                          inputId = "availableCategories",
                          choices = kompetencer$ak)
      }
      else{
        setProgress(0)
        kompetencer$sk <- c(kompetencer$sk, kompetencer$ak)
        setProgress(1 / 3)
        kompetencer$ak <- list()
        
        updateSelectInput(session,
                          inputId = "availableCategories",
                          choices = kompetencer$ak)
      }
      updateSelectInput(session,
                        inputId = "selectedCategories",
                        choices = kompetencer$sk)
      setProgress(2 / 3)
      searchFieldEffects()
      updateCurrentTab()
      setProgress(1)
    })
  })
  
  observeEvent(input$removeAll, {
    withProgress(message = "Fjerner alle kompetencer", expr = {
      setProgress(0)
      kompetencer$ak <- c(kompetencer$ak, kompetencer$sk)
      setProgress(1 / 4)
      kompetencer$sk <- list()
      
      updateSelectInput(session,
                        inputId = "availableCategories",
                        choices = kompetencer$ak)
      updateSelectInput(session,
                        inputId = "selectedCategories",
                        choices = kompetencer$sk)
      setProgress(2 / 4)
      searchFieldEffects()
      setProgress(3 / 4)
      updateCurrentTab()
      setProgress(1)
    })
  })
  observeEvent(input$addDataField, {
    if (!is.null(input$availableDataFields)) {
      datafields$selectedDataFields[[input$dataFieldChoice]] <-
        c(datafields$selectedDataFields[[input$dataFieldChoice]],
          input$availableDataFields)
      updateDataFields()
      updateCurrentTab()
      
    }
  })
  
  observeEvent(input$removeDataField, {
    if (!is.null(input$selectedDataFields)) {
      datafields$selectedDataFields[[input$dataFieldChoice]] <-
        datafields$selectedDataFields[[input$dataFieldChoice]][!datafields$selectedDataFields[[input$dataFieldChoice]] %in% input$selectedDataFields]
      updateDataFields()
      updateCurrentTab()
    }
  })
  
  observeEvent(input$removeAllDataFields, {
    datafields$selectedDataFields[[input$dataFieldChoice]] <- list()
    updateDataFields()
    updateCurrentTab()
  })
  observeEvent(input$addAdId, {
    if (input$adIdSearchField != "") {
      split <-
        strsplit(input$adIdSearchField,
                 split = ",",
                 fixed = TRUE)[[1]]
      for (element in split) {
        elementValue = as.integer(element)
        if (!is.na(elementValue)) {
          datafields$adIds <- c(datafields$adIds, elementValue)
        }
      }
      updateSelectInput(session, inputId = "selectedAdIdSearchTerms", choices = datafields$adIds)
      updateCurrentTab()
    }
  })
  observeEvent(input$addTitle, {
    if (input$titleSearchField != "") {
      split <-
        strsplit(input$titleSearchField,
                 split = ",",
                 fixed = TRUE)[[1]]
      for (element in split) {
        datafields$titles <- c(datafields$titles, element)
      }
      updateSelectInput(session, inputId = "selectedTitleSearchTerms", choices = datafields$titles)
      updateCurrentTab()
    }
  })
  observeEvent(input$addText, {
    if (input$TextSearchField != "") {
      split <-
        strsplit(input$TextSearchField,
                 split = ",",
                 fixed = TRUE)[[1]]
      for (element in split) {
        datafields$texts <- c(datafields$texts, element)
      }
      updateSelectInput(session, inputId = "selectedTextSearchTerms", choices = datafields$texts)
      updateCurrentTab()
    }
  })
  
  observeEvent(input$removeAdId, {
    if (!is.null(input$selectedAdIdSearchTerms)) {
      datafields$adIds <-
        datafields$adIds[!datafields$adIds %in% input$selectedAdIdSearchTerms]
      updateSelectInput(session, inputId = "selectedAdIdSearchTerms", choices = datafields$adIds)
      updateCurrentTab()
    }
  })
  observeEvent(input$removeAllAdIds, {
    datafields$adIds <-
      datafields$adIds[!datafields$adIds %in% datafields$adIds]
    updateSelectInput(session, inputId = "selectedAdIdSearchTerms", choices = datafields$adIds)
    updateCurrentTab()
  })
  
  observeEvent(input$removeTitle, {
    if (!is.null(input$selectedTitleSearchTerms)) {
      datafields$titles <-
        datafields$titles[!datafields$titles %in% input$selectedTitleSearchTerms]
      updateSelectInput(session, inputId = "selectedTitleSearchTerms", choices = datafields$titles)
      updateCurrentTab()
    }
  })
  observeEvent(input$removeAllTitles, {
    datafields$titles <-
      datafields$titles[!datafields$titles %in% datafields$titles]
    updateSelectInput(session, inputId = "selectedTitleSearchTerms", choices = datafields$titles)
    updateCurrentTab()
    
  })
  
  observeEvent(input$removeText, {
    if (!is.null(input$selectedTextSearchTerms)) {
      datafields$texts <-
        datafields$texts[!datafields$texts %in% input$selectedTextSearchTerms]
      updateSelectInput(session, inputId = "selectedTextSearchTerms", choices = datafields$texts)
      updateCurrentTab()
    }
  })
  observeEvent(input$removeAllTexts, {
    datafields$texts <-
      datafields$texts[!datafields$texts %in% datafields$texts]
    updateSelectInput(session, inputId = "selectedTextSearchTerms", choices = datafields$texts)
    updateCurrentTab()
    
  })
  
  #######################################
  #######################################
  observeEvent(input$searchField, ignoreInit = TRUE, {
    searchFieldEffects()
  })
  
  observeEvent(input$progressionDateFormat, ignoreInit = TRUE, {
    updateProgressionDiagram()
  })
  
  observeEvent(input$annonceList, {
    withProgress(message = "Finder annoncetekst", expr = {
      setProgress(0)
      con <-
        dbConnect(
          RMariaDB::MariaDB(),
          port = credentials.port,
          host = credentials.host,
          user = credentials.user,
          password = credentials.password,
          db = credentials.db,
          bigint = c("numeric")
        )
      stopifnot(is.object(con))
      
      id <- unlist(strsplit(input$annonceList, " "))[1]
      
      annonceDataFields <-
        dbGetQuery(
          con,
          paste0(
            'select cvr, "CVR" as name from annonce where _id = ',
            id,
            ' union select dataValue, name from annonce_datafield join datafield where annonce_id = ',
            id,
            ' and datafield._id = dataField_id'
          )
        )
      annonceText <-
        dbGetQuery(
          con,
          paste0(
            'select convert(searchable_body using utf8) as searchable_body from annonce where _id = ',
            id
          )
        )
      annonceKompetencer <-
        dbGetQuery(
          con,
          paste0(
            'select distinct k_prefferredLabel from annonce_kompetence where annonce_id = ',
            id
          )
        )
      
      # Cvr info
      if (nrow(annonceDataFields) == 0 || is.na(annonceDataFields)) {
        annonceDataFields <-
          data.frame("Ingen tilgængelige cvr oplysninger")
        names(annonceDataFields) <- c("") #remove column headers
        output$annonceDataFields <- renderTable(annonceDataFields)
      } else {
        rownames(annonceDataFields) <- annonceDataFields[, 2]
        annonceDataFields[2] <- NULL
        names(annonceDataFields) <- c("") #remove column headers
        output$annonceDataFields <-
          renderTable(annonceDataFields, include.rownames = TRUE)
      }
      # Kompetence info
      names(annonceKompetencer) <- c("") #remove column headers
      output$annonceKompetencer <- renderTable(annonceKompetencer)
      
      #here
      
      setProgress(1 / 2)
      dbDisconnect(con)
      
      output$annonceText <- renderText({
        annonceText[1, 1]
      })
      
      setProgress(1)
    })
  })
  
  ############################################################     FUNCTIONS     ############################################################
  
  updateDataFields <- function() {
    # Update UI to show current data fields selected values
    if (is.null(datafields$selectedDataFields[[input$dataFieldChoice]])) {
      datafields$selectedDataFields[[input$dataFieldChoice]] <- list()
    }
    updateSelectInput(session,
                      inputId = "selectedDataFields",
                      choices = datafields$selectedDataFields[[input$dataFieldChoice]])
    
    # Update UI to show current data fields available values
    con <-
      dbConnect(
        RMariaDB::MariaDB(),
        host = credentials.host,
        user = credentials.user,
        password = credentials.password,
        port = credentials.port,
        db = credentials.db,
        bigint = c("numeric")
      )
    stopifnot(is.object(con))
    query <-
      paste0(
        "select distinct dataValue from annonce_datafield where (select _id from datafield where name = '",
        input$dataFieldChoice,
        "') = datafield_id and dataValue is not null"
      )
    if (input$dataFieldSearchField != "") {
      query <-
        paste0(query,
               "  and dataValue like '%",
               input$dataFieldSearchField,
               "%'")
    }
    count <- paste0("select count(*) from (", query, ") qtable")
    if (dbGetQuery(con, count) > 1000) {
      updateSelectInput(session,
                        inputId = "availableDataFields",
                        choices = list())
      showNotification(i18n$t("Too many results, details search filter!"), type = "error")
    } else {
      datafields$availableDataFields <- as.matrix(dbGetQuery(con, query))
      updateSelectInput(session,
                        inputId = "availableDataFields",
                        choices = datafields$availableDataFields[!datafields$availableDataFields %in% datafields$selectedDataFields[[input$dataFieldChoice]]])
    }
    
    dbDisconnect(con)
  }
  
  updateCurrentTab <- function() {
    if (current$tab == 1) {
      updateKompetenceDiagram()
      tabUpdates$progression <- TRUE
      tabUpdates$annonce <- TRUE
    }
    else if (current$tab == 2) {
      updateProgressionDiagram()
      tabUpdates$kompetence <- TRUE
      tabUpdates$annonce <- TRUE
    }
    else if (current$tab == 3) {
      updateAnnonceList()
      tabUpdates$kompetence <- TRUE
      tabUpdates$progression <- TRUE
    }
  }
  
  searchFieldEffects <- function() {
    if (input$searchField != "") {
      withProgress(message = "Søger efter kompetencer", expr = {
        setProgress(0)
        foundKompetencer <- list()
        for (kompetence in kompetencer$ak) {
          if (grepl(toupper(input$searchField),
                    toupper(kompetence),
                    fixed = TRUE)) {
            foundKompetencer <- c(foundKompetencer, kompetence)
          }
        }
        setProgress(1 / 2)
        foundKompetencer <-
          setdiff(foundKompetencer, kompetencer$sk)
        
        updateSelectInput(session,
                          inputId = "availableCategories",
                          choices = foundKompetencer)
        kompetencer$fk = foundKompetencer
        setProgress(1)
      })
    }
    else{
      updateSelectInput(session,
                        inputId = "availableCategories",
                        choices = kompetencer$ak)
    }
  }
  
  loadChildNodes <- function(parentNodes) {
    
  }
  
  treeUpdateEffects <- function() {
    selectedList <- get_selected(input$tree)
    unselectedList <- get_selected(input$tree)
    if (length(selectedList) > 0) {
      withProgress(message = "Opdaterer tilgængelig list", expr = {
        setProgress(0)
        
        searchList <- list()
        
        for (i in 1:length(selectedList)) {
          searchList <- c(searchList, selectedList[[i]][1])
        }
        setProgress(1 / 6)
        
        con <-
          dbConnect(
            RMariaDB::MariaDB(),
            host = credentials.host,
            user = credentials.user,
            password = credentials.password,
            port = credentials.port,
            db = credentials.db,
            bigint = c("numeric")
          )
        stopifnot(is.object(con))
        
        setProgress(2 / 6)
        
        
        setProgress(3 / 6)
        qq <-
          paste0(
            'select k._id from kompetence k where k.prefferredLabel in (',
            paste(shQuote(searchList), collapse = ","),
            ')'
          )
        #print (qq)
        subData <- dbGetQuery(con, qq)
        searchList <- subData[, 1]
        finalList <- searchList
        oldresultcount <- 0
        resultcount <- length(finalList)
        done <- FALSE
        while (oldresultcount < resultcount) {
          superkompetencelist <-
            paste(if (oldresultcount == 0)
              searchList
              else
                setdiff(searchList, allTreeNodes),
              collapse = ", ")
          qq <-
            paste0(
              'select k_sub._id from kompetence k_sub join kompetence_kategorisering kk ON k_sub.conceptUri = kk.subkompetence and kk.superkompetence in (select k_super.conceptUri from kompetence k_super where k_super._id in (',
              superkompetencelist,
              '))'
            )
          #print (qq)
          subData <- dbGetQuery(con, qq)
          searchList <- subData[, 1]
          finalList <- unique(c(finalList, searchList))
          oldresultcount <- resultcount
          resultcount <- length(finalList)
          #print (resultcount)
        }
        
        
        setProgress(4 / 6)
        if (length(finalList) > 0) {
          qq <-
            paste0(
              'select k.prefferredLabel from kompetence k where k._id in (',
              paste(finalList, collapse = ","),
              ')'
            )
          #print (qq)
          subData <- dbGetQuery(con, qq)
          kompetencer$ak <- str_sort(subData[, 1])
        }
        else
        {
          kompetencer$ak <- c()
        }
        
        dbDisconnect(con)
        setProgress(1)
      })
    }
    else{
      kompetencer$ak <- c()
    }
    
    updateSelectInput(session,
                      inputId = "availableCategories",
                      choices = list())
    lastShinyTree$tree <- selectedList
  }
  
  buildCompetenceQuery <- function (includeSearchedCompetences = TRUE,
                                    includeOtherCompetences  = TRUE,
                                    returnCompetencesDescending = TRUE,
                                    limit = 0)
  {
    if ((includeSearchedCompetences != includeOtherCompetences) &&
        length(kompetencer$sk) != 0) {
      matchIndexes <- list()
      categoryMatrix <- as.matrix(fullCategoryData)
      for (kompetence in kompetencer$sk) {
        matchIndexes <-
          c(matchIndexes, which(categoryMatrix[, 1] == kompetence))
      }
      kompetenceIds <- list()
      for (index in matchIndexes) {
        kompetenceIds <- c(kompetenceIds, categoryMatrix[index, 2])
      }
      
      q2 <-
        paste0(' WHERE k._id',
               ifelse(includeSearchedCompetences, '', ' NOT'),
               ' IN (')
      for (i in 1:length(kompetenceIds)) {
        if (i < length(kompetenceIds)) {
          q2 <- paste0(q2, (paste0(kompetenceIds[i], ', ')))
        }
        else{
          q2 <- paste0(q2, kompetenceIds[i], ') ')
        }
      }
    } else {
      q2 <- ''
      
    }
    
    csvDataQuery <- paste0(
      "SELECT k.prefferredLabel word, count(sak.annonce_id) as freq FROM kompetence k LEFT JOIN (SELECT ak.annonce_id annonce_id, ak.kompetence_id kompetence_id FROM annonce_kompetence ak JOIN ",
      getSearchAdResultTableName(),
      " c ON ak.annonce_id=c.annonce_id) sak ON sak.kompetence_id=k._id ",
      q2,
      " GROUP BY k._id ORDER BY freq ",
      ifelse(isTRUE(returnCompetencesDescending), ' DESC ', ' ASC '),
      ifelse(
        is.numeric(limit) &
          length(limit) > 0 & limit > 0,
        paste0(' LIMIT ', limit, ' '),
        ' '
      )
    )
    
    return(csvDataQuery)
  }
  
  updateKompetenceDiagram <- function() {
    #if(length(kompetencer$sk) != 0){
    withProgress(message = "Opdaterer Diagram", expr = {
      setProgress(0)
      con <-
        dbConnect(
          RMariaDB::MariaDB(),
          host = credentials.host,
          user = credentials.user,
          password = credentials.password,
          port = credentials.port,
          db = credentials.db,
          bigint = c("numeric")
        )
      stopifnot(is.object(con))
      
      setProgress(1 / 5)
      csvDataQuery <- buildCompetenceQuery(
        input$showSearchedCompetences,
        input$showOtherCompetences,
        input$showMostOrLess == "Most",
        input$limitCountCompetences
      )
      #print(csvDataQuery)
      csvData$kompetenceListe <- dbGetQuery(con, csvDataQuery)
      
      setProgress(2 / 5)
      dbDisconnect(con)
      
      setProgress(3 / 5)
      if (dim(csvData$kompetenceListe)[1] != 0) {
        # If i order the table in the correct order in the SQL query the 'Limit 30' option cuts off the biggest instead of the smallest.
        # Which is why the table must be ordered after the query.
        csvData$kompetenceListe <-
          csvData$kompetenceListe[order(csvData$kompetenceListe$freq,
                                        decreasing = (input$showMostOrLess == "Less")), ]
        
        setProgress(4 / 5)
        output$kompetenceDiagram <-
          renderPlot(height = (200 + 18 * input$limitCountCompetences), {
            par(mar = c(5, 18, 4, 2) + 0.1)
            ylim <- c(0, 1.2 * max(csvData$kompetenceListe$freq))
            xx <-
              barplot(
                csvData$kompetenceListe$freq,
                xlim = ylim,
                main = paste0(
                  ifelse(
                    input$showSearchedCompetences,
                    ifelse(input$showOtherCompetences, 'Alle', 'Søgte'),
                    'Andre'
                  ),
                  " kompetencer i jobopslag\n",
                  input$regChoice,
                  " fra ",
                  input$dateRange[1],
                  " til ",
                  input$dateRange[2],
                  "."
                ),
                names.arg = csvData$kompetenceListe$word,
                las = 2,
                horiz = TRUE
              )
            text(
              y = xx,
              x = csvData$kompetenceListe$freq,
              label = csvData$kompetenceListe$freq,
              pos = 4,
              cex = 1.2,
              col = "blue"
            )
          })
        output$kompetenceErrorField <- renderText("")
      }
      else{
        output$kompetenceErrorField <- renderText("Ingen annoncer fundet.")
        output$kompetenceDiagram <- NULL
      }
      
      setProgress(4 / 5)
      
      maxValue = max(csvData$kompetenceListe$freq)
      minValue = min(csvData$kompetenceListe$freq)
      flatten = input$wordcloudFlatten / 100
      wcData <-
        mutate (
          csvData$kompetenceListe,
          word = word,
          freq = ifelse(round(
            minValue - (minValue * flatten) + (freq - minValue) * (1 - flatten)
          ) <= 0, 1, round(
            minValue - (minValue * flatten) + (freq - minValue) * (1 - flatten)
          )),
          digits = 0
        )
      #wcData <- csvData$kompetenceListe
      output$wordcloud  = renderWordcloud2(
        wordcloud2(
          data = wcData,
          color = input$wordcloudColor,
          size = input$wordcloudSize / 100,
          shape = input$wordcloudShape
        )
      )
      
      setProgress(5 / 5)
    })
  }
  
  buildSearchParameterJSON <- function() {
    # Build parameter:
    # {
    #   "period": {
    #     "fromTime": "2019-09-30",
    #     "toTime": "2019-11-30"
    #   },
    #   "idList": [
    #       11
    #   ],
    #   "region": {
    #     "nameList": [
    #       "region-midtjylland"
    #       ],
    #     "idList": [
    #       11
    #       ]
    #   },
    #   "textcontent": {
    #     "titleRegexp": "(?i)Projektleder",
    #     "bodyRegexp": "(?i)Projektleder"
    #   },
    #   "kompetence": {
    #     "minimumMatches": 1,
    #     "idList": [
    #       158861
    #       ],
    #     "nameList": [
    #       "Virksomhed"
    #       ]
    #   },
    #   "metadata": {
    #     "kommune": {
    #       "valueList": [
    #         "Aarhus",
    #         "Aalborg"
    #         ]
    #     },
    #     "branchetekst": {
    #       "valueList": [
    #         "Computerprogrammering",
    #         "Vandforsyning"
    #         ]
    #     }
    #   }
    # }
    
    # Build period criteria
    periodParam <- '"period":'
    periodType <-
      setClass("Period", slots = c(fromTime = "Date", toTime = "Date"))
    periodObject = periodType(fromTime = input$dateRange[1],
                              toTime = input$dateRange[2])
    periodParam <-
      paste0(periodParam,
             toJSON(
               unclass(periodObject),
               force = TRUE,
               auto_unbox = TRUE
             ))
    
    # Build ad id criteria
    adIdParam <- ""
    if (length(datafields$adIds) > 0) {
      adIdParam <-
        paste0(', "idList":[',
               paste0(datafields$adIds, collapse = ','),
               "]")
    }
    
    # Build region criteria
    regionParam <- ""
    if (input$regChoice != "Alle regioner") {
      regionParam <-
        paste0(', "region": {"nameList":["', input$regChoice, '"]}')
    }
    
    # Build region criteria
    regionParam <- ""
    if (input$regChoice != "Alle regioner") {
      regionParam <-
        paste0(', "region": {"nameList":["', input$regChoice, '"]}')
    }
    
    # Build textcontent criteria
    textcontentParam <- ""
    titleSearch <- (length(datafields$titles) > 0)
    textSearch <- (length(datafields$texts) > 0)
    if (titleSearch || textSearch) {
      if (titleSearch) {
        titleRegexpVal <- paste(datafields$titles, collapse = '|')
        titleRegexpVal <- paste0("(?i)(", titleRegexpVal, ")")
      }
      
      if (textSearch) {
        textRegexpVal <- paste(datafields$texts, collapse = '|')
        textRegexpVal <- paste0("(?i)(", textRegexpVal, ")")
      }
      
      if (titleSearch && textSearch) {
        searchcontentType <-
          setClass("Searchcontent",
                   slots = c(titleRegexp = "character", bodyRegexp = "character"))
        searchcontentObject <-
          searchcontentType(bodyRegexp = textRegexpVal, titleRegexp = titleRegexpVal)
      } else if (titleSearch) {
        searchcontentType <-
          setClass("Searchcontent", slots = c(titleRegexp = "character"))
        searchcontentObject <-
          searchcontentType(titleRegexp = titleRegexpVal)
      } else {
        searchcontentType <-
          setClass("Searchcontent", slots = c(bodyRegexp = "character"))
        searchcontentObject <-
          searchcontentType(bodyRegexp = textRegexpVal)
      }
      
      textcontentParam <-
        paste0(
          ', "textcontent":',
          textcontentParam,
          toJSON(
            unclass(searchcontentObject),
            force = TRUE,
            auto_unbox = TRUE
          )
        )
    }
    
    # Build kompetence criteria
    kompetenceParam <- ""
    if (length(kompetencer$sk) > 0) {
      matchIndexes <- list()
      categoryMatrix <- as.matrix(fullCategoryData)
      for (kompetence in kompetencer$sk) {
        matchIndexes <-
          c(matchIndexes, which(categoryMatrix[, 1] == kompetence))
      }
      kompetenceIds <- list()
      for (index in matchIndexes) {
        kompetenceIds <- c(kompetenceIds, categoryMatrix[index, 2])
      }
      kompetenceParam <-
        paste0(
          ', "kompetence": {"minimumMatches":',
          input$minimumMatches,
          ',"idList": [',
          paste(kompetenceIds, collapse = ','),
          ']}'
        )
    }
    
    # Build optional metadata criteria
    fieldconditions <- list()
    for (datafieldname in names(datafields$selectedDataFields)) {
      if (length(datafields$selectedDataFields[[datafieldname]]) > 0) {
        fieldconditions <-
          c(
            fieldconditions,
            paste0(
              '"',
              datafieldname,
              '": {"valueList": ["' ,
              paste(datafields$selectedDataFields[[datafieldname]], collapse = '","'),
              '"]}'
            )
          )
      }
    }
    if (length(fieldconditions) > 0) {
      metadataParam <-
        paste0(', "metadata":{',
               paste(fieldconditions, collapse = ", "),
               "}")
    } else {
      metadataParam <- ""
    }
    
    JSONvalue = paste0(
      "{",
      periodParam,
      adIdParam,
      regionParam,
      textcontentParam,
      kompetenceParam,
      metadataParam,
      "}"
    )
    #print(JSONvalue)
    return(JSONvalue)
  }
  
  getSearchAdResultTableName <- function()  {
    searchParam <- buildSearchParameterJSON()
    dbQuery <-
      paste0("CALL `prepareCacheTable`('", searchParam, "')")
    con <-
      dbConnect(
        RMariaDB::MariaDB(),
        host = credentials.host,
        user = credentials.user,
        password = credentials.password,
        port = credentials.port,
        db = credentials.db,
        bigint = c("numeric")
      )
    stopifnot(is.object(con))
    result <- dbGetQuery(con, dbQuery)
    tableName <- result[["cacheTableName"]]
    #print(paste0("tableName: >", tableName, "<"))
    #print(paste0("lastSearchTable: >", session$userData$lastSearchTable, "<"))
    if (is.null(tableName) || paste0(tableName) == '') {
      tableName <- session$userData$lastSearchTable
    } else {
      session$userData$lastSearchTable <- tableName
    }
    result <-
      dbGetQuery(con,
                 paste0("SELECT count(*) numberOfFoundAds FROM ", tableName))
    numberOfFoundAds <- result[["numberOfFoundAds"]]
    output$resultCountField <-
      renderText(paste0(" (", numberOfFoundAds, ")"))
    
    dbDisconnect(con)
    
    return(tableName)
  }
  
  updateProgressionDiagram <- function() {
    #if(length(kompetencer$sk) != 0){
    withProgress(message = "Opdaterer diagram", expr = {
      if (input$progressionDateFormat == "Uge") {
        periodQuery <- "DATE_FORMAT(timeStamp,'%Y-%U')"
      }
      else if (input$progressionDateFormat == "Måned") {
        periodQuery <- "DATE_FORMAT(timeStamp,'%Y-%m')"
      }
      else if (input$progressionDateFormat == "År") {
        periodQuery <- "DATE_FORMAT(timeStamp,'%Y')"
      }
      else {
        return()
      }
      
      setProgress(0)
      cacheTableName <- getSearchAdResultTableName()
      
      setProgress(1 / 7)
      con <-
        dbConnect(
          RMariaDB::MariaDB(),
          host = credentials.host,
          user = credentials.user,
          password = credentials.password,
          port = credentials.port,
          db = credentials.db,
          bigint = c("numeric")
        )
      stopifnot(is.object(con))
      
      csvData$progressionQuery <-
        paste0(
          "SELECT ",
          periodQuery,
          " period, count(*) amount FROM annonce a JOIN ",
          cacheTableName,
          " c ON a._id=c.annonce_id group by period"
        )
      #print(qq)
      progressionData <- data.frame()
      formattedData <-
        rbind(progressionData,
              dbGetQuery(con, csvData$progressionQuery))
      
      dbDisconnect(con)
      
      setProgress(2 / 7)
      
      setProgress(3 / 7)
      
      setProgress(4 / 7)
      lastDate <- ""
      n <- nrow(formattedData)
      
      setProgress(5 / 7)
      if (n > 0) {
        ##### Check added to prevent crash when there's no job advertisements to be found
        
        n <- length(formattedData$amount)
        
        fda <- c(formattedData$amount)
        fdp <- c(1:n)
        
        result <- lm(formula = fda ~ fdp)
        a <- result$coefficients[1]
        b <- result$coefficients[2]
        v <- round(100 * (b * (n + 1)) / abs(a), digits = 0)
        
        setProgress(6 / 7)
        
        output$progressionDiagram <- renderPlot({
          ylim <- c(0, 1.1 * max(formattedData$amount))
          xx <- barplot(
            formattedData$amount,
            ylim = ylim,
            main = paste0(v, "% vækst i perioden"),
            ylab = "Antal annoncer",
            names.arg = formattedData$period
          )
          
          text(
            x = xx,
            y = formattedData$amount,
            label = formattedData$amount,
            pos = 3,
            cex = 1.2,
            col = "blue"
          )
          
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
        output$progressionErrorField <-
          renderText("Ingen annoncer fundet.")
      }
      setProgress(1)
    })
    #}
    #else{
    #  output$progressionErrorField <- renderText("")
    #  output$progressionDiagram <- NULL
    #}
  }
  
  updateAnnonceList <- function() {
    #if(length(kompetencer$sk) != 0){
    withProgress(message = "Opdaterer annonceliste", expr = {
      setProgress(0)
      con <-
        dbConnect(
          RMariaDB::MariaDB(),
          port = credentials.port,
          host = credentials.host,
          user = credentials.user,
          password = credentials.password,
          db = credentials.db,
          bigint = c("numeric")
        )
      stopifnot(is.object(con))
      qq <-
        paste0(
          "SELECT a.annonce_id FROM ",
          getSearchAdResultTableName(),
          " a ORDER BY a.annonce_id DESC"
        )
      csvData$annonceNumre <- dbGetQuery(con, qq)
      qq <-
        paste0(
          "SELECT a._id, a.title FROM annonce a JOIN ",
          getSearchAdResultTableName(),
          " c ON a._id=c.annonce_id ORDER BY a._id DESC"
        )
      csvData$annonceListe <- dbGetQuery(con, qq)
      dbDisconnect(con)
      
      setProgress(2 / 3)
      annonceListContents <- list()
      if (dim(csvData$annonceListe)[1] != 0) {
        for (i in 1:nrow(csvData$annonceListe)) {
          annonceListContents <-
            c(
              annonceListContents,
              paste0(csvData$annonceListe[i, 1], " || ", csvData$annonceListe[i, 2])
            )
        }
        
        updateSelectInput(session,
                          inputId = "annonceList",
                          choices = annonceListContents)
        output$annonceErrorField <- renderText("")
      }
      else{
        output$annonceErrorField <- renderText("Ingen annoncer fundet.")
        updateSelectInput(session,
                          inputId = "annonceList",
                          choices = list())
      }
      setProgress(1)
    })
    
    #}
    #else{
    #  output$annonceErrorField <- renderText("")
    #  updateSelectInput(session,
    #                    inputId = "annonceList",
    #                    choices = list()
    #  )
    #}
  }
  
  ############################################################################################################################################
  
}

shinyApp(ui = ui, server = server)
