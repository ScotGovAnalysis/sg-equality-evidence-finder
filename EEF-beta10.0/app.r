
# Set up ------------------------------------------------------------------

library(plotly)
library(markdown)
library(lubridate)
library(readxl)
library(openxlsx)
library(dplyr)
library(shiny)
library(shinyjs)
library(tidyr)
library(xts)
library(dygraphs)
library(zoo)
library(googleVis)
library(rmarkdown)
library(knitr)
library(xfun)
library(scales)
library(RColorBrewer)
library(googlesheets)
library(purrr)
library(SPARQL)
library(shinycssloaders)

options(spinner.color="#0080db",spinner.type=1)

#load helper scripts
source("EEF scripts/dygraph-extra-shiny.R",local=TRUE)
source("EEF scripts/helper_funcs.r",local=TRUE) #not required for R 3.6
source("EEF scripts/set_variables.R",local=TRUE)
source("EEF scripts/data_processing.R",local=TRUE)
source("EEF scripts/funcs_modules.R",local=TRUE)
source("EEF scripts/funcs_graphs.R",local=TRUE)

#load data (locally)
load("data/index.rData",environment())
load("data/graph_data.rData",environment())
load("data/sources.rData",envir=environment())
load("data/index.rData",envir=environment())
load("data/publicationLinks.rData",envir=environment())
load("data/dataLinks.rData",envir=environment())
load("data/externalLinks.rData",envir=environment())
load("data/additionalLinks.rData",envir=environment())
load("data/contacts.rData",envir=environment())


#load ui/server scripts
source("EEF scripts/ui_modules.r",local=TRUE)
source("EEF scripts/ui_graphs.r",local=TRUE)
source("EEF scripts/server_modules.r",local=TRUE)
source("EEF scripts/server_graphs.r",local=TRUE)


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$meta(charset="utf-8"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1, shrink-to-fit=no"),
    HTML('<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css" integrity="sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB" crossorigin="anonymous">'),
    HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Lato">'),
    HTML('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">'),
    HTML('<link href="https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900,400italic" rel="stylesheet" type="text/css">'),
    HTML('<link rel="stylesheet" href="https://www.w3schools.com/w3css/4/w3.css">'),
    shinyjs::useShinyjs(),
    includeCSS("EEF scripts/tdstyles.css"),
    includeCSS("EEF scripts/dygraph.css"),
    includeCSS("EEF scripts/EEFextra.css"),
    tags$script(src = "javascript/dygraph-extra-modified-final.js"),
    tags$script(src="javascript/EEF.js"),
    #tags$script(src = "javascript/jquery-ui-1-11-4.min.js"),
    tags$script('$(function(){$(".jui-tip").tooltip();});'),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }")
  ),
  
  ##UI components
  shinyjs::hidden(eefHelp()),
  eefHeader("equality"),
  eefGrid("equality"),
  br(),
  tags$div(role = "main",
           tags$article(shinyjs::hidden(accessibilityStatement(id = "accessibility"))),
           tags$article(shinyjs::hidden(eefContent("equality")),
                        br()),
           tags$article(eefHeaderImage(NS("equality","npf"),"National Performance Framework","EEF/Npfbackground.jpg",active=2,class="eef eef-main eef-summ"),
                        br(),
                        npfText("eef eef-main eef-summ"),
                        eefSection(NS("equality","NPF"),"National Performance Framework Data Explorer",
                                   colour="eef-section-links",
                                   class="eef-section eef eef-main eef-summ",
                                   tabs=equalityButtons9(NS(NS("equality","NPF"),"section"),
                                                         title="Data Explorer",
                                                         active=c(TRUE,equalityCharacteristics%in%distinct(NPFdataIndex,Characteristic)$Characteristic),
                                                         buttonClass=c(paste0("eef-",allPolicyAreasID,collapse=" "),sapply(equalityCharacteristics,FUN=function(x) paste(character(0),policyCSS(filter(NPFdataIndex,Characteristic==x)$policy_area),collapse=" ")))),
                                   content=uiOutputLoading(NS(NS("equality","NPF"),"panel")))),
           tags$article(eefHeaderImage(NS("equality","publications"),"Publications & Data","EEF/parallax-publications2.jpg",active=2,
                                       class=paste(c("eef","eef-main",policyCSS(unique(EEFpublishedIndex$policy_area))),collapse=" ")),
                        br(),
                        eefSection(NS("equality","publications"),"Scottish Government Publications and Outputs",
                                   colour="eef-section-links",
                                   class=paste(c("eef-section","eef-main",policyCSS(unique(EEFpublishedIndex$policy_area))),collapse=" "),
                                   tabs=equalityButtons9(NS(NS("equality","publications"),"section"),
                                                         title="Publications and Outputs",
                                                         active=c(TRUE,equalityCharacteristicsID%in%filter(EEFpublishedIndex,policy_area=="Summary")$Characteristic),
                                                         buttonClass=c(paste0("eef-",allPolicyAreasID,collapse=" "),sapply(equalityCharacteristicsID,FUN=function(x) paste(character(0),policyCSS(filter(EEFpublishedIndex,Characteristic==x)$policy_area),collapse=" ")))),#uiOutput("equality-publications-tabs"),
                                   content=uiOutputLoading(NS(NS("equality","publications"),"panel"))),
                        eefSection(NS("equality","external"),"External Publications and Research Organisations",
                                   colour="eef-section-links",
                                   class=paste(c("eef-section","eef-main",policyCSS(unique(EEFexternalIndex$policy_area))),collapse=" "),
                                   tabs=equalityButtons9(NS(NS("equality","external"),"section"),
                                                         title="External Links",
                                                         active=c(TRUE,equalityCharacteristicsID%in%filter(EEFexternalIndex,policy_area=="Summary")$Characteristic),
                                                         buttonClass=c(paste0("eef-",allPolicyAreasID,collapse=" "),sapply(equalityCharacteristicsID,FUN=function(x) paste(character(0),policyCSS(filter(EEFexternalIndex,Characteristic==x)$policy_area),collapse=" ")))),#uiOutput("equality-publications-tabs"),
                                   content=uiOutputLoading(NS(NS("equality","external"),"panel"))),
                        eefSection(NS("equality","data"),"Data",
                                   colour="eef-section-links",
                                   class=paste(c("eef-section","eef-main",policyCSS(unique(EEFdataLinksIndex$policy_area))),collapse=" "),
                                   tabs=equalityButtons9(NS(NS("equality","data"),"section"),
                                                         title="Data",
                                                         active=c(TRUE,equalityCharacteristicsID%in%filter(EEFdataLinksIndex,policy_area=="Summary")$Characteristic),
                                                         buttonClass=c(paste0("eef-",allPolicyAreasID,collapse=" "),sapply(equalityCharacteristicsID,FUN=function(x) paste(character(0),policyCSS(filter(EEFdataLinksIndex,Characteristic==x)$policy_area),collapse=" ")))),#uiOutput("equality-publications-tabs"),
                                   content=uiOutputLoading(NS(NS("equality","data"),"panel"))),
                        eefSection(NS("equality","collection"),"Data Collection Guidance",
                                   colour="eef-section-links",
                                   class=paste(c("eef-section","eef-main",policyCSS(allPolicyAreasID)),collapse=" "),
                                   tabs=equalityButtons9(NS(NS("equality","collection"),"section"),
                                                         title="Data Collection Guidance",
                                                         active=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE),
                                                         buttonClass=sapply(c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE),
                                                                            function(x) if(x) paste0("eef-",allPolicyAreasID,collapse=" ") else "" )),
                                   #buttonClass=c(paste0("eef-",allPolicyAreasID,collapse=" "),sapply(equalityCharacteristicsID,FUN=function(x) paste(character(0),policyCSS(filter(EEFexternalIndex,Characteristic==x)$policy_area),collapse=" ")))),#uiOutput("equality-publications-tabs"),
                                   content=uiOutputLoading(NS(NS("equality","collection"),"panel"))),
                        #  eefSection(NS("equality","glossary"),"Glossary",
                        #             colour="eef-section-links",
                        #             class=paste(c("eef-section","eef-main",policyCSS(allPolicyAreasID)),collapse=" "),
                        #             tabs=equalityButtons9(NS(NS("equality","glossary"),"section"),
                        #                                   title="Glossary",
                        #                                   active=c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
                        #                                   buttonClass=sapply(c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
                        #                                                      function(x) if(x) paste0("eef-",allPolicyAreasID,collapse=" ") else "" )),
                        #             #buttonClass=c(paste0("eef-",allPolicyAreasID,collapse=" "),sapply(equalityCharacteristicsID,FUN=function(x) paste(character(0),policyCSS(filter(EEFexternalIndex,Characteristic==x)$policy_area),collapse=" ")))),#uiOutput("equality-publications-tabs"),
                        #             content=uiOutputLoading(NS(NS("equality","glossary"),"panel"))),
                        br()),
           tags$article(eefHeaderImage(NS("equality","contact"),"Contact","EEF/parallax-contact.jpg",active=3),
                        br(),
                        eefContactList(allPolicyAreasID[allPolicyAreasID!="summ"]),
                        br(),br())
  ),
  div(id="eef-feedback",role = "contentinfo", 
      div(class="eef-text",p(style="margin:0px 50px 0px 20px;","This is the beta version of the new Equality Evidence Finder. Please complete a ",a(href="https://response.questback.com/scottishgovernment/equality-evidence-finder",target="blank_",span(style="white-space: nowrap;","short user survey"))," and tell us what you think of the new site."))),
  eefFooter("equality"),
  #HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
  HTML("<script src='https://cc.cdn.civiccomputing.com/9/cookieControl-9.x.min.js'></script>"),
  HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-128676670-1'></script>"),#main account
  #HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-128846881-1'></script>"),#test accout
  tags$script(src="javascript/eef-cookies.js")
  #tags$script("CookieControl.load( config );")
  
)


# Server ------------------------------------------------------------------

graphOptions[["incPov-11"]]$updateNPF <- TRUE #temp fix - is this still needed?
server <- function(input,output,session) {
  
  #count number of connections to the server (i.e. number of users visiting site) and store in a google sheet - This is done in the server and doesn't involve personal data so should be compliant with all privacy regs
  observeEvent(session$clientData$url_pathname, {
    if(file.exists("gs_token.rds") & session$clientData$url_pathname == "sg-equality-evidence-finder") {
      gs_token <- readRDS("gs_token.rds")
      session$onSessionEnded(function() {
        gs_auth(gs_token)
        gs <- gs_title("Equality Evidence Finder",verbose=F)
        row <- month(Sys.Date())-3 + (year(Sys.Date())-2019)*12
        gs_edit_cells(gs,input=data.frame(B=gs_read(gs)$pageviews[row] + 1,C=Sys.time()),anchor=paste0("B",row+1),col_names=FALSE) #experimental code for adding last updated time stame
      })
    }
    
  })
  
  #topicLoaded keeps track of what graphs have been initialized. To reduce the amount of code the server runs at start up, the code for charts is only run if/when it's needed
  topicLoaded <- reactiveValues(npf = TRUE) #only npf data is initialiaised when the app loads as the data is required for some of the other panels
  
  #loadData contains all the data for graphs. Wrapped in a reactive expression to allow data to be update "live" from statistics.gov.scot. 
  #The reactive expression also means the data will only be loaded when it's needed (i.e. when a user clicks on the relevant section)
  loadData <- list()
  loadData[["npf"]] <- reactive({
    readData <- readNPFdata()
    if(is.null(readData)) graph[["npf"]] else readData
  })
  callModule(npfServer3, "npf", loadData[["npf"]],panel[["npf"]],graphOptions[["npf"]],filterChar=reactive({input[["equality-NPF-sectionEqualityID"]]}))
  
  chartIDs <- setdiff(names(graph),"npf")
  chartList <- map(seq_along(chartIDs), ~TRUE)
  names(chartList) <- chartIDs
  walk(chartIDs, ~{
    observeEvent(topicLoaded[[.x]], {
      if(topicLoaded[[.x]] == FALSE) {
        message("loading ", .x)
        loadData[[.x]] <- reactive({
          if(identical(graphOptions[[.x]]$updateQuery,TRUE) | (identical(graphOptions[[.x]]$updateQuery,FALSE) & grepl("qa",session$clientData$url_pathname))) {
            EEFsparql(graph[[.x]],graphOptions[[.x]],.x)
          } else if(identical(graphOptions[[.x]]$updateNPF,TRUE)) {
            if(is.null(loadData[["npf"]]())) NULL else loadData[["npf"]]() %>% right_join(distinct(graph[[.x]],Indicator,Characteristic),by=c("Indicator","Characteristic")) %>% mutate(Measure = Indicator)
          } else {
            graph[[.x]]
          }
        })
        if(graphOptions[[.x]]$graphType=="timeSeries0") callModule(timeSeriesServer0,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="timeSeries1") callModule(timeSeriesServer1,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="timeSeries2") callModule(timeSeriesServer2,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="lineChart0") callModule(lineChartServer0,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="lineChart1") callModule(lineChartServer1,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="lineChart2") callModule(lineChartServer2,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="pieChart0") callModule(pieChartServer0,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="pieChart1") callModule(pieChartServer1,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="pieChart2") callModule(pieChartServer2,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="barChart0") callModule(barChartServer0,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="barChart1") callModule(barChartServer1,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="barChart2") callModule(barChartServer2,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        if(graphOptions[[.x]]$graphType=="barChart3") callModule(barChartServer3,.x,loadData[[.x]],panel[[.x]],graphOptions[[.x]],reactive(input$reload))
        topicLoaded[[.x]] <- TRUE 
      }
      
    })
  })
  
  #links sections
  callModule(eefPublicationsSectionServer,"equality-publications",reactive({input$policyID}),updatePublications)
  callModule(eefPublicationsSectionServer,"equality-data",reactive({input$policyID}),updateData)
  callModule(eefPublicationsSectionServer,"equality-external",reactive({input$policyID}),updateExternal)
  callModule(eefPublicationsSectionServer,"equality-collection",reactive({input$policyID}),updateGuidance)
  callModule(eefPublicationsSectionServer,"equality-glossary",reactive({input$policyID}),updateGlossary)
  callModule(npfSectionServer,"equality-NPF")
  
  #summary sections
  lapply(unique(EEFindex$topicID),
         FUN=function(i) {
           topicLoaded <- callModule(eefSummarySectionServer,paste0("equality-",i),i,loadData, topicLoaded)
         })
  
  
}

#run app
shinyApp(ui=ui,server=server)
