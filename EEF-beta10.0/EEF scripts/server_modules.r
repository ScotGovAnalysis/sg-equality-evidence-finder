###################################################################################
#                                                                                 #
# MODULE SERVER FUNCTIONS - PANELS                                                #
#                                                                                 #
###################################################################################
#                                                                                 #
# Author: Jay Ware                                                                #
# Created: 01/08/2018                                                             #
# Last updated: 14/01/2021                                                        #
#                                                                                 #
# Purpose: Shiny module servers for the interactive sections of the Evidence      #
#          Finder (except charts). These are for the NPF section, equality        #
#          evidence sections, and links sections. There's also modules for adding #
#          Rmd commentary live, and creating downloadable csvs of chart data.     #
#                                                                                 #
#          See ui_modules.R for the corresponding UI functions.                   #
#                                                                                 #
# Functions:                                                                      #
#                                                                                 #
# npfSectionServer(): Module server for NPF section. This is mostly a wrapper for #
#                     the chart, though checks whether there is any data to       #
#                     show first.                                                 #
# eefSummarySectionServer(): Module server for the equality evidence panel        #
#                            content. These display a headline, interactive       #
#                            chart or infographic, commentary, data source link,  #
#                            and NPF indicator flag (if applicable).              #
#                            The headline, chart data, commentary and data source #
#                            can be updated live.                                 #
# eefPublicationsSectionServer(): Module server for publication link section.     #
#                                 This is a wrapper function, with the content of #
#                                 of the panel generated by one of the helper     #
#                                 functions.                                      #
# rmdServer(): Module server for generating commentary and data source links live.#
#              This is used when chart data is updated live or commentary depends #
#              on what options a user selected. Data source will look up the most #
#              publication in the series as at the time the chart data was last   #
#              uodated on the open data platform.                                 #
# downloadServer(): Module server for the download png and csv buttons. Currently #
#              only download png buttons are used for time series (dygraphs) and  #
#              some pie chart (google charts).                                    #
#                                                                                 #
###################################################################################

npfSectionServer <- function(input,output,session,policy="Summary") {
  output$panel <- renderUI({
    if(!is.null(input$sectionEqualityID)) 
      if(input$sectionEqualityID %in% equalityCharacteristicsID) 
        if(nrow(filter(NPFdataIndex,policy_area==policy,Characteristic==equalityLabel(input$sectionEqualityID)))==0)
          return(NoNPF(equalityLabel(input$sectionEqualityID)))
    npfUI3("npf")
  })
}


eefSummarySectionServer <- function(input,output,session,id=NULL,loadData=list(), topicLoaded) {
    output$panel <- renderUI({
      if(is.null(input$sectionEqualityID)) return(NULL)
      equality <- equalityLabel(input$sectionEqualityID)
      content <- filter(EEFindex,topicID==id,characteristic==equality)
      
      if(nrow(content)==0) {
        topicName <- EEFindex$topic[match(id,EEFindex$topicID)]
        policy <- filter(EEFindex,topicID==id)$policy_area[1]
        return(EEFpanelNoData(topicName,equality,policy))
      } else {
        uid <- rep(list(TRUE),nrow(content))
        names(uid) <- content$tabUID

        #Check to see if the topic has been loaded - if not then tell the app to load the code through setting the topicLoaded flag to FALSE
        for(i in content$tabUID) {
          if(is.null(topicLoaded[[i]])) topicLoaded[[i]] <- FALSE
        }
        
        #create panel content
        build_panel <- list()
        for(r in 1:nrow(content)) {
          tabId <- content$tabUID[r]
          ns <- NS(tabId,"chart") %>% NS
          panelData <- panel[[tabId]]
          
          Encoding(panelData$dataSourceNotes) <- "UTF-8" #special characters need to be displayed as UTF-8 in Shiny (note this is not the Windows/Microsoft Office default)
          Encoding(panelData$subtitle) <- "UTF-8"
          Encoding(panelData$headline) <- "UTF-8"
          if(!is.null(panelData$headline)) 
            if(panelData$headline %in% c("",NA)) {
              panelData$headline <- NULL 
            } else {
              panelData$headline <- knit(text = panelData$headline, quiet = TRUE, encoding = "UTF-8") %>%
                h3(class="eef-graph-header-text")
            }
          
          if(identical(graphOptions[[tabId]]$updateRmd,TRUE) |
             identical(graphOptions[[tabId]]$updateQuery,TRUE) | 
             identical(graphOptions[[tabId]]$updateNPF,TRUE))  {
            if(topicLoaded[[tabId]] == TRUE) panelData$commentary <- uiOutput(ns("markdown")) else panelData$commentary <- NULL
          } else {
            panelData$commentary <- commentary[[tabId]] %>%HTML
             Encoding(panelData$commentary) <- "UTF-8"
          }
          
          #EXPERIMENTAL
          if(!is.null(panelData$dataSource)) {
            if(panelData$dataSource %in% c("",NA)) {
              panelData$dataSource <- NULL 
              panelData$dataSourceShort <- NULL 
            } else if(identical(graphOptions[[tabId]]$updateRmd,TRUE) |
                        identical(graphOptions[[tabId]]$updateQuery,TRUE) | 
                        identical(graphOptions[[tabId]]$updateNPF,TRUE)) {
              if(topicLoaded[[tabId]] == TRUE) panelData$dataSource <- uiOutput(ns("source")) else panelData$dataSource <- NULL
              if(topicLoaded[[tabId]] == TRUE) panelData$dataSourceShort <- uiOutput(ns("source2")) else panelData$dataSourceShort <- NULL
            } else {
              panelData$dataSourceShort <- HTML(pubSource(panelData$dataSource,notes=NULL,max_date=panelData$last_updated))
              panelData$dataSource <- HTML(pubSource(panelData$dataSource,notes=panelData$dataSourceNotes,max_date=panelData$last_updated))
            }
          }
          if(!is.null(panelData$NPFindicator)) panelData$NPFindicator <- npfCog(tabId,panelData$NPFindicator)
          
          if(is.null(graphOptions[[tabId]]$graphType)) {
            uiGraph <- EEFpanelNoGraph(id=tabId,panelData=panelData)
            panelData$dataSourceShort <- NULL
          } else if(topicLoaded[[tabId]] == TRUE){
            if(graphOptions[[tabId]]$graphType%in%"barChart0") uiGraph <- barChartUI0(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"barChart1") uiGraph <- barChartUI1(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"barChart2") uiGraph <- barChartUI2(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"barChart3") uiGraph <- barChartUI3(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"lineChart0") uiGraph <- lineChartUI0(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"lineChart1") uiGraph <- lineChartUI1(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"lineChart2") uiGraph <- lineChartUI2(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"pieChart0") uiGraph <- pieChartUI0(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"pieChart1") uiGraph <- pieChartUI1(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"pieChart2") uiGraph <- pieChartUI2(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"timeSeries0") uiGraph <- timeSeriesUI0(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"timeSeries1") uiGraph <- timeSeriesUI1(id=tabId,graphOptions=graphOptions[[tabId]])
            if(graphOptions[[tabId]]$graphType%in%"timeSeries2") uiGraph <- timeSeriesUI2(id=tabId,graphOptions=graphOptions[[tabId]])
            uiGraph <- tagList(uiOutput(ns("title")), uiGraph)
          } else {
            uiGraph <- NULL
          }
          build_panel[[r]] <- div(class="eef-text",
                            panelData$headline,
                            #panelData$markdownFile,
                            panelData$commentary,
                            panelData$dataSourceShort,
                            uiGraph,
                            panelData$dataSource,
                            panelData$NPFindicator
          )
          if(nrow(content)>1) build_panel[[r]] <- tabPanel(title=content$tab[r],value=NS("equality",NS("panel",tabId)),build_panel[[r]])
        }
        if(length(build_panel)==1) display <- build_panel[[1]]
        if(length(build_panel)==2) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),build_panel[[1]],build_panel[[2]])
        if(length(build_panel)==3) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),build_panel[[1]],build_panel[[2]],build_panel[[3]])
        if(length(build_panel)==4) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),build_panel[[1]],build_panel[[2]],build_panel[[3]],build_panel[[4]])
        if(length(build_panel)==5) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),build_panel[[1]],build_panel[[2]],build_panel[[3]],build_panel[[4]],build_panel[[5]])
        if(length(build_panel)==6) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),build_panel[[1]],build_panel[[2]],build_panel[[3]],build_panel[[4]],build_panel[[5]],build_panel[[6]])
        if(length(build_panel)==7) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),build_panel[[1]],build_panel[[2]],build_panel[[3]],build_panel[[4]],build_panel[[5]],build_panel[[6]],build_panel[[7]])
        if(length(build_panel)>=8) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),build_panel[[1]],build_panel[[2]],build_panel[[3]],build_panel[[4]],build_panel[[5]],build_panel[[6]],build_panel[[7]],build_panel[[8]])
      }
      return(tagList(
        display,
        tags$script("$('a[target=\"_blank\"]').click(function(){gtag('event', 'link', {'event_category': policyEquality, 'event_label': $(this).attr('href') });});"),
        tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('reload',Math.random());});"))
      ))
    })
    return(topicLoaded)

}

eefPublicationsSectionServer <- function(input,output,session,policyID,updateFUN) {
  ns <- session$ns
  output$panel <- renderUI({
    if(is.null(policyID())) policy <- "" else policy <- policyID()
    if(is.null(input$sectionEqualityID)) equality <- "overview" else equality <- input$sectionEqualityID
    updateFUN(policyLabel(policy),equality) %>% HTML %>% tagList(tags$script("$('a[target=\"_blank\"]').click(function(){gtag('event', 'link', {'event_category': policyEquality, 'event_label': $(this).attr('href') });});"))
    })
}

rmdServer <- function(input,output,session,filterData,panelData,graphOptions) {
  
  output$markdown <- renderUI({
    if(is.null(filterData())) return(NULL)
    graphData <- filterData()
    if(is.null(panelData$markdown_commentary)) return(NULL)
    if(panelData$markdown_commentary %in% c(NA, "")) return(NULL)
    knit(text=panelData$markdown_commentary, quiet = TRUE,encoding="UTF-8") %>%
      markdown::markdownToHTML(text=.,fragment.only=TRUE,encoding="UTF-8") %>%
      HTML
  })
  output$source <- renderUI({
    lastUpdated <- filterData()$LastUpdated[1]
    HTML(pubSource(panelData$dataSource,notes=panelData$dataSourceNotes,max_date=lastUpdated))
  })
  output$source2 <- renderUI({
    lastUpdated <- filterData()$LastUpdated[1]
    HTML(pubSource(panelData$dataSource,notes=NULL,max_date=lastUpdated))
  })
  output$title <- renderUI({
    if(!is.null(graphOptions$graphTitle)) {
      graphTitle <- graphOptions$graphTitle 
    } else {
      graphTitle <- filterData()$graphTitle[1]
    } 
    if(is.na(graphTitle)) graphTitle <- ""
    h5(paste("Chart:", graphTitle))
  })
}

downloadServer <- function(input,output,session,filterGraph=reactive({NULL}),graphOptions=list()) {
  ns <- session$ns
  
  
  output$png <- downloadHandler(
    filename= function() {
      paste(isolate(filterGraph())$Indicator[1],"Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"png",suspendWhenHidden=FALSE)
  output$csv <- downloadHandler(
    filename= function() {
      paste(isolate(filterGraph())$Indicator[1]," Data.csv")
    },
    content=function(file) {
      graph <- filterGraph() %>%
        select(Indicator,Disaggregation=Measure,Breakdown,DateCode,Figure) 
      if(!is.null(graphOptions$digits)) graph <- mutate(graph,Figure=round(Figure,graphOptions$digits))
      write.csv(graph,file,row.names=FALSE)
    }
  )
}
