npfSectionServer <- function(input,output,session,policy="Summary") {
  output$panel <- renderUI({
    if(!is.null(input$sectionEqualityID)) 
      if(input$sectionEqualityID %in% equalityCharacteristicsID) 
        if(nrow(filter(NPFdataIndex,policy_area==policy,Characteristic==equalityLabel(input$sectionEqualityID)))==0)
          return(NoNPF(equalityLabel(input$sectionEqualityID)))
    npfUI3("npf")
  })
}


eefSummarySectionServer <- function(input,output,session,id=NULL,loadData=list()) {
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
        #loaded <- isolate({modifyList(loaded,uid)})
        #loaded$new <- content$tabUID 
        
        panel <- list()
        for(r in 1:nrow(content)) {
          tabId <- content$tabUID[r]
          ns <- NS(tabId,"chart") %>% NS
          #panelData <- as.list(graphOptions[[tabId]]$panelData)
          panelData <- as.list(content[r,])
          Encoding(panelData$dataSourceNotes) <- "UTF-8" #special characters need to be displayed as UTF-8 in Shiny (note this is not the Windows/Microsoft default)
          Encoding(panelData$subtitle) <- "UTF-8"
          Encoding(panelData$headline) <- "UTF-8"
          if(!is.null(panelData$headline)) if(panelData$headline %in% c("",NA)) panelData$headline <- NULL else panelData$headline <- h2(panelData$headline,class="eef-graph-header-text")
          
          if(panelData$markdownFile %in% c("",NA) | (!file.exists(as.character(panelData$markdownFile)))) {
             panelData$markdownFile <- NULL
          } else if(identical(graphOptions[[tabId]]$updateRmd,TRUE))  {
             panelData$markdownFile <- uiOutput(ns("markdown"))
          } else if(identical(graphOptions[[tabId]]$updateQuery,TRUE) | 
                    identical(graphOptions[[tabId]]$updateNPF,TRUE)) {
             graphData <- loadData[[tabId]]()
             panelData$markdownFile <- knit(graphOptions[[tabId]]$panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
               markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
               HTML
          } else {
             panelData$markdownFile <- includeMarkdown(as.character(panelData$markdownFile))
          }
          
          if(identical(graphOptions[[tabId]]$updateRmd,TRUE))  {
            panelData$commentary <- uiOutput(ns("markdown"))
          } else if(identical(graphOptions[[tabId]]$updateQuery,TRUE)) {
            graphData <- loadData[[tabId]]()
            panelData$commentary <- knit(text=readLines(graphOptions[[tabId]]$panelData$inputMarkdownFile,encoding="UTF-8"), quiet = TRUE,encoding="UTF-8") %>%
              markdown::markdownToHTML(text=.,fragment.only=TRUE,encoding="UTF-8") %>%
              HTML
          } else {
            panelData$commentary <- commentary[[tabId]] %>%HTML
            # panelData$commentary <- HTML(panelData$commentary)
             Encoding(panelData$commentary) <- "UTF-8"
          }
          if(tabId %in% names(loadData)) lastUpdated <- loadData[[tabId]]()$LastUpdated[1] else lastUpdated <- content$index_updated[r]
          if(!is.null(panelData$dataSource)) if(panelData$dataSource %in% c("",NA)) panelData$dataSource <- NULL else panelData$dataSource <- HTML(pubSource(panelData$dataSource,notes=panelData$dataSourceNotes,max_date=lastUpdated))
          if(!is.null(graphOptions[[tabId]]$NPFindicator)) panelData$NPFindicator <- npfCog(tabId,graphOptions[[tabId]]$NPFindicator)
          
          if(is.null(graphOptions[[tabId]]$graphType)) {
            uiGraph <- EEFpanelNoGraph(id=tabId,panelData=panelData)
          } else {
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
          }
          panel[[r]] <- div(class="eef-text",
                            panelData$headline,
                            uiGraph,
                            #panelData$markdownFile,
                            panelData$commentary,
                            panelData$dataSource,
                            panelData$NPFindicator
          )
          if(nrow(content)>1) panel[[r]] <- tabPanel(title=content$tab[r],value=NS("equality",NS("panel",tabId)),panel[[r]])
        }
        if(length(panel)==1) display <- panel[[1]]
        if(length(panel)==2) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),panel[[1]],panel[[2]])
        if(length(panel)==3) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),panel[[1]],panel[[2]],panel[[3]])
        if(length(panel)==4) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),panel[[1]],panel[[2]],panel[[3]],panel[[4]])
        if(length(panel)==5) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]])
        if(length(panel)==6) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]])
        if(length(panel)==7) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]],panel[[7]])
        if(length(panel)>=8) display <- tabsetPanel(id=NS("equalityTabs",NS(equality,id)),panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]],panel[[7]],panel[[8]])
      }
      return(tagList(
        display,
        tags$script("$('a[target=\"_blank\"]').click(function(){gtag('event', 'link', {'event_category': policyEquality, 'event_label': $(this).attr('href') });});"),
        tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('reload',Math.random());});"))
      ))
    })
   # return(loaded)

}

eefPublicationsSectionServer <- function(input,output,session,policyID,updateFUN) {
  ns <- session$ns
  output$panel <- renderUI({
    if(is.null(policyID())) policy <- "" else policy <- policyID()
    if(is.null(input$sectionEqualityID)) equality <- "overview" else equality <- input$sectionEqualityID
    updateFUN(policyLabel(policy),equality) %>% HTML %>% tagList(tags$script("$('a[target=\"_blank\"]').click(function(){gtag('event', 'link', {'event_category': policyEquality, 'event_label': $(this).attr('href') });});"))
    })
}