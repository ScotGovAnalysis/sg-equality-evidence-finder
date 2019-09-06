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
          id <- content$tabUID[r]
          ns <- NS(id,"chart") %>% NS
          #panelData <- as.list(graphOptions[[id]]$panelData)
          panelData <- as.list(content[r,])
          Encoding(panelData$dataSourceNotes) <- "UTF-8" #special characters need to be displayed as UTF-8 in Shiny (note this is not the Windows/Microsoft default)
          Encoding(panelData$subtitle) <- "UTF-8"
          Encoding(panelData$headline) <- "UTF-8"
          if(!is.null(panelData$headline)) if(panelData$headline %in% c("",NA)) panelData$headline <- NULL else panelData$headline <- h2(panelData$headline,class="eef-graph-header-text")
          if(panelData$markdownFile %in% c("",NA) | (!file.exists(as.character(panelData$markdownFile)))) {
             panelData$markdownFile <- NULL
          } else if(!is.null(graphOptions[[id]]$updateRmd))  {
             panelData$markdownFile <- uiOutput(ns("markdown"))
          } else if(!is.null(graphOptions[[id]]$updateQuery)) {
             graphData <- loadData[[id]]()
             panelData$markdownFile <- knit(graphOptions[[id]]$panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
               markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
               HTML
          } else {
             panelData$markdownFile <- includeMarkdown(as.character(panelData$markdownFile))
          }
          if(id %in% names(loadData)) lastUpdated <- loadData[[id]]()$LastUpdated[1] else lastUpdated <- Sys.Date()
          if(!is.null(panelData$dataSource)) if(panelData$dataSource %in% c("",NA)) panelData$dataSource <- NULL else panelData$dataSource <- HTML(pubSource(panelData$dataSource,notes=panelData$dataSourceNotes,max_date=lastUpdated))
          if(!is.null(graphOptions[[id]]$NPFindicator)) panelData$NPFindicator <- npfCog(id,graphOptions[[id]]$NPFindicator)
          
          if(is.null(graphOptions[[id]]$graphType)) {
            uiGraph <- EEFpanelNoGraph(id=id,panelData=panelData)
          } else {
            if(graphOptions[[id]]$graphType%in%"barChart0") uiGraph <- barChartUI0(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"barChart1") uiGraph <- barChartUI1(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"barChart2") uiGraph <- barChartUI2(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"barChart3") uiGraph <- barChartUI3(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"lineChart0") uiGraph <- lineChartUI0(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"lineChart1") uiGraph <- lineChartUI1(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"lineChart2") uiGraph <- lineChartUI2(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"pieChart0") uiGraph <- pieChartUI0(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"pieChart1") uiGraph <- pieChartUI1(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"pieChart2") uiGraph <- pieChartUI2(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"timeSeries0") uiGraph <- timeSeriesUI0(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"timeSeries1") uiGraph <- timeSeriesUI1(id=id,graphOptions=graphOptions[[id]])
            if(graphOptions[[id]]$graphType%in%"timeSeries2") uiGraph <- timeSeriesUI2(id=id,graphOptions=graphOptions[[id]])
          }  
          panel[[r]] <- div(class="eef-text",
                            panelData$headline,
                            uiGraph,
                            panelData$markdownFile,
                            panelData$dataSource,
                            panelData$NPFindicator
          )
          if(nrow(content)>1) panel[[r]] <- tabPanel(title=content$tab[r],value=NS("equality",NS("panel",id)),panel[[r]])
        }
        if(length(panel)==1) display <- panel[[1]]
        if(length(panel)==2) display <- tabsetPanel(panel[[1]],panel[[2]])
        if(length(panel)==3) display <- tabsetPanel(panel[[1]],panel[[2]],panel[[3]])
        if(length(panel)==4) display <- tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]])
        if(length(panel)==5) display <- tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]])
        if(length(panel)==6) display <- tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]])
        if(length(panel)==7) display <- tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]],panel[[7]])
        if(length(panel)>=8) display <- tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]],panel[[7]],panel[[8]])
      }
      return(tagList(
        display,
        tags$script("$('a[target=\"_blank\"]').click(function(){gtag('event', 'link', {'event_category': policyEquality, 'event_label': $(this).attr('href') });});")
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