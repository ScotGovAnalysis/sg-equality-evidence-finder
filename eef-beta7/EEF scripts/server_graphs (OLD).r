# ##TO DO - add wrapper timeSeries0 etc
# ##TO DO - correct module IDs
# 
# selectBreakdownServer <- function(input,output,session,filterData,defaultSelected=NULL) {
#   output$optionBreakdown <- renderUI({
#     if(is.null(filterData)) return(NULL)
#     optionsList <- unique(filterData$Breakdown)
#     if(is.null(defaultSelected)) defaultSelected <- optionsList
#     selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
#     if(length(selectedList)==0) selectedList <- defaultSelected
#     optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
#     if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
#     checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
#                        choices = optionsList,
#                        selected=selectedList)
#     
#   })
#   
#   filterGraph <- reactive({
#     filter(filterData,
#            Breakdown %in% input$selectBreakdown
#     )
#   })
# }
# 
# selectMeasureBreakdownServer <- function(input,output,session,filterData,defaultSelected=NULL) {
#   output$optionMeasure <- renderUI({
#     if(nrow(filterData)==0) return(NULL)
#     selectInput(ns("selectMeasure"),NULL,
#                 choices = unique(filterData$Measure),selected=isolate(input$selectMeasure))
#     
#   })
#   
#   filterOptions <- reactive({
#     if(is.null(filterData)|is.null(input$selectMeasure)) return(NULL)
#     filter(filterData,Measure %in% input$selectMeasure)
#   })
#   
#   output$optionBreakdown <- renderUI({
#     if(is.null(filterOptions())) return(NULL)
#     optionsList <- unique(filterOptions()$Breakdown)
#     if(is.null(defaultSelected)) defaultSelected <- optionsList
#     selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
#     if(length(selectedList)==0) selectedList <- defaultSelected
#     optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
#     if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
#     checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
#                        choices = optionsList,
#                        selected=selectedList)
#     
#   })
#   filterGraph <- reactive({
#     if(is.null(filterOptions())) return(NULL)
#     filter(filterOptions(),
#            Breakdown %in% input$selectBreakdown
#     )
#   })  
#   return(filterGraph())
# }
# 
# downloadServer <- function(input,output,session,filterGraph=reactive({NULL})) {
#   output$png <- downloadHandler(
#     filename= function() {
#       paste(isolate(filterGraph())$Indicator[1],"Chart.png")
#     },
#     content=function(file) {
#       output <- file(file, "wb")
#       on.exit(close(output))
#       io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$URI))
#       writeBin(io, output)
#     }
#   )
#   outputOptions(output,"chart",suspendWhenHidden=FALSE)
#   output$csv <- downloadHandler(
#     filename= function() {
#       paste(isolate(filterGraph())$Indicator[1]," Data.csv")
#     },
#     content=function(file) {
#       filterGraph() %>%
#         select(Indicator,Disaggregation=Measure,Breakdown,DateCode,Figure) %>%
#         write.csv(file,row.names=FALSE)
#     }
#   )
# }
# 
# dygraphServer <- function(input,output,session,filterGraph=reactive({NULL}),graphOptions=list(),maxRange=c(NA,NA),colours=data.frame(Breakdown=character(0),stringsAsFactors = F)) {
#   ns <- session$ns
#   if(is.null(filterGraph())) return(NULL)
#   panelData <- graphOptions$panelData
#   if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
#   if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
#   if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
#   if(is.null(graphOptions$ylabel))  ylabel <- filterGraph()$Measure[1] else ylabel <- graphOptions$ylabel
#   if(is.null(graphOptions$digits)) yFormat <- NULL else yFormat <- jsFormatCommas(graphOptions$digits)
#   
#   #id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
#   #if(!is.null(graphOptions$digits)) filterGraph() <- mutate(filterGraph(),Figure=round(Figure,graphOptions$digits))
#   series <- left_join(select(filterGraph(),-seriesColour),colours,by="Breakdown") %>%
#     distinct(Breakdown,.keep_all = TRUE)
#   
#   zoomLevel <- reactiveVal(0)
#   shinyjs::onclick("zoomIn", {
#     if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
#   })
#   shinyjs::onclick("zoomOut", {
#     if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
#   })
#   
#   shinyjs::onclick("info",{
#     show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries1")
#   })
#   
#   if(!is.null(graphOptions$updateRmd)) {
#     output$markdown <- renderUI({
#       if(is.na(panelData$inputMarkdownFile)) return(NULL)
#       graphData <- filterGraph()
#       knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
#         markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
#         HTML
#     })
#   }
#   
#   #plot graph
#   redraw <- reactiveVal(0)
#   output$plot <- renderDygraph({
#     if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
#     if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
#     if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
#     if(nrow(filterGraph())==0) return(NULL)
#     
#     graphData <- filterGraph()
#     graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
#     if(zoomLevel()==0) {
#       graphRange <- maxRange
#     } else {
#       minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure),minZoom)
#       graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
#     }
#     series <- left_join(select(filterGraph(),-seriesColour),colours,by="Breakdown") %>%
#       distinct(Breakdown,.keep_all = TRUE)
#     
#     graphData <- select(filterGraph(),Date,Figure,Breakdown)%>%
#       group_by(Date)%>%
#       spread(Breakdown,Figure) %>%
#       select(Date,series$Breakdown) %>%
#       ungroup
#     
#     if(!is.null(isolate({input$plot_date_window}))) dateRange <- as.Date(isolate({input$plot_date_window[2]}))-as.Date(isolate({input$plot_date_window[1]})) else dateRange <- max(graphData$Date,na.rm=TRUE)-min(graphData$Date,na.rm=TRUE)
#     axisFormats <- eefDygraphFormatter(graphData,intervalType=filterGraph()$intervalType[1],isolate({input$plot_date_window}),graphOptions$xFormat)
#     graphData <- axisFormats$data
#     ticker <- axisFormats$ticker
#     axisLabelFormatter <- axisFormats$axisLabelFormatter
#     valueFormatter <- axisFormats$valueFormatter
#     
#     xtsData <- xts(graphData[-1],graphData[[1]]) %>% padxts2
#     
#     dygraph(xtsData,main=graphTitle) %>%
#       dyRangeSelector(height = 40, strokeColor = "") %>%
#       dyHighlight(highlightCircleSize = 0,
#                   highlightSeriesBackgroundAlpha = 0.2,
#                   hideOnMouseOut = TRUE,
#                   highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
#       dyOptions(titleHeight=40,rightGap = 25,strokeWidth = 3,colors=series$seriesColour,retainDateWindow = TRUE) %>%
#       #dyLegend(labelsDiv = ns("legend"),show = "always",labelsSeparateLines=TRUE) %>%
#       dyLegend(show = "follow",labelsSeparateLines = FALSE,width=150) %>%
#       dyAxis("x",
#              drawGrid=FALSE,
#              axisLabelFormatter=axisLabelFormatter,
#              valueFormatter=valueFormatter,
#              ticker=ticker
#       ) %>%
#       dyAxis("y", drawGrid=FALSE,label= ylabel,valueRange=graphRange,valueFormatter = yFormat)%>%
#       dyCallbacks(drawCallback = paste0("function(dygraph) {if($('#",ns("plot")," div').height()>0){DygraphExport.Register(dygraph);}}"))
#   })
#   
#   #toggle button
#   shinyjs::onclick("toggledash", {
#     shinyjs::toggle("displayOptions")
#     shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
#     shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
#     redraw(redraw()+1)
#   })
# }
# 



#Graph package: dygraph
#Graph type: Line
#options: Outcome, Indicator, Disaggregation, Breakdown
#Plots: Measure
#Scope: All years
npfServer3 <- function(input,output,session,filterData=NULL,graphOptions=list(),filterChar=reactive({NULL})) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  filterTotal <- filter(filterData,Characteristic %in% "Total")
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  colours <- graphOptions$colours
  
  defaultSelected <- graphOptions$defaultSelected
  digits <- graphOptions$digits
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  
  filterOptions0 <- reactive({
    if(is.null(filterData)|is.null(filterChar())) return(filterData)
    if(filterChar()=="overview") {
      return(filterData)
    } else {
      return(filter(filterData,Characteristic %in% equalityLabel(filterChar())))
    }
  })
  
  output$Dashboardoutcomes <- renderUI({
    if(is.null(filterOptions0())) return(NULL)
    if(nrow(filterOptions0())==0) return(NULL)
    selectInput(ns("Dashboardoutcome"),"Select Outcome",
                choices = c("All",unique(filterOptions0()$Outcome)),selected=isolate(input$Dashboardoutcome))
    
  })
  
  filterOptions1 <- reactive({
    if(is.null(filterOptions0())|is.null(input$Dashboardoutcome)) return(NULL)
    if(input$Dashboardoutcome=="All") filterOptions0() else filter(filterOptions0(),Outcome %in% input$Dashboardoutcome)
  })
  
  output$Dashboardindicators <- renderUI({
    if(is.null(filterOptions1())) return(NULL)
    if(nrow(filterOptions1())==0) return(NULL)
    selectInput(ns("Dashboardindicator"),"Select Indicator",
                choices = sort(unique(filterOptions1()$Indicator)),selected=isolate(input$Dashboardindicator))
  })
  
  filterOptions2 <- reactive({
    if(is.null(filterOptions1())|is.null(input$Dashboardindicator)) return(NULL)
    filter(filterOptions1(),Indicator %in% input$Dashboardindicator)
  })
  
  output$Dashboardoptions <- renderUI({
    if(is.null(filterOptions2())) return(NULL)
    if(nrow(filterOptions2())==0) return(NULL)
    optionsList <- unique(filterOptions2()$Disaggregation)
    selectedList <- isolate({input$Dashboardoption}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- "Total"
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    selectInput(ns("Dashboardoption"),"Select Breakdown",
                choices = optionsList,selected=selectedList)
  })
  
  filterOptions3 <- reactive({
    if(is.null(filterOptions2())|is.null(input$Dashboardoption)) return(NULL)
    
    filteredData <- filter(filterOptions2(),Disaggregation %in% input$Dashboardoption)
    if(input$Dashboardoption!="Total")
      filteredData <- bind_rows(filteredData,filter(filterTotal,Indicator %in% input$Dashboardindicator,!Breakdown%in%filteredData$Breakdown))
    return(filteredData)
  })
  
  output$optionBreakdown <- renderUI({
    if(is.null(filterOptions3())) return(NULL)
    optionsList <- unique(filterOptions3()$Breakdown)
    if(length(optionsList)<9) {
      #if(is.null(defaultSelected)) defaultSelected <- optionsList
      selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
      if(length(selectedList)==0) selectedList <- optionsList[grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)]
      if(length(selectedList)==0) selectedList <- optionsList
      checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                         choices = optionsList,
                         selected=selectedList)
    } else {
      selectedList <- optionsList[grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)]
      if(length(selectedList)==0) selectedList <- optionsList[1]
      selectInput(ns("selectBreakdown"),"Choose what to plot:",
                  choices=optionsList,
                  selected=selectedList,
                  multiple=TRUE)
    }
    
  })
  
  filterGraph <- reactive({
    if(is.null(filterOptions3())|is.null(input$selectBreakdown)) return(NULL)
    colours <- distinct(filterOptions3(),Breakdown,.keep_all = TRUE) %>%
      #mutate(seriesColour=ifelse(grepl("\\b(All|Total)\\b",Breakdown,ignore.case=T),"blue",seriesColour)) %>%
     # mutate(seriesColour=ifelse(Characteristic=="Total","blue",seriesColour)) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette)) %>% 
      select(Breakdown,seriesColour)
      #mutate(seriesColour=replace(seriesColour,grepl("\\b(All|Total)\\b",Breakdown,ignore.case=T),eefColours(NA,"blue"))) %>% #totals are a shade of blue
      #mutate(seriesColour=replace(seriesColour,!grepl("\\b(All|Total)\\b",Breakdown,ignore.case=T),eefColours(seriesColour,"not-blue"))) #non-totals are other colours

    left_join(select(filterOptions3(),-seriesColour),colours,by="Breakdown") %>%
      filter(Breakdown %in% input$selectBreakdown)
  })  
  
  #buttons
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries2")
  })
  

  output$downloadChart <- downloadHandler(
    filename= function() {paste0(filterGraph()$Indicator[1],"_Chart.png")},
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {paste0(filterGraph()$Indicator[1],"_Data.csv")},
    content=function(file) {
      out <- select(filterGraph(),Outcome,Indicator,Disaggregation,Breakdown,Date=Yearlab,Figure)
      write.csv(out,file,row.names=FALSE)
    }
  )
  
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderDygraph({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    maxRange <- ifelse(is.na(maxZoom),padRange(c(0,isolate(filterOptions3())$Figure),padding=0.25),maxZoom)
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }

    series <- distinct(filterGraph(),Breakdown,.keep_all = TRUE)
    
    if(is.null(digits)) dispDigits <- max(0,nchar(sub("^[^.]*(\\.|$)","",as.character(filterGraph()$Figure))),na.rm=TRUE) else dispDigits <- digits
    mag <- ceiling(log10(max(abs(filterGraph()$Figure),na.rm=TRUE)))
    dispDigits <- min(dispDigits,3-mag) ##graph will pad with zeros to this many dp
    
    graphData <- select(filterGraph(),Date,Figure,Breakdown)%>%
      mutate(Figure=round(Figure,dispDigits)) %>%
      group_by(Date)%>%
      spread(Breakdown,Figure) %>%
      select(Date,series$Breakdown) %>%
      ungroup
    
    ylabel <- filter(NPFindex,Indicator %in% input$Dashboardindicator,Disaggregation %in% input$Dashboardoption)$Measure[1]
    dateRange <- max(graphData$Date,na.rm=TRUE)-min(graphData$Date,na.rm=TRUE)
    perYear <- dateRange/nrow(graphData)
    
    if(filterGraph()$EEFformat[1]%in%c("",NA)) xFormat <- NULL else xFormat <- filterGraph()$EEFformat[1]
    axisFormats <- eefDygraphFormatter(graphData,intervalType=filterGraph()$intervalType[1],NULL,xFormat)
    graphData <- axisFormats$data
    ticker <- axisFormats$ticker
    axisLabelFormatter <- axisFormats$axisLabelFormatter
    valueFormatter <- axisFormats$valueFormatter
    
    if(dateRange==0) drawPoints <- TRUE else drawPoints <- FALSE
    xtsData <- xts(graphData[-1],graphData[[1]]) %>% padxts2

    dygraph(xtsData,main=ylabel) %>%
      dyRangeSelector(height = 40, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
      dyOptions(drawAxesAtZero=TRUE,titleHeight=40,drawPoints=drawPoints,pointSize=5,rightGap = 25,strokeWidth = 3,colors=series$seriesColour,retainDateWindow = TRUE) %>%
      dyLegend(show = "follow",width=150) %>%
      dyAxis("x",
             drawGrid=FALSE,
             axisLabelFormatter=axisLabelFormatter,
             valueFormatter=valueFormatter,
             ticker=ticker
      ) %>%
       dyAxis("y", drawGrid=FALSE,valueRange=graphRange,valueFormatter = jsFormatCommas(dispDigits))%>%
      dyCallbacks(drawCallback = paste0("function(dygraph) {if($('#",ns("plot")," div').height()>0){DygraphExport.Register(dygraph);}}"))
    #dyCallbacks(drawCallback = paste0("function(dygraph) {if($('#",ns("plot")," div').height()>0){DygraphExport.Register(dygraph);}}"))
  })
    
  output$title <- renderUI({h3(paste0("National Indicator: ",filterGraph()$Indicator[1]))})
  output$measure <- renderUI({h3(filterGraph()$Measure[1])})
  output$source <- renderUI({
    if(is.null(filterGraph())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    #pubSource(filterGraph()$Source[1])
    p(strong("Source: "),filterGraph()$Source[1])
  })
  
  output$Dashboardtable <- renderDataTable({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    out <- select(filterGraph(),Outcome,Indicator,Disaggregation,Breakdown,Date=Yearlab,Figure)
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
  shinyjs::onclick("toggledash2", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
  
  
}

#Graph package: dygraph
#Graph type: Line
#options: None
#Plots: Measure
#Scope: All years
#filterCharacteristic, filterMeasure, filterBreakdown no longer used - use filterData instead
timeSeriesServer0 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  colours <- distinct(filterData,Breakdown,.keep_all = TRUE) %>%
    select(Breakdown,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  defaultSelected <- graphOptions$defaultSelected
  ylabel <- graphOptions$ylabel
  if(is.null(graphOptions$digits)) yFormat <- NULL else yFormat <- jsFormatCommas(graphOptions$digits)
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(is.null(ylabel)) ylabel <- filterData$Measure[1]
  if(!is.null(graphOptions$digits)) filterData <- mutate(filterData,Figure=round(Figure,graphOptions$digits))
 
  #input options

  
  filterGraph <- filterData
  series <- left_join(select(filterGraph,-seriesColour),colours,by="Breakdown") %>%
    distinct(Breakdown,.keep_all = TRUE)
  
  #buttons
  if(!is.null(filterData)) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries0")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- filterGraph
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- filterGraph
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      if(filterGraph$yearOnly[1]) {
        out <- select(filterGraph,Measure,Characteristic,Breakdown,Year,Value=Figure)
      } else {
        out <- select(filterGraph,Measure,Characteristic,Breakdown,Date,Value=Figure)
      }
      write.csv(out,file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderDygraph({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph)|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph)==0) return(NULL)
    
    graphData <- filterGraph
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph$Figure),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    graphData <- select(filterGraph,Year,Date,Figure,Breakdown)%>%
      group_by(Date)%>%
      spread(Breakdown,Figure) %>%
      select(Date,series$Breakdown) %>%
      ungroup

    axisFormats <- eefDygraphFormatter(graphData,intervalType=filterGraph()$intervalType[1],isolate({input$plot_date_window}),graphOptions$xFormat)
    graphData <- axisFormats$data
    ticker <- axisFormats$ticker
    axisLabelFormatter <- axisFormats$axisLabelFormatter
    valueFormatter <- axisFormats$valueFormatter
    
    xtsData <- xts(graphData[-1],graphData[[1]]) %>% padxts2
    dygraph(xtsData,main=graphTitle) %>%
      dyRangeSelector(height = 40, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
      dyOptions(titleHeight=40,rightGap = 25,strokeWidth = 3,colors=series$seriesColour,retainDateWindow = TRUE) %>%
      dyLegend(show = "follow",labelsSeparateLines = FALSE,width=150) %>%
      dyAxis("x",
             drawGrid=FALSE,
             axisLabelFormatter=axisLabelFormatter,
             valueFormatter=valueFormatter,
             ticker=ticker
      ) %>%
      dyAxis("y", drawGrid=FALSE,label= ylabel,valueRange=graphRange,valueFormatter = yFormat)%>%
    dyCallbacks(drawCallback = paste0("function(dygraph) {if($('#",ns("plot")," div').height()>0){DygraphExport.Register(dygraph);}}"))
  })
}


#Graph package: dygraph
#Graph type: Line
#options: Breakdown
#Plots: Measure
#Scope: All years
#filterCharacteristic, filterMeasure, filterBreakdown no longer used - use filterData instead
timeSeriesServer1 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  colours <- distinct(filterData,Breakdown,.keep_all = TRUE) %>%
    select(Breakdown,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  defaultSelected <- graphOptions$defaultSelected
  ylabel <- graphOptions$ylabel
  if(is.null(graphOptions$digits)) yFormat <- NULL else yFormat <- jsFormatCommas(graphOptions$digits)
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(is.null(ylabel)) ylabel <- filterData$Measure[1]
  if(!is.null(graphOptions$digits)) filterData <- mutate(filterData,Figure=round(Figure,graphOptions$digits))

  #input options

  output$optionBreakdown <- renderUI({
    if(is.null(filterData)) return(NULL)
    optionsList <- unique(filterData$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  filterGraph <- reactive({
    filter(filterData,
           Breakdown %in% input$selectBreakdown
    )
  })
  
  #buttons
  if(!is.null(filterData)) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)

  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries1")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      if(filterGraph()$yearOnly[1]) {
        out <- select(filterGraph(),Measure,Characteristic,Breakdown,Year,Value=Figure)
      } else {
        out <- select(filterGraph(),Measure,Characteristic,Breakdown,Date,Value=Figure)
      }
      write.csv(out,file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph()
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderDygraph({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    
    graphData <- filterGraph()
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    series <- left_join(select(filterGraph(),-seriesColour),colours,by="Breakdown") %>%
      distinct(Breakdown,.keep_all = TRUE)
    
    graphData <- select(filterGraph(),Date,Figure,Breakdown)%>%
      group_by(Date)%>%
      spread(Breakdown,Figure) %>%
      select(Date,series$Breakdown) %>%
      ungroup

    if(!is.null(isolate({input$plot_date_window}))) dateRange <- as.Date(isolate({input$plot_date_window[2]}))-as.Date(isolate({input$plot_date_window[1]})) else dateRange <- max(graphData$Date,na.rm=TRUE)-min(graphData$Date,na.rm=TRUE)
    axisFormats <- eefDygraphFormatter(graphData,intervalType=filterGraph()$intervalType[1],isolate({input$plot_date_window}),graphOptions$xFormat)
    graphData <- axisFormats$data
    ticker <- axisFormats$ticker
    axisLabelFormatter <- axisFormats$axisLabelFormatter
    valueFormatter <- axisFormats$valueFormatter
    
    xtsData <- xts(graphData[-1],graphData[[1]]) %>% padxts2
    
    dygraph(xtsData,main=graphTitle) %>%
      dyRangeSelector(height = 40, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
      dyOptions(titleHeight=40,rightGap = 25,strokeWidth = 3,colors=series$seriesColour,retainDateWindow = TRUE) %>%
      #dyLegend(labelsDiv = ns("legend"),show = "always",labelsSeparateLines=TRUE) %>%
      dyLegend(show = "follow",labelsSeparateLines = FALSE,width=150) %>%
      dyAxis("x",
             drawGrid=FALSE,
             axisLabelFormatter=axisLabelFormatter,
             valueFormatter=valueFormatter,
             ticker=ticker
      ) %>%
      dyAxis("y", drawGrid=FALSE,label= ylabel,valueRange=graphRange,valueFormatter = yFormat)%>%
    dyCallbacks(drawCallback = paste0("function(dygraph) {if($('#",ns("plot")," div').height()>0){DygraphExport.Register(dygraph);}}"))
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
}


#Graph package: dygraph
#Graph type: Line
#options: Breakdown, Measure
#Plots: Measure
#Scope: All years
timeSeriesServer2 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  panelData <- graphOptions$panelData
  if(!is.null(graphOptions$query)) {
    filterData <- eefSparql(graphOptions$query,graphOptions$intervalType,panelData$characteristic)
    graphOptions$ylabel <- filterData$yLabel[1]
    output$dataSource <- renderUI({
      if(panelData$dataSource %in% c("",NA)) {
        return(NULL) 
      }else {
        return(HTML(pubSource(panelData$dataSource,notes=panelData$dataSourceNotes,extra_details=paste0("Last updated: ",format(as_datetime(filterData$LastUpdated[1]),"%B %Y"),". Next updated: ",filterData$NextUpdated[1]))))
      }
    })
  }
  
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  colours <- distinct(filterData,Breakdown,.keep_all = TRUE) %>%
    select(Breakdown,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  defaultSelected <- graphOptions$defaultSelected
  if(is.null(graphOptions$digits)) yFormat <- NULL else yFormat <- jsFormatCommas(graphOptions$digits)
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(graphOptions$digits)) filterData <- mutate(filterData,Figure=round(Figure,graphOptions$digits))

  #input options
  output$optionMeasure <- renderUI({
    if(nrow(filterData)==0) return(NULL)
    selectInput(ns("selectMeasure"),NULL,
                choices = unique(filterData$Measure),selected=isolate(input$selectMeasure))
    
  })
  
  filterOptions <- reactive({
    if(is.null(filterData)|is.null(input$selectMeasure)) return(NULL)
    filter(filterData,Measure %in% input$selectMeasure)
  })
  
  output$optionBreakdown <- renderUI({
    if(is.null(filterOptions())) return(NULL)
    optionsList <- unique(filterOptions()$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  filterGraph <- reactive({
    if(is.null(filterOptions())) return(NULL)
    filter(filterOptions(),
           Breakdown %in% input$selectBreakdown
    )
  })  
  
  #buttons
  if(!is.null(filterData)) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries2")
  })
  

  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData<-isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      if(filterGraph()$yearOnly[1]) {
        out <- select(filterGraph(),Measure,Characteristic,Breakdown,Year,Value=Figure)
      } else {
        out <- select(filterGraph(),Measure,Characteristic,Breakdown,Date,Value=Figure)
      }
      write.csv(out,file,row.names=FALSE)
    }
  )
  

  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph()
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderDygraph({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    
    if(!is.null(graphOptions$forceRescale)) if(graphOptions$forceRescale==TRUE) {
      maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterOptions()$Figure),padding=0.25),maxZoom)
    }
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    
    series <- left_join(select(filterGraph(),-seriesColour),colours,by="Breakdown") %>%
      distinct(Breakdown,.keep_all = TRUE)
    
    graphData <- filterGraph()
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    if(is.null(graphOptions$ylabel)) graphOptions$ylabel <- filterOptions()$Measure[1]
    
    graphData <- select(filterGraph(),Date,Figure,Breakdown)%>%
      group_by(Date)%>%
      spread(Breakdown,Figure) %>%
      select(Date,series$Breakdown) %>%
      ungroup
    
    if(!is.null(isolate({input$plot_date_window}))) dateRange <- as.Date(isolate({input$plot_date_window[2]}))-as.Date(isolate({input$plot_date_window[1]})) else dateRange <- max(graphData$Date,na.rm=TRUE)-min(graphData$Date,na.rm=TRUE)
    axisFormats <- eefDygraphFormatter(graphData,intervalType=filterGraph()$intervalType[1],isolate({input$plot_date_window}),graphOptions$xFormat)
    graphData <- axisFormats$data
    ticker <- axisFormats$ticker
    axisLabelFormatter <- axisFormats$axisLabelFormatter
    valueFormatter <- axisFormats$valueFormatter
    
    xtsData <- xts(graphData[-1],graphData[[1]]) %>% padxts2
    
    dygraph(xtsData,main=graphTitle) %>%
      dyRangeSelector(height = 40, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
      dyOptions(titleHeight=40,rightGap = 25,strokeWidth = 3,colors=series$seriesColour,retainDateWindow = TRUE) %>%
      dyLegend(show = "follow",labelsSeparateLines = FALSE,width=150) %>%
      dyAxis("x",
             drawGrid=FALSE,
             axisLabelFormatter=axisLabelFormatter,
             valueFormatter=valueFormatter,
             ticker=ticker
      ) %>%
      dyAxis("y", drawGrid=FALSE,label= graphOptions$ylabel,valueRange=graphRange,valueFormatter = yFormat)%>%
      dyCallbacks(drawCallback = paste0("function(dygraph) {if($('#",ns("plot")," div').height()>0){DygraphExport.Register(dygraph);}}"))
  })

  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
}

#Graph package: googleVis
#Graph type: Line
#options: None
#Plots: Measure
#Scope: Measure (can be discrete)
lineChartServer0 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  colours <- distinct(filterData,Breakdown,.keep_all = TRUE) %>%
    select(Breakdown,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  defaultSelected <- graphOptions$defaultSelected
  ylabel <- graphOptions$ylabel
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(is.null(ylabel)) ylabel <- filterData$Measure[1]
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  
  #input options
  filterGraph <- filterData
  series <- left_join(select(filterGraph,-seriesColour),colours,by="Breakdown") %>%
    distinct(Breakdown,.keep_all = TRUE)

  #buttons
  if(!is.null(filterData)) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-lineChart0")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- filterGraph 
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- filterGraph
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterGraph,Indicator,Characteristic,Breakdown,Variable=Measure,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph)|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph)==0) return(NULL)
    
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph$Figure,0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    
    graphData <- filterGraph
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(colors=paste0("[",paste0("'",series$seriesColour,"'",collapse=", "),"]"),
                         height="400px",
                         title=graphTitle,
                         chartArea="{top:20,bottom:50}",
                         vAxis=paste0("{gridlines: {color: 'transparent'},titleTextStyle: {italic: false}, viewWindow: {min:",graphRange[1],", max:",graphRange[2],"}, title:'",ylabel,"'}"),
                         #                                vAxis=paste0("{titleTextStyle: {italic: false}, minValue:",graphRange[1],", maxValue:",graphRange[2],", title:'",ylabel,"'}"),
                         legend="{position:'top',maxLines:3}")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    graphData <- select(filterGraph,Measure,Figure,Breakdown)%>%
      group_by(Measure) %>%
      spread(Breakdown,Figure) %>% 
      #mutate(`% in relative poverty AHC.style`=switch(tolower(Breakdown),total="blue","red"))%>%
      gvisLineChart("Measure",
                    unique(filterGraph$Breakdown),
                    #paste0(unique(filterGraph$Breakdown),c("",".style")),
                    options=chartOptions) %>%
      gvisRegister(ns("plot"))
    
  })
  

}



#Graph package: googleVis
#Graph type: Line
#options: Breakdown
#Plots: Measure
#Scope: Measure (can be discrete)
lineChartServer1 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  colours <- distinct(filterData,Breakdown,.keep_all = TRUE) %>%
    select(Breakdown,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  defaultSelected <- graphOptions$defaultSelected
  ylabel <- graphOptions$ylabel
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(is.null(ylabel)) ylabel <- filterData$Measure[1]
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  
  
  #input options
  output$optionBreakdown <- renderUI({
    if(is.null(filterData)) return(NULL)
    optionsList <- unique(filterData$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  output$optionMeasure <- renderUI({
    if(is.null(filterData)) return(NULL)
    range <- unique(filterData$Measure)
    sliderInputLabels(ns("selectMeasure"),NULL,range,
                      value=c(range[1],range[length(range)]),
                      width="90%")
  })
  
  filterGraph <- reactive({
    if(is.null(filterData)) return(NULL)
    if(is.null(input$selectMeasure)) return(NULL)
    filter(filterData,
           Breakdown %in% input$selectBreakdown,
           Measure %in% unique(filterData$Measure)[(input$selectMeasure[1]+1):(input$selectMeasure[2]+1)]
    )
    
  })
  
  #buttons
  if(!is.null(filterData)) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-lineChart1")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterGraph(),Measure,Characteristic,Breakdown,Variable=Measure,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph()
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure,0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    
    series <- left_join(select(filterGraph(),-seriesColour),colours,by="Breakdown") %>%
      distinct(Breakdown,.keep_all = TRUE)
    
    graphData <- filterGraph()
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(colors=paste0("[",paste0("'",series$seriesColour,"'",collapse=", "),"]"),
                         height="400px",
                         title=graphTitle,
                         chartArea="{top:20,bottom:50}",
                         vAxis=paste0("{baseline: ",graphRange[1],", gridlines: {color: 'transparent'},titleTextStyle: {italic: false}, viewWindow: {min:",graphRange[1],", max:",graphRange[2],"}, title:'",ylabel,"'}"),
                         #                                vAxis=paste0("{titleTextStyle: {italic: false}, minValue:",graphRange[1],", maxValue:",graphRange[2],", title:'",ylabel,"'}"),
                         legend="{position:'top',maxLines:3}")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    graphData <- select(filterGraph(),Measure,Figure,Breakdown)%>%
      group_by(Measure) %>%
      spread(Breakdown,Figure) %>% 
      #mutate(`% in relative poverty AHC.style`=switch(tolower(Breakdown),total="blue","red"))%>%
      gvisLineChart("Measure",
                    unique(filterGraph()$Breakdown),
                    #paste0(unique(filterGraph$Breakdown),c("",".style")),
                    options=chartOptions) %>%
      gvisRegister(ns("plot"))
    
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
  
  
}


#Graph package: googleVis
#Graph type: Line
#options: Breakdown, Measure
#Plots: Measure
#Scope: Measure (can be discrete)
lineChartServer2 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  colours <- distinct(filterData,Breakdown,.keep_all = TRUE) %>%
    select(Breakdown,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  defaultSelected <- graphOptions$defaultSelected
  ylabel <- graphOptions$ylabel
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(is.null(ylabel)) ylabel <- filterData$Measure[1]
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  
  #input options
  output$optionMeasure <- renderUI({
    if(nrow(filterData)==0) return(NULL)
    selectInput(ns("selectMeasure"),NULL,
                choices = unique(filterData$Measure),selected=isolate(input$selectMeasure))
    
  })
  
  filterOptions <- reactive({
    if(is.null(filterData)|is.null(input$selectMeasure)) return(NULL)
    filter(filterData,Measure %in% input$selectMeasure)
  })
  
  output$optionBreakdown <- renderUI({
    if(is.null(filterOptions())) return(NULL)
    optionsList <- unique(filterOptions()$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  output$optionMeasure <- renderUI({
    if(is.null(filterOptions())) return(NULL)
    range <- unique(filterOptions()$Measure)
    sliderInputLabels(ns("selectMeasure"),NULL,range,
                      value=c(range[1],range[length(range)]),
                      width="90%")
  })
  
  filterGraph <- reactive({
    if(is.null(filterOptions())) return(NULL)
    if(is.null(input$selectMeasure)) return(NULL)
    filter(filterOptions(),
           Breakdown %in% input$selectBreakdown,
           Measure %in% unique(filterOptions()$Measure)[(input$selectMeasure[1]+1):(input$selectMeasure[2]+1)]
    )
  })
  
  #buttons
  if(!is.null(filterData)) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-lineChart2")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterGraph(),Measure,Characteristic,Breakdown,Variable=Measure,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph()
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure,0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    
    series <- left_join(select(filterGraph(),-seriesColour),colours,by="Breakdown") %>%
      distinct(Breakdown,.keep_all = TRUE)
    
    graphData <- filterGraph()
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(colors=paste0("[",paste0("'",series$seriesColour,"'",collapse=", "),"]"),
                         height="400px",
                         title=graphTitle,
                         #titlePosition="in",
                         #chartArea="{top:20,bottom:50}",
                         vAxis=paste0("{gridlines: {color: 'transparent'},titleTextStyle: {italic: false}, viewWindow: {min:",graphRange[1],", max:",graphRange[2],"}, title:'",ylabel,"'}"),
                         legend="{position:'top',maxLines:3}")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    graphData <- select(filterGraph(),Measure,Figure,Breakdown)%>%
      group_by(Measure) %>%
      spread(Breakdown,Figure) %>% 
      gvisLineChart("Measure",
                    unique(filterGraph()$Breakdown),
                    options=chartOptions) %>%
      gvisRegister(ns("plot"))
    return(graphData)
  })
  

  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
  
  
}

#Graph package: googleVis
#Graph type: Pie/Doughnut
#options: None
#Plots: Measure
pieChartServer0 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$pieHole)) pieHole <- 0.5 else pieHole <- graphOptions$pieHole
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  defaultSelected <- graphOptions$defaultSelected
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  filterData <- mutate(filterData,Breakdown=ifelse(is.na(Breakdown),Measure,Breakdown))
  
  #input options
  filterGraph <- filterData
  
  #buttons
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-pieChart0")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- filterData
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- filterData
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterData,Year,Measure,Characteristic,Breakdown,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterData
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot1 <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterData)) return(NULL)

    if(nrow(filterGraph)==0) return(NULL)
    
    graphData <- filterGraph
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(height="400px",
                         pieHole=pieHole,
                         title=graphTitle,
                         tooltip="{text:'percentage'}",
                         legend="{position:'right',maxLines:3}")
    seriesColours <- filterGraph$seriesColour
    
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(Breakdown=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- c("blue","very-light-blue")
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours,colourPalette),"'",collapse=", "),"]")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    
    select(filterGraph,Breakdown,Figure) %>%
      gvisPieChart("Breakdown","Figure",options=chartOptions)%>%
      gvisRegister(ns("plot"))
    
    
  })
  
  

}

#Graph package: googleVis
#Graph type: Pie/Doughnut
#options: Breakdown
#Plots: Measure
pieChartServer1 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$pieHole)) pieHole <- 0.5 else pieHole <- graphOptions$pieHole
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  defaultSelected <- graphOptions$defaultSelected
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  
  #input options
  output$optionBreakdown <- renderUI({
    if(nrow(filterData)==0) return(NULL)
    if(is.null(defaultSelected)) defaultSelected <- filterData$Breakdown[1]
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(filterData$Breakdown)
    if(length(selectedList)==0) selectedList <- defaultSelected
    selectInput(ns("selectBreakdown"),NULL,
                choices = unique(filterData$Breakdown),selected=selectedList)
    
  })
  
  #buttons
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-pieChart1")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- filterData
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- filterData
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterData,Year,Measure,Characteristic,Breakdown,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filter(filterData,Breakdown %in% input$selectBreakdown)
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot1 <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterData)) return(NULL)
    
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown,
                          Measure %in% unique(filterData$Measure)
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    graphData <- filterGraph
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(height="400px",
                         pieHole=pieHole,
                         title=graphTitle,
                         tooltip="{text:'percentage'}",
                         legend="{position:'right',maxLines:3}")
    seriesColours <- filterGraph$seriesColour
    
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(Measure=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- c("blue","very-light-blue")
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours,colourPalette),"'",collapse=", "),"]")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    
    select(filterGraph,Measure,Figure) %>%
      gvisPieChart("Measure","Figure",options=chartOptions)%>%
      gvisRegister(ns("plot"))
    
    
  })
  
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
}

#Graph package: googleVis
#Graph type: Pie/Doughnut Array
#options: Breakdown
#Plots: Measure
pieChartServer2 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$pieHole)) pieHole <- 0.5 else pieHole <- graphOptions$pieHole
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  defaultSelected <- graphOptions$defaultSelected
  digits <- graphOptions$digits
  icon <- graphOptions$icon
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  filterData <- mutate(filterData,Measure=ifelse(is.na(Measure),Measure,Measure))
  
  #input options
  output$optionBreakdown <- renderUI({
    if(nrow(filterData)==0) return(NULL)
    if(is.null(defaultSelected)) defaultSelected <- filterData$Breakdown[1]
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(filterData$Breakdown)
    if(length(selectedList)==0) selectedList <- defaultSelected
    selectInput(ns("selectBreakdown"),NULL,
                choices = unique(filterData$Breakdown),selected=selectedList)
    
  })
  
  #buttons
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-pieChart2")
  })
  
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- filterData
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterData,Year,Measure,Characteristic,Breakdown,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot1 <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterData)) return(NULL)
    
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown,
                          Measure %in% unique(filterData$Measure)[1]
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    chartOptions <- list(height="300px",
                         pieHole=pieHole,
                         tooltip="{text:'percentage'}",
                         legend="{position:'top',maxLines:3}")
    seriesColours <- filterGraph$seriesColour
    
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(Measure=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$title <- unique(filterData$Measure)[1]
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- c("blue","very-light-blue")
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours,colourPalette),"'",collapse=", "),"]")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    
    select(filterGraph,Measure,Figure) %>%
      gvisPieChart("Measure","Figure",options=chartOptions)
  })
  
  output$plot2 <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterData)) return(NULL)
    if(is.na(unique(filterData$Measure)[2])) return(NULL)
    
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown,
                          Measure %in% unique(filterData$Measure)[2]
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    chartOptions <- list(height="300px",
                         pieHole=pieHole,
                         tooltip="{text:'percentage'}",
                         legend="{position:'top',maxLines:3}")
    
    seriesColours <- filterGraph$seriesColour
    
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(Measure=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$title <- unique(filterData$Measure)[2]
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- c("blue","very-light-blue")
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours,colourPalette),"'",collapse=", "),"]")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    select(filterGraph,Measure,Figure) %>%
      gvisPieChart("Measure","Figure",options=chartOptions)
  })
  
  output$plot3 <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterData)) return(NULL)
    if(is.na(unique(filterData$Measure)[3])) return(NULL)
    
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown,
                          Measure %in% unique(filterData$Measure)[3]
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    chartOptions <- list(height="300px",
                         pieHole=pieHole,
                         tooltip="{text:'percentage'}",
                         legend="{position:'top',maxLines:3}")
    seriesColours <- filterGraph$seriesColour
    
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(Measure=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$title <- unique(filterData$Measure)[3]
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- c("blue","very-light-blue")
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours,colourPalette),"'",collapse=", "),"]")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    
    select(filterGraph,Measure,Figure) %>%
      gvisPieChart("Measure","Figure",options=chartOptions)
  })
  
  output$plot4 <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterData)) return(NULL)
    if(is.na(unique(filterData$Measure)[4])) return(NULL)
    
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown,
                          Measure %in% unique(filterData$Measure)[4]
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    chartOptions <- list(height="300px",
                         pieHole=pieHole,
                         tooltip="{text:'percentage'}",
                         legend="{position:'top',maxLines:3}")
    seriesColours <- filterGraph$seriesColour
    
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(Measure=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$title <- unique(filterData$Measure)[4]
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- c("blue","very-light-blue")
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours,colourPalette),"'",collapse=", "),"]")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    select(filterGraph,Measure,Figure) %>%
      gvisPieChart("Measure","Figure",options=chartOptions)
  })
  
  #graph div template - n.b. column1-3 will only show in seperate columns on smaller screens
  output$graph <- renderUI({
    icon1 <- NULL
    icon2 <- NULL
    icon3 <- NULL
    icon4 <- NULL
    if(!is.null(icon) & pieHole>=0.5) {
      if(!is.na(icon[1])) icon1 <- div(class="w3-center eef-pie-chart-overlay",img(src=icon[1],height=60,width=60))
      if(!is.na(icon[2])) icon2 <- div(class="w3-center eef-pie-chart-overlay",img(src=icon[2],height=60,width=60))
      if(!is.na(icon[3])) icon3 <- div(class="w3-center eef-pie-chart-overlay",img(src=icon[3],height=60,width=60))
      if(!is.na(icon[4])) icon4 <- div(class="w3-center eef-pie-chart-overlay",img(src=icon[4],height=60,width=60))
    }
    
    eefGraph(ns,
             graph=div(class="eef-clearfix",
                       div(class="float eef-pie-chart-container",uiOutput(ns("plot1"),width=300, height = 300),icon1),
                       div(class="float eef-pie-chart-container",uiOutput(ns("plot2"),width=300, height = 300),icon2),
                       div(class="float eef-pie-chart-container",uiOutput(ns("plot3"),width=300, height = 300),icon3),
                       div(class="float eef-pie-chart-container",uiOutput(ns("plot4"),width=300, height = 300),icon4)),
             column1=uiOutput(ns("optionBreakdown"))
    )
  })
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filter(filterData,Breakdown %in% input$selectBreakdown)
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
}

#Graph package: googleVis
#Graph type: Bar
#options: None
#Plots: Measure
#Scope: Latest year
barChartServer0 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$sortOrder)) sortOrder <- "decreasing" else sortOrder <- graphOptions$sortOrder
  if(is.null(graphOptions$sortTop)) sortTop <- "\\b((total)|(all))\\b" else sortTop <- graphOptions$sortTop
  if(is.null(graphOptions$sortBottom)) sortBottom <- "^$" else sortBottom <- graphOptions$sortBottom
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  defaultSelected <- graphOptions$defaultSelected
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits))  filterData <- mutate(filterData,Figure=round(Figure,digits))
  colours <- distinct(filterData,Measure,.keep_all = TRUE) %>%
    select(Measure,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  
  #initial filter to select only the data needed for the graph
  
  #input options
  filterGraph <- filterData
  series <- left_join(select(filterGraph,-seriesColour),colours,by="Measure") %>%
    distinct(Measure,.keep_all = TRUE)
  
  #buttons
  if(!gvisOptions$isStacked) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  if(gvisOptions$isStacked) {
    stackData <- group_by(filterData,Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
    maxRange <- ifelse(is.na(maxZoom),padRange(c(0,stackData$Figure),padding=0.25),maxZoom)
  }
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-barChart0")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- filterGraph
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- filterGraph
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterGraph,Year,Measure,Characteristic,Breakdown,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph)|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph)==0) return(NULL)
    
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph$Figure,0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    
    graphData <- filterGraph
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(height="400px",
                         chartArea="{left: '25%', width: '100%'}",
                         colors=paste0("[",paste0("'",series$seriesColour,"'",collapse=", "),"]"),
                         title=graphTitle,
                     legend="{position:'top',maxLines:3}",
                     hAxis=paste0("{gridlines: {color: 'transparent'}, baseline:",graphRange[1],", viewWindow: {min:",graphRange[1],", max:",graphRange[2],"}}"))
    
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    graphData <- select(filterGraph,Figure,Breakdown,Measure)%>%
      group_by(Breakdown) %>%
      spread(Measure,Figure)%>%
      ungroup
    
    if(gvisOptions$isStacked) graphData <- mutate(graphData,total=rowSums(select(graphData,series$Measure))) %>%
      arrangeBreakdown("total",sortOrder,top=sortTop,bottom=sortBottom)
    if(!gvisOptions$isStacked) graphData <- arrangeBreakdown(graphData,2,sortOrder,top=sortTop,bottom=sortBottom)
    
    gvisBarChart(graphData,"Breakdown",
                   series$Measure,
                   #paste0(unique(filterGraph$Measure),c("",".style")),
                   options=chartOptions) %>%
      gvisRegister(ns("plot"))
    
  })
  
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("column1")#toggle column1 
  })
  
  
}

#Graph package: googleVis
#Graph type: Bar
#options: Breakdown
#Plots: Measure
#Scope: Latest year
barChartServer1 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$sortOrder)) sortOrder <- "decreasing" else sortOrder <- graphOptions$sortOrder
  if(is.null(graphOptions$sortOrder)) sortOrder <- "decreasing" else sortOrder <- graphOptions$sortOrder
  if(is.null(graphOptions$sortTop)) sortTop <- "\\b((total)|(all))\\b" else sortTop <- graphOptions$sortTop
  if(is.null(graphOptions$sortBottom)) sortBottom <- "^$" else sortBottom <- graphOptions$sortBottom
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  defaultSelected <- graphOptions$defaultSelected
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  colours <- distinct(filterData,Measure,.keep_all = TRUE) %>%
    select(Measure,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  
  #initial filter to select only the data needed for the graph
  #input options
  output$optionBreakdown <- renderUI({
    if(is.null(filterData)) return(NULL)
    optionsList <- unique(filterData$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  filterGraph <- reactive({
    if(is.null(filterData)) return(NULL)
    filter(filterData,
           Breakdown %in% input$selectBreakdown
    )
  }) 
  
  
  #buttons
  if(!gvisOptions$isStacked) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  if(gvisOptions$isStacked) {
    stackData <- group_by(filterData,Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
    maxRange <- ifelse(is.na(maxZoom),padRange(c(0,stackData$Figure),padding=0.25),maxZoom)
  }
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-barChart1")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterGraph(),Year,Measure,Characteristic,Breakdown,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph()
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else if(!gvisOptions$isStacked) {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure,0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    } else {
      stackData <- group_by(filterGraph(),Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
      minRange <- ifelse(is.na(minZoom),padRange(c(0,stackData$Figure),0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    
    series <- left_join(select(filterGraph(),-seriesColour),colours,by="Measure") %>%
      distinct(Measure,.keep_all = TRUE)
    
    graphData <- filterGraph()
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(chartArea="{left: '25%', width: '100%'}",
                         height="400px",
                         title=graphTitle,
                         colors=paste0("[",paste0("'",series$seriesColour,"'",collapse=", "),"]"),
                         legend="{position:'top',maxLines:3}",
                         hAxis=paste0("{gridlines: {color: 'transparent'}, baseline:",graphRange[1],", viewWindow: {min:",graphRange[1],", max:",graphRange[2],"}}")
                         )
    
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    graphData <- select(filterGraph(),Figure,Breakdown,Measure)%>%
      group_by(Breakdown) %>%
      spread(Measure,Figure)%>%
      ungroup
    
    if(gvisOptions$isStacked) graphData <- mutate(graphData,total=rowSums(select(graphData,series$Measure))) %>%
      arrangeBreakdown("total",sortOrder,top=sortTop,bottom=sortBottom)
    if(!gvisOptions$isStacked) graphData <- arrangeBreakdown(graphData,2,sortOrder,top=sortTop,bottom=sortBottom)
    
    gvisBarChart(graphData,
                 "Breakdown",
                  series$Measure,
                  #paste0(unique(filterGraph$Measure),c("",".style")),
                   options=chartOptions) %>%
      gvisRegister(ns("plot"))
    
  })
  
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
  
  
}

#Graph package: googleVis
#Graph type: Bar
#options: Breakdown, Measure
#Plots: Measure
#Scope: Latest year
barChartServer2 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$sortOrder)) sortOrder <- "decreasing" else sortOrder <- graphOptions$sortOrder
  if(is.null(graphOptions$sortTop)) sortTop <- "\\b((total)|(all))\\b" else sortTop <- graphOptions$sortTop
  if(is.null(graphOptions$sortBottom)) sortBottom <- "^$" else sortBottom <- graphOptions$sortBottom
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  defaultSelected <- graphOptions$defaultSelected
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  colours <- distinct(filterData,Measure,.keep_all = TRUE) %>%
    select(Measure,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  
  
  output$optionMeasure <- renderUI({
    if(nrow(filterData)==0) return(NULL)
    selectInput(ns("selectMeasure"),NULL,
                choices = unique(filterData$Measure),selected=isolate(input$selectMeasure))
    
  })
  
  filterOptions <- reactive({
    if(is.null(filterData)|is.null(input$selectMeasure)) return(NULL)
    filter(filterData,Measure %in% input$selectMeasure)
  })
  
  output$optionBreakdown <- renderUI({
    if(is.null(filterOptions())) return(NULL)
    optionsList <- unique(filterOptions()$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  filterGraph <- reactive({
    if(is.null(filterOptions())) return(NULL)
    filter(filterOptions(),
           Breakdown %in% input$selectBreakdown
           )
  }) 
  
  #buttons
  if(!gvisOptions$isStacked) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  if(gvisOptions$isStacked) {
    stackData <- select(filterData,Breakdown,Measure,Figure) %>% group_by(Breakdown,Measure) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
    maxRange <- ifelse(is.na(maxZoom),padRange(c(0,stackData$Figure),padding=0.25),maxZoom)
  }
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-barChart2")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterGraph(),Year,Measure,Characteristic,Breakdown,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph()
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else if(!gvisOptions$isStacked) {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure,0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    } else {
      stackData <- group_by(filterGraph(),Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
      minRange <- ifelse(is.na(minZoom),padRange(c(0,stackData$Figure),0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    
    series <- left_join(select(filterGraph(),-seriesColour),colours,by="Measure") %>%
      distinct(Measure,.keep_all = TRUE)
    
    graphData <- filterGraph()
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(colors=paste0("[",paste0("'",series$seriesColour,"'",collapse=", "),"]"),
                         height="400px",
                         title=graphTitle,
                         chartArea="{left: '25%', width: '100%', bottom:50}",
                         legend="{position:'top',maxLines:3}",
                         hAxis=paste0("{gridlines: {color: 'transparent'}, baseline:",graphRange[1],", viewWindow: {min:",graphRange[1],", max:",graphRange[2],"}}"))
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    graphData <- select(filterGraph(),Figure,Breakdown,Measure)%>%
      group_by(Breakdown) %>%
      spread(Measure,Figure) %>%
      ungroup
    
    if(gvisOptions$isStacked) graphData <- mutate(graphData,total=rowSums(select(graphData,series$Measure))) %>%
      arrangeBreakdown("total",sortOrder,top=sortTop,bottom=sortBottom)
    if(!gvisOptions$isStacked) graphData <- arrangeBreakdown(graphData,2,sortOrder,top=sortTop,bottom=sortBottom)
    
    gvisBarChart(graphData,
                 "Breakdown",
                 series$Measure
                 ,options=chartOptions) %>%
      gvisRegister(ns("plot"))
    
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
}

#Graph package: googleVis
#Graph type: Bar
#options: Breakdown, Measure
#Plots: Measure
#Scope: Latest year
barChartServer3 <- function(input,output,session,filterData=NULL,graphOptions=list()) {
  if(is.null(filterData)) return(NULL)
  if(nrow(filterData)==0) return(NULL)
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$sortOrder)) sortOrder <- "decreasing" else sortOrder <- graphOptions$sortOrder
  if(is.null(graphOptions$sortTop)) sortTop <- "\\b((total)|(all))\\b" else sortTop <- graphOptions$sortTop
  if(is.null(graphOptions$sortBottom)) sortBottom <- "^$" else sortBottom <- graphOptions$sortBottom
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  defaultSelected <- graphOptions$defaultSelected
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  ns <- session$ns
  
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  colours <- distinct(filterData,Measure,.keep_all = TRUE) %>%
    select(Measure,seriesColour) %>%
    mutate(seriesColour=eefColours(seriesColour,colourPalette))
  
  
  output$optionBreakdown <- renderUI({
    if(is.null(filterData)) return(NULL)
    optionsList <- unique(filterData$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  filterOptions <- reactive({
    if(is.null(filterData)|is.null(input$selectBreakdown)) return(NULL)
    filter(filterData,Breakdown %in% input$selectBreakdown)
  })
  
  output$optionMeasure <- renderUI({
    if(is.null(filterOptions())) return(NULL)
    optionsList <- unique(filterOptions()$Measure)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectMeasure}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectMeasure"), NULL,
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  filterGraph <- reactive({
    if(is.null(filterOptions())) return(NULL)
    filter(filterOptions(),
           Measure %in% input$selectMeasure
    )
  }) 
  
  #buttons
  if(!gvisOptions$isStacked) maxRange <- ifelse(is.na(maxZoom),padRange(c(0,filterData$Figure),padding=0.25),maxZoom)
  if(gvisOptions$isStacked) {
    stackData <- select(filterData,Breakdown,Figure) %>% group_by(Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
    maxRange <- ifelse(is.na(maxZoom),padRange(c(0,stackData$Figure),padding=0.25),maxZoom)
  }
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-barChart3")
  })
  
  output$downloadChart <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Chart.png")
    },
    content=function(file) {
      output <- file(file, "wb")
      on.exit(close(output))
      io <- .Call(base64enc:::B64_decode, sub("^[^,]+,","",input$plotURI))
      writeBin(io, output)
    }
  )
  outputOptions(output,"downloadChart",suspendWhenHidden=FALSE)
  output$downloadData <- downloadHandler(
    filename= function() {
      graphData <- isolate({filterGraph()})
      paste0(knit(text=panelData$subtitle,quiet=TRUE),"_Data.csv")
    },
    content=function(file) {
      select(filterGraph(),Year,Measure,Characteristic,Breakdown,Value=Figure) %>%
        write.csv(file,row.names=FALSE)
    }
  )
  
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      graphData <- filterGraph()
      knit(panelData$inputMarkdownFile, quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(input$reload)) tmp <- 1 #this is only to add a dependency between output$plot and input$reload
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    if(zoomLevel()==0) {
      graphRange <- maxRange
    } else if(!gvisOptions$isStacked) {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure,0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    } else {
      stackData <- group_by(filterGraph(),Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
      minRange <- ifelse(is.na(minZoom),padRange(c(0,stackData$Figure),0.1),minZoom)
      graphRange <- maxRange - (maxRange-minRange)*zoomLevel()/nZoomLevels
    }
    
    series <- left_join(select(filterGraph(),-seriesColour),colours,by="Measure") %>%
      distinct(Measure,.keep_all = TRUE)
    
    graphData <- filterGraph()
    graphTitle <- knit(text=panelData$subtitle,quiet=TRUE)
    
    chartOptions <- list(colors=paste0("[",paste0("'",series$seriesColour,"'",collapse=", "),"]"),
                         height="400px",
                         title=graphTitle,
                         chartArea="{left: '25%', width: '100%', bottom:50}",
                         legend="{position:'top',maxLines:3}",
                         hAxis=paste0("{gridlines: {color: 'transparent'}, baseline:",graphRange[1],", viewWindow: {min:",graphRange[1],", max:",graphRange[2],"}}"))
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    graphData <- select(filterGraph(),Figure,Breakdown,Measure)%>%
      group_by(Breakdown) %>%
      spread(Measure,Figure) %>%
      ungroup
    
    if(gvisOptions$isStacked) graphData <- mutate(graphData,total=rowSums(select(graphData,series$Measure))) %>%
      arrangeBreakdown("total",sortOrder,top=sortTop,bottom=sortBottom)
    if(!gvisOptions$isStacked) graphData <- arrangeBreakdown(graphData,2,sortOrder,top=sortTop,bottom=sortBottom)
    
    gvisBarChart(graphData,
                 "Breakdown",
                 series$Measure
                 ,options=chartOptions) %>%
      gvisRegister(ns("plot"))
    
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
}
