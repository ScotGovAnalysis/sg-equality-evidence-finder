rmdServer <- function(input,output,session,filterGraph,graphOptions) {
  if(!is.null(graphOptions$updateRmd)) {
    output$markdown <- renderUI({
      if(is.null(filterGraph())) return(NULL)
      graphData <- filterGraph()
      panelData <- graphOptions$panelData
      if(is.na(panelData$inputMarkdownFile)) return(NULL)
      knit(text=readLines(panelData$inputMarkdownFile,warn=FALSE,encoding="UTF-8"), quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(text=.,fragment.only=TRUE,encoding="UTF-8") %>%
        HTML
    })
  }
  
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

dygraphServer <- function(input,output,session,filteredGraph=reactive({NULL}),graphOptions=list(),maxRange=reactive({c(NA,NA)}),reload=reactive({NULL})) {
  ns <- session$ns
  
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  if(!is.null(graphOptions$digits)) yFormat <- NULL else yFormat <- jsFormatCommas(graphOptions$digits)
  #yFormat <- jsFormatCommas(graphOptions$digits)
  #id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(is.null(graphOptions$digits)) graphOptions$digits <- 2
  
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderDygraph({
    if(!is.null(reload())) tmp <- 1 #this is only to add a dependency between output$plot and reload()
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filteredGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filteredGraph())==0) return(NULL)
    
    graphData <- filteredGraph()
    
    #if(!is.null(panelData$subtitle)) graphTitle <- knit(text=panelData$subtitle,quiet=TRUE) else graphTitle <- filteredGraph()$graphTitle[1]
    if(zoomLevel()==0) {
      graphRange <- maxRange()  
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filteredGraph()$Figure),minZoom)
      graphRange <- maxRange() - (maxRange()-minRange)*zoomLevel()/nZoomLevels
    }
    series <- filteredGraph() %>%
      distinct(Breakdown,.keep_all = TRUE)
    
    if("annotationText"%in%names(filteredGraph())) annotations <- filter(filteredGraph(),!is.na(annotationText)) else annotations <- data.frame()

    graphData <- select(filteredGraph(),Date,Figure,Breakdown)%>%
      group_by(Date)%>%
      spread(Breakdown,Figure) %>%
      select(Date,series$Breakdown) %>%
      ungroup
    
    dateRange <- max(graphData$Date,na.rm=TRUE)-min(graphData$Date,na.rm=TRUE)
    if(dateRange==0) drawPoints <- TRUE else drawPoints <- FALSE
    
    #axisFormats <- eefDygraphFormatter(graphData,intervalType=filteredGraph()$intervalType[1],isolate({input$plot_date_window}),graphOptions$xFormat)
    axisFormats <- eefDygraphFormatter(graphData,intervalType=filteredGraph()$intervalType[1],NULL,graphOptions$xFormat)
    graphData <- axisFormats$data
    ticker <- axisFormats$ticker
    axisLabelFormatter <- axisFormats$axisLabelFormatter
    valueFormatter <- axisFormats$valueFormatter
    
    xtsData <- xts(graphData[-1],graphData[[1]]) %>% padxts2
    
    plot <- dygraph(xtsData,main=filteredGraph()$graphTitle[1]) %>%
      dyRangeSelector(height = 40, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
      dyOptions(maxNumberWidth=7,digitsAfterDecimal=graphOptions$digits,drawAxesAtZero=TRUE,titleHeight=40,drawPoints=drawPoints,pointSize=5,rightGap = 25,strokeWidth = 2,colors=series$seriesColour,retainDateWindow = TRUE) %>%
      #dyLegend(labelsDiv = ns("legend"),show = "always",labelsSeparateLines=TRUE) %>%
      dyLegend(show = "follow",labelsSeparateLines = FALSE,width=150) %>%
      dyAxis("x",
             drawGrid=FALSE,
             axisLabelFormatter=axisLabelFormatter,
             valueFormatter=valueFormatter,
             ticker=ticker
      ) %>%
      dyAxis("y", drawGrid=FALSE,label= filteredGraph()$yLabel[1],valueRange=graphRange,valueFormatter = yFormat)
    
    
    #To add dygraph "events" (vertical lines) shading and annotations
    #These are read from the graphOptions list for the particular panel and should be a list of the form
    #dyEvent=list(list(<PARAMETERS FOR EVENT 1>),list(<PARAMETERS FOR EVENT 2>),...)
    #e.g. graphOptions[["incpov-23"]] = list(dyEvent=list(list(x=as.Date("2008-01-01"),label="Financial Crisis")))
    if(!is.null(graphOptions$dyEvent)) 
      for(i in 1:length(graphOptions$dyEvent)) 
        plot <- do.call(function(...) {dyEvent(plot,...)},graphOptions$dyEvent[[i]])
    if(!is.null(graphOptions$dyShading)) 
      for(i in 1:length(graphOptions$dyShading)) 
        plot <- do.call(function(...) {dyShading(plot,...)},graphOptions$dyShading[[i]])
    if(!is.null(graphOptions$dyAnnotation)) 
      for(i in 1:length(graphOptions$dyAnnotation)) 
        plot <- do.call(function(...) {dyAnnotation(plot,...)},graphOptions$dyAnnotation[[i]])
    
    if(nrow(annotations)>0) for(r in 1:nrow(annotations)) {
      plot <- plot %>% dyEvent(annotations$Date[r])
      plot <- dyAnnotation(plot,annotations$Date[r],annotations$annotationText[r],
                           tooltip=annotations$annotationText[r],
                           series=annotations$Breakdown[r],
                           width=16+7*nchar(annotations$annotationText[r]),
                           height=25,attachAtBottom = T)
    }
    plot %>%
      dyCallbacks(drawCallback = paste0("function(dygraph) {if($('#",ns("plot")," div').height()>0){DygraphExport.Register(dygraph);}}"))
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
  })
  
}

pieChartServer <- function(input,output,session,filterGraph=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$pieHole)) pieHole <- 0.5 else pieHole <- graphOptions$pieHole
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(reload())) tmp <- 1 #this is only to add a dependency between output$plot and reload()
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    
    graphData <- filterGraph()
    
    chartOptions <- list(height="400px",
                         pieHole=pieHole,
                         title=filterGraph()$graphTitle[1],
                         tooltip="{text:'percentage'}",
                         legend="{position:'right',maxLines:3}")
    if(graphOptions$graphType=="pieChart2") chartOptions$height="300px"
    seriesColours <- graphData$seriesColour
    
    if(nrow(graphData)==1) {
      graphData <- bind_rows(graphData,data.frame(Breakdown=NA,Figure=(100-graphData$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- c("blue","very-light-blue") %>% eefColours()
    }  
    chartOptions$colors <- paste0("[",paste0("'",seriesColours,"'",collapse=", "),"]")
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    
    select(graphData,Breakdown,Figure) %>%
      gvisPieChart("Breakdown","Figure",options=chartOptions)%>%
      gvisRegister(ns("plot"))
  })
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
  })
}

barChartServer <- function(input,output,session,filterGraph=reactive({NULL}),graphOptions=list(),maxRange=reactive({c(NA,NA)}),reload=reactive(NULL)) {
  ns <- session$ns
  
  panelData <- graphOptions$panelData
  if(is.null(graphOptions$sortOrder)) sortOrder <- "decreasing" else sortOrder <- graphOptions$sortOrder
  if(is.null(graphOptions$sortTop)) sortTop <- "\\b((total)|(all))\\b" else sortTop <- graphOptions$sortTop
  if(is.null(graphOptions$sortBottom)) sortBottom <- "^$" else sortBottom <- graphOptions$sortBottom
  
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  if(is.null(graphOptions$minZoom)) minZoom <- c(NA,NA) else minZoom <- graphOptions$minZoom
  if(is.null(graphOptions$nZoomLevels)) nZoomLevels <- 2 else nZoomLevels <- graphOptions$nZoomLevels
  digits <- graphOptions$digits
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  
  
  zoomLevel <- reactiveVal(0)
  shinyjs::onclick("zoomIn", {
    if(zoomLevel()<nZoomLevels) zoomLevel(zoomLevel()+1)
  })
  shinyjs::onclick("zoomOut", {
    if(zoomLevel()>0) zoomLevel(zoomLevel()-1)
  })
  
  #plot graph
  redraw <- reactiveVal(0)
  output$plot <- renderGvis({
    if(!is.null(reload())) tmp <- 1 #this is only to add a dependency between output$plot and reload()
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())|is.null(zoomLevel())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    
    if(zoomLevel()==0) {
      graphRange <- maxRange()
    } else {
      minRange <- ifelse(is.na(minZoom),padRange(filterGraph()$Figure,0.1),minZoom)
      graphRange <- maxRange() - (maxRange()-minRange)*zoomLevel()/nZoomLevels
    }
    if(!is.null(graphOptions$gvisOptions$isStacked)) if(graphOptions$gvisOptions$isStacked %in% c("relative","percent")) graphRange <- c(0,1)

    
    graphData <- filterGraph()
    series <- filterGraph() %>%
      distinct(Measure,.keep_all = TRUE)
    
    chartOptions <- list(height="400px",
                         chartArea="{left: '25%', width: '100%'}",
                         colors=paste0("[",paste0("'",series$seriesColour,"'",collapse=", "),"]"),
                         title=filterGraph()$graphTitle[1],
                         legend="{position:'top',maxLines:3}",
                         hAxis=paste0("{gridlines: {color: 'transparent'}, baseline:",graphRange[1],", viewWindow: {min:",graphRange[1],", max:",graphRange[2],"}}"))
    
    chartOptions <- modifyList(chartOptions,gvisOptions)
    
    graphData <- select(filterGraph(),Figure,Breakdown,Measure)%>%
      group_by(Breakdown) %>%
      spread(Measure,Figure)%>%
      ungroup %>%
      inner_join(distinct(filterGraph(),Breakdown),.,by="Breakdown")
    
    if(gvisOptions$isStacked %in% TRUE) {
      graphData <- mutate(graphData,total=rowSums(select(graphData,series$Measure))) %>%
        arrangeBreakdown("total",sortOrder,top=sortTop,bottom=sortBottom)
    } else if(gvisOptions$isStacked %in% c("percent","relative")) {
      graphData <- mutate(graphData,pct=.data[[series$Measure[1]]]/rowSums(select(graphData,series$Measure))) %>%
        arrangeBreakdown("pct",sortOrder,top=sortTop,bottom=sortBottom)
    } else {
      graphData <- arrangeBreakdown(graphData,2,sortOrder,top=sortTop,bottom=sortBottom)
    }
    
    gvisBarChart(graphData,"Breakdown",
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
  })
}



dataTableServer <- function(input,output,session,filterGraph=reactive({NULL})) {
  ns <- session$ns
  redraw <- reactiveVal(0)
  output$Dashboardtable <- renderDataTable({
    #if(!is.null(reload())) tmp <- 1 #this is only to add a dependency between output$plot and reload()
    if(!is.null(redraw())) tmp <- 1 #this is only to add a dependency between output$plot and redraw()
    if(is.null(filterGraph())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    out <- select(filterGraph(),Outcome,Indicator,Disaggregation=Measure,Breakdown,DateCode,Figure)
  })
  shinyjs::onclick("toggledash2", {
    shinyjs::toggle("displayOptions")
    shinyjs::toggleClass("displayPlot","col-sm-9")#toggle width between 75% and 100%
    shinyjs::toggleClass("displayPlot","col-sm-12")#toggle width between 75% and 100%
    redraw(redraw()+1)
    #shinyjs::toggle("column1")
  })
  
}
####WRAPPER MODULES####


timeSeriesServer0 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  #help
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries0")
  })
  
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    if(nrow(filterData())==0) return(NULL)
    distinct(filterData(),Breakdown,.keep_all = TRUE) %>%
      select(Breakdown,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })  
  
  
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  maxRange <- reactive({
    ifelse(is.na(maxZoom),padRange(c(0,filterData()$Figure),padding=0.25),maxZoom)
  })
  
  filterGraph <- reactive({
    left_join(select(filterData(),-seriesColour),colours(),by="Breakdown")
  })
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(dygraphServer,"chart",filterGraph,graphOptions,maxRange,reload)
  
}

timeSeriesServer1 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  #help
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries1")
  })
  
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    if(nrow(filterData())==0) return(NULL)
    distinct(filterData(),Breakdown,.keep_all = TRUE) %>%
      select(Breakdown,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })
  
  defaultSelected <- graphOptions$defaultSelected
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  maxRange <- reactive({
    ifelse(is.na(maxZoom),padRange(c(0,filterData()$Figure),padding=0.25),maxZoom)
  })
  
  output$optionBreakdown <- renderUI({
    if(is.null(filterData())) return(NULL)
    optionsList <- unique(filterData()$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=defaultSelected)
    
  })
  
  filterGraph <- reactive({
    graphData <- filterData()
    filter(graphData,
           Breakdown %in% input$selectBreakdown
    ) %>% select(-seriesColour) %>%
      left_join(colours(),by="Breakdown")
    
  })
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(dygraphServer,"chart",filterGraph,graphOptions,maxRange,reload)
  
}

timeSeriesServer2 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  #help
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries2")
  })
  
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    distinct(filterData(),Breakdown,.keep_all = TRUE) %>%
      select(Breakdown,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })
  
  defaultSelected <- graphOptions$defaultSelected
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  maxRange <- reactive({
    if(identical(graphOptions$rescale,TRUE)) {
      ifelse(is.na(maxZoom),padRange(c(0,filterOptions()$Figure),padding=0.25),maxZoom)
    } else {
      ifelse(is.na(maxZoom),padRange(c(0,filterData()$Figure),padding=0.25),maxZoom)
    }
  })
  
  #select inputs
  output$optionMeasure <- renderUI({
    if(nrow(filterData())==0) return(NULL)
    selectInput(ns("selectMeasure"),NULL,
                choices = unique(filterData()$Measure),selected=isolate(input$selectMeasure))
    
  })
  
  filterOptions <- reactive({
    if(is.null(filterData())|is.null(input$selectMeasure)) return(NULL)
    filter(filterData(),Measure %in% input$selectMeasure)
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
                       selected=defaultSelected)
    
  })
  filterGraph <- reactive({
    if(is.null(filterOptions())) return(NULL)
    filter(filterOptions(),
           Breakdown %in% input$selectBreakdown
    ) %>% select(-seriesColour) %>%
      left_join(colours(),by="Breakdown")
  })
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(dygraphServer,"chart",filterGraph,graphOptions,maxRange,reload)
  
}

npfServer3 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),filterChar=reactive({NULL})) {
  ns <- session$ns
  
  
  #help
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-timeSeries2")
  })
  
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  
  filterOptions0 <- reactive({
    if(is.null(filterData())|is.null(filterChar())) return(filterData())
    if(filterChar()=="overview") {
      return(filterData())
    } else {
      return(filter(filterData(),Characteristic %in% equalityLabel(filterChar()) | Characteristic2 %in% equalityLabel(filterChar())))
    }
  })
  
  output$Dashboardoutcomes <- renderUI({
    if(is.null(filterOptions0())) return(NULL)
    if(nrow(filterOptions0())==0) return(NULL)
    selectInput(ns("Dashboardoutcome"),"Select Outcome",
                choices = c("All",unique(filterOptions0()$Outcome)%>%sort),
                selected=isolate(input$Dashboardoutcome),
                selectize = FALSE)
    
  })
  
  filterOptions1 <- reactive({
    if(is.null(filterOptions0())|is.null(input$Dashboardoutcome)) return(NULL)
    if(input$Dashboardoutcome=="All") filterOptions0() else filter(filterOptions0(),Outcome %in% input$Dashboardoutcome)
  })
  
  output$Dashboardindicators <- renderUI({
    if(is.null(filterOptions1())) return(NULL)
    if(nrow(filterOptions1())==0) return(NULL)
    selectInput(ns("Dashboardindicator"),"Select Indicator",
                choices = sort(unique(filterOptions1()$Indicator)),
                selected=isolate(input$Dashboardindicator),
                selectize = FALSE)
  })
  
  filterOptions2 <- reactive({
    if(is.null(filterOptions1())|is.null(input$Dashboardindicator)) return(NULL)
    filter(filterOptions1(),Indicator %in% input$Dashboardindicator)
  })
  
  output$Dashboardoptions <- renderUI({
    if(is.null(filterOptions2())) return(NULL)
    if(nrow(filterOptions2())==0) return(NULL)
    optionsList <- unique(filterOptions2()$Measure)
    selectedList <- isolate({input$Dashboardoption}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- "Total"
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    selectInput(ns("Dashboardoption"),"Select Breakdown",
                choices = optionsList,selected=selectedList,
                selectize = FALSE)
  })
  
  filterOptions3 <- reactive({
    if(is.null(filterOptions2())|is.null(input$Dashboardoption)) return(NULL)
    filterTotal <- filter(filterData(),Characteristic %in% "Total",Measure %in% "Total")
    
    filteredData <- filter(filterOptions2(),Measure %in% input$Dashboardoption)
    #if(input$Dashboardoption!="Total")
    #  filteredData <- bind_rows(filteredData,filter(filterTotal,Indicator %in% input$Dashboardindicator,!Breakdown%in%filteredData$Breakdown))
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
                         selected=optionsList)
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
      mutate(seriesColour=eefColours(seriesColour,colourPalette)) %>% 
      select(Breakdown,seriesColour)
    
    left_join(select(filterOptions3(),-seriesColour),colours,by="Breakdown") %>%
      filter(Breakdown %in% input$selectBreakdown)
  })  
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  maxRange <- reactive({
    ifelse(is.na(maxZoom),padRange(c(0,filterOptions3())$Figure,padding=0.25),maxZoom)
  })
  
  
  callModule(downloadServer,"chart",filterGraph)
  callModule(dataTableServer,"chart",filterGraph)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(dygraphServer,"chart",filterGraph,graphOptions,maxRange)
  
  output$title <- renderUI({h3(paste0("National Indicator: ",filterGraph()$Indicator[1]))})
  output$title2 <- renderUI({h3(paste0("National Indicator: ",filterGraph()$Indicator[1]))})
  output$measure <- renderUI({h3(filterGraph()$Measure[1])})
  output$source <- renderUI({
    if(is.null(filterGraph())) return(NULL)
    if(nrow(filterGraph())==0) return(NULL)
    
    tagList(
      p(strong("Source: "),filterGraph()$Source[1]),
      p(strong("Data: "),
        a(href="https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fnational-performance-framework",
          target="_blank",
          "statistics.gov.scot"),
        "(Last updated: ",filterGraph()$LastUpdated[1]%>%format("%d %b %Y"),")")
    )
  })
  
}

pieChartServer0 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-pieChart0")
  })
  #input options
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    distinct(filterData(),Breakdown,.keep_all = TRUE) %>%
      select(Breakdown,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })
  
  filterGraph <- reactive({
    filterData()%>%
      select(-seriesColour) %>%
      left_join(colours(),by="Breakdown")
  })
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(pieChartServer,"chart",filterGraph,graphOptions,reload)
  
}

pieChartServer1 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  defaultSelected <- graphOptions$defaultSelected
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-pieChart1")
  })
  
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    distinct(filterData(),Breakdown,.keep_all = TRUE) %>%
      select(Breakdown,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })
  
  
  #input options
  output$optionMeasure <- renderUI({
    if(nrow(filterData())==0) return(NULL)
    if(is.null(defaultSelected)) defaultSelected <- filterData()$Measure[1]
    selectedList <- isolate({input$selectMeasure}) %>% intersect(filterData()$Measure)
    if(length(selectedList)==0) selectedList <- defaultSelected
    selectInput(ns("selectMeasure"),NULL,
                choices = unique(filterData()$Measure),selected=selectedList)
    
  })
  
  filterGraph <- reactive({
    filter(filterData(),
           Measure %in% input$selectMeasure
    ) %>%
      select(-seriesColour) %>%
      left_join(colours(),by="Breakdown")
  })
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(pieChartServer,"chart",filterGraph,graphOptions,reload)
}

pieChartServer2 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  defaultSelected <- graphOptions$defaultSelected
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-pieChart2")
  })
  
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    distinct(filterData(),Breakdown,.keep_all = TRUE) %>%
      select(Breakdown,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })
  
  icon <- graphOptions$icon
  
  
  #input options
  output$optionMeasure <- renderUI({
    if(nrow(filterData())==0) return(NULL)
    if(is.null(defaultSelected)) defaultSelected <- filterData()$Measure[1]
    selectedList <- isolate({input$selectMeasure}) %>% intersect(filterData()$Measure)
    if(length(selectedList)==0) selectedList <- defaultSelected
    selectInput(ns("selectMeasure"),NULL,
                choices = unique(filterData()$Measure),selected=selectedList)
    
  })
  
  filterGraph <- reactive({
    filter(filterData(),
           Measure %in% input$selectMeasure
    ) %>%
      select(-seriesColour) %>%
      left_join(colours(),by="Breakdown")
  })
  
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  
  observeEvent(filterData(),{
    allPlots <- unique(filterData()$Indicator)
    lapply(1:length(allPlots),
           function(x) {
             callModule(pieChartServer,
                        paste0("chart",x),
                        reactive({filterGraph()%>%filter(Indicator==allPlots[x])}),
                        graphOptions,reload)
           }
    )
  })
}

barChartServer0 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  
  maxRange <- reactive({
    if(!gvisOptions$isStacked  %in% TRUE) mrange <- padRange(c(0,filterData()$Figure),padding=0.25)
    if(gvisOptions$isStacked  %in% TRUE) {
      stackData <- group_by(filterData(),Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
      mrange <- padRange(c(0,stackData$Figure),padding=0.25)
    }
    mrange <- ifelse(is.na(maxZoom),mrange,maxZoom)
    return(mrange)
  })  
  
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-barChart0")
  })
  
  #input options
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    distinct(filterData(),Measure,.keep_all = TRUE) %>%
      select(Measure,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })  
  filterGraph <- reactive({
    filterData()%>%
      select(-seriesColour) %>%
      left_join(colours(),by="Measure")
  })
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(barChartServer,"chart",filterGraph,graphOptions,maxRange,reload)
  
}

barChartServer1 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  defaultSelected <- graphOptions$defaultSelected
  
  
  maxRange <- reactive({
    if(!gvisOptions$isStacked  %in% TRUE) mrange <- padRange(c(0,filterData()$Figure),padding=0.25)
    if(gvisOptions$isStacked  %in% TRUE) {
      stackData <- group_by(filterData(),Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
      mrange <- padRange(c(0,stackData$Figure),padding=0.25)
    }
    mrange <- ifelse(is.na(maxZoom),mrange,maxZoom)
    return(mrange)
  })  
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-barChart0")
  })
  
  #input options
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    distinct(filterData(),Measure,.keep_all = TRUE) %>%
      select(Measure,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })
  
  output$optionBreakdown <- renderUI({
    if(is.null(filterData())) return(NULL)
    optionsList <- unique(filterData()$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=defaultSelected)
    
  })
  
  filterGraph <- reactive({
    if(is.null(filterData())) return(NULL)
    filter(filterData(),
           Breakdown %in% input$selectBreakdown
    )%>% select(-seriesColour) %>%
      left_join(colours(),by="Measure")
  }) 
  
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(barChartServer,"chart",filterGraph,graphOptions,maxRange,reload)
  
}

barChartServer2 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  defaultSelected <- graphOptions$defaultSelected
  
  maxRange <- reactive({
    if(!gvisOptions$isStacked  %in% TRUE) mrange <- padRange(c(0,filterData()$Figure),padding=0.25)
    if(gvisOptions$isStacked  %in% TRUE) {
      stackData <- group_by(filterData(),Indicator,Breakdown) %>% 
        summarise(Figure=sum(Figure,na.rm=TRUE))
      mrange <- padRange(c(0,max(stackData$Figure)),padding=0.25)
    }
    mrange <- ifelse(is.na(maxZoom),mrange,maxZoom)
    return(mrange)
  })  
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-barChart0")
  })
  
  #input options
  #input options
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterOptions())) return(NULL)
    distinct(filterOptions(),Measure,.keep_all = TRUE) %>%
      select(Measure,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })
  
  output$optionIndicator <- renderUI({
    if(nrow(filterData())==0) return(NULL)
    selectInput(ns("selectIndicator"),NULL,
                choices = unique(filterData()$Indicator),selected=isolate(input$selectIndicator))
    
  })
  
  filterOptions <- reactive({
    if(is.null(filterData())|is.null(input$selectIndicator)) return(NULL)
    filter(filterData(),Indicator %in% input$selectIndicator)
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
                       selected=defaultSelected)
    
  })
  
  filterGraph <- reactive({
    if(is.null(filterOptions())) return(NULL)
    filter(filterOptions(),
           Breakdown %in% input$selectBreakdown
    )%>% select(-seriesColour) %>%
      left_join(colours(),by="Measure")
  }) 
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(barChartServer,"chart",filterGraph,graphOptions,maxRange,reload)
  
}

barChartServer3 <- function(input,output,session,filterData=reactive({NULL}),graphOptions=list(),reload=reactive(NULL)) {
  ns <- session$ns
  
  if(is.null(graphOptions$gvisOptions)) gvisOptions <- list() else gvisOptions <- graphOptions$gvisOptions
  if(is.null(gvisOptions$isStacked)) gvisOptions$isStacked <- FALSE
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  defaultSelected <- graphOptions$defaultSelected
  
  if(is.null(graphOptions$maxZoom)) maxZoom <- c(NA,NA) else maxZoom <- graphOptions$maxZoom
  
  maxRange <- reactive({
    if(!gvisOptions$isStacked  %in% TRUE) mrange <- padRange(c(0,filterData()$Figure),padding=0.25)
    if(gvisOptions$isStacked  %in% TRUE) {
      stackData <- group_by(filterData(),Breakdown) %>% summarise(Figure=sum(Figure,na.rm=TRUE))
      mrange <- padRange(c(0,stackData$Figure),padding=0.25)
    }
    mrange <- ifelse(is.na(maxZoom),mrange,maxZoom)
    return(mrange)
  })  
  
  shinyjs::onclick("info",{
    show(selector="#eef-help-blank,#eef-help-close,#eef-help-barChart0")
  })
  
  #input options
  if(is.null(graphOptions$colourPalette)) colourPalette <- "restricted" else colourPalette <- graphOptions$colourPalette
  colours <- reactive({
    if(is.null(filterData())) return(NULL)
    distinct(filterData(),Measure,.keep_all = TRUE) %>%
      select(Measure,seriesColour) %>%
      mutate(seriesColour=eefColours(seriesColour,colourPalette))
  })
  output$optionBreakdown <- renderUI({
    if(is.null(filterData())) return(NULL)
    optionsList <- unique(filterData()$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), NULL,
                       choices = optionsList,
                       selected=defaultSelected)
    
  })
  
  filterOptions <- reactive({
    if(is.null(filterData())|is.null(input$selectBreakdown)) return(NULL)
    filter(filterData(),Breakdown %in% input$selectBreakdown)
  })
  
  output$optionMeasure <- renderUI({
    if(is.null(filterOptions())) return(NULL)
    optionsList <- unique(filterOptions()$Measure)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectMeasure}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("\\b(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectMeasure"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  filterGraph <- reactive({
    if(is.null(filterOptions())) return(NULL)
    filter(filterOptions(),
           Measure %in% input$selectMeasure
    )%>% select(-seriesColour) %>%
      left_join(colours(),by="Measure")
  }) 
  
  callModule(downloadServer,"chart",filterGraph,graphOptions)
  if(!is.null(graphOptions$updateRmd)) callModule(rmdServer,"chart",filterGraph,graphOptions)
  callModule(barChartServer,"chart",filterGraph,graphOptions,maxRange,reload)
  
}