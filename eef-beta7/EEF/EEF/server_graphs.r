
#REDUNDANT CODE - DELETE
lineGraphServer <- function(input,output,session,filterCharacteristic=NULL,filterMeasure=NULL,filterBreakdown=NULL) {
  ns <- session$ns
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  
  output$options <- renderUI({
    filterOptions <- filter(EEFdata,Characteristic %in% filterCharacteristic,
                            Measure %in% filterMeasure)
    if(is.null(filterOptions)) return(NULL)
    #optionsList <- levels(droplevels(filterOptions$Breakdown))
    optionsList <- unique(filterOptions$Breakdown)
    if("Total"%in%optionsList) optionsList <- c(optionsList[optionsList!="Total"],"Total")
    if("All"%in%optionsList) optionsList <- c(optionsList[optionsList!="All"],"All")
    #display options
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=optionsList)
    
  })    
  output$plot <- renderDygraph({
      filterData <- filter(EEFdata,
                           Characteristic %in% filterCharacteristic,
                           Measure %in% filterMeasure,
                           Breakdown %in% input$selectBreakdown
      )
      if(nrow(filterData)==0) return(NULL)
        
      
      
      graphData <- mutate(filterData,Year= as.Date(paste(sep="",as.character(Year),"-01-01"), format = "%Y-%m-%d")) %>%
        select(Year,Figure,Breakdown)%>%
        group_by(Year)%>%
        spread(Breakdown,Figure)
      
      xtsData <- xts(graphData[-1],graphData[[1]]) #%>% padxts
     

      dygraph(xtsData) %>%
        dyRangeSelector(height = 40, strokeColor = "") %>%
        dyHighlight(highlightCircleSize = 0,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = TRUE,
                    highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
        dyOptions(rightGap = 25,strokeWidth = 3) %>%
        dyLegend(labelsDiv = ns("legend"),show = "always",labelsSeparateLines=TRUE) %>%
        dyAxis("x",drawGrid=FALSE) %>%
        dyAxis("y", label= filterMeasure)

    })
  
  output$graph <- renderUI({
    fixedRow(div(class="float ",style="width:800px",
                 dygraphOutput(ns("plot"), height = 400)),
             div(class="float ",style="width:220px",
                 tags$button(id = ns("toggledash"), "Show/Hide", href = "#", class="greyscalebutton"),
                 uiOutput(ns("options")),
                 div(id = ns("legend"), class = "dylegend"))
    )
  })
  
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("options")
  })
    
  
}


#Graph package: dygraph
#Graph type: Line
#options: Breakdown
#Plots: Measure
#Scope: All years
#filterCharacteristic, filterMeasure, filterBreakdown no longer used - use filterData instead
timeSeriesServer1 <- function(input,output,session,filterData,defaultSelected=NULL,ylabel=NULL,filterCharacteristic=NULL,filterMeasure=NULL,filterBreakdown=NULL,digits=NULL) {
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
    optionsTotal <- grepl("(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  #plot graph
  output$plot <- renderDygraph({
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    graphData <- select(filterGraph,Date,Figure,Breakdown)%>%
      group_by(Date)%>%
      spread(Breakdown,Figure)
    
    xtsData <- xts(graphData[-1],graphData[[1]]) #%>% padxts
    
    
    dygraph(xtsData) %>%
      dyRangeSelector(height = 40, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
      dyOptions(rightGap = 25,strokeWidth = 3) %>%
      dyLegend(labelsDiv = ns("legend"),show = "always",labelsSeparateLines=TRUE) %>%
      dyAxis("x",drawGrid=FALSE,
#             axisLabelFormatter=paste0("function(d, gran, opts) {return Dygraph.dateAxisLabelFormatter(new Date('",filterGraph$Date[which.min(filterGraph$Date)],"'), gran, opts);}")
             #axisLabelFormatter=paste0("function(d, gran, opts) {var date=new Date(d.getTime());return Dygraph.dateAxisLabelFormatter(date.setMonth(date.getMonth()+",month(graphData$Date[which.min(graphData$Date)])-1,")), Dygraph.ANNUAL, opts);}")
axisLabelFormatter=paste0("function(d, gran, opts) {var date=new Date(d.getTime());date.setMonth(date.getMonth()+",month(graphData$Date[which.min(graphData$Date)])-1,");return Dygraph.dateAxisLabelFormatter(date, Dygraph.ANNUAL, opts);}")
#axisLabelFormatter=paste0("function(d, gran, opts) {var date=new Date(d.getTime());date.setMonth(date.getMonth()+0);return Dygraph.dateAxisLabelFormatter(date, Dygraph.ANNUAL, opts);}")

             
             ) %>%
      #dyAxis("x",drawGrid=FALSE) %>%
      dyAxis("y", label= ylabel,valueRange=c(0,max(filterData$Figure,na.rm=TRUE)))
    
  })
  
  #graph div template - n.b. column1-3 will only show in seperate columns on small screens
  output$graph <- renderUI({
    eefGraph(ns,
             graph=dygraphOutput(ns("plot"), height = 400),
             column1=uiOutput(ns("optionBreakdown")),
             column2=div(id = ns("legend"), class = "dylegend")
    )
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("column1")#toggle column1 
  })
  
  
}


#Graph package: dygraph
#Graph type: Line
#options: Breakdown, Measure
#Plots: Measure
#Scope: All years
timeSeriesServer2 <- function(input,output,session,filterData,defaultSelected=NULL,ylabel=NULL,filterCharacteristic=NULL,filterMeasure=NULL,filterBreakdown=NULL,digits=NULL) {
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
    optionsTotal <- grepl("(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  
  #plot graph
  output$plot <- renderDygraph({
    if(is.null(filterOptions())) return(NULL)
    filterGraph <- filter(filterOptions(),
#                          Measure %in% input$selectMeasure,
                          Breakdown %in% input$selectBreakdown
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    
    
    graphData <- select(filterGraph,Date,Figure,Breakdown)%>%
      group_by(Date)%>%
      spread(Breakdown,Figure)
    
    xtsData <- xts(graphData[-1],graphData[[1]]) #%>% padxts
    
    
    dygraph(xtsData) %>%
      dyRangeSelector(height = 40, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 3, highlightCircleSize = 4)) %>%
      dyOptions(rightGap = 25,strokeWidth = 3,valueRange=) %>%
      dyLegend(labelsDiv = ns("legend"),show = "always",labelsSeparateLines=TRUE) %>%
      dyAxis("x",drawGrid=FALSE) %>%
      dyAxis("y", label= ylabel,valueRange=c(0,max(filterData$Figure,na.rm=TRUE)))
    
  })
  
  #graph div template - n.b. column1-3 will only show in seperate columns on smaller screens
  output$graph <- renderUI({
    eefGraph(ns,
             graph=dygraphOutput(ns("plot"), height = 400),
             column1=uiOutput(ns("optionMeasure")),
             column2=uiOutput(ns("optionBreakdown")),
             column3=div(id = ns("legend"), class = "dylegend")
    )
  })
  
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("column1")
    shinyjs::toggle("column2")#toggle column1 and column2
  })
  
  
}

#Graph package: googleVis
#Graph type: Line
#options: Breakdown
#Plots: Measure
#Scope: subBreakdown (can be discrete)
lineChartServer1 <- function(input,output,session,filterData,defaultSelected=NULL,ylabel=NULL,digits=NULL) {
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
    optionsTotal <- grepl("(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  output$optionSubBreakdown <- renderUI({
    if(is.null(filterData)) return(NULL)
    range <- unique(filterData$SubBreakdown)
    sliderInputLabels(ns("selectSubBreakdown"),NULL,range,
                      value=c(range[1],range[length(range)]),
                      width="90%")
  })
  
  #plot graph
  output$plot <- renderGvis({
    if(is.null(filterData)) return(NULL)
    if(is.null(input$selectSubBreakdown)) return(NULL)
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown,
                          SubBreakdown %in% unique(filterData$SubBreakdown)[(input$selectSubBreakdown[1]+1):(input$selectSubBreakdown[2]+1)]
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    graphData <- select(filterGraph,SubBreakdown,Figure,Breakdown)%>%
      group_by(SubBreakdown) %>%
      spread(Breakdown,Figure) %>% 
      #mutate(`% in relative poverty AHC.style`=switch(tolower(Breakdown),total="blue","red"))%>%
      gvisLineChart("SubBreakdown",
                   unique(filterGraph$Breakdown),
                   #paste0(unique(filterGraph$Breakdown),c("",".style")),
                   options=list(height="400px",
                                chartArea="{top:20,bottom:50}",
                                vAxis=paste0("{titleTextStyle: {italic: false}, minValue:0, maxValue:",max(filterData$Figure,na.rm=TRUE),", title:'",ylabel,"'}"),
                                legend="{position:'top'}"))
    
  })
  
  #graph div template - n.b. column1-3 will only show in seperate columns on small screens
  output$graph <- renderUI({
    eefGraph(ns,
             graph=uiOutput(ns("plot"), height = 400),
             column1=uiOutput(ns("optionBreakdown")),
             column2=uiOutput(ns("optionSubBreakdown"))
    )
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("column1")
    shinyjs::toggle("column2")#toggle column1 and column2
  })
  
  
}


#Graph package: googleVis
#Graph type: Line
#options: Breakdown, Measure
#Plots: Measure
#Scope: subBreakdown (can be discrete)
lineChartServer2 <- function(input,output,session,filterData,defaultSelected=NULL,ylabel=NULL,digits=NULL) {
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
    optionsTotal <- grepl("(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  output$optionSubBreakdown <- renderUI({
    if(is.null(filterOptions())) return(NULL)
    range <- unique(filterOptions()$SubBreakdown)
    sliderInputLabels(ns("selectSubBreakdown"),NULL,range,
                      value=c(range[1],range[length(range)]),
                      width="90%")
  })
  
  
  #plot graph
  output$plot <- renderGvis({
    if(is.null(filterOptions())) return(NULL)
    if(is.null(input$selectSubBreakdown)) return(NULL)
    
    filterGraph <- filter(filterOptions(),
                          Breakdown %in% input$selectBreakdown,
                          SubBreakdown %in% unique(filterOptions()$SubBreakdown)[(input$selectSubBreakdown[1]):(input$selectSubBreakdown[2])]
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    graphData <- select(filterGraph,SubBreakdown,Figure,Breakdown)%>%
      group_by(SubBreakdown) %>%
      spread(Breakdown,Figure) %>% 
      #mutate(`% in relative poverty AHC.style`=switch(tolower(Breakdown),total="blue","red"))%>%
      gvisLineChart("SubBreakdown",
                    unique(filterGraph$Breakdown),
                    #paste0(unique(filterGraph$Breakdown),c("",".style")),
                    chartid="plotID",
                    options=list(height="400px",
                                 chartArea="{top:20,bottom:50}",
                                 vAxis=paste0("{titleTextStyle: {italic: false}, minValue:0, maxValue:",max(filterData$Figure,na.rm=TRUE),", title:'",ylabel,"'}"),
                                 legend="{position:'top'}"))
    gvisInsertJS <- paste0("google.visualization.events.addListener(chart, 'ready', function () {Shiny.onInputChange('",ns("plotURI"),"',chart.getImageURI());});\nchart.draw(data,options);")
    graphData$html$chart["jsDrawChart"] <- sub("chart\\.draw\\(data,options\\);",gvisInsertJS,graphData$html$chart["jsDrawChart"])
    return(graphData)
  })
  
  #graph div template - n.b. column1-3 will only show in seperate columns on smaller screens
  output$graph <- renderUI({
    eefGraph(ns,
             graph=uiOutput(ns("plot"), height = 400),
             column1=uiOutput(ns("optionMeasure")),
             column2=uiOutput(ns("optionBreakdown")),
             column3=uiOutput(ns("optionSubBreakdown"))
    )
  })
  
  ##insert download URI code
  shinyjs::onclick("download-chart",{
    #shinyjs::runjs(paste0("window.open(",sub("data:image/png", "data:application/octet-stream",input$plotURI),")"))
    shinyjs::runjs(paste0("var tab=window.open();tab.document.write(\"<img src='",input$plotURI,"'>\");"))
    #shinyjs::runjs(paste0("var tab=window.open();tab.document.location.href='",sub("data:image/png", "data:application/octet-stream",input$plotURI),"';"))
    
  })
   
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("column1")
    shinyjs::toggle("column2")
    shinyjs::toggle("column3")#toggle column1-column3
  })
  
  
}

#Graph package: googleVis
#Graph type: Pie/Doughnut
#options: Breakdown
#Plots: Measure
pieChartServer1 <- function(input,output,session,filterData,pieHole=0.5,colours=NULL,defaultSelected=NULL,ylabel=NULL,digits=NULL) {
  ns <- session$ns
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(is.null(ylabel)) ylabel <- filterData$Measure[1]
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
  
  #plot graph
  output$plot1 <- renderGvis({
    if(is.null(filterData)) return(NULL)
    
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown,
                          Measure %in% unique(filterData$Measure)
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    chartOptions <- list(height="400px",
                         pieHole=pieHole,
                         tooltip="{text:'percentage'}",
                         legend="{position:'right'}")
    if(is.null(colours)) {
      seriesColours <- filterGraph$seriesColour
    } else {
      seriesColours <- colours
    }
    
    filterGraph <- mutate(filterGraph,SubBreakdown=ifelse(is.na(SubBreakdown),Measure,SubBreakdown))
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(SubBreakdown=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- eefColours(c("blue","very-light-blue"))
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours),"'",collapse=", "),"]")
    
    
    select(filterGraph,SubBreakdown,Figure) %>%
      gvisPieChart("SubBreakdown","Figure",options=chartOptions)
    
    
  })
  
  #graph div template - n.b. column1-3 will only show in seperate columns on smaller screens
  output$graph <- renderUI({
    eefGraph(ns,
             graph=uiOutput(ns("plot1"), height = 400),
             column1=uiOutput(ns("optionBreakdown")),
    )
  })
  
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("column1")#toggle column1
  })
}

#Graph package: googleVis
#Graph type: Pie/Doughnut Array
#options: Breakdown
#Plots: Measure
pieChartServer2 <- function(input,output,session,filterData,pieHole=0.5,colours=NULL,icon=NULL,defaultSelected=NULL,ylabel=NULL,digits=NULL) {
  ns <- session$ns
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(is.null(ylabel)) ylabel <- filterData$Measure[1]
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
  
  #plot graph
  output$plot1 <- renderGvis({
    if(is.null(filterData)) return(NULL)
    
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown,
                          Measure %in% unique(filterData$Measure)[1]
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    chartOptions <- list(height="300px",
                         pieHole=pieHole,
                         tooltip="{text:'percentage'}",
                         legend="{position:'top'}")
    if(is.null(colours)) {
      seriesColours <- filterGraph$seriesColour
    } else {
      seriesColours <- colours
    }
    
    filterGraph <- mutate(filterGraph,SubBreakdown=ifelse(is.na(SubBreakdown),Measure,SubBreakdown))
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(SubBreakdown=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$title <- unique(filterData$Measure)[1]
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- eefColours(c("blue","very-light-blue"))
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours),"'",collapse=", "),"]")
    
    
    select(filterGraph,SubBreakdown,Figure) %>%
      gvisPieChart("SubBreakdown","Figure",options=chartOptions)
  })
  
  output$plot2 <- renderGvis({
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
                         legend="{position:'top'}")
    
    if(is.null(colours)) {
      seriesColours <- filterGraph$seriesColour
    } else {
      seriesColours <- colours
    }
    
    filterGraph <- mutate(filterGraph,SubBreakdown=ifelse(is.na(SubBreakdown),Measure,SubBreakdown))
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(SubBreakdown=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$title <- unique(filterData$Measure)[2]
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- eefColours(c("blue","very-light-blue"))
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours),"'",collapse=", "),"]")
    
    select(filterGraph,SubBreakdown,Figure) %>%
      gvisPieChart("SubBreakdown","Figure",options=chartOptions)
  })
  
  output$plot3 <- renderGvis({
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
                         legend="{position:'top'}")
    if(is.null(colours)) {
      seriesColours <- filterGraph$seriesColour
    } else {
      seriesColours <- colours
    }
    
    filterGraph <- mutate(filterGraph,SubBreakdown=ifelse(is.na(SubBreakdown),Measure,SubBreakdown))
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(SubBreakdown=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$title <- unique(filterData$Measure)[3]
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- eefColours(c("blue","very-light-blue"))
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours),"'",collapse=", "),"]")
    
    
    select(filterGraph,SubBreakdown,Figure) %>%
      gvisPieChart("SubBreakdown","Figure",options=chartOptions)
  })
  
  output$plot4 <- renderGvis({
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
                         legend="{position:'top'}")
    if(is.null(colours)) {
      seriesColours <- filterGraph$seriesColour
    } else {
      seriesColours <- colours
    }
    
    filterGraph <- mutate(filterGraph,SubBreakdown=ifelse(is.na(SubBreakdown),Measure,SubBreakdown))
    if(nrow(filterGraph)==1) {
      filterGraph <- bind_rows(filterGraph,data.frame(SubBreakdown=NA,Figure=(100-filterGraph$Figure)))
      chartOptions$legend <- "{position:'none'}"
      chartOptions$title <- unique(filterData$Measure)[4]
      chartOptions$slices <- paste0("[{textStyle: {color: 'white'}}, {textStyle: {color: '",eefColours("very-light-blue"),"'}}]")
      seriesColours <- eefColours(c("blue","very-light-blue"))
    }  
    chartOptions$colors <- paste0("[",paste0("'",eefColours(seriesColours),"'",collapse=", "),"]")
    
    select(filterGraph,SubBreakdown,Figure) %>%
      gvisPieChart("SubBreakdown","Figure",options=chartOptions)
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
                       div(class="float eef-pie-chart-container",uiOutput(ns("plot1"), height = 300),icon1),
                       div(class="float eef-pie-chart-container",uiOutput(ns("plot2"), height = 300),icon2),
                       div(class="float eef-pie-chart-container",uiOutput(ns("plot3"), height = 300),icon3),
                       div(class="float eef-pie-chart-container",uiOutput(ns("plot4"), height = 300),icon4)),
             column1=uiOutput(ns("optionBreakdown"))
    )
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
barChartServer1 <- function(input,output,session,filterData,defaultSelected=NULL,filterCharacteristic=NULL,filterMeasure=NULL,filterBreakdown=NULL,digits=NULL) {
  ns <- session$ns
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  
  #initial filter to select only the data needed for the graph

  #input options
  output$optionBreakdown <- renderUI({
    if(is.null(filterData)) return(NULL)
    optionsList <- unique(filterData$Breakdown)
    if(is.null(defaultSelected)) defaultSelected <- optionsList
    selectedList <- isolate({input$selectBreakdown}) %>% intersect(optionsList)
    if(length(selectedList)==0) selectedList <- defaultSelected
    optionsTotal <- grepl("(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  #plot graph
  output$plot <- renderGvis({
    if(is.null(filterData)) return(NULL)
    filterGraph <- filter(filterData,
                          Breakdown %in% input$selectBreakdown
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    graphData <- select(filterGraph,Figure,Breakdown,Measure)%>%
      group_by(Breakdown) %>%
      spread(Measure,Figure) %>% 
      arrangeBreakdown(2,decreasing=TRUE) %>%
      #mutate(`% in relative poverty AHC.style`=switch(tolower(Breakdown),total="blue","red"))%>%
      gvisBarChart("Breakdown",
                   unique(filterGraph$Measure),
                   #paste0(unique(filterGraph$Measure),c("",".style")),
                   options=list(height="400px",
                                 legend="{position:'bottom'}",
                                 hAxis=paste0("{minValue:0, maxValue:",max(filterData$Figure,na.rm=TRUE),"}")))
    
  })
  
  #graph div template - n.b. column1-3 will only show in seperate columns on small screens
  output$graph <- renderUI({
    eefGraph(ns,
             graph=uiOutput(ns("plot")),
             column1=uiOutput(ns("optionBreakdown"))
    )
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("column1")#toggle column1 
  })
  
  
}

#Graph package: googleVis
#Graph type: Bar
#options: Breakdown, Measure
#Plots: Measure
#Scope: Latest year
barChartServer2 <- function(input,output,session,filterData,defaultSelected=NULL,filterCharacteristic=NULL,filterMeasure=NULL,filterBreakdown=NULL,digits=NULL) {
  ns <- session$ns
  id <- substr(ns(""),1,nchar(ns(""))-nchar(ns.sep))
  if(!is.null(digits)) filterData <- mutate(filterData,Figure=round(Figure,digits))
  
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
    #if("Total"%in%optionsList) optionsList <- c(optionsList[optionsList!="Total"],"Total")
    #if("All"%in%optionsList) optionsList <- c(optionsList[optionsList!="All"],"All")
    optionsTotal <- grepl("(All|Total)\\b",optionsList,ignore.case=T)
    if(any(optionsTotal)) optionsList <- c(optionsList[!optionsTotal],optionsList[optionsTotal])
    checkboxGroupInput(ns("selectBreakdown"), "Choose what to plot:",
                       choices = optionsList,
                       selected=selectedList)
    
  })
  
  
  #plot graph
  output$plot <- renderGvis({
    if(is.null(filterOptions())) return(NULL)
    filterGraph <- filter(filterOptions(),
                          Breakdown %in% input$selectBreakdown
    )
    if(nrow(filterGraph)==0) return(NULL)
    
    seriesColour <- switch
    
    select(filterGraph,Figure,Breakdown,Measure)%>%
      group_by(Breakdown) %>%
      spread(Measure,Figure) %>% 
      arrangeBreakdown(2,decreasing=TRUE) %>%
      gvisBarChart("Breakdown",
                   unique(filterGraph$Measure)
                   ,options=list(height="400px",
                                 legend="{position:'bottom'}",
                                 hAxis=paste0("{minValue:0, maxValue:",max(filterGraph$Figure,na.rm=TRUE),"}")))
    
  })
  
  #graph div template - n.b. column1-3 will only show in seperate columns on small screens
  output$graph <- renderUI({
    eefGraph(ns,
             graph=uiOutput(ns("plot")),
             column1=uiOutput(ns("optionMeasure")),
             column2=uiOutput(ns("optionBreakdown"))
    )
  })
  
  #toggle button
  shinyjs::onclick("toggledash", {
    shinyjs::toggle("column1")
    shinyjs::toggle("column2")
  })
}
