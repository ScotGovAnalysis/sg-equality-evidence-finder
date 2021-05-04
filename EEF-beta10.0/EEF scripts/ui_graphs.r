###################################################################################
#                                                                                 #
# UI COMPONENTS - PANELS (INCLUDING CHARTS)                                       #
#                                                                                 #
###################################################################################
#                                                                                 #
# Author: Jay Ware                                                                #
# Created: 01/08/2018                                                             #
# Last updated: 14/01/2021                                                        #
#                                                                                 #
# Purpose: Shiny UI functions for the interactive charts of the Evidence Finder.  #
#          There are 3 chart types, each with various different user input menus  #
#          and buttons. The code for each of these are very similar, but differ   #
#          in which user options are applicable.                                  #
#                                                                                 #
#          See server_modules.R for the corresponding server functions.           #
#                                                                                 #
# Functions:                                                                      #
#                                                                                 #
# NoNPF(): UI function for NPF panel where no data is available                   #
# EEFpanelNoData(): UI function for equality evidence panel where no evidence is  #
#                   available. Displays a generic message to contact analysts.    #
# EEFpanelNoGraph(): UI function for equality evidence panel where no interactive #
#                    chart is included. It only displays headline, infographic,   #
#                    commentary, data source link, and NPF indicator flag         #
#                                                                                 #
# Panels with charts: UI functions for the various panel combinations of chart    #
#                     types and user options.                                     #
#                                                                                 #
###################################################################################

NoNPF <- function(equality) {
  div(p(paste0("National Performance Indicator breakdowns are not currently available for ",equality,".")))
}

EEFpanelNoData <- function(topic,equality,policy) {
  
  div(p(paste0("Information is not currently available in this section on ",
               topic,
               " relating to ",
               switch(equality,Transgender="transgender people",tolower(equality)),
               ". Additional content may be available in the publications and links section or through contacting the ",
               switch(policy,
                      `Summary`="Equality Evidence Finder",
                      `Demographics`="National Records of Scotland",
                      `Employability, Skills and Lifelong Learning`="Advanced Learning and Skills",
                      `Health, Social Care and Sport`="Health and Social Care",
                      `Income and Poverty`="Poverty",
                      `Labour Market and Social Security`="Labour Market",
                      `Transport and Travel`="Transport Scotland",
                      policy),
               " analysts.")))
}

EEFpanelNoGraph <- function(id,panelData=list()) {
  ns <- NS(id)
  if(!is.null(panelData[["image"]])) if(panelData[["image"]] %in% c("",NA)) panelData[["image"]] <- NULL else panelData[["image"]] <- img(src=panelData[["image"]],alt=panelData$subtitle,title=panelData$subtitle,width="680")
  if(!is.null(panelData[["subtitle"]])) if(panelData[["subtitle"]] %in% c("",NA)) panelData[["subtitle"]] <- NULL else panelData[["subtitle"]] <- p(strong(panelData[["subtitle"]]))
  tagList(panelData[["subtitle"]],
      panelData[["image"]]
  )
}

timeSeriesUI0 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          tags$button(id=nsChart("zoomIn"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-in.svg",width=20)),
          tags$button(id=nsChart("zoomOut"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-out.svg",width=20)),
          dyDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
          downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(12,id=nsChart("displayPlot"),class="eef-plot",dygraphOutput(nsChart("plot"), height = 400))#,
        #column(3,div(id = ns("legend"), class = "dylegend"))
      )
  )
}

timeSeriesUI1 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          #actionButton(ns("zoom"),img(src="icons/zoom-in.svg",width=20),class=paste0(ns("btn")," eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          tags$button(id=nsChart("zoomIn"),class=paste0(ns("btn")," btn eef-graph-header-button action-button shiny-bound-input"),type="button",img(src="icons/zoom-in.svg",width=20)),
          tags$button(id=nsChart("zoomOut"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-out.svg",width=20)),
          dyDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
          downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
          tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(9,id=nsChart("displayPlot"),class="eef-plot ",dygraphOutput(nsChart("plot"), height = 400)),
        column(3,id=nsChart("displayOptions"),
               #tags$button(id = nsChart("toggledash"), "Show/Hide", href = "#", class="eef-options-toggle greyscalebutton"),
               div(id=ns("column1"),uiOutput(ns("optionBreakdown"))),
               div(id = ns("legend"), class = "dylegend"))
      )
  )
}

timeSeriesUI2 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          #actionButton(ns("zoom"),img(src="icons/zoom-in.svg",width=20),class=paste0(ns("btn")," eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          tags$button(id=nsChart("zoomIn"),class=paste0(ns("btn")," btn eef-graph-header-button action-button shiny-bound-input"),type="button",img(src="icons/zoom-in.svg",width=20)),
          tags$button(id=nsChart("zoomOut"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-out.svg",width=20)),
          dyDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
          downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
          tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(9,id=nsChart("displayPlot"),class="eef-plot ",dygraphOutput(nsChart("plot"), height = 400)),
        column(3,id=nsChart("displayOptions"),
               #tags$button(id = nsChart("toggledash"), "Show/Hide", href = "#", class="eef-options-toggle greyscalebutton"),
               div(id=ns("column1"),uiOutput(ns("optionMeasure")),uiOutput(ns("optionBreakdown"))),
               div(id = ns("legend"), class = "dylegend"))
      )
    )
}

npfUI3 <- function(id) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  panelData <- as.list(graphOptions$panelData)
  div(class="eef-text",
      fluidRow(
        column(3,id=nsChart("displayOptions"),
               #tags$button(id = nsChart("toggledash"), "Show/Hide", href = "#", class="eef-options-toggle greyscalebutton"),
               #tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," greyscalebutton"),"Show/Hide"),
               div(id=ns("column1"),
                   uiOutput(ns("Dashboardoutcomes")),
                   uiOutput(ns("Dashboardindicators")),
                   uiOutput(ns("Dashboardoptions")),
                   uiOutput(ns("optionBreakdown"))
                   ),
               div(id = ns("legend"), class = "dylegend")),
        #column(9,class="eef-plot ",dygraphOutput(nsChart("plot"), height = 400),uiOutput(ns("source")))
        column(9,id=nsChart("displayPlot"),class="eef-plot ",
               tabsetPanel(tabPanel("Chart",br(),
                                    div(class="eef-graph-header",
                                        tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
                                        dyDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
                                        #downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
                                        tags$button(id=nsChart("zoomIn"),class=paste0(ns("btn")," btn eef-graph-header-button action-button shiny-bound-input"),type="button",img(src="icons/zoom-in.svg",width=20)),
                                        tags$button(id=nsChart("zoomOut"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-out.svg",width=20)),
                                        tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
                                        tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
                                    ),
                                    uiOutput(ns("indicator")),
                                    uiOutput(ns("title")),
                                    dygraphOutput(nsChart("plot"), height = 400)%>%withSpinner()
                                    ),
                           tabPanel("Table",br(),
                                    div(class="eef-graph-header",
                                        tags$button(id=nsChart("toggledash2"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
                                        downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button"))
                                    ),
                                    uiOutput(ns("indicator2")),
                                    uiOutput(ns("title2")),
                                    DT::dataTableOutput(nsChart("Dashboardtable"))%>%withSpinner()
                           )
               ),
               uiOutput(ns("source")),
               p("Visit the",
               a(href="http://www.nationalperformance.gov.scot",target="blank_","National Performance Framework website"),
               "for more information on the National Indicators.")
        )
        
      )
#      uiOutput(ns("DashboardTable"))
      
  )
}

pieChartUI0 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          gvisDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
          downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(12,id=nsChart("displayPlot"),class="eef-plot",uiOutput(nsChart("plot"), height = 400))
      )#,
      #tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('",ns("reload"),"',Math.random());});"))
  )
}

pieChartUI1 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          #actionButton(ns("zoom"),img(src="icons/zoom-in.svg",width=20),class=paste0(ns("btn")," eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          gvisDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
          downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
          tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(9,id=nsChart("displayPlot"),class="eef-plot",uiOutput(nsChart("plot"), height = 400)),
        column(3,id=nsChart("displayOptions"),
               #tags$button(id = nsChart("toggledash"), "Show/Hide", href = "#", class="eef-options-toggle greyscalebutton"),
               div(id=ns("column1"),uiOutput(ns("optionMeasure")))
        )
      )#,
      #tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('",ns("reload"),"',Math.random());});"))
  )
}

pieChartUI2 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  nsChart1 <- NS(NS(id,"chart1"))
  nsChart2 <- NS(NS(id,"chart2"))
  nsChart3 <- NS(NS(id,"chart3"))
  nsChart4 <- NS(NS(id,"chart4"))
  icon1 <- NULL
  icon2 <- NULL
  icon3 <- NULL
  icon4 <- NULL
  if(!is.null(graphOptions$icon)) {
    if(!is.na(graphOptions$icon[1])) icon1 <- div(class="w3-center eef-pie-chart-overlay",img(src=graphOptions$icon[1],height=60,width=60))
    if(!is.na(graphOptions$icon[2])) icon2 <- div(class="w3-center eef-pie-chart-overlay",img(src=graphOptions$icon[2],height=60,width=60))
    if(!is.na(graphOptions$icon[3])) icon3 <- div(class="w3-center eef-pie-chart-overlay",img(src=graphOptions$icon[3],height=60,width=60))
    if(!is.na(graphOptions$icon[4])) icon4 <- div(class="w3-center eef-pie-chart-overlay",img(src=graphOptions$icon[4],height=60,width=60))
  }
  tagList(
    div(class="eef-graph-header",
          #actionButton(ns("zoom"),img(src="icons/zoom-in.svg",width=20),class=paste0(ns("btn")," eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
          tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      #panelData$subtitle,
      fluidRow(
        column(9,id=nsChart("displayPlot"),div(class="eef-clearfix",
                     div(class="float eef-pie-chart-container",uiOutput(nsChart1("plot"), height = 300),icon1),
                     div(class="float eef-pie-chart-container",uiOutput(nsChart2("plot"), height = 300),icon2),
                     div(class="float eef-pie-chart-container",uiOutput(nsChart3("plot"), height = 300),icon3),
                     div(class="float eef-pie-chart-container",uiOutput(nsChart4("plot"), height = 300),icon4))),
        column(3,id=nsChart("displayOptions"),
               #tags$button(id = nsChart("toggledash"), "Show/Hide", href = "#", class="eef-options-toggle greyscalebutton"),
               div(id=ns("column1"),uiOutput(ns("optionMeasure")))
        )
      )#,
      #tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('",ns("reload"),"',Math.random());});"))
  )
}

barChartUI0 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          #actionButton(ns("zoom"),img(src="icons/zoom-in.svg",width=20),class=paste0(ns("btn")," eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          tags$button(id=nsChart("zoomIn"),class=paste0(ns("btn")," btn eef-graph-header-button action-button shiny-bound-input"),type="button",img(src="icons/zoom-in.svg",width=20)),
          tags$button(id=nsChart("zoomOut"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-out.svg",width=20)),
        #downloadLink(nsChart("plotPNG"),tagList(icon("download"),"download chart"),class=paste0(ns("btn")," btn eef-graph-header-button")),
        #gvisDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
          downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
        tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(12,id=nsChart("displayPlot"),class="eef-plot ",plotlyOutput(nsChart("plot"), height = 400))
      )#,
      #tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('",ns("reload"),"',Math.random());});"))
  )
}


barChartUI1 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          #actionButton(ns("zoom"),img(src="icons/zoom-in.svg",width=20),class=paste0(ns("btn")," eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          tags$button(id=nsChart("zoomIn"),class=paste0(ns("btn")," btn eef-graph-header-button action-button shiny-bound-input"),type="button",img(src="icons/zoom-in.svg",width=20)),
          tags$button(id=nsChart("zoomOut"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-out.svg",width=20)),
        #  gvisDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
        #downloadLink(nsChart("plotPNG"),tagList(icon("download"),"download chart"),class=paste0(ns("btn")," btn eef-graph-header-button")),
        downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
        tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(9,id=nsChart("displayPlot"),class="eef-plot ",plotlyOutput(nsChart("plot"), height = 400)),
        column(3,id=nsChart("displayOptions"),
               #tags$button(id = nsChart("toggledash"), "Show/Hide", href = "#", class="eef-options-toggle greyscalebutton"),
               div(id=ns("column1"),uiOutput(ns("optionBreakdown")))
        )
      )#,
      #tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('",ns("reload"),"',Math.random());});"))
  )
}

barChartUI2 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          #actionButton(ns("zoom"),img(src="icons/zoom-in.svg",width=20),class=paste0(ns("btn")," eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          tags$button(id=nsChart("zoomIn"),class=paste0(ns("btn")," btn eef-graph-header-button action-button shiny-bound-input"),type="button",img(src="icons/zoom-in.svg",width=20)),
          tags$button(id=nsChart("zoomOut"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-out.svg",width=20)),
        #downloadLink(nsChart("plotPNG"),tagList(icon("download"),"download chart"),class=paste0(ns("btn")," btn eef-graph-header-button")),
        #gvisDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
          downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
        tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(9,id=nsChart("displayPlot"),class="eef-plot ",plotlyOutput(nsChart("plot"), height = 400)),
        column(3,id=nsChart("displayOptions"),
               #tags$button(id = nsChart("toggledash"), "Show/Hide", href = "#", class="eef-options-toggle greyscalebutton"),
               div(id=ns("column1"),uiOutput(ns("optionIndicator")),uiOutput(ns("optionBreakdown")))
        )
      )#,
      #tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('",ns("reload"),"',Math.random());});"))
  )
}

barChartUI3 <- function(id,graphOptions=list()) {
  ns <- NS(id)
  nsChart <- NS(NS(id,"chart"))
  tagList(
    div(class="eef-graph-header",
          #actionButton(ns("zoom"),img(src="icons/zoom-in.svg",width=20),class=paste0(ns("btn")," eef-graph-header-button")),
          tags$button(id=ns("info"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/information.svg",width=20)),
          tags$button(id=nsChart("zoomIn"),class=paste0(ns("btn")," btn eef-graph-header-button action-button shiny-bound-input"),type="button",img(src="icons/zoom-in.svg",width=20)),
          tags$button(id=nsChart("zoomOut"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/zoom-out.svg",width=20)),
          gvisDownload(nsChart("plot"),nsChart("png"), "download chart", asbutton = TRUE,class=paste0(ns("btn")," eef-graph-header-button")),
        #downloadLink(nsChart("plotPNG"),tagList(icon("download"),"download chart"),class=paste0(ns("btn")," btn eef-graph-header-button")),
        #downloadLink(nsChart("csv"),tagList(icon("download"),"download data"),class=paste0(ns("btn")," btn eef-graph-header-button")),
        tags$button(id=nsChart("toggledash"),class=paste0(ns("btn")," btn eef-graph-header-button"),img(src="icons/menu.svg",width=20)),
          tags$script(paste0("$('.",ns("btn"),"').click(function(){gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});"))
      ),
      fluidRow(
        column(9,id=nsChart("displayPlot"),class="eef-plot ",plotlyOutput(nsChart("plot"), height = 400)),
        column(3,id=nsChart("displayOptions"),
               #tags$button(id = nsChart("toggledash"), "Show/Hide", href = "#", class="eef-options-toggle greyscalebutton"),
               div(id=ns("column1"),uiOutput(ns("optionMeasure")),hr(),uiOutput(ns("optionBreakdown")))
        )
      )#,
      #tags$script(paste0("  $('a[data-toggle=\"tab\"]').on('shown.bs.tab', function (e) {Shiny.onInputChange('",ns("reload"),"',Math.random());});"))
  )
}

