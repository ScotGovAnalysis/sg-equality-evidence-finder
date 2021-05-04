###################################################################################
#                                                                                 #
# HELPER FUNCTIONS - GRAPHS                                                       #
#                                                                                 #
###################################################################################
#                                                                                 #
# Author: Jay Ware                                                                #
# Created: 01/08/2018                                                             #
# Last updated: 14/01/2021                                                        #
#                                                                                 #
# Purpose: Functions used for formatting chart axes, arranging bar chart order,   #
#          and padding axes ranges to avoid anything being cut off the ends       #
#                                                                                 #
# Functions:                                                                      #
#                                                                                 #
# eefDygraphFormatter(): dygraph axes and values are formatted using javascript.  #
#                        This function generates the relevant format code for use #
#                        in time series charts. The interval type is set in       #
#                        graphOptions$intervalType                                #
#                                                                                 #
# padRange(): takes the range of the data and adds a bit more either side for     #
#             plots (defaults to +/- 10% of the range)                            #
#                                                                                 #
# arrangeBreakdown(): sorts bar charts into ascending/descending order, and       #
#                     places particular categories (e.g. total) at top/bottom)    #
#                     [col - specifies the column to sort by; top - regular       #
#                      expression for specifying categories to appear first;      #
#                      bottom - regular expression for specifying categories to   #
#                      appear last; sortOrder - "decreasing"/"increasing"/NA]     #
#                                                                                 #
###################################################################################

#function providing the axisLabelFormatter, and valueFormatter javascript functions required by Dygraphs. If necessary the graphData is also shifted by months to ensure the ticker is at the required month
eefDygraphFormatter <- function(graphData,intervalType="Year",dateRange=NULL,xFormat=NULL) {
  
  #Goes through each of the interval types and adds the corresponding javascript statement. Dygraphs uses this javascript to display x-axis/values
  if(!is.null(xFormat)) {
    if(is.list(xFormat)) axisLabelFormat <- xFormat$xAxisFormat else axisLabelFormat <- xFormat
    if(is.list(xFormat)) valueFormat <- xFormat$xValueFormat else valueFormat <- xFormat
  } else if(intervalType=="Year") {#TO DO - Add more intervalTypes
    axisLabelFormat <- "d.toLocaleString('en-GB',{year: 'numeric'});"
    valueFormat <- "date.toLocaleString('en-GB',{year: 'numeric'});"
  } else if(intervalType=="Quarterly") {
    axisLabelFormat <- "d.toLocaleString('en-GB',{year: 'numeric',month: 'short'});"
    valueFormat <- "date.toLocaleString('en-GB',{year: 'numeric',month: 'short'});"
  } else if(intervalType=="Month") {
    axisLabelFormat <- "d.toLocaleString('en-GB',{year: 'numeric',month: 'short'});"
    valueFormat <- "date.toLocaleString('en-GB',{year: 'numeric',month: 'short'});"
  } else if(intervalType=="Academic") {#TO DO - Add more intervalTypes
    axisLabelFormat <- "(year-1)+'/'+year.toString().substr(2,2);"
    valueFormat <- "(year-1)+'/'+year.toString().substr(2,2);"
  } else if(intervalType=="1 year") {#TO DO - Add more intervalTypes
    axisLabelFormat <- "(year-1)+'-'+year.toString().substr(2,2);"
    valueFormat <- "(year-1)+'-'+year.toString().substr(2,2);"
  } else if(intervalType=="2 year") {#TO DO - Add more intervalTypes
    axisLabelFormat <- "(year-2)+'-'+year.toString().substr(2,2);"
    valueFormat <- "(year-2)+'-'+year.toString().substr(2,2);"
  } else if(intervalType=="3 year") {#TO DO - Add more intervalTypes
    axisLabelFormat <- "(year-3)+'-'+year.toString().substr(2,2);"
    valueFormat <- "(year-3)+'-'+year.toString().substr(2,2);"
  } else if(intervalType=="4 year") {#TO DO - Add more intervalTypes
    axisLabelFormat <- "(year-4)+'-'+year.toString().substr(2,2);"
    valueFormat <- "(year-4)+'-'+year.toString().substr(2,2);"
  } else if(intervalType=="Numeric") {#TO DO - Add more intervalTypes
    axisLabelFormat <- NULL
    valueFormat <- NULL
  } else {#if not one of the standard interval types treats intervalType as a javascript function. If it isn't a javascript function then the graph will break!
    axisLabelFormat <- intervalType
    valueFormat <- intervalType
  }
  
  if(intervalType=="Month") {
    if(is.null(dateRange)) dateRange <- max(graphData$Date,na.rm=TRUE)-min(graphData$Date,na.rm=TRUE) else dateRange <- as.Date(dateRange[2])-as.Date(dateRange[1])
    maxYear <- year(max(graphData$Date,na.rm=TRUE))
    axisMonth <- month(max(graphData$Date,na.rm=TRUE))
    ticker <- "function (a, b, pixels, opts, dygraph, vals) {return Dygraph.getDateAxis(a, b, Dygraph.MONTHLY, opts, dygraph);}"
    axisLabelFormatter <- paste0("function(d,gran) {if(d.getMonth()==",axisMonth-1,") {var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    if(dateRange>2000) axisLabelFormatter <- paste0("function(d,gran) {if((d.getMonth()==",axisMonth-1,")&&(((d.getFullYear()-",maxYear,") % 2)==0)){var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    if(dateRange>5000) axisLabelFormatter <- paste0("function(d,gran) {if((d.getMonth()==",axisMonth-1,")&&(((d.getFullYear()-",maxYear,") % 5)==0)){var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    valueFormatter <- paste0("function(d) {var date = new Date(d);var month = date.getMonth();var year = date.getFullYear();return ",valueFormat,"}")
  } else if(intervalType %in% c("Year", "Quarterly", "Academic", "1 year", "2 year", "3 year", "4 year")){
    if(is.null(dateRange)) dateRange <- max(graphData$Date,na.rm=TRUE)-min(graphData$Date,na.rm=TRUE) else dateRange <- as.Date(dateRange[2])-as.Date(dateRange[1])
    maxYear <- year(max(graphData$Date,na.rm=TRUE))
    axisMonth <- month(max(graphData$Date,na.rm=TRUE))
    ticker <- "function (a, b, pixels, opts, dygraph, vals) {return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph);}"
    axisLabelFormatter <- paste0("function(d,gran) {var year = d.getFullYear();return ",axisLabelFormat,"}")
    if(dateRange>2000) axisLabelFormatter <- paste0("function(d,gran) {if(((d.getFullYear()-",maxYear,") % 2)==0){var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    if(dateRange>5000) axisLabelFormatter <- paste0("function(d,gran) {if(((d.getFullYear()-",maxYear,") % 5)==0){var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    valueFormatter <- paste0("function(d) {var date = new Date(d);var month = date.getMonth();var year = date.getFullYear();return ",valueFormat,"}")
  } else {
    ticker <- NULL
    axisLabelFormatter <- axisLabelFormat
    valueFormatter <- valueFormat
  }
  
  return(list(data=graphData,
              ticker=ticker,
              axisLabelFormatter=axisLabelFormatter,
              valueFormatter=valueFormatter))
}

padRange <- function(d,padding=0.1) {
  d <- na.omit(d)
  if(min(d) >= 0) {
    padmin <- min(d) - padding*(min(d))
  } else {
    padmin <- min(d) + padding*(min(d))
  }
  if(max(d) >= 0) {
    padmax <- max(d) + padding*(max(d))
  } else {
    padmax <- max(d) - padding*(max(d))
  }
  return(c(padmin, padmax))
}

#utility function used for arranging bar charts into ascending/descending order with "Total" moved to the top/bottom
#extension of dplyr::arrange
arrangeBreakdown <- function(dataset,col,top="\\b((total)|(all))\\b",bottom="^$",sortOrder="decreasing",...) {
  if(is.numeric(col)) col <- as.character(names(dataset)[col])
  if(tolower(sortOrder)%in%"decreasing") return(arrange(dataset,grepl(bottom,tolower(Breakdown)),!grepl(top,tolower(Breakdown)),desc(.data[[col]])))
  if(tolower(sortOrder)%in%"increasing") return(arrange(dataset,grepl(bottom,tolower(Breakdown)),!grepl(top,tolower(Breakdown)),.data[[col]])) 
  return(arrange(dataset,grepl(bottom,tolower(Breakdown)),!grepl(top,tolower(Breakdown))))
}  



