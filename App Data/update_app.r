###################################################################################
#                                                                                 #
# Author: Jaye Ware                                                               #
# Last updated: 26/07/2019                                                        #
#                                                                                 #
# Purpose: Updates the app files. Updates data from statistics.gov.scot and       #
#          converts Rmd to markdown. This is done before deployment to reduce     #
#          processing done live in the app                                        #
#                                                                                 #
# Functions:                                                                      #
# updateAppData(): Function that updates the EEF App data file and converts Rmd   #
#                  files. Called automatically by updateEEF() and updateNPF()     #
#                  Required parameters:                                           #
#                     - appDir: path to the directory containing the app          #
#                     - dataDir: path to the directory containing the data        #
#                  Optional:                                                      #
#                     - policyArea: only update files for a single policy area    #
#                                                                                 #
###################################################################################


#packages
library(lubridate)
library(readxl)
library(shiny)
library(shinyjs)
library(tidyr)
library(dplyr)
library(xts)
library(dygraphs)
library(zoo)
library(googleVis)
library(rmarkdown)
library(knitr)
onScots <- !require(SPARQL) #package that doesn't work on Scots. onScots is a flag used to switch off functionality incompatible with Scots

updateAppData <- function(appDir,dataDir,policyArea=NULL) {
  
  #read in latest data
  load(file.path(dataDir,"EEF.rData"))
  
  #load existing graph data
  load(file.path(appDir,"EEF/graph_data.rData"))
  
  source(file.path(appDir,"EEF scripts/helper_funcs.r"),local=T)
  source(file.path(dataDir,"pre_process_EEF.r"),local=TRUE)
  source(file.path(dataDir,"graph_data.r"),local=TRUE)
  
  if(is.null(policyArea)) {
    updateGraphs <- intersect(EEFindex$tabUID,names(filterData))
    tabNotFound <- setdiff(names(filterData),EEFindex$tabUID)
    if(length(tabNotFound)>0) warning("EVIDENCE FINDER WARNING: TabUID(s) not found in EEFindex. Check graph has been specified correctly for:-\n",paste0(tabNotFound,collapse="\n"))
  } else {
    updateGraphs <- intersect(filter(EEFindex,policy_area%in%policyArea)$tabUID,names(filterData))
  }
  
  dataNotFound <- character(0)
  if(length(updateGraphs)>0) for(i in updateGraphs) {
    if(is.null(graphOptions[[i]])) graphOptions[[i]] <- list()

    #check that data was found for tabUID
    if(nrow(filterData[[i]])==0) {
      dataNotFound <- c(dataNotFound,i)
      next
    }
    
    graphOptions[[i]]$panelData <- filter(EEFindex,tabUID==i)%>%mutate(subtitle=ifelse(is.na(subtitle),"",subtitle))
    if(is.null(graphOptions[[i]]$query)) {
      if(!is.null(graphOptions[[i]]$intervalType)) filterData[[i]]$intervalType <- graphOptions[[i]]$intervalType
      if(!is.null(graphOptions[[i]]$ylabel)) filterData[[i]]$yLabel <- graphOptions[[i]]$ylabel else filterData[[i]]$yLabel <- filterData[[i]]$Measure
      if(length(graphOptions[[i]]$panelData$index_updated)==1) filterData[[i]]$LastUpdated <- graphOptions[[i]]$panelData$index_updated else filterData[[i]]$LastUpdated <- Sys.Date()
      filterData[[i]] <- mutate(filterData[[i]],
                                DateCode=as.character(Year),
                                DateCode=ifelse(intervalType=="Month",format(Date,"%b %Y"),DateCode),
                                DateCode=ifelse(intervalType=="Quarterly",paste0(Year,"-Q",1+month(Date)%/%3),DateCode),
                                DateCode=ifelse(intervalType=="Academic",paste0(Year-1,"/",substr(Year,3,4)),DateCode),
                                DateCode=ifelse(intervalType=="1 year",paste0(Year-1,"-",substr(Year,3,4)),DateCode),
                                DateCode=ifelse(intervalType=="2 year",paste0(Year-2,"-",substr(Year,3,4)),DateCode),
                                DateCode=ifelse(intervalType=="3 year",paste0(Year-3,"-",substr(Year,3,4)),DateCode),
                                DateCode=ifelse(intervalType=="4 year",paste0(Year-4,"-",substr(Year,3,4)),DateCode)
      )
      if(!is.null(graphOptions[[i]]$digits)) filterData[[i]] <- mutate(filterData[[i]],Figure=round(Figure,graphOptions[[i]]$digits))
    } else {
      #update data from statistics.gov.scot (this will also run the validation checks)
      if(!onScots) filterData[[i]] <- EEFsparql(filterData[[i]],graphOptions[[i]],ns=NS(i)) else filterData[[i]] <- graph[[i]]#
    }
    graph[[i]] <- filterData[[i]]
  }
  if(length(dataNotFound)>0) warning("EVIDENCE FINDER WARNING: No data found for TabUID(s). Check graph has been specified correctly for:-\n",paste0(dataNotFound,collapse="\n"))
  
  #TEMP FIX
  #graph[["npf"]] <- mutate(graph[["npf"]],DateCode=Yearlab)
  
  rmdFiles <- filter(EEFindex,!is.na(inputMarkdownFile),file.exists(inputMarkdownFile),grepl("\\.Rmd$",inputMarkdownFile,ignore.case=TRUE))
  # if(!is.null(policyArea)) rmdFiles <- filter(rmdFiles,policy_area%in%policyArea)
  # if(length(rmdFiles)>0) for(r in rmdFiles$tabUID) {
  #   panelData <- filter(EEFindex,tabUID==r)
  #   graphData <- graph[[r]]
  #   render(file.path(appDir,panelData$inputMarkdownFile),output_file=file.path(appDir,panelData$markdownFile),
  #          output_format = "md_document",encoding="UTF-8",quiet=TRUE)
  # }
  if(length(rmdFiles)>0) updatedRmd <- lapply(rmdFiles$tabUID,
                                              function(r){
                                                panelData <- filter(EEFindex,tabUID==r)
                                                graphData <- graph[[r]]
                                                render(file.path(appDir,panelData$inputMarkdownFile),output_file=file.path(appDir,panelData$markdownFile),
                                                       output_format = "md_document",encoding="UTF-8",quiet=TRUE)
                                                
                                 })

  #dataset summarising the data taken from ODP.
  SPARQLindex <- lapply(names(graphOptions),
                        function(x) {
                          if(!is.null(graphOptions[[x]]$query)) {
                            graph[[x]] %>% distinct(Indicator,Measure,Breakdown,graphTitle,seriesColour,LastUpdated,NextUpdated) %>% mutate(tabUID=x)
                          }
                        }) %>% bind_rows
  View(SPARQLindex) #for checking SPARQL datasets look okay, e.g. Indicator, Measure, Breakdowns as expected; data is up to date (note: SPARQL data is only for the policyAreas specified when running updateAppData())
  
  save(EEFsources,
       EEFlatestSource,
       EEFindex,
       #NPFdata,
       NPFindex,
       #NPFextra,
       EEFpublished,
       EEFdataLinks,
       EEFexternal,
       EEFadditional,
       EEFpublishedIndex,
       EEFdataLinksIndex,
       EEFexternalIndex,
       NPFdataIndex,
       file=file.path(appDir,"EEF/index.rData"),envir=environment())
  save(graph,
       graphOptions,
       SPARQLindex,
       file=file.path(appDir,"EEF/graph_data.rData"),envir=environment())
}

testSparql <- function(tabID,dataDir,appDir=getwd()) {
  #read in latest data
  load(file.path(dataDir,"EEF.rData"))
  
  source(file.path(appDir,"EEF scripts/helper_funcs.r"),local=T)
  source(file.path(dataDir,"pre_process_EEF.r"),local=TRUE)
  source(file.path(dataDir,"graph_data.r"),local=TRUE)
  
  sparql <- list()
  EEFlookup <- list()
  dataset <- list()
  if(length(tabID)>0) for(i in tabID) {
    if(is.null(graphOptions[[i]])) graphOptions[[i]] <- list()
    
    if(!is.null(graphOptions[[i]]$query)) 
      #update data from statistics.gov.scot (this will also run the validation checks)
      sparql[[i]] <- SPARQL("http://statistics.gov.scot/sparql",graphOptions[[i]]$query)$results
      EEFlookup[[i]] <- filterData[[i]]
      dataset[[i]] <- EEFsparql(filterData[[i]],graphOptions[[i]],ns=NS(i)) 
      
      cat(i,"Sparql data from query:\n")
      print(sparql[[i]])
      cat(i,"EEF lookup table:\n")
      print(EEFlookup[[i]])
  }
  
  invisible(dataset)
}