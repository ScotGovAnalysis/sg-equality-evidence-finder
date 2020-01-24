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
#                     - policyArea: only update files for specified policy areas  #
#                     - tabUIDs: only update files for specified tabUIDs          #
#                                                                                 #
###################################################################################


#packages
library(lubridate)
library(readxl)
library(openxlsx)
library(shiny)
library(shinyjs)
library(tidyr)
library(dplyr)
library(xts)
library(dygraphs)
library(zoo)
library(googleVis)
library(rmarkdown)
library(markdown)
library(knitr)
library(purrr)
onScots <- !require(SPARQL) #package that doesn't work on Scots. onScots is a flag used to switch off functionality incompatible with Scots

updateAppData <- function(appDir,dataDir,scriptDir,policyArea=NULL,tabUIDs=NULL,dataFile="EEF.rData",test=FALSE) {
  message("Updating ",appDir)
  #read in latest data
  load(file.path(dataDir,dataFile))
  #load existing graph data
  load(file.path(dataDir,"graph_data.rData"))
  
  source(file.path(appDir,"EEF scripts/helper_funcs.r"),local=T)
  source(file.path(scriptDir,"pre_process_EEF.r"),local=TRUE)
  source(file.path(scriptDir,"graph_data.r"),local=TRUE)
  
  updateTabUIDs <- filter(EEFindex,!is.na(tabUID),is.na(cloneTabID))
  if(!is.null(policyArea)) updateTabUIDs <- filter(updateTabUIDs,policy_area %in% policyArea)
  if(!is.null(tabUIDs)) updateTabUIDs <- filter(updateTabUIDs,tabUID %in% tabUIDs)
  updateTabUIDs <- updateTabUIDs$tabUID
     
  tabNotFound <- setdiff(names(setGraphData),EEFindex$tabUID)
  if(length(tabNotFound)>0) warning("EVIDENCE FINDER WARNING: Graph TabUID(s) not found in EEFindex. Check graph has been specified correctly for:-\n",paste0(tabNotFound,collapse="\n"))

  #add panels to the update list that are duplicates of the panels being updated
  updateTabUIDs <- c(updateTabUIDs,filter(EEFindex,cloneTabID %in%updateTabUIDs)$tabUID) %>% unique

  dataNotFound <- character(0)
  if(length(updateTabUIDs)>0) for(i in updateTabUIDs) {
    indexData <- filter(EEFindex,tabUID==i)
    
    #copy graph data and options if the cloneTabID column is filled (copies the graph for cloneTabID)
    #only if setGraphOptions[[i]] isn't already specified
    if(is.null(setGraphOptions[[i]])) {
      if(!indexData$cloneTabID[1]%in%c("",NA)) {
        setGraphData[[i]] <- setGraphData[[indexData$cloneTabID[1]]]
        setGraphOptions[[i]] <- setGraphOptions[[indexData$cloneTabID[1]]]
      } else {
        setGraphOptions[[i]] <- list()
      }
    }
    if(is.null(setGraphData[[i]])) {
      graph[[i]] <- NULL
      graphOptions[[i]] <- NULL
      next
    }
    
    setGraphOptions[[i]]$panelData <- indexData%>%mutate(subtitle=ifelse(is.na(subtitle),"",subtitle))
    
    #check that data was found for tabUID
    if(nrow(setGraphData[[i]])==0) {
      dataNotFound <- c(dataNotFound,i)
      next
    }
    
    if(is.null(setGraphOptions[[i]]$query)) {
      if(!is.null(setGraphOptions[[i]]$intervalType)) setGraphData[[i]]$intervalType <- setGraphOptions[[i]]$intervalType
      if(!is.null(setGraphOptions[[i]]$ylabel)) setGraphData[[i]]$yLabel <- setGraphOptions[[i]]$ylabel else setGraphData[[i]]$yLabel <- setGraphData[[i]]$Measure
      if(is.null(setGraphData[[i]]$LastUpdated)) {
        if(length(setGraphOptions[[i]]$panelData$index_updated)==1) setGraphData[[i]]$LastUpdated <- setGraphOptions[[i]]$panelData$index_updated else setGraphData[[i]]$LastUpdated <- Sys.Date()
      }
      
      setGraphData[[i]] <- addDateCode(setGraphData[[i]])
      #if(!is.null(setGraphOptions[[i]]$digits)) setGraphData[[i]] <- mutate(setGraphData[[i]],Figure=round(Figure,setGraphOptions[[i]]$digits))
    } else {
      #update data from statistics.gov.scot (this will also run the validation checks)
      #if(!onScots) setGraphData[[i]] <- EEFsparql(setGraphData[[i]],setGraphOptions[[i]],ns=NS(i)) else setGraphData[[i]] <- graph[[i]]#
      setGraphData[[i]] <- EEFsparql(setGraphData[[i]],setGraphOptions[[i]],id=i) 
      if(is.null(setGraphData[[i]])) setGraphData[[i]] <- graph[[i]]#
    }
    graph[[i]] <- setGraphData[[i]]
    graphOptions[[i]] <- setGraphOptions[[i]]
    #View(graph[[i]])
  }
  if(length(dataNotFound)>0) warning("EVIDENCE FINDER WARNING: No data found for TabUID(s). Check graph has been specified correctly for:-\n",paste0(dataNotFound,collapse="\n"))
  
  #Convert markdown files to HTML (knitting if appropriate)
  # commentary <- list()
  # commentary[EEFindex$tabUID] <- sapply(1:nrow(EEFindex),function(r){
  #   panelData <- EEFindex[r,]
  #   if(nrow(panelData)==0) return("")
  #   graphData <- graph[[panelData$tabUID[1]]]
  #   if(is.na(file.path(appDir,panelData$inputMarkdownFile))) return("")
  #   if(!file.exists(file.path(appDir,panelData$inputMarkdownFile))) return("")
  #   knit(text=readLines(file.path(appDir,panelData$inputMarkdownFile),warn=FALSE,encoding="UTF-8"), quiet = TRUE,encoding="UTF-8") %>%
  #     markdown::markdownToHTML(text=.,fragment.only=TRUE,encoding="UTF-8") %>%
  #     HTML
  # })

    if(!exists("commentary")) commentary <- character()
  if(length(updateTabUIDs)>0) {
    commentary[updateTabUIDs] <- sapply(updateTabUIDs,function(id){
      panelData <- filter(EEFindex,tabUID==id)
      if(nrow(panelData)==0) return("")
      graphData <- graph[[id]]

      if(is.na(file.path(appDir,panelData$inputMarkdownFile))) return("")
      if(!file.exists(file.path(appDir,panelData$inputMarkdownFile))) return("")
      tryCatch(
        knit(text=readLines(file.path(appDir,panelData$inputMarkdownFile),warn=FALSE,encoding="UTF-8"), quiet = TRUE,encoding="UTF-8") %>%
        markdown::markdownToHTML(text=.,fragment.only=TRUE,encoding="UTF-8") %>%
        HTML,
        error = function(e) {
          if(interactive()) View(graphData)
          stop("Unable to update commentary for ",id," (",panelData$markdownFile,")")
          }
      )
    })
  }
  
  # rmdFiles <- filter(EEFindex,!is.na(inputMarkdownFile),file.exists(inputMarkdownFile),grepl("\\.Rmd$",inputMarkdownFile,ignore.case=TRUE))
  # if(!is.null(policyArea)) rmdFiles <- filter(rmdFiles,policy_area%in%policyArea)
  #  if(length(rmdFiles)>0) updatedRmd <- lapply(rmdFiles$tabUID,
  #                                             function(r){
  #                                               panelData <- filter(EEFindex,tabUID==r)
  #                                               graphData <- graph[[r]]
  #                                               render(file.path(appDir,panelData$inputMarkdownFile),output_file=file.path(appDir,panelData$markdownFile),
  #                                                      output_format = "md_document",encoding="UTF-8",quiet=TRUE)
  #                                               
  #                                })

  #dataset summarising the data taken from ODP.
  # SPARQLindex <- lapply(names(graphOptions),
  #                       function(x) {
  #                         if(!is.null(graphOptions[[x]]$query)) {
  #                           graph[[x]] %>% distinct(Indicator,Measure,Breakdown,graphTitle,seriesColour,LastUpdated,NextUpdated) %>% mutate(tabUID=x)
  #                         }
  #                       }) %>% bind_rows
  # View(SPARQLindex) #for checking SPARQL datasets look okay, e.g. Indicator, Measure, Breakdowns as expected; data is up to date (note: SPARQL data is only for the policyAreas specified when running updateAppData())
  # 
  save(EEFsources,
       EEFlatestSource,
       EEFindex,
       #NPFdata,
       NPFindex,
       #NPFcharacteristics,
       #NPFextra,
       EEFpublished,
       EEFdataLinks,
       EEFexternal,
       EEFadditional,
       EEFpublishedIndex,
       EEFdataLinksIndex,
       EEFexternalIndex,
       NPFdataIndex,
       file=file.path(appDir,"EEF/index.rData"),envir=environment(),version=2)
  save(graph,
       NPFcharacteristics,
       graphOptions,
       SPARQLindex,
       commentary,
       file=file.path(appDir,"EEF/graph_data.rData"),envir=environment(),version=2)
  if(test!=TRUE) 
    save(graph,
         graphOptions,
         SPARQLindex,
         NPFcharacteristics,
         commentary,
         file=file.path(dataDir,"graph_data.rData"),envir=environment(),version=2)
  message("Updated ",length(updateTabUIDs)," panels")
}

#This function will trigger all the ODP query strings used in the app.
#Validation errors/warnings will be flagged up in the console
testSparql <- function(appDir=getwd()) {
  source(file.path(appDir,"EEF Scripts/helper_funcs.r"),local=T)
  load(file.path(appDir,"EEF/graph_data.rData"))
  load(file.path(appDir,"EEF/index.rData"))
  
  #test NPF data
  readNPF <- readNPFdata()
  
  #test ODP queries
  readODP <- lapply(names(graph),
         function(x) {
           if(identical(graphOptions[[x]]$updateQuery,TRUE)) {
             EEFsparql(graph[[x]],graphOptions[[x]],x)
           } else if(identical(graphOptions[[x]]$updateNPF,TRUE) & !is.null(readNPF)) {
              readNPF %>% right_join(distinct(graph[[x]],Indicator,Characteristic),by=c("Indicator","Characteristic"))
           } else NA
         })
}

