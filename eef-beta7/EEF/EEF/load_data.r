#packages
library(lubridate)
library(readxl)
library(dplyr)
library(shiny)
require(shinyjs)
library(tidyr)
library(xts)
library(dygraphs)
library(zoo)
library(googleVis)

#load data
load("EEF/preload.dat")
# EEFsources <- read.csv("EEF/data sources.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))
# EEFsources <- as.tbl(EEFsources) %>%
#   select(-policy_area) %>%
#   mutate(published_date=as.Date(published_date),
#          next_updated=as.Date(next_updated),
#          published_year=ifelse(published_year%in%c(NA,"",0),as.numeric(substr(published_date,1,4)),published_year),
#          sort_date=ifelse(published_date%in%c(NA,"",0),ymd(paste0(published_year,"-1-1"),quiet=TRUE),published_date))
# 
# EEFlatestSource <- 
#   EEFsources %>%
#   group_by(series_name) %>%
#   summarise(sort_date=max(sort_date,na.rm=TRUE)) %>%
#   inner_join(EEFsources,by=c("series_name","sort_date"))
# 
# EEFpublished <- read.csv("EEF/publication links.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))
# EEFpublished <- inner_join(EEFpublished,EEFlatestSource,by="series_name")
# 
# EEFdataLinks <- read.csv("EEF/data links.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))
# 
# EEFexternal <- read.csv("EEF/external links.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))
# 
# EEFindex <- read.csv("EEF/modules.csv",stringsAsFactors = FALSE,na.strings=c("","NA")) %>%    
#   mutate(cloneTabID=as.character(cloneTabID)) 
# 
# EEFindex <- left_join(EEFindex,
#                       select(EEFindex,tabUID,headline,graphID,subtitle,image,dataSource,dataSourceNotes,markdownFile),
#                       by=c("cloneTabID"="tabUID")) %>%
#   mutate(headline=ifelse(is.na(headline.x),headline.y,headline.x),
#          graphID=ifelse(is.na(graphID.x),graphID.y,graphID.x),
#          subtitle=ifelse(is.na(subtitle.x),subtitle.y,subtitle.x),
#          image=ifelse(is.na(image.x),image.y,image.x),
#          dataSource=ifelse(is.na(dataSource.x),dataSource.y,dataSource.x),
#          dataSourceNotes=ifelse(is.na(dataSourceNotes.x),dataSourceNotes.y,dataSourceNotes.x),
#          markdownFile=ifelse(is.na(markdownFile.x),markdownFile.y,markdownFile.x)
#   )
# EEFindex <- group_by(EEFindex,policy_area) %>% mutate(topicID=match(topic,unique(topic)))
# 
# EEFdata <- read.csv("EEF/data.csv",stringsAsFactors=FALSE,na.strings=c("","NA")) %>% arrange(Characteristic)
# 
# 
# EEFlatest <- EEFdata %>%
#   group_by(Breakdown,SubBreakdown,Characteristic,Topic) %>%
#   summarise(Year=max(Year,na.rm=TRUE)) %>%
#   merge(EEFdata)
# 
# 
# 
# 
# #Section headings - don't need (probably...)
# # topicName <- character(0)
# # for(i in unique(EEFindex$TopicUID)) topicName[i] <- as.character(EEFindex$Topic[match(i,EEFindex$TopicUID)])
# # topicTitle <- character(0)
# # for(i in unique(EEFindex$TopicUID)) topicTitle[i] <- as.character(EEFindex$Title[match(i,EEFindex$TopicUID)])
# 
