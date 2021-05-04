###################################################################################
#                                                                                 #
# DATA PROCESSING                                                                 #
#                                                                                 #
###################################################################################
#                                                                                 #
# Author: Jay Ware                                                                #
# Created: 01/08/2018                                                             #
# Last updated: 14/01/2021                                                        #
#                                                                                 #
# Purpose: Functions for processing data that is done within the app itself. This #
#          includes reading data in from the open data platform, adding interval  #
#          types (year, month, multiple years, etc.), and adding a label for      #
#          dates.                                                                 #
#                                                                                 #
# Functions:                                                                      #
#                                                                                 #
# addIntervalType(): Takes a dataset with a Year and/or Date column and adds an   #
#                    appropriate intervalType column. The function can detect     #
#                    commonly used types e.g. 2020/21, 2016-19, etc. Alternatively#
#                    intervalType can be specified directly in graphOptions       # 
# readNPFdata(): Reads the NPF dataset in from the open data platform and         #
#                wrangles it into the standard data structure used in the app     #
# addDateCode(): Adds a date label based on the intervalType.                     #
# EEFsparql(): Reads in a dataset from the open data platform using a SPARQL      #
#              query specified in graphOptions$query. intervalType is added based #
#              on the open data platform metadata                                 #
#                                                                                 #
###################################################################################

#helper function for adding the intervalType to a dataset 
#and converting the Year and Date to the correct types (Year=numeric; Date=Date)
#converting Year into a numeric Year and categorical intervalType
addIntervalType <- function(df) {
  if((!"Year"%in%names(df)) & (!"Date"%in%names(df))) stop("dataset must contain Year or Date variable")
  if(!"Year"%in%names(df)) df$Year <- NA
  if(!"Date"%in%names(df)) df$Date <- NA
  
  #validate Year
  validYear <- df$Year %in% c(NA,0,"") | grepl("Q(\\d) \\d{4}", df$Year) | grepl("^\\d{4,5}$",df$Year) | grepl("^\\s*\\d{2}(\\d{2})[-/0-9 ]+(\\d{2})?(\\d{1,2})[A-Za-z ]*$",df$Year) | grepl("^(\\d{2}/\\d{2}/\\d{4})|(\\d{4}-\\d{2}-\\d{2})$",df$Year)
  if(!all(validYear)) warning("Invalid Year format found: ",paste0(unique(df$Year[!validYear]),collapse="; "))
  
  #validate Date
  #validDate <- df$Date %in% c(NA,"") | grepl("^(\\d{2}/\\d{2}/\\d{4})|(\\d{4}-\\d{2}-\\d{2})$",df$Date)
  validDate <- df$Date %in% c(NA,"") | !is.na(as.Date(df$Date,tryFormats=c("%d/%m/%Y","%Y-%m-%d","%d %B %Y")))
  if(!all(validDate)) warning("Invalid Date format found: ",paste0(unique(df$Date[!validDate]),collapse="; "))
  
  singleYearDate <- df$Year %in% c(NA,0,"") | df$Date %in% c(NA,0,"")
  if(!all(singleYearDate)) warning("Year and Date entered for following rows. Only date will be used: ",paste(which(!singleYearDate),collapse="; "))
  
  missingYearDate <- (df$Year %in% c(NA,0,"") | !validYear) & (df$Date %in% c(NA,0,"") | !validDate)
  if(any(missingYearDate)) warning("No valid Year or Date entered for following rows: ",paste(which(missingYearDate),collapse="; "))
  
  #remove invalid Dates/Years
  df <- df %>%
    mutate(Year = replace(Year, ! validYear, NA),
           Date = replace(Date, ! validDate, NA)) %>%
    filter((! Year %in% c(NA, 0, "")) | (! Date %in% c(NA, "")))
  
  df %>% mutate(Date=as.Date(Date,tryFormats=c("%d/%m/%Y","%Y-%m-%d","%d %B %Y")), #Excel and R date formats
                Date=ifelse(grepl("^(\\d{2}/\\d{2}/\\d{4})|(\\d{4}-\\d{2}-\\d{2})$",Year),Year,as.character(Date)),
                Date=ifelse(grepl("^\\d{5}$", Year), Year %>% as.numeric() %>% as.Date(origin = "1899-12-30") %>% as.character(), Date),
                Date=ifelse(grepl("Q1 (\\d{4})",Year),paste(sub("Q1 (\\d{4})", "\\1", Year), "03", "31", sep = "-"),as.character(Date)),
                Date=ifelse(grepl("Q2 (\\d{4})",Year),paste(sub("Q2 (\\d{4})", "\\1", Year), "06", "30", sep = "-"),as.character(Date)),
                Date=ifelse(grepl("Q3 (\\d{4})",Year),paste(sub("Q3 (\\d{4})", "\\1", Year), "09", "30", sep = "-"),as.character(Date)),
                Date=ifelse(grepl("Q4 (\\d{4})",Year),paste(sub("Q4 (\\d{4})", "\\1", Year), "12", "31", sep = "-"),as.character(Date)),
                # Date=case_when(grepl("^(\\d{2}/\\d{2}/\\d{4})|(\\d{4}-\\d{2}-\\d{2})$",Year) ~
                #                  as.character(Year),
                #                grepl("^\\d{5}$", Year) ~
                #                  Year %>% as.numeric() %>% as.Date(origin = "1899-12-30") %>% as.character(),
                #                grepl("Q1 \\d{4}", Year) ~
                #                  paste(sub("Q1 (\\d{4})", "\\1", Year), "03", "31", sep = "-"),
                #                grepl("Q2 \\d{4}", Year) ~
                #                  paste(sub("Q2 (\\d{4})", "\\1", Year), "06", "30", sep = "-"),
                #                grepl("Q3 \\d{4}", Year) ~
                #                  paste(sub("Q3 (\\d{4})", "\\1", Year), "09", "30", sep = "-"),
                #                grepl("Q4 \\d{4}", Year) ~
                #                  paste(sub("Q4 (\\d{4})", "\\1", Year), "12", "31", sep = "-"),
                #                TRUE ~ as.character(Date)),
                Year = gsub("Q\\d\\s*", "", Year),
                Year = gsub("[A-Za-z]", "", Year),
                Year=ifelse(grepl("^\\s*(\\d{2})?(\\d{2})/(\\d{2})?(\\d{2})[- ]+(\\d{2})?(\\d{2})/(\\d{2})?(\\d{1,2})\\s*$",Year),
                            sub("^\\s*(\\d{2})?(\\d{2})/(\\d{2})?(\\d{2})[- ]+(\\d{2})?(\\d{2})/(\\d{2})?(\\d{1,2})\\s*$","\\1\\2/\\7\\8",Year),
                            Year),
                intervalType=ifelse(is.na(Date),"Year","Month"),
                intervalType=ifelse(grepl("^\\s*(\\d{2})?(\\d{2})[-/ ]+(\\d{2})?(\\d{1,2})\\s*$",Year),
                                    paste((as.numeric(sub("^\\s*(\\d{2})?(\\d{2})[-/ ]+(\\d{2})?(\\d{1,2}).*$","\\4",Year))-as.numeric(sub("^\\s*(\\d{2})?(\\d{2})[-/ ]+(\\d{2})?(\\d{1,2}).*$","\\2",Year)))%%100,"year"),
                                    intervalType),
                #intervalType=ifelse(grepl("^\\s*(\\d{2})?(\\d{2})/(\\d{2})?(\\d{2})[- ]+(\\d{2})?(\\d{2})/(\\d{2})?(\\d{1,2})\\s*$",Year),
                #                    paste((as.numeric(sub("^\\s*(\\d{2})?(\\d{2})/(\\d{2})?(\\d{2})[- ]+(\\d{2})?(\\d{2})/(\\d{2})?(\\d{1,2})\\s*$","\\8",Year))-as.numeric(sub("^\\s*(\\d{2})?(\\d{2})/(\\d{2})?(\\d{2})[- ]+(\\d{2})?(\\d{2})/(\\d{2})?(\\d{1,2})\\s*$","\\2",Year)))%%100,"year"),
                #                    intervalType),
                Year=ifelse(Date%in%c(NA,""),
                            #as.numeric(sub("^\\s*(\\d*)(\\D.*)?$","\\1",Year))+(as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/0-9 ]+(\\d{2})?(\\d{1,2}).*$","\\3",Year))-as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/0-9 ]+(\\d{2})?(\\d{1,2}).*$","\\1",Year)))%%100,#get final year if entered as a range.
                            as.numeric(sub("^\\s*(\\d*)(\\D.*)?$","\\1",Year))+ifelse(intervalType%in%"1 year",1,0)+ifelse(intervalType%in%"2 year",2,0)+ifelse(intervalType%in%"3 year",3,0)+ifelse(intervalType%in%"4 year",4,0),
                            NA),
                DateString=ifelse(Date%in%c("",NA),paste(sep="",as.character(Year),"-01-01"),as.character(Date)),
                Date=as.Date(DateString,tryFormats=c("%d/%m/%Y","%Y-%m-%d")),
                Year=ifelse(is.na(Year),year(Date),as.numeric(Year))) %>%
    select(-DateString)
}
#Test scripts for addInvervalType() - when amending the function, make sure that it still passes all these checks
# testthat::expect_equal(data.frame(Year=2000,Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2000,Date=as.Date("2000-01-01"),intervalType="Year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="Q1 2000",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2000,Date=as.Date("2000-03-31"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="Q2 2001",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-06-30"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="Q3 2002",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2002,Date=as.Date("2002-09-30"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="Q4 2003",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2003,Date=as.Date("2003-12-31"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2000,Date=as.Date("2000-01-01"),intervalType="Year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000-01",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="1 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000/01-2001/02",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2002,Date=as.Date("2002-01-01"),intervalType="2 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000/01",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="1 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000-1",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="1 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000-02",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2002,Date=as.Date("2002-01-01"),intervalType="2 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000-03",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2003,Date=as.Date("2003-01-01"),intervalType="3 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000-04",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2004,Date=as.Date("2004-01-01"),intervalType="4 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000-2001",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="1 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000/2001",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="1 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="2000/2001 text",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="1 year",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year=NA,Date="01/01/2001",stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="",Date="01/01/2001",stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year=NA,Date="2001-01-01",stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::test_that("Test invalid year",{
#   testthat::expect_equal(data.frame(Year=c(2001, "200"),Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                          data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Year",stringsAsFactors = F))
#   testthat::expect_warning(data.frame(Year=c(2001, "200"),Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                            "Invalid Year format found")
#   testthat::expect_warning(data.frame(Year=c(2001, "200"),Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                            "No valid Year or Date entered for following rows")
# 
# })
# testthat::test_that("Test no valid date or year",{
#   testthat::expect_equal(data.frame(Year=NA,Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                          data.frame(Year = logical(0), Date = as.Date(logical(0)), intervalType = logical(0)))
#   testthat::expect_warning(data.frame(Year=NA,Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                            "No valid Year or Date entered for following rows")
# })
# testthat::test_that("Test no valid date or year",{
#   testthat::expect_equal(data.frame(Year="",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                          data.frame(Year = logical(0), Date = as.Date(logical(0)), intervalType = logical(0)))
#   testthat::expect_warning(data.frame(Year="",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                            "No valid Year or Date entered for following rows")
# })
# testthat::expect_equal(data.frame(Year="",Date="1/1/2019",stringsAsFactors = F)%>%addIntervalType(),
#                          data.frame(Year=2019,Date=as.Date("2019-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="01/01/2001",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::test_that("Test inconsistent date/year",{
#   testthat::expect_equal(data.frame(Year=2000,Date="01/01/2001",stringsAsFactors = F)%>%addIntervalType(),
#                          data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
#   testthat::expect_warning(data.frame(Year=2000,Date="01/01/2001",stringsAsFactors = F)%>%addIntervalType(),
#                            "Year and Date entered for following rows. Only date will be used: ")
# })


#Access NPF dataset via ODP
#function will return NULL if an error is encountered, otherwise will return the NPF data.frame
readNPFdata <- function() {
  query <- "
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  SELECT  ?LastUpdated ?DownloadURL
  WHERE {
  <http://statistics.gov.scot/data/national-performance-framework>
  <http://purl.org/dc/terms/modified> ?LastUpdated ;
  <http://publishmydata.com/def/dataset#downloadURL> ?DownloadURL.
  }"
  
  npfMetaData <- tryCatch(SPARQL("http://statistics.gov.scot/sparql",query)$results,
                          error=function(x){
                            tryCatch(SPARQL("http://statistics.gov.scot/sparql",query)$results,
                                     error=warning)
                          })
  
  if(!is.data.frame(npfMetaData)) {
    warning("Sparql query failed to access metadata for NPF")
    return(NULL) #exit this function empty if couldn't connect to ODP
  }
  if(nrow(npfMetaData)!=1) {
    warning("Sparql query failed to retrieve metadata for NPF")
    return(NULL) #exit this function empty if couldn't connect to ODP
  }
  
  message("Reading NPF data from statistic.gov.scot")
  
  NPFdata <- tryCatch(gsub("[<>]","",npfMetaData$DownloadURL) %>%
                        openxlsx::read.xlsx(detectDates=TRUE,na.strings=c("","NA")),
                      error=warning)
  if(!is.data.frame(NPFdata)) {
    warning("Sparql query failed to retrieve data for NPF")
    return(NULL) #exit this function empty if couldn't connect to ODP
  }
  
  #Validation for no data found
  if(nrow(NPFdata)==0) {
    warning(paste("No data found on statistics.gov.scot for NPF"))
    return(NULL) #return empty as trying to continue will cause an error
  }
  
  #Validation for missing variables
  missingColumns <- setdiff(c("Outcome","Indicator","Disaggregation","Measure","Breakdown","Source","Year","Figure"),names(NPFdata))
  if(length(missingColumns)>0) {
    warning("The following variables are missing from the sparql query:\n",
            paste(missingColumns,collapse=", "))  
    return(NULL) #return empty as trying to continue will cause an error
  }
  
  NPFdata <- NPFdata %>%
    filter(!is.na(Year)) %>%
    filter(Outcome!="All") %>%
    addIntervalType() %>%
    addDateCode() %>%
    mutate(seriesColour=NA,
           Disaggregation=as.character(Disaggregation),
           LastUpdated=as.Date(as_datetime(npfMetaData$LastUpdated)),
           yLabel = "", 
           Figure=as.numeric(Figure),
           row_id=row_number()
    ) %>%
    select(Outcome,Indicator,Characteristic, Measure=Disaggregation,Breakdown,graphTitle=Measure,yLabel, Source,seriesColour,intervalType,Date,Year,DateCode,Figure,LastUpdated,row_id) %>%
    
    #Validations and corrections
    #If Measure is total then characteristic should be total
    #if Measure is not total then Chararcteristic should not be total - set Characteristic to None otherwise
    #Characteristic must be a valid equality group, Total or None - set to None if otherwise
    #Characteristic should be the same for each "chart" (Outcome, Indicator, Measure) specifies a single chart - if multiple characteristics are found, choose in order of preference: "Total", "Age", "Disability",...., "None"
    #Each chart should only have one Value for each Breakdown and Date (i.e. Outcome, Indicator, Measure, Breakdown, Date combination should have only one row) - if multiple values are found then the first is kept
    #   - Note date is derived from "Year" within the app (see addIntervalType and addDateCode functions)
    mutate(Characteristic = replace(Characteristic, Measure == "Total", "Total"),
           Characteristic = replace(Characteristic, Characteristic == "Total" & Measure != "Total", "None"),
           Characteristic = replace(Characteristic, ! Characteristic %in% c("Total", equalityCharacteristics), "None")) %>%
    group_by(Outcome, Indicator, Measure) %>%
    mutate(char_match = match(Characteristic, c("Total", equalityCharacteristics, "None")) %>% min()) %>% 
    mutate(Characteristic = c("Total", equalityCharacteristics, "None")[char_match]) %>%
    ungroup() %>%
    select(-char_match)
  
  
  #Add Totals
  NPFtotals <- NPFdata %>% filter(Characteristic=="Total") %>% 
    select(-Characteristic,-Measure) %>%
    inner_join(NPFdata %>% distinct(Outcome,Indicator,Characteristic,Measure,Year),
               by=c("Outcome","Indicator","Year")) %>%
    mutate(seriesColour="blue")
  
  NPFdata <- bind_rows(NPFdata%>%filter(Characteristic!="Total"),
                       NPFtotals) %>%
    group_by(Characteristic,Date,Year,Outcome,Indicator,Measure,Breakdown) %>%
    summarise_all(first) %>%  #remove duplicated rows - 
    ungroup%>%
    arrange(Outcome,Indicator,Characteristic,Measure,row_id)
  
  #Ad hoc fixes
  NPFdata <- mutate(NPFdata,
                    Characteristic2=ifelse(grepl("Healthy life expectancy",Indicator),"Gender",NA),#IS THIS STILL NEEDED? - YES Characteristic2 used for intersectional breakdowns
                    Outcome=ifelse(Outcome=="Human Rights","Human rights",Outcome),
                    Outcome=ifelse(Outcome=="Fair Work & Business","Fair work and business",Outcome))
  
  return(NPFdata) 
}

addDateCode <- function(dataset) {
  if(!"intervalType"%in%names(dataset)) stop("intervalType missing from data.frame")
  if(!"Year"%in%names(dataset)) stop("Year missing from data.frame")
  if(!"Date"%in%names(dataset)) stop("Date missing from data.frame")
  mutate(dataset,
         DateCode=as.character(Year),
         DateCode=ifelse(intervalType=="Month",format(as.Date(Date),"%b %Y"),DateCode),
         DateCode=ifelse(intervalType=="Quarterly",paste0(Year,"-Q",1+month(Date)%/%3),DateCode),
         DateCode=ifelse(intervalType=="Academic",paste0(Year-1,"/",substr(Year,3,4)),DateCode),
         DateCode=ifelse(intervalType=="1 year",paste0(Year-1,"-",substr(Year,3,4)),DateCode),
         DateCode=ifelse(intervalType=="2 year",paste0(Year-2,"-",substr(Year,3,4)),DateCode),
         DateCode=ifelse(intervalType=="3 year",paste0(Year-3,"-",substr(Year,3,4)),DateCode),
         DateCode=ifelse(intervalType=="4 year",paste0(Year-4,"-",substr(Year,3,4)),DateCode)
  )
}

#reads data from open data platform. If an error occurs, the original dataset is returned
EEFsparql <- function(graphData,graphOptions=list(),id="") {
  if(is.null(graphOptions$query)) {
    return(graphData)
  } else {
    #this will try to connect to the ODP. If it fails it will try a second time, before giving up
    #If connection doesn't work the second time the graph won't display, but shouldn't crash the app
    sparql <- tryCatch(SPARQL("http://statistics.gov.scot/sparql",graphOptions$query)$results,
                       error=function(x){
                         tryCatch(SPARQL("http://statistics.gov.scot/sparql",graphOptions$query)$results,
                                  error=warning)
                       })
    if(!is.data.frame(sparql)) {
      warning("Sparql query failed to retrieve data for ",id)
      return(NULL) #exit this function empty if couldn't connect to ODP
    }
    
    #sparql <- SPARQL("http://statistics.gov.scot/sparql",graphOptions$query)$results
    
    message("...reading ",id," data from statistic.gov.scot")
    
    #Validation for no data found
    if(nrow(sparql)==0) {
      warning(paste("No data found on statistics.gov.scot for",id))
      return(NULL) #return empty as trying to continue will cause an error
    }
    
    #Validation for missing variables
    missingColumns <- setdiff(c("Indicator","Measure","Breakdown","DateCode","Figure","yLabel","LastUpdated","Interval"),names(sparql))
    if(length(missingColumns)>0) {
      warning("The following variables are missing from the sparql query:\n",
              paste(missingColumns,collapse=", "))  
      return(NULL) #return empty as trying to continue will cause an error
    }
    
    #Validation for expected Breakdowns not found
    missingRows <- graphData %>%
      distinct(Indicator,Measure,Breakdown) %>%
      anti_join(sparql,by=c("Indicator","Measure","Breakdown"))
    if(nrow(missingRows)>0) warning("Sparql query didn't provide the breakdowns specified on the spreadsheet for:\n",
                                    paste(missingRows$Indicator,missingRows$Measure,missingRows$Breakdown,collapse="\n"))
    
    #join sparql data to EEF data to ensure ordered correctly (EEF data only needs the possible Indicator/Measure/Breakdown cominations without any dates or values)
    
    #by default the app will filter the sparql data against the EEF data. Setting graphOptions$filterSparql parameter equal to FALSE will 
    if(is.null(graphOptions$filterSparql)) graphOptions$filterSparql <- TRUE
    
    if(!is.null(graphData)) {
      if(graphOptions$filterSparql==TRUE) {
        graphData <- graphData %>% 
          ungroup %>%
          distinct(Indicator,Measure,Breakdown,graphTitle,seriesColour) %>%
          mutate(row=row_number()) %>%
          inner_join(sparql,by=c("Indicator","Measure","Breakdown")) %>% #only include breakdowns/indicators/measures if on spreadsheet
          arrange(row)
      } else {
        graphData <- graphData %>% 
          ungroup %>%
          distinct(Indicator,Measure,Breakdown,graphTitle,seriesColour) %>%
          mutate(row=row_number()) %>%
          right_join(sparql,by=c("Indicator","Measure","Breakdown")) %>% #only include breakdowns/indicators/measures if on spreadsheet
          arrange(row)
      }
    } else graphData <- sparql %>% mutate(graphTitle=Indicator,seriesColour=NA)
    
    
    graphData <- graphData %>% 
      mutate(intervalType=NA,
             Date=as.Date(Date)-1,
             Year=year(Date),
             #DateString=as.character(as.Date(Date)-1),#sparql query template uses interval end date as the Date (sparql end date is first day AFTER the interval ends so need to subtract 1 day)
             LastUpdated=as.Date(as_datetime(LastUpdated)),
             intervalType=ifelse(grepl("<http://reference.data.gov.uk/id/year/(\\d{4})>",Interval),
                                 "Year",intervalType),
             intervalType=ifelse(grepl("<http://reference.data.gov.uk/id/quarter/\\d{4}-Q\\d>",Interval),
                                 "Quarterly",intervalType),
             intervalType=ifelse(grepl("<http://reference.data.gov.uk/id/gregorian-interval/(\\d{4}-\\d{2}-\\d{2})T\\d{2}:\\d{2}:\\d{2}/P(\\d)Y>",Interval),
                                 sub("<http://reference.data.gov.uk/id/gregorian-interval/(\\d{4}-\\d{2}-\\d{2})T\\d{2}:\\d{2}:\\d{2}/P(\\d)Y>","\\2 year",Interval),intervalType)) %>%
      mutate(DateString=as.character(Date),
             DateString=ifelse(intervalType%in%c("Year"),paste0(year(Date),"-01-01"),DateString),
             Date=as.Date(DateString))
    
    # mutate(DateString=ifelse(intervalType%in%c("Year"),paste0(year(Date),"-01-01"),DateString),#set x-coordinate to Jan 1st
    #       #                   as.Date(sub("<http://reference.data.gov.uk/id/gregorian-interval/(\\d{4}-\\d{2}-\\d{2})T\\d{2}:\\d{2}:\\d{2}/P(\\d)Y>","\\1",Interval))+years(3),DateString),#TO DO - add based on SPARQL interval type URIs
    #       # DateString=ifelse(intervalType%in%"Year",
    #       #                   as.Date(sub("<http://reference.data.gov.uk/id/year/(\\d{4})>","\\1-01-01",Interval)),DateString),#TO DO - add based on SPARQL interval type URIs
    #        Date=as.Date(DateString),
    #        Year=year(Date)) %>%
    
    #Validation for missing interval types - possible causes are incorrect query used or interval type hasn't been coded for in the previous lines
    missingInterval <- filter(graphData,is.na(intervalType))
    if(nrow(missingInterval)>0) {
      Warning("Sparql query has missing or unknown interval types for",id,paste("\n-",missingInterval$interval,collapse=""))
    }
    
    graphData <- filter(graphData,!is.na(intervalType))
    if(!is.null(graphOptions$intervalType)) graphData <- graphData %>% filter(intervalType==graphOptions$intervalType)
    if(!is.null(graphOptions$digits)) graphData <- mutate(graphData,Figure=round(Figure,graphOptions$digits))
    
    #Validation for duplicate rows being found
    numberOfRows <- graphData %>%
      group_by(Indicator,Measure,Breakdown,DateCode) %>%
      summarise(n=n()) %>%
      ungroup %>%
      filter(n>1)
    if(nrow(numberOfRows)>0) {
      warning("Sparql query provides duplicates for the following breakdowns. Check that all necessary dimensions have been locked correctly:\n",
              paste(numberOfRows$Indicator,numberOfRows$Measure,numberOfRows$Breakdown,collapse="\n"))
      return(NULL) #return empty as trying to continue will cause the app to crash
    }
  }
  
  return(graphData)
}

