#helper function for adding the intervalType to a dataset 
#and converting the Year and Date to the correct types (Year=numeric; Date=Date)
#converting Year into a numeric Year and categorical intervalType
#requires dplyr
#note: for spreadsheets only - ODP data uses different variables
addIntervalType <- function(df) {
  if(!"Year"%in%names(df)) stop("data frame must contain Year variable")
  if(!"Date"%in%names(df)) df$Date <- NA
  
  #validate Year
  validYear <- df$Year %in% c(NA,0,"") | grepl("^\\d{4}$",df$Year) | grepl("^\\s*\\d{2}(\\d{2})[-/0-9 ]+(\\d{2})?(\\d{1,2})\\s*$",df$Year) | grepl("^(\\d{2}/\\d{2}/\\d{4})|(\\d{4}-\\d{2}-\\d{2})$",df$Year)
  if(!all(validYear)) stop("Invalid Year format found: ",paste0(df$Year[!validYear],collapse="; "))
  
  #validate Date
  #validDate <- df$Date %in% c(NA,"") | grepl("^(\\d{2}/\\d{2}/\\d{4})|(\\d{4}-\\d{2}-\\d{2})$",df$Date)
  validDate <- df$Date %in% c(NA,"") | !is.na(as.Date(df$Date,tryFormats=c("%d/%m/%Y","%Y-%m-%d","%d %B %Y")))
  if(!all(validDate)) stop("Invalid Date format found: ",paste0(df$Date[!validDate],collapse="; "))
  
  singleYearDate <- df$Year %in% c(NA,0,"") | df$Date %in% c(NA,0,"")
  if(!all(singleYearDate)) warning("Year and Date entered for following rows. Only date will be used: ",paste(which(!singleYearDate),collapse="; "))
  
  df %>% mutate(Date=as.Date(Date,tryFormats=c("%d/%m/%Y","%Y-%m-%d","%d %B %Y")), #Excel and R date formats
                Date=ifelse(grepl("^(\\d{2}/\\d{2}/\\d{4})|(\\d{4}-\\d{2}-\\d{2})$",Year),Year,as.character(Date)),
                Date=ifelse(grepl("^(\\d{2}/\\d{2}/\\d{4})|(\\d{4}-\\d{2}-\\d{2})$",Year),Year,as.character(Date)),
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
# testthat::expect_equal(data.frame(Year=2000,Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2000,Date=as.Date("2000-01-01"),intervalType="Year",stringsAsFactors = F))
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
# testthat::expect_equal(data.frame(Year=NA,Date="01/01/2001",stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year="",Date="01/01/2001",stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_equal(data.frame(Year=NA,Date="2001-01-01",stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::expect_error(data.frame(Year="200",Date=NA,stringsAsFactors = F)%>%addIntervalType())
# testthat::expect_error(data.frame(Year=NA,Date=NA,stringsAsFactors = F)%>%addIntervalType())
# testthat::expect_error(data.frame(Year="",Date=NA,stringsAsFactors = F)%>%addIntervalType())
# testthat::expect_error(data.frame(Year="",Date="1/1/2019",stringsAsFactors = F)%>%addIntervalType())
# testthat::expect_equal(data.frame(Year="01/01/2001",Date=NA,stringsAsFactors = F)%>%addIntervalType(),
#                        data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
# testthat::test_that("Test inconsistent date/year",{
#   testthat::expect_equal(data.frame(Year=2000,Date="01/01/2001",stringsAsFactors = F)%>%addIntervalType(),
#                          data.frame(Year=2001,Date=as.Date("2001-01-01"),intervalType="Month",stringsAsFactors = F))
#   testthat::expect_warning(data.frame(Year=2000,Date="01/01/2001",stringsAsFactors = F)%>%addIntervalType(),
#                            "Year and Date entered for following rows. Only date will be used: ")
# })

# 


#Access NPF dataset via ODP
#function will return NULL if an error is encountered, otherwise will return the NPF data.frame
readNPFdata <- function(NPFchar=NPFcharacteristics) {
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
  missingColumns <- setdiff(c("Outcome","Indicator","Dissaggregation","Measure","Breakdown","Source","Year","Figure"),names(NPFdata))
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
           Disaggregation=as.character(Dissaggregation),
           LastUpdated=as.Date(as_datetime(npfMetaData$LastUpdated)),
           Figure=as.numeric(Figure),
           row_id=row_number()
    ) %>%
    select(Outcome,Indicator,Measure=Disaggregation,Breakdown,graphTitle=Measure,Source,seriesColour,intervalType,Date,Year,DateCode,Figure,LastUpdated,row_id) %>%
    right_join(NPFchar,by=c("Indicator","Measure"))
  
  #Add Totals
  NPFtotals <- NPFdata %>% filter(Characteristic=="Total") %>% 
    select(-Characteristic,-Measure) %>%
    inner_join(NPFdata %>% distinct(Outcome,Indicator,Characteristic,Measure,Year),
               by=c("Outcome","Indicator","Year")) %>%
    mutate(seriesColour="blue")
  
  NPFdata <- bind_rows(NPFdata%>%filter(Characteristic!="Total"),
                       NPFtotals) %>%
    group_by(Characteristic,Date,Year,Outcome,Indicator,Measure,Breakdown) %>%
    summarise_all(first) %>%
    ungroup%>%
    arrange(Outcome,Indicator,Measure,row_id)
  
  #Ad hoc fixes
  NPFdata <- mutate(NPFdata,
                    Characteristic2=ifelse(grepl("Healthy life expectancy",Indicator),"Gender",NA),#IS THIS STILL NEEDED?
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
    
    message("Reading ",id," data from statistic.gov.scot")
    
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

#no longer used???
EEFgraphData <- function(graphData,graphOptions=list(),ns=NS("")) {
  if(is.null(graphData)) return(NULL)
  
  if(is.null(graphOptions$query)) {
    if(!is.null(graphOptions$intervalType)) graphData$intervalType <- graphOptions$intervalType
    if(!is.null(graphOptions$ylabel)) graphData$yLabel <- graphOptions$ylabel else graphData$yLabel <- graphData$Measure
    if(!is.null(graphOptions$panelData$index_updated)) graphData$LastUpdated <- graphOptions$panelData$index_updated else graphData$LastUpdated <- Sys.Date()
    graphData <- mutate(graphData,
                        DateCode="",
                        DateCode=ifelse(intervalType=="Year",as.character(Year),DateCode),
                        DateCode=ifelse(intervalType=="Quarterly",paste0(Year,"-Q",1+month(Date)%/%3),DateCode),
                        DateCode=ifelse(intervalType=="Academic",paste0(substr(Year-1,3,4),"/",Year),DateCode),
                        DateCode=ifelse(intervalType=="1 year",paste0(substr(Year-1,3,4),"-",Year),DateCode),
                        DateCode=ifelse(intervalType=="2 year",paste0(substr(Year-2,3,4),"-",Year),DateCode),
                        DateCode=ifelse(intervalType=="3 year",paste0(substr(Year-3,3,4),"-",Year),DateCode),
                        DateCode=ifelse(intervalType=="4 year",paste0(substr(Year-4,3,4),"-",Year),DateCode)
    )
  } else {
    sparql <- SPARQL("http://statistics.gov.scot/sparql",graphOptions$query)$results
    
    #Validation for no data found
    if(nrow(sparql)==0) {
      warning(paste("No data found on statistics.gov.scot for",ns("")))
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
    
    graphData <- graphData %>% 
      ungroup %>%
      distinct(Indicator,Measure,Breakdown,graphTitle,seriesColour) %>%
      mutate(row=row_number()) %>%
      inner_join(sparql,by=c("Indicator","Measure","Breakdown")) %>%
      arrange(row) %>%
      mutate(intervalType=NA,
             DateString=NA,
             intervalType=ifelse(grepl("<http://reference.data.gov.uk/id/gregorian-interval/(\\d{4}-\\d{2}-\\d{2})T\\d{2}:\\d{2}:\\d{2}/P(\\d)Y>",Interval),
                                 sub("<http://reference.data.gov.uk/id/gregorian-interval/(\\d{4}-\\d{2}-\\d{2})T\\d{2}:\\d{2}:\\d{2}/P(\\d)Y>","\\2 year",Interval),intervalType)) %>%
      mutate(DateString=ifelse(intervalType%in%"3 year",
                               as.Date(sub("<http://reference.data.gov.uk/id/gregorian-interval/(\\d{4}-\\d{2}-\\d{2})T\\d{2}:\\d{2}:\\d{2}/P(\\d)Y>","\\1",Interval))+years(3),DateString),#TO DO - add based on SPARQL interval type URIs
             Date=as.Date(DateString),
             Year=year(Date)) %>%
      filter(!is.na(intervalType))
    if(!is.null(graphOptions$intervalType)) graphData <- graphData %>% filter(intervalType==graphOptions$intervalType)
    
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


#sparql wrapper
# eefSparql <- function(query,intervalType,Characteristic) {
#   if(intervalType%in%c("3 year")) {
#     print("reading data from statistics.gov.scot")
#     filterData <- SPARQL("http://statistics.gov.scot/sparql",query)$results
#     filterData <- filterData %>%
#       rename(Year=Period) %>%
#       mutate(yearOnly=TRUE,seriesColour=NA,Characteristic=Characteristic,Date=as.Date(sub("^.*(\\d\\d)\\D*$","\\1",Year),format="%y"))
#   } else warning("EVIDENCE FINDER ERROR: intervalType not valid")
# }  

#function providing the axisLabelFormatter, and valueFormatter javascript functions required by Dygraphs. If necessary the graphData is also shifted by months to ensure the ticker is at the required month


eefDygraphFormatter <- function(graphData,intervalType="Year",dateRange=NULL,xFormat=NULL) {
  if(is.null(dateRange)) dateRange <- max(graphData$Date,na.rm=TRUE)-min(graphData$Date,na.rm=TRUE) else dateRange <- as.Date(dateRange[2])-as.Date(dateRange[1])
  maxYear <- year(max(graphData$Date,na.rm=TRUE))
  axisMonth <- month(max(graphData$Date,na.rm=TRUE))
  if(is.null(xFormat)) {
    #Goes through each of the interval types and adds the corresponding javascript statement. Dygraphs uses this javascript to display x-axis/values
    if(intervalType=="Year") {#TO DO - Add more intervalTypes
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
    } else {#if not one of the standard interval types treats intervalType as a javascript statement. If it isn't a javascript statement then the graph will break!
      axisLabelFormat <- intervalType
      valueFormat <- intervalType
    }
 
  } else { #version 6 of the app used the xFormat graphOptions parameter to specify the javascript format directly. This is replaced by intervalType from app version 7, but this old code has been retained for backwards compatability for interval types that have not been added yet
    if(is.list(xFormat)) axisLabelFormat <- xFormat$xAxisFormat else axisLabelFormat <- xFormat
    if(is.list(xFormat)) valueFormat <- xFormat$xValueFormat else valueFormat <- xFormat
  }
  if(intervalType=="Month") {
    ticker <- "function (a, b, pixels, opts, dygraph, vals) {return Dygraph.getDateAxis(a, b, Dygraph.MONTHLY, opts, dygraph);}"
    axisLabelFormatter <- paste0("function(d,gran) {if(d.getMonth()==",axisMonth-1,") {var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    if(dateRange>2000) axisLabelFormatter <- paste0("function(d,gran) {if((d.getMonth()==",axisMonth-1,")&&(((d.getFullYear()-",maxYear,") % 2)==0)){var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    if(dateRange>5000) axisLabelFormatter <- paste0("function(d,gran) {if((d.getMonth()==",axisMonth-1,")&&(((d.getFullYear()-",maxYear,") % 5)==0)){var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    valueFormatter <- paste0("function(d) {var date = new Date(d);var month = date.getMonth();var year = date.getFullYear();return ",valueFormat,"}")
  } else {
    ticker <- "function (a, b, pixels, opts, dygraph, vals) {return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph);}"
    axisLabelFormatter <- paste0("function(d,gran) {var year = d.getFullYear();return ",axisLabelFormat,"}")
    if(dateRange>2000) axisLabelFormatter <- paste0("function(d,gran) {if(((d.getFullYear()-",maxYear,") % 2)==0){var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    if(dateRange>5000) axisLabelFormatter <- paste0("function(d,gran) {if(((d.getFullYear()-",maxYear,") % 5)==0){var month = d.getMonth();var year = d.getFullYear();return ",axisLabelFormat,"} else {return '';}}")
    valueFormatter <- paste0("function(d) {var date = new Date(d);var month = date.getMonth();var year = date.getFullYear();return ",valueFormat,"}")
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


padxts2 =
  ## Adds padding of NA before and after the data of the given xts
  ## This creates some white-space margins on either side of the data
  ##  making is easier to see the edge values.
  ## The amount of padding is determined by the ideal padding (ipad)
  ##  padding is {nrow(xts) * ipad} on either side of the data
  ## Default is 4%, which is what R does to its graphs by default.
  function(curxts, ipad = 0.04){
    xdates = index(curxts)
    padDist = diff(range(xdates)) * ipad
    if(padDist==0) padDist = days(1)
    padMat = matrix(as.numeric(NA), nrow = 1, ncol = ncol(curxts))
    colnames(padMat) = names(curxts)
    padTop = xts(padMat, order.by = first(xdates) - padDist)
    padBot = xts(padMat, order.by = last(xdates) + padDist)
    rbind(padTop, curxts, padBot)
  }

eefTooltip <- function(policyArea,char) {
  contents <- filter(EEFindex,policy_area==gsub("&","and",policyArea),characteristic==char)$topic %>%
    unique %>%
    c("Publications & Data")
  paste0(policyArea,", ",char,paste0("\n - ",contents,collapse=""))
}

jsFormatCommas <- function(d) {
  if(is.null(d)) d <- -1
  paste0("
  function addCommas(nStr){
    var d = ",d,";
    nStr += '';
    var x = nStr.split('.');
    if(d>0) {
      if(x.length==1) {x[1] = '0';}
      while (x[1].length<d) {x[1]+='0';}
    }
    x[1] = x.length > 1 ? '.' + x[1] : '';
    if(d==0) x[1] ='';
    var rgx = /(\\d+)(\\d{3})/;
    while (rgx.test(x[0])) {
      x[0] = x[0].replace(rgx, '$1' + ',' + '$2');
    }
    return x[0] + x[1];
  }
")
}

uiOutputLoading <- function (outputId, inline = FALSE, container = if (inline) span else div,  ...) {
  options(warn=-1) #temporarily disable R warnings as Shiny will warn that the spinner gif will be replaced by content (when it is loaded)
  #html <- uiOutput(outputId,inline,container,style="position:relative;min-height:66px",img(src="icons/spinner.gif",style="position:absolute;margin-left:-33px;margin-top:-33px;left:50%;top:50%"))
  html <- uiOutput(outputId,inline,container,div(class="panel-load-container load1",div(class="loader","Loading")))
  
  options(warn=0)
  return(html)
}


#17-colour pallete (including 6 shades of "eef" blue and 4 shades of "eef" orange, with other colours chosen to be colour-blind friendly based on 15-colour pallete from mkweb.bcgsc.ca/biovis2012 - with blue and orange modified slightly to fit eef scheme)
eefColours <- function(colour,colourPalette="restricted") {
  defaultColours <- c("blue","dark-red","purple","dark-green","orange","light-blue","grey","lilac","lime","brown",
                      "very-dark-blue","dark-orange","mid-red","black","mid-purple","red","mid-green","very-dark-orange",
                      "dark-yellow","dark-blue","yellow","green","dark-grey","mid-blue","light-orange","light-lilac",
                      "light-green","light-grey","very-light-blue","dark-brown","olive","dark-purple","very-dark-grey")
                        
                      
  #if(length(colour)<3)
  if(colourPalette=="neutral") defaultColours <- c("black","tundora","gray","silver","white")
  if(colourPalette=="diverging" & length(colour)<12) defaultColours <- brewer.pal(length(colour),"RdYlBu")
  if(colourPalette=="blue" & length(colour)<12) defaultColours <- c("very-dark-blue","mid-dark-blue","dark-blue","light-dark-blue","blue","dark-mid-blue","mid-blue","light-mid-blue","light-blue","mid-light-blue","very-light-blue")
  if(colourPalette=="blue" & length(colour)<7) defaultColours <- c("very-dark-blue","dark-blue","blue","mid-blue","light-blue","very-light-blue")
  if(colourPalette=="blue" & length(colour)==4)  defaultColours <- c("very-dark-blue","dark-blue","blue","light-blue")
  if(colourPalette=="blue" & length(colour)<4)  defaultColours <- c("very-dark-blue","blue","light-blue")
  if(colourPalette=="orange" & length(colour)<5) defaultColours <- c("very-dark-orange","dark-orange","orange","light-orange")
  if(colourPalette=="restricted" & length(colour)<11) defaultColours <- c("blue","dark-red","purple","dark-green","orange","light-blue","grey","lilac","lime","brown")
  if(colourPalette=="not-blue" & length(colour)<9) defaultColours <- c("dark-red","purple","dark-green","orange","grey","lilac","lime","brown")
  if(colourPalette=="not-blue" & length(colour)>=9) defaultColours <- c("dark-red","purple","dark-green","orange","grey","lilac","lime","brown",
                                                                        "dark-orange","mid-red","black","mid-purple","red","mid-green","very-dark-orange",
                                                                        "dark-yellow","yellow","green","dark-grey","light-orange","light-lilac",
                                                                        "light-green","light-grey","very-light-blue","dark-brown","olive","dark-purple","very-dark-grey")
  unusedColours <- setdiff(defaultColours,colour)
  
  palette <- tolower(colour) %>%
    ifelse(.%in%c(defaultColours,"dark-gray","gray","light-gray","very-dark-gray","very-light-blue"),.,unusedColours) %>% #"dark-gray","gray","light-gray","very-dark-gray","very-light-blue" will always be available as valid colours, but won't be automatically selected in some of the palettes.
    ifelse(.=="black","#000000",.) %>%
    ifelse(.=="tundora","#4B4B4B",.) %>%
    ifelse(.=="gray","#828282",.) %>%
    ifelse(.=="grey","#828282",.) %>%
    ifelse(.=="silver","#b9b9b9",.) %>%
    ifelse(.=="white","#FFFFFF",.) %>%
    ifelse(.=="blue","#0080db",.) %>%
    ifelse(.=="dark-red","#920000",.) %>%
    ifelse(.=="purple","#490092",.) %>%
    ifelse(.=="dark-green","#004949",.) %>%
    ifelse(.=="orange","#ff8021",.) %>%
    ifelse(.=="light-blue","#6bc1ff",.) %>%
    ifelse(.=="lilac","#b66dff",.) %>%
    ifelse(.=="lime","#24ff24",.) %>%
    ifelse(.=="brown","#924900",.) %>%
    ifelse(.=="very-dark-blue","#002d4d",.) %>%
    ifelse(.=="dark-orange","#db5f00",.) %>%
    ifelse(.=="mid-red","#ff0000",.) %>%
    ifelse(.=="black","#000000",.) %>%
    ifelse(.=="mid-purple","#8000ff",.) %>%
    ifelse(.=="red","#cc0000",.) %>%
    ifelse(.=="mid-green","#00adad",.) %>%
    ifelse(.=="very-dark-orange","#a84900",.) %>%
    ifelse(.=="dark-yellow","#cccc00",.) %>%
    ifelse(.=="dark-blue","#005999",.) %>%
    ifelse(.=="mid-dark-blue","#004475",.) %>%
    ifelse(.=="dark-mid-blue","#0095ff",.) %>%
    ifelse(.=="mid-light-blue","#8fd0ff",.) %>%
    ifelse(.=="light-dark-blue","#006bb8",.) %>%
    ifelse(.=="yellow","#ffff33",.) %>%
    ifelse(.=="green","#009900",.) %>%
    ifelse(.=="dark-grey","#666666",.) %>%
    ifelse(.=="dark-gray","#666666",.) %>%
    ifelse(.=="mid-blue","#24a4ff",.) %>%
    ifelse(.=="light-mid-blue","#47b3ff",.) %>%
    ifelse(.=="light-orange","#ffa35c",.) %>%
    ifelse(.=="light-lilac","#d4a8ff",.) %>%
    ifelse(.=="light-green","#80ff80",.) %>%
    ifelse(.=="light-grey","#aaaaaa",.) %>%
    ifelse(.=="light-gray","#aaaaaa",.) %>%
    ifelse(.=="very-light-blue","#b8e1ff",.) %>%
    ifelse(.=="dark-brown","#4d2600",.) %>%
    ifelse(.=="olive","#666600",.) %>%
    ifelse(.=="dark-purple","#4d004d",.) %>%
    ifelse(.=="very-dark-grey","#3f3f3f",.) %>%
    ifelse(.=="very-dark-gray","#3f3f3f",.)
  names(palette) <- names(colour)
  return(palette)
}

policyCSS <- function(policy_area) {
  policy_area <- ifelse(policy_area=="Summary","summ",policy_area)
  policy_area <- ifelse(policy_area=="Business, Enterprise and Tourism","busEnt",policy_area)
  policy_area <- ifelse(policy_area=="Children and Families","chiFam",policy_area) 
  policy_area <- ifelse(policy_area=="Crime and Justice","criJus",policy_area) 
  policy_area <- ifelse(policy_area=="Culture, Communities and Society","culCom",policy_area) 
  policy_area <- ifelse(policy_area=="Demographics","dem",policy_area) 
  policy_area <- ifelse(policy_area=="Employability, Skills and Lifelong Learning","empSLL",policy_area) 
  policy_area <- ifelse(policy_area=="Advanced Learning and Skills","empSLL",policy_area) 
  policy_area <- ifelse(policy_area=="Health, Social Care and Sport","health",policy_area) 
  policy_area <- ifelse(policy_area=="Housing and Regeneration","houReg",policy_area) 
  policy_area <- ifelse(policy_area=="Income and Poverty","incPov",policy_area) 
  policy_area <- ifelse(policy_area=="Labour Market and Social Security","labSoc",policy_area) 
  policy_area <- ifelse(policy_area=="Local Government and Third Sector","locThi",policy_area) 
  policy_area <- ifelse(policy_area=="Rural and Environment","rurEnv",policy_area) 
  policy_area <- ifelse(policy_area=="School Education","schEdu",policy_area) 
  policy_area <- ifelse(policy_area=="Transport and Travel","transp",policy_area)
  policy_area <- ifelse(policy_area=="National Performance Framework","npf",policy_area)
  policy_area <- paste0("eef-",policy_area)
  return(policy_area)
}

equalityCSS <- function(char) {
  char <- ifelse(char=="Socio-Economic Status","socioEconomicStatus",
                 ifelse(char=="Sexual Orientation","sexualOrientation",tolower(char)))
  char <- paste0("eef-",char)
  return(char)
}

equalityLabel <- function(id) switch(id,
                                     overview="Overview",
                                     age="Age",
                                     disability="Disability",
                                     ethnicity="Ethnicity",
                                     gender="Gender",
                                     religion="Religion",
                                     sexualOrientation="Sexual Orientation",
                                     socioEconomicStatus="Socio-Economic Status",
                                     transgender="Transgender",
                                     "Overview")

policyLabel <- function(id) switch(id,
                                   summ="Summary",
                                   busEnt="Business, Enterprise and Tourism",
                                   chiFam="Children and Families",
                                   criJus="Crime and Justice",
                                   culCom="Culture, Communities and Society",
                                   cult="Culture",#not currently used
                                   dem="Demographics",
                                   #empSLL="Employability, Skills and Lifelong Learning",
                                   empSLL="Advanced Learning and Skills",
                                   health="Health, Social Care and Sport",
                                   houReg="Housing and Regeneration",
                                   incPov="Income and Poverty",
                                   labMar="Labour Market",#not currently used
                                   labSoc="Labour Market and Social Security",
                                   locGov="Local Government",#not currently used
                                   locThi="Local Government and Third Sector",
                                   rurEnv="Rural and Environment",
                                   schEdu="School Education",
                                   socSer="Social Security",#not currently used
                                   thiSec="Third Sector",#not currently used
                                   transp="Transport and Travel",
                                   "")
                                   
#modified sliderInput with discrete categories - no longer needed
# sliderInputLabels <- function(inputId,label,tickLabels,value,min=NULL,max=NULL,step=NULL,...) {
#   args <- list(inputId=inputId,label=label,value=match(value,tickLabels)-1,min=1,max=length(tickLabels),step=1,...)
#   html <- do.call('sliderInput', args)
#   html$children[[2]]$attribs[['data-values']] <- paste0(tickLabels, collapse=',')
#   return(html)
# }

equalityCharacteristics <- c("Age","Disability","Ethnicity","Gender","Religion","Sexual Orientation","Transgender","Socio-Economic Status")
equalityCharacteristicsID <- c("age","disability","ethnicity","gender","religion","sexualOrientation","transgender","socioEconomicStatus")
allPolicyAreasID <- c("summ","busEnt","chiFam","criJus","cult","dem","empSLL","health","houReg","incPov","labMar","locGov","rurEnv","schEdu","thiSec","transp")
allPolicyAreasID <- c("summ","busEnt","chiFam","criJus","culCom","dem","empSLL","health","houReg","incPov","labSoc","locThi","rurEnv","schEdu","transp")
allPagesID <- c("home2","Approach","equality")

#shinyjs bugfix - shinyjs version 1.0 and earlier contains a bug when using modules so overwrite with the shinyjs 1.01 version - temp fix 
if(packageVersion("shinyjs") <= "1.0"){
  show <- function (id = NULL, anim = FALSE, animType = "slide", time = 0.5, 
            selector = NULL) 
  {
    fxn <- "show"
    params <- list(id = id, anim = anim, animType = animType, 
                   time = time, selector = selector)
    jsFuncHelper(fxn, params)
  }
  
  hide <- function (id = NULL, anim = FALSE, animType = "slide", time = 0.5, 
                    selector = NULL) 
  {
    fxn <- "hide"
    params <- list(id = id, anim = anim, animType = animType, 
                   time = time, selector = selector)
    jsFuncHelper(fxn, params)
  }
  
  removeClass <- function(id = NULL, class = NULL, selector = NULL) { 
    fxn <- "removeClass" 
    params <- list(id = id, class = class, selector = selector) 
    jsFuncHelper(fxn, params) 
  } 
  
  jsFuncHelper <- function(fxn, params) { 
    # get the Shiny session 
    session <- getSession() 
    
    
    fxn <- paste0("shinyjs-", fxn) 
    
    
    # respect Shiny modules/namespaces 
    if (inherits(session , "session_proxy")) { 
      if ("id" %in% names(params) && !is.null(params[['id']])) { 
        params[['id']] <- session$ns(params[['id']]) 
      } 
    } 
    
    
    # call the javascript function 
    session$sendCustomMessage( 
      type = fxn, 
      message = params) 
    
    
    invisible(NULL) 
  } 
  errMsg <- function(x) { 
    stop(sprintf("shinyjs: %s", x), call. = FALSE) 
  } 
  
  
  # get the shiny session object 
  getSession <- function() { 
    session <- shiny::getDefaultReactiveDomain() 
    
    
    if (is.null(session)) { 
      errMsg(paste( 
        "could not find the Shiny session object. This usually happens when a", 
        "shinyjs function is called from a context that wasn't set up by a Shiny session." 
      )) 
    } 
    
    
    session 
  } 
}

eefContact <- function(p) {
  options(warn=-1) #temporarily disable R warnings as R will moan that we are not enforcing an obselete convention from decades ago regarding the "proper" way of ending a text file...
  mdFile <- includeMarkdown(file.path("EEF",policyLabel(p),"Contact.md"))
  options(warn=0)
  column(6,id=NS(policyCSS(p),"contact"),
                              class=paste0("eef-text eef eef-",p),
                              mdFile)
}

 
eefContactList <- function(policy) {
  options(warn=-1)
  mdFile <- includeMarkdown(file.path("EEF","Summary","Contact.md"))
  options(warn=0)
  fluidRow(class="w3-content",style="width:100%",
    column(6,id=NS("eef-main","contact"),class="eef-text",mdFile),
    shinyjs::hidden(lapply(policy,eefContact))
  )
}

#filter EEFadditional dataset for "Data Collection" publication type and particular theme/characteristic selected
updateGuidance <- function(theme,characteristic) {
  if(theme=="") theme <- "Summary"
  html <- character(0)
  match_guidance <- EEFadditional[which(EEFadditional$publication_type=="Data Collection Guidance" & 
                                          EEFadditional[,characteristic]>0),]
  
  match_guidance <- arrange_(match_guidance,paste0("`",characteristic,"`"),"name")
  
  if(nrow(match_guidance)>0) {
    html_header <- "<h3>Collecting Equality Information Series</h3>\n\n"
    html_text <- htmlPublications(match_guidance)
    html <- c(html_header,html,html_text)
  }
  
  html <- paste(html,collapse="")
  return(html)
}

#filter EEFadditional dataset for "Glossary" publication type and particular theme/characteristic selected
#html is generated using R Markdown
updateGlossary <- function(theme,characteristic) {
  if(theme=="") theme <- "Summary"
  match_glossary <- EEFadditional[which(EEFadditional$publication_type=="Glossary" & 
                                          EEFadditional$name!="" & 
                                          EEFadditional[,characteristic]>0),]
  
  match_glossary <- arrange_(match_glossary,paste0("`",characteristic,"`"),"name")
  
  if(nrow(match_glossary)>0) {
    readLines("EEF scripts/glossary.rmd",warn=FALSE) %>%
      knit(text=., quiet = TRUE,encoding="UTF-8") %>%
      markdown::markdownToHTML(text=.,fragment.only=TRUE,encoding="UTF-8") %>%
      HTML
  } else NULL
  # 
  #   includeMarkdown("EEF Scripts/Glossary.rmd")
  #   html_header <- "<h3>Collecting Equality Information Series</h3>\n\n"
  #   html_text <- htmlPublications(match_guidance)
  #   html <- c(html_header,html,html_text)
  # }
  
  #  html <- paste(html,collapse="")
  #  return(html)
}


#filter EEFpublished dataset for particular theme/characteristic selected
#raw html is used here - using R markdown would have been simpler
updatePublications <- function(theme,characteristic) {
  if(theme=="") theme <- "Summary"
  html <- character(0)
  #if(theme=="Summary") html <- "<p class='info'>This contains a selected list of available publications. More links can be found on the relevant policy area pages, by clicking on the grid above.</p>"
  if(theme=="Summary") html <- "<div class='row'>
                                   <div class='col-sm-1'><img src='icons/infoBlue.svg' width='30' height='30' style='margin:30px 20px'/></div>
                                   <div class='col-sm-11'><p>This contains a selected list of available publications and links. More links can be found on the relevant policy area pages, by clicking on the grid above.</p></div>
                                </div>"
  match_internal <- EEFpublished[which(EEFpublished$policy_area==theme &
                                         EEFpublished$internal_external=="Internal" & 
                                         EEFpublished[,characteristic]>0),]
  
  match_internal <- arrange_(match_internal,paste0("`",characteristic,"`"),"desc(sort_date)","name")
  
  if(nrow(match_internal)>0) {
    html_header <- "<h3>Publications and Outputs</h3>\n\n"
    html_text <- htmlPublications(match_internal)
    html <- c(html,html_text)
  }
  
  html <- paste(html,collapse="")
  return(html)
}

#filter EEFdataLinks dataset for particular theme/characteristic selected
#raw html is used here - using R markdown would have been simpler
updateData <- function(theme,characteristic) {
  if(theme=="") theme <- "Summary"
  html <- character(0)
  match_data <- EEFdataLinks[which(EEFdataLinks$policy_area==theme &
                                     EEFdataLinks[,characteristic]>0),]
  
  match_data <- arrange_(match_data,characteristic,"name")
  match_data1 <- filter(match_data,grepl("Scottish Government Survey Data",publication_type,ignore.case = TRUE))
  match_data2 <- filter(match_data,grepl("Scotland'?s? Census",publication_type,ignore.case = TRUE))
  match_data3 <- filter(match_data,grepl("Official Statistics",publication_type,ignore.case = TRUE))
  match_data4 <- filter(match_data,grepl("External Links",publication_type,ignore.case = TRUE))
  match_data0 <- filter(match_data,!grepl("(Scottish Government Survey Data)|(Scotland'?s? Census)|(Official Statistics)|(External Links)",publication_type,ignore.case = TRUE))
  
  #  if(nrow(match_data)>0) {
  #   html <- c(html,"<h3>Data</h3>")
  # }
  if(nrow(match_data0)>0) {
    html_header <- "\n\n"
    html_text <- htmlData(match_data0)
    html <- c(html,html_header,html_text)
  }
  if(nrow(match_data1)>0) {
    html_header <- "\n\n<h3>Scottish Government Survey Data</h3>\n\n"
    html_text <- htmlData(match_data1)
    html <- c(html,html_header,html_text)
  }
  if(nrow(match_data2)>0) {
    html_header <- "\n\n<h3>Scotland's Census</h3>\n\n"
    html_text <- htmlData(match_data2)
    html <- c(html,html_header,html_text)
  }
  if(nrow(match_data3)>0) {
    html_header <- "\n\n<h3>Official Statistics</h3>\n\n"
    html_text <- htmlData(match_data3)
    html <- c(html,html_header,html_text)
  }
  if(nrow(match_data4)>0) {
    html_header <- "\n\n<h3>External Links</h3>\n\n
                    <div class='row'>
                      <div class='col-sm-1'><img src='icons/infoBlue.svg' width='30' height='30' style='margin:30px 20px'/></div>
                      <div class='col-sm-11'><p>Please note that you will leave the Scottish Government web site by clicking on any of the following links, and that the Scottish Government and its staff are not responsible for content external to this web site. The research below has been carried out independently of the Scottish Government and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p></div>
                    </div>"
    #html_header <- "\n\n<h3>External Links</h3>\n\n<p class=\"info\">Please note that you will leave the Scottish Government web site by clicking on any of the following links, and that the Scottish Government and its staff are not responsible for content external to this web site. The research below has been carried out independently of the Scottish Government and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p>\n\n"
    html_text <- htmlData(match_data4)
    html <- c(html,html_header,html_text)
  }
  html <- paste(html,collapse="")
  return(html)
}

#filter EEFpublished dataset for external publications and particular theme/characteristic selected
#filter EEFexternal dataset for particular theme/characteristic selected
#raw html is used here - using R markdown would have been simpler
updateExternal <- function(theme,characteristic) {
  if(theme=="") theme <- "Summary"
  html <- character(0)
  #if(theme=="Summary") html <- "<p class='info'>This contains a selected list of available publications and links. More links can be found on the relevant policy area pages, by clicking on the grid above.</p>"
  if(theme=="Summary") html <- "<div class='row'>
                                   <div class='col-sm-1'><img src='icons/infoBlue.svg' width='30' height='30' style='margin:30px 20px'/></div>
                                   <div class='col-sm-11'><p>This contains a selected list of available publications and links. More links can be found on the relevant policy area pages, by clicking on the grid above.</p></div>
                                </div>"
  
  match_external_pub <- EEFpublished[which(EEFpublished$policy_area==theme &
                                         EEFpublished$internal_external=="External" & 
                                         EEFpublished[,characteristic]>0),]
  
  match_external_pub <- arrange_(match_external_pub,paste0("`",characteristic,"`"),"desc(sort_date)","name")
  
  match_external_org <- EEFexternal[which(EEFexternal$policy_area==theme &
                                            EEFexternal[,characteristic]>0),]
  
 match_external_org <- arrange_(match_external_org,paste0("`",characteristic,"`"),"name")
  
  if(nrow(match_external_pub)>0|nrow(match_external_org)>0) {
    html <- c(html,
              "<div class='row'><div class='col-sm-1'><img src='icons/infoBlue.svg' width='30' height='30' style='margin:30px 20px'/></div><div class='col-sm-11'><p>Please note that you will leave the Scottish Government web site by clicking on any of the following links, and that the Scottish Government and its staff are not responsible for content external to this web site. The research below has been carried out independently of the Scottish Government and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p></div></div>")
              
              
    #html <- c(html,"<p class=\"info\">Please note that you will leave the Scottish Government web site by clicking on any of the following links, and that the Scottish Government and its staff are not responsible for content external to this web site. The research below has been carried out independently of the Scottish Government and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p>")
  }
  if(nrow(match_external_pub)>0) {
    html_header <- "\n\n<h3>External Publications and Outputs</h3>\n\n"
    html_text <- htmlPublications(match_external_pub)
    html <- c(html,html_header,html_text)
  }
  if(nrow(match_external_org)>0) {
    match_external_org <- mutate(match_external_org,published_date=NA,published_year=NA,published_by=NA)
    html_header <- "\n\n<h3>External Research Organisations</h3>\n\n"
    html_text <- htmlExternal(match_external_org)
    html <- c(html,html_header,html_text)
  }
  
  html <- paste(html,collapse="")
  return(html)
}

#generate html code for publications lists...
#more raw html - R markdown would be simpler
htmlPublications <- function(eef_data) {
  eef_data <- mutate(eef_data,description=gsub("&(amp;)?","&amp;",description))
  eef_data <- mutate(eef_data,link=gsub("&(amp;)?","&amp;",link))
  eef_data <- mutate(eef_data,published_by=gsub("&","&amp;",published_by))
  eef_data <- mutate(eef_data,html_link=ifelse(link%in%c("",NA),paste("<span style='text-decoration:underline'>",name,"</span>"),paste("<a target=\"_blank\" href=\"",link,"\">",name,"</a>",sep="")))
  eef_data <- mutate(eef_data,text_date=ifelse(published_date%in%c("",0,NA),published_year,format(as.Date(published_date,origin="1900-1-1"),"%B %Y")))
  eef_data <- mutate(eef_data,html_details=ifelse(published_by%in%c("",NA)&text_date%in%c("",NA)," - ",paste(" (",ifelse(published_by%in%c("",NA),"",published_by),ifelse(published_by%in%c("",NA)|text_date%in%c("",NA),"",", "),ifelse(text_date%in%c("",NA),"",text_date),") ",sep="")))
  html_additional <- ""
  
  for(i in 1:20) {#loop through the additional links up to 20 times and add <li> list tags for additional links
    name <- paste0("name",i)
    link <- paste0("link",i)
    if(name %in% names(eef_data)) html_additional <- ifelse(eef_data[[name]]%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",eef_data[[link]],"\">",eef_data[[name]],"</a></li>",sep="")) %>%paste0(html_additional,.)
  }
  #wrap in bullet point type list <ul> tag
  html_additional <- ifelse(grepl("\\S+",html_additional),paste0("\n<ul>",html_additional,"\n</ul>"),html_additional)
  
  # if("name1" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional1=ifelse(name1%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link1,"\">",name1,"</a></li>",sep="")))
  # if("name2" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional2=ifelse(name2%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link2,"\">",name2,"</a></li>",sep="")))
  # if("name3" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional3=ifelse(name3%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link3,"\">",name3,"</a></li>",sep="")))
  # if("name4" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional4=ifelse(name4%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link4,"\">",name4,"</a></li>",sep="")))
  # if("name5" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional5=ifelse(name5%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link5,"\">",name5,"</a></li>",sep="")))
  # if("name6" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional6=ifelse(name6%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link6,"\">",name6,"</a></li>",sep="")))
  # if("name7" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional7=ifelse(name7%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link7,"\">",name7,"</a></li>",sep="")))
  # if("name8" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional8=ifelse(name8%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link8,"\">",name8,"</a></li>",sep="")))
  #if("name1" %in% names(eef_data)) eef_data <- mutate(eef_data,html_additional=ifelse(html_additional1%in%c("",NA),"",paste("\n<ul>",html_additional1,html_additional2,html_additional3,html_additional4,html_additional5,html_additional6,html_additional7,html_additional8,"\n</ul>\n<div>&nbsp;</div>",sep=""))) else eef_data <- mutate(eef_data,html_additional="")
  eef_data <- mutate(eef_data,html=paste("<p>",html_link,html_details,ifelse(description%in%c("",NA),"",description),"</p>",html_additional,sep=""))    
  html_text <- paste(eef_data$html,sep="",collapse="\n\n")
  return(html_text)
}

#generate html code for data links...
#even more raw html - R markdown would be simpler
htmlData <- function(eef_data) {
  eef_data <- mutate(eef_data,description=gsub("&(amp;)?","&amp;",description))
  eef_data <- mutate(eef_data,link=gsub("&(amp;)?","&amp;",link))
  eef_data <- mutate(eef_data,published_by=gsub("&","&amp;",published_by))
  eef_data <- mutate(eef_data,html_link=ifelse(link%in%c("",NA),paste("<span style='text-decoration:underline'>",name,"</span>"),paste("<a target=\"_blank\" href=\"",link,"\">",name,"</a>",sep="")))
  eef_data <- mutate(eef_data,html_details=ifelse(published_by%in%c("",NA)," - ",paste(" (",ifelse(published_by%in%c("",NA),"",published_by),") ",sep="")))
  eef_data <- mutate(eef_data,html_additional1=ifelse(name1%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link1,"\">",name1,"</a></li>",sep="")))
  eef_data <- mutate(eef_data,html_additional2=ifelse(name2%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link2,"\">",name2,"</a></li>",sep="")))
  eef_data <- mutate(eef_data,html_additional3=ifelse(name3%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link3,"\">",name3,"</a></li>",sep="")))
  eef_data <- mutate(eef_data,html_additional4=ifelse(name4%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link4,"\">",name4,"</a></li>",sep="")))
  eef_data <- mutate(eef_data,html_additional5=ifelse(name5%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link5,"\">",name5,"</a></li>",sep="")))
  eef_data <- mutate(eef_data,html_additional6=ifelse(name6%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link6,"\">",name6,"</a></li>",sep="")))
  eef_data <- mutate(eef_data,html_additional7=ifelse(name7%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link7,"\">",name7,"</a></li>",sep="")))
  eef_data <- mutate(eef_data,html_additional8=ifelse(name8%in%c("",NA),"",paste("\n\t<li><a target=\"_blank\" href=\"",link8,"\">",name8,"</a></li>",sep="")))
  eef_data <- mutate(eef_data,html_additional=ifelse(html_additional1%in%c("",NA),"",paste("\n<ul>",html_additional1,html_additional2,html_additional3,html_additional4,html_additional5,html_additional6,html_additional7,html_additional8,"\n</ul>\n<div>&nbsp;</div>",sep="")))
  eef_data <- mutate(eef_data,html=paste("<p>",html_link,html_details,ifelse(description%in%c("",NA),"",description),"</p>",html_additional,sep=""))    
  html_text <- paste(eef_data$html,sep="",collapse="\n\n")
  return(html_text)
}


#generate html code for external links...
#more raw html - R markdown would be simpler
htmlExternal <- function(eef_data) {
  eef_data <- mutate(eef_data,description=gsub("&(amp;)?","&amp;",description))
  eef_data <- mutate(eef_data,link=gsub("&(amp;)?","&amp;",link))
  eef_data <- mutate(eef_data,html_link=ifelse(link%in%c("",NA),paste("<span style='text-decoration:underline'>",name,"</span>"),paste("<a target=\"_blank\" href=\"",link,"\">",name,"</a>",sep="")))
  eef_data <- mutate(eef_data,text_date=ifelse(published_date%in%c("",0,NA),published_year,format(as.Date(published_date,origin="1900-1-1"),"%B %Y")))
  eef_data <- mutate(eef_data,html_details=ifelse(published_by%in%c("",NA)&text_date%in%c("",NA)," - ",paste(" (",ifelse(published_by%in%c("",NA),"",published_by),ifelse(published_by%in%c("",NA)|text_date%in%c("",NA),"",", "),ifelse(text_date%in%c("",NA),"",text_date),") ",sep="")))
  eef_data <- mutate(eef_data,html=paste("<p>",html_link,html_details,ifelse(description%in%c("",NA),"",description),"</p>",sep=""))    
  html_text <- paste(eef_data$html,sep="",collapse="\n\n")
  return(html_text)
}


#utility function for looking up data source. First searches for a match in the series_name column. Looks for a match in the name column if it didn't find a series name match
getSource <- function(x,EEFlatestSource=EEFlatestSource,EEFsources=EEFsources) {
  match_source <- filter(EEFlatestSource,series_name %in% x)
  if(nrow(match_source)==0) {
    match_source <- filter(EEFsources,name %in% x)
  }
  return(match_source)
}


#generates html for panel data source and data source notes - looks up latest publication in the series. There are various cases as Internal/External; Routine/Ad Hoc; and multiple sources are displayed (slightly) differently
#this works (just) but is overly complicated and I wouldn't recommend reusing for anything else. TO DO: rewrite (and tidy/simplify) 
# note: multiple sources is no longer used in EEF and this code stopped being maintained in version beta1 (TO DO: remove obsolete code)
pubSource <- function(x,extra_details="",notes="",max_date=Sys.Date()) {
  if(x %in% c("",NA)) return(NULL)
  if(is.na(max_date)) max_date <- Sys.Date()
  html <- character(0)
  x <- strsplit(x,"(\n|\r)+")[[1]] %>% .[!.%in%c(NA,"")]
  notes <- paste0("Additional analysis and methodology details can be found in the source publication\n",notes[!notes%in%c("",NA)])
  
  match_source <- filter(EEFsources,series_name %in% x | name %in% x,sort_date<=max_date) %>%
    group_by(series_name) %>%
    arrange(desc(published_date),desc(source_updated),desc(name)) %>%
    filter(row_number()==1) %>%
    ungroup
  
  unmatched_x <- x[(!x%in%EEFlatestSource$series_name)&(!x%in%EEFsources$name)]
  
  match_internal <- filter(match_source,internal_external=="Internal")
  
  if(nrow(match_internal)>0) {
    #html_header <- "<p><strong>Source: </strong>"
    html_text <- htmlSource(match_internal,ifelse(tolower(match_internal$routine_adhoc)%in%"ad hoc"|extra_details!="","Published: ","Last updated: "),extra_details)
    html_text <- c(html_text,unmatched_x)
    if(length(html_text)==1) {
      html_header <- "<p><strong>Source: </strong>" 
    } else {
      html_header <- "<p><strong>Sources: </strong>"
      html_text <- c("<ol>",paste0("<li>",html_text,"</li>",collapse=""),"</ol>")
    }
    
    if(!notes%in%c(NA,"")) {
      notes <- sub("^\\s*Notes?:?\\s?","",notes) #ignore "Note:" if entered in the spreadsheet text
      notes <- strsplit(notes,"\n|\r")[[1]] #treat new lines in excel cell as seperate (numbered) notes
      notes <- notes[grepl("\\S",notes)] #ignore blank lines
      if(length(notes)>1) html_text <- c(html_text,paste0("\n\n<p><strong>Notes: </strong>",
                                                          "<ol>",paste0("<li>",notes,"</li>",collapse=""),"</ol>",
                                                          "</p>"))
      if(length(notes)==1) html_text <- c(html_text,paste0("\n\n<p><strong>Note: </strong>",notes,"</p>"))
    }
    html_footer <- "</p>\n<div>&nbsp;</div>"
    html <- c(html,html_header,html_text,"</p>")
  }
  
  match_external <- filter(match_source,internal_external=="External")
  
  if(nrow(match_external)>0) {
    html_text <- htmlSource(match_external,"Published: ",extra_details)
    if(nrow(match_internal)==0) html_text <- c(html_text,unmatched_x)
    if(length(html_text)==1) {
      html_header <- "<p><strong>External Source: </strong>" 
    } else {
      html_header <- "<p><strong>External Sources: </strong>"
      html_text <- c("<ol>",paste0("<li>",html_text,"</li>",collapse=""),"</ol>")
    }
    
    if(!notes%in%c(NA,"")) {
      notes <- sub("^\\s*Notes?:?\\s?","",notes) #ignore "Note:"
      notes <- strsplit(notes,"(\n|\r)+")[[1]] #treat new lines in excel cell as seperate (numbered) notes
      notes <- notes[grepl("\\S",notes)] #ignore blank lines
      if(length(notes)>1) html_text <- c(html_text,paste0("\n\n<p><strong>Notes: </strong>",
                                                          "<ol>",paste0("<li>",notes,"</li>",collapse=""),"</ol>",
                                                          "</p>"))
      if(length(notes)==1) html_text <- c(html_text,paste0("\n\n<p><strong>Note: </strong>",notes,"</p>"))
    }
    html_footer <- "<br/><div markdown class='row'><div class='col-sm-1'><img src='icons/infoBlue.svg' width='30' height='30' style='margin:30px 20px'/></div><div class='col-sm-11'><p>This research has been carried out independently of the Scottish Government, the results are hosted on an external website and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p></div></div>"
    #html_footer <- "</p>\n<p class=\"info\">This research has been carried out independently of the Scottish Government, the results are hosted on an external website and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p><div>&nbsp;</div>"
    html <- c(html,html_header,html_text,html_footer)
  }
  
  
  if(length(html)==0) {
    html_header <- "<p><strong>Source: </strong>"
    html_text <- x
    if(!notes%in%c(NA,"")) {
      notes <- sub("^note:? ","",notes,ignore.case = TRUE)
      notes <- strsplit(notes,"(\n|\r)+")[[1]]
      notes <- notes[notes!=""]
      #print(notes)
      if(length(notes)>1) notes <- paste0("<ol>",paste0("<li>",notes,"</li>"),"</ol>")
      #print(notes)
      html_text <- c(html_text,paste0("\n\n<p><strong>Note: </strong>",notes,"</p>"))
    }
    html_footer <- "</p>\n<div>&nbsp;</div>"
    html <- c(html,html_header,html_text)
  }
  
  html <- paste(html,collapse="")
  return((html))
}

htmlSource <- function(eef_data,prefix_date="Last updated: ",extra_details) {
  eef_data$extra_details <- extra_details
  eef_data$prefix_date <- prefix_date
  eef_data <- mutate(eef_data,link=gsub("&","&amp;",link))
  eef_data <- mutate(eef_data,published_by=gsub("&","&amp;",published_by))
  eef_data <- mutate(eef_data,html_links=ifelse(link%in%c("",NA),paste("<span style='text-decoration:underline'>",name,"</span>"),paste("<a target=\"_blank\" href=\"",link,"\">",name,"</a>",sep="")))
  eef_data <- mutate(eef_data,text_date=ifelse(published_date%in%c("",0,NA),published_year,format(as.Date(published_date,origin="1900-1-1"),"%B %Y")))
  eef_data <- mutate(eef_data,html_updated=paste(ifelse(text_date%in%c(NA,""),"",paste(prefix_date,text_date,sep="")),ifelse(published_by%in%c(NA,"")|text_date%in%c(NA,""),"",", "),ifelse(published_by%in%c(NA,""),"",published_by),sep=""))
  eef_data <- mutate(eef_data,details_updated=paste(ifelse(html_updated%in%c("",NA),"",html_updated),ifelse(html_updated%in%c(NA,"")|extra_details%in%c(NA,""),"",". "),ifelse(extra_details%in%c(NA,""),"",extra_details),sep=""))
  eef_data <- mutate(eef_data,html_details=ifelse(details_updated%in%c("",NA),"",paste(" (",details_updated,")",sep="")))
  
  #html_text <- paste(ifelse(eef_data$link%in%c(NA,""),eef_data$name,eef_data$html_links),eef_data$html_details,sep="",collapse="<strong>, </strong>")
  html_text <- paste(ifelse(eef_data$link%in%c(NA,""),eef_data$name,eef_data$html_links),eef_data$html_details,sep="")
  #if(length(html_text)>1) html_text <- c("<ol>",paste0("<li>",html_text,"</li>",collapse=""),"</ol>")
  
  return(html_text)
}

#utility function used for arranging bar charts into ascending/descending order with "Total" moved to the top/bottom
#extension of dplyr::arrange
arrangeBreakdown <- function(dataset,col,top="\\b((total)|(all))\\b",bottom="^$",sortOrder="decreasing",...) {
  if(is.numeric(col)) col <- as.character(names(dataset)[col])
 # browser()
  if(tolower(sortOrder)%in%"decreasing") return(arrange(dataset,grepl(bottom,tolower(Breakdown)),!grepl(top,tolower(Breakdown)),desc(.data[[col]])))
  if(tolower(sortOrder)%in%"increasing") return(arrange(dataset,grepl(bottom,tolower(Breakdown)),!grepl(top,tolower(Breakdown)),.data[[col]])) 
  return(arrange(dataset,grepl(bottom,tolower(Breakdown)),!grepl(top,tolower(Breakdown))))
}  

###experimental code not currently being used for EEF. potentially could be used for automating publication links from the forthcoming publications XML - may be worthwhile pursuing but check with Gregor Boyd whether the XML is still being updated

# OrganisationList <- c("ASD001","ASD002","ASD003","ASD004","ASD005","ASD006","ASD007","ASD008","ASD009","ASD010","ASD011","ASD012","ASD013","ASD014","ASD015","ASD016","ASD017","ASD018","ASD019")
# OrganisationList<- "ASD001,ASD002,ASD003,ASD004,ASD005,ASD006,ASD007,ASD008,ASD009,ASD010,ASD011,ASD012,ASD013,ASD014,ASD015,ASD016,ASD017,ASD018,ASD019"
# 
# forthcomingPubs <- function(ASD="ASD001,ASD002,ASD003,ASD004,ASD005,ASD006,ASD007,ASD008,ASD009,ASD010,ASD011,ASD012,ASD013,ASD014,ASD015,ASD016,ASD017,ASD018,ASD019") {
#   readXML <- paste0("https://procxed.scotxed.net/procxedwebservice/ProcXedDataReturn.asmx/GetForthcomingPublications?OrganisationIDList=",ASD) %>%
#     readLines() %>%
#     paste(collapse="\n") %>%
#     XML::xmlTreeParse(asText=T) %>%
#     XML::xmlToList()
#   
#   output <- data.frame()
#   for(i in 1:length(readXML)) if(is.list(readXML[[i]])) 
#     for(j in 1:length(readXML[[i]])) if(is.list(readXML[[i]][[j]])) 
#        output <- bind_rows(output,
#                           data.frame(seriesName=paste0("",readXML[[i]][[j]]$SeriesName),
#                                      EditionName=paste0("",readXML[[i]][[j]]$Editions$EditionName),
#                                      publicationDate=paste0("",readXML[[i]][[j]]$Editions$PublicationDate),
#                                      link=paste0("",readXML[[i]][[j]]$Editions$UrlAddress),
#                                      frequency=paste0("",readXML[[i]][[j]]$Editions$Frequency),
#                                      publishedBy=paste0("",readXML[[i]][[j]]$ProducerOrganisationOther),
#                                      stringsAsFactors=F))
#   
#    return(output)                                  
# }