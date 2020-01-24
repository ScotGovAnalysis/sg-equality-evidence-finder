###################################################################################
#                                                                                 #
# Author: Jaye Ware                                                               #
# Last updated: 07/12/2018                                                        #
#                                                                                 #
# Purpose: Update the EEF datasets for use in the testApp and publish App scripts #
#                                                                                 #
# History: replaces update_EEF2.r (combines code for test app and main app)       #
#                                                                                 #
# Functions: updateNPFdata - reads data in from NPF spreadsheet and updates the   #
#                            App Data. Also calls updateAppData to update the app #
#                            as well.                                             #
#            updateEEFdata - reads data in from EEF spreadsheets and updates the  #
#                            App Data. Also calls updateAppData to update the app #
#                            as well.                                             #
###################################################################################

###Required parameters
# dataDirIn - path for the EEF data folder - for reading app data - must be a single location
# dataDirOut - path for the EEF data folder - for saving app data - can be multiple folders (useful for synciing main  and test apps)
# appDir - path for the EEF app folder
# scriptDir - path for folder containing pre-processing scripts
# updatesDir - path for the folder containing updates spreadsheets
# policyArea - policy area to be updated
# tabUIDs - for specifying which panels to be updated. If null then all panels for the policy area will be updated [default: NULL]
# mdDir - markdown directory (testApp: sandbox/temp; publish App: appDir/EEF)
# imgPath - path for App www folder (testApp: update-www; publish App: EEF)
# mdPath - path for App markdown folder (testApp: updatesDir/temp; publish App: EEF)

updateEEFdata <- function(updatePolicyArea,appDir,scriptDir,updatesDir,dataDirIn,dataDirOut,tabUIDs=NULL) {
 
  load(file.path(dataDirIn,"EEF.rData"))
  rmdFiles <- character(0)
  
  ODPdata <- read_excel(paste0(scriptDir,"/sparql.xlsx")) %>%
    select(policy_area,Indicator=Topic,Measure,Characteristic,Breakdown,graphTitle,seriesColour)
  
  for(currAppDir in appDir) for(policyArea in updatePolicyArea) {
    if(file.exists(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"))) {
      imgPath <- file.path("EEF",policyArea)
      mdPath <-  file.path("EEF",policyArea)
      mdDir <-   file.path(currAppDir,"EEF")
      message(paste("Updating",policyArea,"data"))
      
      
      readSources <- read_excel(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"),skip=3,sheet=1) %>%
        select(series_name,name,next_updated,published_year,published_date,published_by,internal_external,routine_adhoc,link,
               matches("^name\\d+$"),matches("^link\\d+$")) %>%
        mutate(policy_area=policyArea,
               published_date=as.Date(published_date),
               next_updated=as.Date(next_updated),
               published_year=as.numeric(published_year),
               source_updated=Sys.Date())
      
      EEFsources <- EEFsources %>%
        filter(policy_area!=policyArea|(!name%in%readSources$name)) %>%
        bind_rows(readSources)%>% 
        arrange(policy_area,series_name,name) %>%
        mutate(published_year=ifelse(published_year%in%c(NA,"",0),as.numeric(substr(published_date,1,4)),published_year),
               sort_date=ifelse(published_date%in%c(NA,"",0),ymd(paste0(published_year,"-1-1"),quiet=TRUE),published_date),
               sort_date=ifelse(is.na(sort_date),0,sort_date))
      
      readData <- read_excel(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"),guess_max=10000,skip=1,sheet=3) %>%
        select(Year,Date,Figure,Indicator=Topic,Measure,Characteristic,Breakdown,graphTitle,seriesColour,annotationText) %>%
        addIntervalType() %>%
        mutate(policy_area=policyArea,
               # Date=as.Date(Date),
               # intervalType=ifelse(is.na(Date),"Year","Month"),
               # intervalType=ifelse(grepl("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$",Year),
               #                     paste((as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$","\\3",Year))-as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$","\\1",Year)))%%100,"year"),
               #                     intervalType),
               # Year=as.numeric(sub("^\\s*(\\d*)(\\D.*)?$","\\1",Year))+(as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$","\\3",Year))-as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$","\\1",Year)))%%100,#get final year if entered as a range.
               # Year=ifelse(is.na(Year),0,Year),
               Indicator=as.character(Indicator),
               Measure=as.character(Measure),
               Breakdown=as.character(Breakdown),
               #Date=as.Date(ifelse(is.na(Date),as.Date(paste(sep="",as.character(Year),"-01-01")),Date)),
               Figure=as.numeric(Figure))
      
      
      EEFdata <- EEFdata %>%
        filter(policy_area!=policyArea) %>%
        bind_rows(readData) %>% 
        arrange(policy_area,Characteristic)
      
      readPublished <- read_excel(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"),skip=1,sheet=4) %>%
        select(series_name,description,overview,age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
        mutate(policy_area=policyArea)
      
      EEFpublished <- EEFpublished %>%
        filter(policy_area!=policyArea) %>%
        bind_rows(readPublished) %>% 
        arrange(policy_area)
      
      readDataLinks <- read_excel(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"),skip=1,sheet=5) %>%
        select(name,link,published_by,publication_type,description,
               name1,link1,name2,link2,name3,link3,name4,link4,name5,link5,name6,link6,name7,link7,name8,link8,
               overview,age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
        mutate(policy_area=policyArea)
      
      EEFdataLinks <- EEFdataLinks %>%
        filter(policy_area!=policyArea) %>%
        bind_rows(readDataLinks) %>% 
        arrange(policy_area)
      
      
      readExternal <- read_excel(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"),skip=1,sheet=6) %>%
        select(name,link,description,overview,age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
        mutate(policy_area=policyArea)
      
      EEFexternal <- EEFexternal %>%
        filter(policy_area!=policyArea) %>%
        bind_rows(readExternal) %>% 
        arrange(policy_area)
      
      if("Additional Links"%in%excel_sheets(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"))) {
        readAdditional <- read_excel(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"),skip=1,sheet="Additional Links") %>%
          select(name,link,description,publication_type,published_year,published_date,overview,age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
          mutate(policy_area=policyArea)
        
        # EEFadditional <- EEFadditional %>%
        #   filter(policy_area!=policyArea) %>%
        #   bind_rows(readAdditional) %>% 
        #   arrange(policy_area)
        EEFadditional <- readAdditional %>%
          mutate(published_by="",
                 published_year=ifelse(published_year%in%c(NA,"",0),
                                       as.numeric(substr(published_date,1,4)),
                                       published_year))
        
      }
      
      readIndex <- read_excel(paste0(updatesDir,"/",policyArea,"/",policyArea,".xlsx"),skip=1,sheet=2) %>%
        select(topic,characteristic,tabUID,cloneTabID,tab,headline,subtitle,image,dataSource,dataSourceNotes,markdownFile) %>%
        mutate(policy_area=policyArea,
               markdownFile=ifelse(grepl("\\.",markdownFile)|is.na(markdownFile),markdownFile,paste0(markdownFile,".md")),
               inputMarkdownFile=markdownFile,
               index_updated=Sys.Date())
      
      if(!is.null(tabUIDs)) readIndex <- readIndex%>%filter(tabUID %in% tabUIDs)
      
      inputMdFiles <- file.path(updatesDir,policyArea,"markdown",tolower(readIndex$markdownFile))
      outputMdFiles <- file.path(mdDir,policyArea,tolower(readIndex$markdownFile))
      inputImgFiles <- file.path(updatesDir,policyArea,"www",readIndex$image)
      outputImgFiles <- file.path(currAppDir,"www","EEF",policyArea,tolower(readIndex$image))
      fileIndex <- mutate(readIndex,
                          inputMdFiles=file.path(updatesDir,policyArea,"markdown",tolower(markdownFile)),
                          outputMdFiles=file.path(mdDir,policyArea,tolower(markdownFile)),
                          inputImgFiles=file.path(updatesDir,policyArea,"www",image),
                          outputImgFiles=file.path(currAppDir,"www","EEF",policyArea,tolower(image)))
      
      readIndex <- mutate(readIndex,
                          image=ifelse(is.na(image),NA,file.path(imgPath,tolower(image))),
                          inputMarkdownFile=ifelse(is.na(markdownFile),NA,file.path(mdPath,tolower(markdownFile))),
                          markdownFile=ifelse(is.na(markdownFile),NA,sub("\\.rmd$","\\.md",inputMarkdownFile,ignore.case = TRUE)))
      
      if(is.null(tabUIDs)) {
        EEFindex <- EEFindex %>%
          filter(policy_area!=policyArea) %>%
          mutate(cloneTabID=as.character(cloneTabID)) %>%
          #mutate(markdownFile=ifelse(is.na(markdownFile),NA,file.path(currAppDir,markdownFile))) %>%##testApp only???
          bind_rows(readIndex) %>% 
          arrange(policy_area)
      } else {
        EEFindex <- EEFindex %>%
          filter(!tabUID %in% tabUIDs) %>%
          mutate(cloneTabID=as.character(cloneTabID)) %>%
          #mutate(markdownFile=ifelse(is.na(markdownFile),NA,file.path(currAppDir,markdownFile))) %>%##testApp only???
          bind_rows(readIndex) %>% 
          arrange(policy_area)
      }
      
      #create markdown sub-directories
      newDir <- unique(dirname(c(character(0),outputMdFiles)))
      newDir <- newDir[!dir.exists(newDir)]
      if(length(newDir)>0) for(d in newDir) dir.create(d)
      
      #converts all files to "UTF-8" - this is needed for special characters that are saved saved in a file encoded with Microsoft's file encoding (Shiny uses the international UTF-8 standard instead)
      inFiles <- c(inputMdFiles[file.exists(inputMdFiles)],
                   file.path(updatesDir,policyArea,"Contact.md"))
      outFiles <- c(outputMdFiles[file.exists(inputMdFiles)],
                    file.path(currAppDir,"EEF",policyArea,"Contact.md"))
      if(length(inFiles)>0) for(f in 1:length(inFiles)) {
        readLines(inFiles[f],encoding="UTF-8",warn=FALSE)%>%
          paste(collapse="\n") %>%
          #gsub("\n\n\\* ","\n* ",.) %>%
          sub("\\x{FEFF}","",.) %>%
          writeLines(outFiles[f],useBytes=TRUE)
      }
      
      
      #copy www and contact files
      inputFiles <- inputImgFiles[ file.exists(inputImgFiles) ]
      outputFiles <- outputImgFiles[ file.exists(inputImgFiles) ]
      
      newDir <- unique(dirname(c(character(0),outputFiles)))
      newDir <- newDir[!dir.exists(newDir)]
      if(length(newDir)>0) for(d in newDir) dir.create(d)
      
      file.copy(inputFiles[!duplicated(inputFiles)],outputFiles[!duplicated(inputFiles)],overwrite=TRUE)  
      rmdFiles <- c(rmdFiles,
                    filter(fileIndex,!is.na(outputMdFiles),file.exists(outputMdFiles),grepl("\\.Rmd$",outputMdFiles,ignore.case=TRUE))$tabUID)
      message(paste(length(unique(fileIndex$tabUID)),"panels updated"))
      
      
      ####VALIDATIONS####
      if(nrow(readSources)>0) {
        duplicates <- EEFsources%>%
          distinct(policy_area,series_name) %>%
          filter(policy_area!=policyArea) %>%
          inner_join(readSources %>% distinct(series_name),
                     by="series_name")
        
        ###old validation
        # duplicates <- distinct(EEFsources,policy_area,series_name) %>%
        #   group_by(series_name) %>%
        #   summarise(n=n()) %>%
        #   ungroup %>%
        #   filter(n>1) %>%
        #   left_join(distinct(EEFsources,policy_area,series_name),by="series_name") %>%
        #   filter(policy_area==updatePolicyArea)
        # 
        if(nrow(duplicates)>0) {
          for(r in 1:nrow(duplicates)) warning(paste0("EVIDENCE FINDER ERROR: Data source \"",duplicates$series_name[r],"\" already exists on the ",duplicates$policy_area[r]," spreadsheet. Please update the data source on the ",duplicates$policy_area[r]," spreadsheet instead or contact the Evidence Finder team."))
          
          #remove duplicated data source from the main dataset and continue - revision: changed this from an error to a warning to allow the update to continue though with the duplicate removed (JW Sep 2019)
          EEFsources <- EEFsources %>%
            anti_join(duplicates%>%mutate(policy_area=policyArea),
                      by=c("policy_area","series_name"))
        }
      }
      
      if(nrow(readIndex)>0) {
        #validate chart title
        #chartTitleNotFound <- filter(readIndex,subtitle%in%c("",NA),cloneTabID%in%c("",NA),tabUID%in%names(graphOptions))
        #if(nrow(chartTitleNotFound)>0) warning(paste("EVIDENCE FINDER WARNING: Graph Title is missing for module:",chartTitleNotFound$tabUID,"\n"))
        
        #validate headline
        panelHeadlineNotFound <- filter(readIndex,headline%in%c("",NA),cloneTabID%in%c("",NA),!tabUID%in%NA)
        if(nrow(panelHeadlineNotFound)>0) warning(paste("EVIDENCE FINDER WARNING: Headline is missing for module:",panelHeadlineNotFound$tabUID,"\n"))
        
        #validate duplicate tabUID
        duplicatedTabUID <- readIndex$tabUID[duplicated(readIndex$tabUID) & !is.na(readIndex$tabUID)]
        if(length(duplicatedTabUID)>0) stop(paste("EVIDENCE FINDER ERROR: Duplicate entries found on modules sheet for:",paste(duplicatedTabUID,sep=", "),"\n"))
        
        #validate module data sources
        dataSourceNotFound <- readIndex$dataSource %>%
          strsplit("\r|\n") %>%
          sapply(function(x){
            paste(x[!is.na(x)&(!x%in%EEFsources$series_name)&(!x%in%EEFsources$name)],collapse=", ")
          })
        if(any(dataSourceNotFound!="")) stop(paste("EVIDENCE FINDER ERROR: Module data source not found for:",dataSourceNotFound[which(dataSourceNotFound!="")],"(tabUID: ",readIndex$tabUID[which(dataSourceNotFound!="")],")\n"))
        
        
        # dataSourceNotFound <- filter(EEFindex,
        #                              !dataSource %in% EEFsources$series_name,
        #                              !dataSource %in% EEFsources$name,
        #                              !dataSource %in% c("",NA))
        # if(nrow(dataSourceNotFound)>0) stop(paste("EVIDENCE FINDER ERROR: Module data source not found for:",dataSourceNotFound$dataSource,"\n"))
        
        #validate markdown file names
        mdFileNames <- inputMdFiles[!is.na(readIndex$markdownFile)]
        if(length(mdFileNames)>0) {
          fileNotFound <- mdFileNames[!file.exists(mdFileNames)]
          if(length(fileNotFound)>0) stop(paste("EVIDENCE FINDER ERROR: Markdown file not found for:",fileNotFound,"\n"))
        }
        
        #validate markdown file encoding (must be UTF-8 compatible)
        if(length(mdFileNames)>0) {
          invalidEncoding <- sapply(mdFileNames,FUN=function(x) !all(validUTF8(readLines(x,warn=FALSE))))
          if(any(invalidEncoding)>0) stop("EVIDENCE FINDER ERROR: invalid character found. Please select 'UTF-8' encoding in the Notepad save as dialogue box when saving the markdown files for:\n",paste(unique(mdFileNames[invalidEncoding]),"\n"))
        }
        
      }
      
      if(nrow(readPublished)>0) {
        
        #validate publication name
        publicationNotFound <- anti_join(readPublished,EEFsources,by="series_name")
        if(nrow(publicationNotFound)>0) stop(paste("EVIDENCE FINDER ERROR: Publication data source not found for:",publicationNotFound$series_name))
        
      }
    }
  }
  
  source(file.path(scriptDir,"update_app.r"),local=T)
  for(i in seq_along(dataDirOut)) {
    save(EEFsources,EEFdata,EEFpublished,EEFdataLinks,EEFexternal,EEFadditional,EEFindex,EEFtopics,ODPdata,NPFdata,
         file=file.path(dataDirOut[i],"EEF.rData"),
         envir=environment(),version=2)
    if(is.null(tabUIDs)) {
      if(!is.na(appDir[i])) updateAppData(appDir[i],dataDirOut[i],scriptDir,policyArea=updatePolicyArea)
    } else {
      if(!is.na(appDir[i])) updateAppData(appDir[i],dataDirOut[i],scriptDir,tabUIDs=tabUIDs)
    }
  }
  
  select(EEFsources,series_name) %>% 
    arrange(series_name) %>% 
    unique() %>% 
    write.csv(file.path(updatesDir,"temp","series names.csv"),row.names=F)
  
}

updateEEFlinks <- function(updatePolicyArea,tabUIDs=character(0),...) updateEEFdata(updatePolicyArea=updatePolicyArea,tabUIDs=tabUIDs,...)


#NPF dataset Characteristic column is either:
# -equality characteristic
# -"None" - for other breakdowns, e.g. Local Authority
# -"Total" - the total if the total isn't provided for individual disaggregations
#N.B. Total should either be included in the equality (and other "None") breakdowns OR be included under the Total characteristic
updateNPFdata_file <- function(file,appDir,scriptDir,updatesDir,dataDirIn,dataDirOut) {
  load(file.path(dataDirIn,"EEF.rData"))
  currentNPF <- NPFdata %>% distinct(Indicator,Characteristic) %>% filter(Characteristic%in%equalityCharacteristics)
  
  NPFcharacteristics <- openxlsx::read.xlsx(file.path(scriptDir,"NPF characteristics.xlsx"),na.strings=c("","NA"))
  
  #NPFdata <- read.csv(file,na=c("","NA"),stringsAsFactors = FALSE) %>%
  NPFdata <- #read_excel(file.path(updatesDir,file),na=c("","NA"),guess_max=100000) %>%
    openxlsx::read.xlsx(file.path(updatesDir,file),detectDates=TRUE,na.strings=c("","NA")) %>%
    filter(!is.na(Year)) %>%
    filter(Outcome!="All") %>%
    addIntervalType() %>%
    mutate(seriesColour=NA,
           Disaggregation=as.character(Dissaggregation),
           Figure=as.numeric(Figure),
           row_id=row_number(),
           isTotal=grepl("\\b((total)|(all))\\b",Dissaggregation,ignore.case=TRUE)#,
           #        Date=as.Date(Date,format="%d/%m/%Y"),
           #        intervalType=ifelse(is.na(Date),"Year","Month"),
           #        intervalType=ifelse(grepl("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$",Year),
           #                            paste((as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$","\\3",Year))-as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$","\\1",Year)))%%100,"year"),
           #                            intervalType),
           #        Year=as.numeric(sub("^\\s*(\\d*)(\\D.*)?$","\\1",Year))+(as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$","\\3",Year))-as.numeric(sub("^\\s*\\d{2}(\\d{2})[-/](\\d{2})?(\\d{2})\\s*$","\\1",Year)))%%100#get final year if entered as a range.
    ) #%>%
  #filter(!is.na(Date)|!is.na(YYYY)) %>%
  # mutate(DateString=ifelse(Date%in%c("",NA),paste(sep="",as.character(Year),"-01-01"),as.character(Date)),
  #        Date=as.Date(DateString),
  #        Year=ifelse(Year==0,year(Date),Year)
  # )
  # 
  NPFdata <- NPFdata %>%
    bind_rows(NPFdata %>% filter(Indicator=="Healthy weight",Disaggregation=="Total",Breakdown=="All Children (aged 2-15)") %>% mutate(Disaggregation="Gender",Characteristic="Gender")) %>%
    bind_rows(NPFdata %>% filter(Indicator=="Healthy weight",Disaggregation=="Total",Breakdown=="All Children (aged 2-15)") %>% mutate(Disaggregation="Age",Characteristic="Age"))
  
  
  #Add Totals
  NPFtotals <- NPFdata %>% filter(Characteristic=="Total") %>% 
    select(-Characteristic,-Disaggregation) %>%
    inner_join(NPFdata %>% distinct(Outcome,Indicator,Characteristic,Disaggregation,Year),
               by=c("Outcome","Indicator","Year")) %>%
    mutate(seriesColour="blue")
  
  NPFdata <- bind_rows(NPFdata%>%filter(Characteristic!="Total"),
                       NPFtotals) %>%
    group_by(Characteristic,Date,Year,Outcome,Indicator,Disaggregation,Breakdown) %>%
    summarise_all(first) %>%
    ungroup%>%
    arrange(Outcome,Indicator,Disaggregation,row_id)%>%
    select(Characteristic,Date,Year,Figure,Outcome,Indicator,Measure=Disaggregation,Breakdown,graphTitle=Measure,Source,seriesColour,intervalType)
  
  #Ad hoc fixes
  NPFdata <- mutate(NPFdata,
                    Characteristic2=ifelse(grepl("Healthy life expectancy",Indicator),"Gender",NA),
                    Outcome=ifelse(Outcome=="Human Rights","Human rights",Outcome),
                    Outcome=ifelse(Outcome=="Fair Work & Business","Fair work and business",Outcome))
  
  if(file=="20190607 - NPF Database -EEF - HLE - JW.xlsx") NPFdata <- NPFdata %>% filter(Indicator!="Gender balance in organisations"|Characteristic!="Age")
  
  #NPFdata <- readNPFdata()
  
  View(NPFdata%>%distinct(Outcome,Indicator,graphTitle))
  EEFindex <- bind_rows(filter(EEFindex,policy_area!="National Performance Framework"),
                        data.frame(policy_area="National Performance Framework",
                                   tabUID="npf",
                                   index_updated=Sys.Date(),stringsAsFactors = FALSE))
  
  newNPF <- NPFdata %>% distinct(Indicator,Characteristic) %>% 
    filter(Characteristic%in%equalityCharacteristics) %>%
    anti_join(currentNPF, by = c("Characteristic", "Indicator"))
  if(nrow(newNPF)>0) cat("New NPF equality breakdowns added:\n",paste0(" - ",newNPF$Indicator,": ",newNPF$Characteristic,collapse="\n"),"\n",sep="")
  oldNPF <- currentNPF %>% anti_join(NPFdata, by = c("Characteristic", "Indicator")) %>%
    distinct(Indicator,Characteristic) %>% 
    filter(Characteristic%in%equalityCharacteristics) 
  if(nrow(oldNPF)>0) cat("NPF equality breakdowns removed:\n",paste0(" - ",oldNPF$Indicator,": ",oldNPF$Characteristic,collapse="\n"),"\n",sep="")
  
  source(file.path(scriptDir,"update_app.r"),local=T)
  
  for(i in seq_along(dataDirOut)) {
    save(EEFsources,EEFdata,EEFpublished,EEFdataLinks,EEFexternal,EEFadditional,EEFindex,EEFtopics,ODPdata,NPFdata,NPFcharacteristics,
         file=file.path(dataDirOut[i],"EEF.rData"),
         envir=environment(),version=2)
    if(!is.na(appDir[i])) updateAppData(appDir[i],dataDirOut[i],scriptDir,policyArea="National Performance Framework")
  }
  
}
#NPF dataset Characteristic column is either:
# -equality characteristic
# -"None" - for other breakdowns, e.g. Local Authority
# -"Total" - the total if the total isn't provided for individual disaggregations
#N.B. Total should either be included in the equality (and other "None") breakdowns OR be included under the Total characteristic
updateNPFdata <- function(appDir,scriptDir,dataDirIn,dataDirOut) {
  load(file.path(dataDirIn,"EEF.rData"))
  currentNPF <- NPFdata %>% distinct(Indicator,Characteristic) %>% filter(Characteristic%in%equalityCharacteristics)
  
  ODPdata <- read_excel(paste0(scriptDir,"/sparql.xlsx")) %>%
    select(policy_area,Indicator=Topic,Measure,Characteristic,Breakdown,graphTitle,seriesColour)
  
  newNPFcharacteristics <- openxlsx::read.xlsx(file.path(scriptDir,"NPF characteristics.xlsx"),na.strings=c("","NA")) %>%
    mutate(yLabel=as.character(yLabel))
  
  NPFdata <- readNPFdata(newNPFcharacteristics)
  if(is.null(NPFdata)) return(invisible(NULL))
  
  NPFcharacteristics <- distinct(NPFdata,Indicator,Measure,Characteristic)
    openxlsx::write.xlsx(NPFcharacteristics,file.path(scriptDir,"NPF characteristics.xlsx"))
  
  EEFindex <- bind_rows(filter(EEFindex,policy_area!="National Performance Framework"),
                        data.frame(policy_area="National Performance Framework",
                                   tabUID="npf",
                                   index_updated=Sys.Date(),stringsAsFactors = FALSE))
  
  newNPF <- NPFdata %>% distinct(Indicator,Characteristic) %>% 
    filter(Characteristic%in%equalityCharacteristics) %>%
    anti_join(currentNPF, by = c("Characteristic", "Indicator"))
  if(nrow(newNPF)>0) cat("New NPF equality breakdowns added:\n",paste0(" - ",newNPF$Indicator,": ",newNPF$Characteristic,collapse="\n"),"\n",sep="")
  oldNPF <- currentNPF %>% anti_join(NPFdata, by = c("Characteristic", "Indicator")) %>%
    distinct(Indicator,Characteristic) %>% 
    filter(Characteristic%in%equalityCharacteristics) 
  if(nrow(oldNPF)>0) cat("NPF equality breakdowns removed:\n",paste0(" - ",oldNPF$Indicator,": ",oldNPF$Characteristic,collapse="\n"),"\n",sep="")
  
  source(file.path(scriptDir,"update_app.r"),local=T)
  
  for(i in seq_along(dataDirOut)) {
    save(EEFsources,EEFdata,EEFpublished,EEFdataLinks,EEFexternal,EEFadditional,EEFindex,EEFtopics,ODPdata,NPFdata,NPFcharacteristics,
         file=file.path(dataDirOut[i],"EEF.rData"),
         envir=environment(),version=2)
    if(!is.na(appDir[i])) updateAppData(appDir[i],dataDirOut[i],scriptDir,policyArea="National Performance Framework")
  }
  
}

