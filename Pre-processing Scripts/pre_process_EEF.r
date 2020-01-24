EEFdata <- mutate(EEFdata,source="EEF")
NPFdata <- mutate(NPFdata,source="NPF")
ODPdata <- mutate(ODPdata,source="ODP")

EEFsources <- EEFsources %>%
  mutate(published_year=ifelse(published_year%in%c(NA,"",0),as.numeric(substr(published_date,1,4)),published_year),
         sort_date=ifelse(published_date%in%c(NA,"",0),ymd(paste0(published_year,"-1-1"),quiet=TRUE),published_date),
         sort_date=ifelse(is.na(sort_date),0,sort_date))

EEFlatestSource <- 
  select(EEFsources,-policy_area) %>%
  group_by(series_name) %>%
  arrange(source_updated,sort_date) %>% 
  summarise_all(dplyr::last) %>%
  ungroup()
  #inner_join(select(EEFsources,-policy_area),by=c("series_name","sort_date")) %>%
  #filter(!duplicated(series_name))

EEFpublished <- inner_join(select(EEFpublished,policy_area,series_name,description,overview,age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus),
                           EEFlatestSource,by="series_name")


# NPFextra <- mutate(NPFextra,Date=as.Date(Date),
#                    Year=as.numeric(Year),
#                    Figure=as.numeric(Figure)) %>%
#   mutate(yearOnly=is.na(Date),
#          DateString=ifelse(Date%in%c("",NA),paste(sep="",as.character(Year),"-01-01"),as.character(Date)),
#          Date=as.Date(DateString),
#          Year=ifelse(Year==0,year(Date),Year),
#          seriesColour=NA)



##add blue seriesColour to totals
# NPFdata <- NPFdata %>% 
#   filter(Characteristic=="Total") %>% 
#   distinct(Outcome,Indicator,Disaggregation,Breakdown) %>%
#   group_by(Outcome,Indicator) %>%
#   mutate(seriesColour=replace(NA,n()==1,"blue"))%>%#,
#          #seriesColour=replace(seriesColour,n()%in%2:6,c("very-dark-blue","dark-blue","blue","mid-blue","light-blue","very-light-blue")[1:n()]),
#          #seriesColour=replace(seriesColour,n()>6,c("very-dark-blue","mid-dark-blue","dark-blue","light-dark-blue","blue","dark-mid-blue","mid-blue","light-mid-blue","light-blue","mid-light-blue","very-light-blue")[1:n()])) %>%
#   ungroup %>%
#   full_join(select(NPFdata,-seriesColour),by=c("Outcome","Indicator","Disaggregation","Breakdown")) %>%
#   mutate(Measure=Disaggregation)

NPFindex <- NPFdata %>% group_by(Outcome,Indicator,Measure) %>% summarise(graphTitle=graphTitle[1],Source=Source[1]) %>% ungroup

# EEFdata <- mutate(EEFdata,
#                   #yearOnly=is.na(Date),
#                   intervalType=ifelse(is.na(intervalType),ifelse(is.na(Date),"Year","Month"),intervalType),
#                   DateString=ifelse(Date%in%c("",NA),paste(sep="",as.character(Year),"-01-01"),as.character(Date)),
#                   Date=as.Date(DateString),
#                   Year=ifelse(Year==0,year(Date),Year)) %>%
#   select(-DateString)

EEFlatest <- EEFdata %>%
  group_by(Characteristic,Measure,policy_area) %>%
  summarise(Date=max(Date,na.rm=TRUE)) %>%
  ungroup
EEFlatest <- inner_join(EEFdata,EEFlatest,by=c("Date","Measure","Characteristic","policy_area"))

NPFdataIndex <- distinct(NPFdata,Characteristic) %>% mutate(policy_area="Summary")

EEFpublishedIndex <- filter(EEFpublished,internal_external=="Internal") %>%
  group_by(policy_area) %>%
  gather("Characteristic","Priority",age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
  group_by(policy_area,Characteristic) %>%
  summarise(Priority=sum(Priority,na.rm=TRUE)) %>%
  filter(Priority>0)

EEFdataLinksIndex <- group_by(EEFdataLinks,policy_area) %>%
  gather("Characteristic","Priority",age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
  group_by(policy_area,Characteristic) %>%
  summarise(Priority=sum(Priority,na.rm=TRUE)) %>%
  filter(Priority>0)

EEFexternalIndex <- filter(EEFpublished,internal_external=="External") %>%
  bind_rows(EEFexternal) %>%
  group_by(policy_area) %>%
  gather("Characteristic","Priority",age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
  group_by(policy_area,Characteristic) %>%
  summarise(Priority=sum(Priority,na.rm=TRUE)) %>%
  filter(Priority>0)


EEFindex <- filter(EEFindex,!is.na(tabUID))
EEFindex <- mutate(EEFindex,cloneTabID=as.character(cloneTabID)) %>%
  left_join(select(EEFindex,tabUID,topic,characteristic,tab,headline,graphID,subtitle,image,dataSource,dataSourceNotes,markdownFile,inputMarkdownFile),
                      by=c("cloneTabID"="tabUID")) %>%
  mutate(topic=ifelse(is.na(topic.x),topic.y,topic.x),
         characteristic=ifelse(is.na(characteristic.x),characteristic.y,characteristic.x),
         tab=ifelse(is.na(tab.x),tab.y,tab.x),
         headline=ifelse(is.na(headline.x),headline.y,headline.x),
         graphID=ifelse(is.na(graphID.x),graphID.y,graphID.x),
         subtitle=ifelse(is.na(subtitle.x),subtitle.y,subtitle.x),
         image=ifelse(is.na(image.x),image.y,image.x),
         dataSource=ifelse(is.na(dataSource.x),dataSource.y,dataSource.x),
         dataSourceNotes=ifelse(is.na(dataSourceNotes.x),dataSourceNotes.y,dataSourceNotes.x),
         markdownFile=ifelse(is.na(markdownFile.x),markdownFile.y,markdownFile.x),
         inputMarkdownFile=ifelse(is.na(inputMarkdownFile.x),inputMarkdownFile.y,inputMarkdownFile.x)
  ) %>%
  select(policy_area,tabUID,cloneTabID,topic,characteristic,tab,headline,graphID,subtitle,image,dataSource,dataSourceNotes,markdownFile,inputMarkdownFile,index_updated)


#EEFindex <- group_by(EEFindex,policy_area) %>% mutate(topicID=match(topic,unique(topic))) %>% ungroup

#EEFtopics <- read.csv(file.path(dataDir,"topic IDs.csv"),stringsAsFactors = FALSE) %>%
EEFtopics <- EEFtopics %>%
  select(policy_area,topicID,topic) %>%
  full_join(EEFindex,by=c("policy_area","topic")) %>%
  mutate(class=policyCSS(policy_area)) %>%
  distinct(topicID,class,policy_area,topic)
EEFtopics$topicID[is.na(EEFtopics$topicID)] <- max(EEFtopics$topicID,na.rm=TRUE) + 1:sum(is.na(EEFtopics$topicID))
#write.csv(EEFtopics,file=file.path(dataDir,"topic IDs.csv"),row.names = FALSE)



#EEFindex <- ungroup(EEFindex) %>% 
#  mutate(class=policyCSS(policy_area))
#EEFtopics <- distinct(EEFindex,topic,class,policy_area) %>% mutate(topicID=row_number())

EEFindex <- left_join(EEFindex,EEFtopics,by=c("topic","policy_area"))
message(paste(length(unique(EEFindex$topicID[!is.na(EEFindex$topicID)])),"topic sections loaded"))
#tell app that UTF-8 encoding is used for special characters (only needed for character columns that may contain special (non-ascii) characters - e.g. excel treats ... as a special character)
Encoding(EEFdata$Breakdown) <- "UTF-8"
Encoding(EEFdata$SubBreakdown) <- "UTF-8"
Encoding(EEFdata$Measure) <- "UTF-8"
Encoding(EEFpublished$description) <- "UTF-8"
Encoding(EEFdataLinks$description) <- "UTF-8"
Encoding(EEFexternal$description) <- "UTF-8"
Encoding(EEFindex$dataSource) <- "UTF-8"
Encoding(EEFindex$dataSourceNotes) <- "UTF-8"
