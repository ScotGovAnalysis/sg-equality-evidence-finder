EEFlatestSource <- 
  EEFsources %>%
  group_by(series_name) %>%
  summarise(sort_date=max(sort_date,na.rm=TRUE)) %>%
  inner_join(EEFsources,by=c("series_name","sort_date")) %>%
  filter(!duplicated(series_name))

EEFdata <- mutate(EEFdata,Year=ifelse(Year==0,year(Date),Year))

EEFlatest <- EEFdata %>%
  group_by(Breakdown,SubBreakdown,Characteristic,Measure,policy_area) %>%
  summarise(Date=max(Date,na.rm=TRUE)) %>%
  ungroup
EEFlatest <- inner_join(EEFdata,EEFlatest,by=c("Date","Measure","Breakdown","SubBreakdown","Characteristic","policy_area"))

EEFpublishedIndex <- group_by(EEFpublished,policy_area) %>%
  gather("Characteristic","Priority",age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
  group_by(policy_area,Characteristic) %>%
  summarise(Priority=sum(Priority,na.rm=TRUE)) %>%
  filter(Priority>0)

EEFdataLinksIndex <- group_by(EEFdataLinks,policy_area) %>%
  gather("Characteristic","Priority",age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
  group_by(policy_area,Characteristic) %>%
  summarise(Priority=sum(Priority,na.rm=TRUE)) %>%
  filter(Priority>0)

EEFexternalIndex <- group_by(EEFexternal,policy_area) %>%
  gather("Characteristic","Priority",age,disability,ethnicity,gender,religion,sexualOrientation,transgender,socioEconomicStatus) %>%
  group_by(policy_area,Characteristic) %>%
  summarise(Priority=sum(Priority,na.rm=TRUE)) %>%
  filter(Priority>0)


EEFindex <- mutate(EEFindex,cloneTabID=as.character(cloneTabID)) %>%
  left_join(select(EEFindex,tabUID,topic,characteristic,tab,headline,graphID,subtitle,image,dataSource,dataSourceNotes,markdownFile),
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
         markdownFile=ifelse(is.na(markdownFile.x),markdownFile.y,markdownFile.x)
  )

EEFindex <- group_by(EEFindex,policy_area) %>% mutate(topicID=match(topic,unique(topic)))


#Graph data
filterData <- list()
filterData[["incPov-1"]] <- filter(EEFdata,Characteristic=="Gender",Measure%in%c("% in relative poverty AHC"))
filterData[["incPov-4"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure%in%c("% in relative poverty AHC","% in relative poverty BHC"))
filterData[["incPov-8"]] <- filter(EEFlatest,Characteristic=="Religion",Measure%in%c("% in relative poverty AHC","% in relative poverty BHC"))
filterData[["incPov-6"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% in relative poverty AHC")
filterData[["sch-Edu-1"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
filterData[["sch-Edu-2"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
filterData[["sch-Edu-3"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
filterData[["sch-Edu-4"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers in a positive follow-up destination")
filterData[["sch-Edu-5"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
filterData[["sch-Edu-6"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
filterData[["sch-Edu-7"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
filterData[["sch-Edu-8"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers in a positive follow-up destination")
filterData[["sch-Edu-21"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
filterData[["sch-Edu-22"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
filterData[["sch-Edu-23"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
filterData[["sch-Edu-24"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% school leavers in a positive follow-up destination")
filterData[["sch-Edu-9"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
filterData[["sch-Edu-10"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
filterData[["sch-Edu-11"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
filterData[["sch-Edu-12"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers in a positive follow-up destination")
filterData[["sch-Edu-14"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure=="Pupil ethnicity")
filterData[["sch-Edu-15"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure=="Teacher ethnicity")
filterData[["sch-Edu-41"]] <- filter(EEFlatest,Characteristic=="Gender",Measure%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
filterData[["sch-Edu-13"]] <- filter(EEFdata,Measure=="Teachers (headcount)",Characteristic=="Age")
