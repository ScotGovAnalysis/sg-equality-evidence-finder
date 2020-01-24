#Graph data
setGraphData <- list()
setGraphOptions <- list()

####NPF Data####

setGraphOptions[["npf"]] <- list(ylabel="",graphType="npfDataExplorer")
setGraphData[["npf"]] <- NPFdata

####Summary####

setGraphData[["disability-1"]] <- filter(EEFlatest,policy_area=="Summary",Characteristic=="Disability",Indicator=="% of adult population who were disabled")
setGraphOptions[["disability-1"]] <- list(graphType="barChart1",
                                       graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"disability-1"],
                                       digits=0)

setGraphData[["disability-2"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Disability",Indicator=="% of adult population who were disabled") 
setGraphOptions[["disability-2"]] <- list(graphType="timeSeries1",
                                       graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"disability-2"],
                                       digits=0)
setGraphData[["Demog-46"]] <- setGraphData[["disability-2"]]
setGraphOptions[["Demog-46"]] <- setGraphOptions[["disability-2"]]

setGraphData[["disability-13"]] <- filter(EEFlatest,policy_area=="Summary",Characteristic=="Disability",Indicator=="% of children who were disabled") 
setGraphOptions[["disability-13"]] <- list(graphType="barChart1",
                                        graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"disability-13"],
                                        digits=0)
setGraphData[["chifam-13"]] <- setGraphData[["disability-13"]]
setGraphOptions[["chifam-13"]] <- setGraphOptions[["disability-13"]]

setGraphData[["disability-14"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Disability",Indicator=="% of children who were disabled") 
setGraphOptions[["disability-14"]] <- list(graphType="timeSeries1",
                                        graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"disability-14"],
                                        digits=0)
setGraphData[["chifam-14"]] <- setGraphData[["disability-14"]]
setGraphOptions[["chifam-14"]] <- setGraphOptions[["disability-14"]]

setGraphData[["ethnicity-1"]] <- filter(EEFlatest,policy_area=="Summary",Characteristic=="Ethnicity",Indicator=="% of Adult Population") 
setGraphOptions[["ethnicity-1"]] <- list(graphType="pieChart0",
                                      colourPalette="full",
                                      digits=1)

setGraphData[["ethnicity-2"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Ethnicity",Indicator=="% of Population")
setGraphOptions[["ethnicity-2"]] <- list(graphType="barChart0",
                                      colourPalette="full",
                                      gvisOptions=list(isStacked="percent"))

setGraphData[["religion-1"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Religion",Indicator=="% of Adult Population") 
setGraphOptions[["religion-1"]] <- list(graphType="pieChart0",
                                     colourPalette="full",
                                     graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"religion-1"],
                                     digits=1)
setGraphData[["Demog-18"]] <- setGraphData[["religion-1"]]
setGraphOptions[["Demog-18"]] <- setGraphOptions[["religion-1"]]

setGraphData[["sexualOrientation-1"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Sexual Orientation",Indicator=="Overview") 
setGraphOptions[["sexualOrientation-1"]] <- list(graphType="pieChart0",
                                              colourPalette="full",
                                              graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"sexualOrientation-1"],
                                              digits=1)
setGraphData[["Demog-11"]] <- setGraphData[["sexualOrientation-1"]]
setGraphOptions[["Demog-11"]] <- setGraphOptions[["sexualOrientation-1"]]


setGraphData[["sexualOrientation-2"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Sexual Orientation",Indicator=="% of Adult Population")
setGraphOptions[["sexualOrientation-2"]] <- list(graphType="barChart1",
                                              digits=1,
                                              gvisOptions=list(isStacked=TRUE),
                                              colourPalette="full")
setGraphData[["Demog-12"]] <- setGraphData[["sexualOrientation-2"]]
setGraphOptions[["Demog-12"]] <- setGraphOptions[["sexualOrientation-2"]]
setGraphData[["Demog-41"]] <- setGraphData[["sexualOrientation-2"]]
setGraphOptions[["Demog-41"]] <- setGraphOptions[["sexualOrientation-2"]]


####Business, Enterprise and Tourism####

setGraphData[["busEnt-1"]] <- filter(NPFdata,Indicator=="Entrepreneurial activity",Characteristic=="Age",Year>=2010) %>%
  mutate(Measure=Indicator) 
setGraphOptions[["busEnt-1"]] <- list(graphType="timeSeries1",
                                   ylabel="% of working-age population",
                                   NPFindicator="Entrepreneurial activity",
                                   digits=1)
setGraphData[["age-91"]] <- setGraphData[["busEnt-11"]]
setGraphOptions[["age-91"]] <- setGraphOptions[["busEnt-11"]]


setGraphData[["busEnt-3"]] <- filter(ODPdata,Indicator=="Self Employment",Characteristic=="Age")
setGraphOptions[["busEnt-3"]] <- list(graphType="timeSeries1",
                                   query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                   SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
                                   WHERE {
                                   ?obs 
                                   <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
                                   <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
                                   <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
                                   
                                   <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/self-employment> ;
                                   <http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all>;
                                   <http://purl.org/linked-data/cube#dataSet> ?i ;
                                   <http://purl.org/linked-data/cube#dataSet> ?m ;
                                   <http://statistics.gov.scot/def/dimension/age> ?b ;
                                   <http://statistics.gov.scot/def/measure-properties/ratio> ?Figure.
                                   
                                   <http://statistics.gov.scot/data/self-employment> 
                                   <http://purl.org/dc/terms/modified> ?LastUpdated ;
                                   <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
                                   
                                   ?Interval rdfs:label ?DateCode.
                                   ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
                                   ?d rdfs:label ?Date.
                                   ?i rdfs:label ?Indicator.
                                   ?m rdfs:label ?Measure.
                                   ?b rdfs:label ?Breakdown.
                                   ?l rdfs:label ?yLabel.
                                   }",
                                   digits=1)

# setGraphData[["busEnt-3"]] <- filter(EEFlatest,Indicator=="Self-employment",Characteristic=="Age")
# setGraphOptions[["busEnt-3"]] <- list(graphType="barChart3",
#                                    ylabel="% of working-age population",
#                                    digits=1)


setGraphData[["busEnt-5"]] <- filter(EEFlatest,Indicator=="Self-employment",Characteristic=="Disability")
setGraphOptions[["busEnt-5"]] <- list(graphType="barChart3",
                                   ylabel="% of working-age population",
                                   digits=1)

setGraphData[["busEnt-7"]] <- filter(EEFlatest,Indicator=="Self-employment",Characteristic=="Ethnicity")
setGraphOptions[["busEnt-7"]] <- list(graphType="barChart3",
                                      ylabel="% of working-age population",
                                      digits=1)

setGraphData[["busEnt-8"]] <- filter(EEFdata,Indicator=="SME Employers",Characteristic=="Ethnicity")
setGraphOptions[["busEnt-8"]] <- list(graphType="timeSeries1",
                                      ylabel="Percentage of SME Employers",
                                      digits=1)

setGraphData[["busEnt-9"]] <- filter(ODPdata,Indicator=="Self Employment",Characteristic=="Gender")
setGraphOptions[["busEnt-9"]] <- list(graphType="timeSeries1",
                                   query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                   SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
                                   WHERE {
                                   ?obs 
                                   <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
                                   <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
                                   <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
                                   
                                   <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/self-employment> ;
                                   <http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/16-and-over>;
                                   <http://purl.org/linked-data/cube#dataSet> ?i ;
                                   <http://purl.org/linked-data/cube#dataSet> ?m ;
                                   <http://statistics.gov.scot/def/dimension/gender> ?b ;
                                   <http://statistics.gov.scot/def/measure-properties/ratio> ?Figure.
                                   
                                   <http://statistics.gov.scot/data/self-employment> 
                                   <http://purl.org/dc/terms/modified> ?LastUpdated ;
                                   <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
                                   
                                   ?Interval rdfs:label ?DateCode.
                                   ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
                                   ?d rdfs:label ?Date.
                                   ?i rdfs:label ?Indicator.
                                   ?m rdfs:label ?Measure.
                                   ?b rdfs:label ?Breakdown.
                                   ?l rdfs:label ?yLabel.
                                   }",
                                   digits=1)
# setGraphData[["busEnt-9"]] <- filter(EEFdata,Indicator=="Self-employment",Characteristic=="Gender")
# setGraphOptions[["busEnt-9"]] <- list(graphType="timeSeries1",
#                                    ylabel="% of working-age population",
#                                    digits=1)


setGraphData[["busEnt-10"]] <- filter(EEFdata,Indicator=="SME Employers",Characteristic=="Gender")
setGraphOptions[["busEnt-10"]] <- list(graphType="timeSeries1",
                                      ylabel="Percentage of SME Employers",
                                      digits=1)

setGraphData[["busEnt-11"]] <- filter(NPFdata,Indicator=="Entrepreneurial activity",Characteristic=="Gender") %>%
  mutate(Measure=Indicator) 
setGraphOptions[["busEnt-11"]] <- list(graphType="timeSeries1",
                                    ylabel="% of working-age population",
                                    NPFindicator="Entrepreneurial activity",
                                    digits=1)
setGraphData[["gender-92"]] <- setGraphData[["busEnt-1"]]
setGraphOptions[["gender-92"]] <- setGraphOptions[["busEnt-1"]]

setGraphData[["busEnt-12"]] <- filter(NPFdata,Indicator=="Entrepreneurial activity",Characteristic=="Socio-Economic Status") %>%
  mutate(Measure=Indicator) 
setGraphOptions[["busEnt-12"]] <- list(graphType="timeSeries1",
                                       ylabel="% of working-age population",
                                       NPFindicator="Entrepreneurial activity",
                                       digits=1)


####Children and Families####

setGraphData[["chifam-1"]] <- filter(EEFdata,Measure=="Percentage of young people aged 17 and under looked after 31st July",Characteristic=="Age")
setGraphOptions[["chifam-1"]] <- list(graphType="timeSeries0",
                                   ylabel="% Young People",
                                   digits=1)

setGraphData[["chifam-2"]] <- filter(EEFdata,Measure=="Count of young people looked after, 31st July",Characteristic=="Age")
setGraphOptions[["chifam-2"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of Young People",
                                   digits=0)
setGraphData[["age-3"]] <- setGraphData[["chifam-2"]]
setGraphOptions[["age-3"]] <- setGraphOptions[["chifam-2"]]

setGraphData[["chifam-3"]] <- filter(EEFdata,Measure=="Count of young people on the child protection register, 31st July",Characteristic=="Age")
setGraphOptions[["chifam-3"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of Young People",
                                   digits=0)

setGraphData[["chifam-4"]] <- filter(EEFdata,Measure=="Count of Early Learning and Childcare registrations, September",Characteristic=="Age")
setGraphOptions[["chifam-4"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of Young People",
                                   digits=0)

setGraphData[["chifam-5"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="Percentage of young people looked after with a disability, 31st July")
setGraphOptions[["chifam-5"]] <- list(graphType="timeSeries0",
                                   digits=1)
setGraphData[["disability-3"]] <- setGraphData[["chifam-5"]]
setGraphOptions[["disability-3"]] <- setGraphOptions[["chifam-5"]]

setGraphData[["chifam-6"]] <- filter(EEFdata,Measure=="Percentage of young people looked after by ethnic group, 31st July",Characteristic=="Ethnicity")
setGraphOptions[["chifam-6"]] <- list(graphType="timeSeries1",
                                   ylabel="% Young People",
                                   digits=1)

setGraphData[["chifam-7"]] <- filter(EEFdata,Measure=="Percentage of young people on the child protection register by ethnic group, 31st July",Characteristic=="Ethnicity")
setGraphOptions[["chifam-7"]] <- list(graphType="timeSeries1",
                                   ylabel="% Young People",
                                   digits=1)

setGraphData[["chifam-8"]] <- filter(EEFdata,Measure=="Percentage of Early Learning and Childcare registrations with a home language other than English, September",Characteristic=="Ethnicity")
setGraphOptions[["chifam-8"]] <- list(graphType="timeSeries0",
                                   ylabel="% Young People",
                                   digits=1)
setGraphData[["ethnicity-3"]] <- setGraphData[["chifam-8"]]
setGraphOptions[["ethnicity-3"]] <- setGraphOptions[["chifam-8"]]

setGraphData[["chifam-9"]] <- filter(EEFdata,Measure=="Percentage of young people looked after by gender, 31st July",Characteristic=="Gender")
setGraphOptions[["chifam-9"]] <- list(graphType="timeSeries1",
                                   ylabel="% Young People",
                                   digits=1)
setGraphData[["gender-3"]] <- setGraphData[["chifam-9"]]
setGraphOptions[["gender-3"]] <- setGraphOptions[["chifam-9"]]

setGraphData[["chifam-10"]] <- filter(EEFdata,Measure=="Percentage of young people on the child protection register by gender, 31st July",Characteristic=="Gender")
setGraphOptions[["chifam-10"]] <- list(graphType="timeSeries1",
                                    ylabel="% Young People",
                                    digits=1)

setGraphData[["chifam-11"]] <- filter(EEFdata,Measure=="Percentage of young people looked after by religion, 31st July",Characteristic=="Religion")
setGraphOptions[["chifam-11"]] <- list(graphType="timeSeries1",
                                    ylabel="% Young People",
                                    digits=1)
setGraphData[["religion-3"]] <- setGraphData[["chifam-11"]]
setGraphOptions[["religion-3"]] <- setGraphOptions[["chifam-11"]]

setGraphData[["chifam-12"]] <- filter(EEFdata,Measure=="Percentage of young people on the child protection register by religion, 31st July",Characteristic=="Religion")
setGraphOptions[["chifam-12"]] <- list(graphType="timeSeries1",
                                    ylabel="% Young People",
                                    digits=1)

####Crime and Justice####

# setGraphData[["JAS1"]] <- filter(NPFdata,Characteristic=="Age",Indicator%in%c("Crime victimisation"))  %>%
#   mutate(Indicator=paste0(Year-1,"-",substr(Year,3,4)),
#          Measure=paste("Crime victimisation:",Indicator)) %>%
#   arrange(desc(Indicator))
# setGraphOptions[["JAS1"]] <- list(graphType="barChart2",
#                                    sortOrder="",
#                                    NPFindicator="Crime victimisation",
#                                    digits=1)

# setGraphData[["JAS15"]] <- filter(NPFdata,Characteristic=="Gender",Indicator%in%c("Crime victimisation"))  %>%
#   mutate(Measure=Indicator)
# setGraphOptions[["JAS15"]] <- list(graphType="timeSeries1",
#                                    NPFindicator="Crime victimisation",
#                                    digits=1)
# 
setGraphData[["JAS34"]] <- filter(NPFdata,Characteristic=="Socio-Economic Status",Indicator%in%c("Crime victimisation"))  %>%
  mutate(Measure=Indicator)
setGraphOptions[["JAS34"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Crime victimisation",
                                   digits=1)

setGraphData[["JAS39"]] <- filter(EEFdata,policy_area=="Crime and Justice",Measure=="Hate Crime Charges",Characteristic=="Disability")
setGraphOptions[["JAS39"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of charges reported",
                                   digits=0)

setGraphData[["JAS40"]] <- filter(EEFdata,policy_area=="Crime and Justice",Measure=="Hate Crime Charges",Characteristic=="Ethnicity")
setGraphOptions[["JAS40"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of charges reported",
                                   digits=0)

setGraphData[["JAS41"]] <- filter(EEFdata,policy_area=="Crime and Justice",Measure=="Hate Crime Charges",Characteristic=="Religion")
setGraphOptions[["JAS41"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of charges reported",
                                   digits=0)

setGraphData[["JAS42"]] <- filter(EEFdata,policy_area=="Crime and Justice",Measure=="Hate Crime Charges",Characteristic=="Sexual Orientation")
setGraphOptions[["JAS42"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of charges reported",
                                   digits=0)

setGraphData[["JAS43"]] <- filter(EEFdata,policy_area=="Crime and Justice",Measure=="Hate Crime Charges",Characteristic=="Transgender")
setGraphOptions[["JAS43"]] <- list(graphType="barChart1",
                                   ylabel="Number of charges reported",
                                   digits=0)


####Culture, Communities and Society####

setGraphData[["culCom-1"]] <- filter(NPFdata,Characteristic%in%c("Gender"),Indicator%in%c("Attendance at cultural events or places of culture")) %>%
  mutate(Measure=Indicator) %>%
  mutate(Breakdown=ifelse(Breakdown=="Male","Men",Breakdown),
         Breakdown=ifelse(Breakdown=="Female","Women",Breakdown)
  )
setGraphOptions[["culCom-1"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)

setGraphData[["culCom-2"]] <- filter(NPFdata,Characteristic%in%c("Gender"),Indicator%in%c("Participation in a cultural activity"))  %>%
  mutate(Measure=Indicator) %>%
  mutate(Breakdown=ifelse(Breakdown=="Male","Men",Breakdown),
         Breakdown=ifelse(Breakdown=="Female","Women",Breakdown)
  )
setGraphOptions[["culCom-2"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Participation in a cultural activity",
                                   digits=1)
setGraphOptions[["gender-5"]] <- setGraphOptions[["culCom-2"]]
setGraphData[["gender-5"]] <- setGraphData[["culCom-2"]]

setGraphData[["culCom-3"]] <- filter(NPFdata,Characteristic%in%c("Age"),Indicator%in%c("Attendance at cultural events or places of culture"))  %>%
  mutate(Measure=Indicator) 
setGraphOptions[["culCom-3"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)
setGraphData[["age-5"]] <- setGraphData[["culCom-3"]]
setGraphOptions[["age-5"]] <- setGraphOptions[["culCom-3"]]

setGraphData[["culCom-4"]] <- filter(NPFdata,Characteristic%in%c("Age"),Indicator%in%c("Participation in a cultural activity"))  %>%
  mutate(Measure=Indicator) 
setGraphOptions[["culCom-4"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Participation in a cultural activity",
                                   digits=1)

setGraphData[["culCom-5"]] <- filter(NPFdata,Characteristic%in%c("Socio-Economic Status"),Indicator%in%c("Attendance at cultural events or places of culture"))  %>%
  mutate(Measure=Indicator) 
setGraphOptions[["culCom-5"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)
setGraphData[["socioEconomic-51"]] <- setGraphData[["culCom-5"]]
setGraphOptions[["socioEconomic-51"]] <- setGraphOptions[["culCom-5"]]

setGraphData[["culCom-6"]] <- filter(NPFdata,Characteristic%in%c("Socio-Economic Status"),Indicator%in%c("Participation in a cultural activity"))  %>%
  mutate(Measure=Indicator) 
setGraphOptions[["culCom-6"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Participation in a cultural activity",
                                   digits=1)
setGraphData[["socioEconomic-52"]] <- setGraphData[["culCom-6"]]
setGraphOptions[["socioEconomic-52"]] <- setGraphOptions[["culCom-6"]]

setGraphData[["culCom-7"]] <- filter(NPFdata,Characteristic%in%c("Disability"),Indicator%in%c("Attendance at cultural events or places of culture"))  %>%
  mutate(Measure=Indicator) 
setGraphOptions[["culCom-7"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)

setGraphData[["disability-5"]] <- setGraphData[["culCom-7"]]
setGraphOptions[["disability-5"]] <- setGraphOptions[["culCom-7"]]

setGraphData[["culCom-8"]] <- filter(NPFdata,Characteristic%in%c("Disability"),Indicator%in%c("Participation in a cultural activity"))  %>%
  mutate(Measure=Indicator) 
setGraphOptions[["culCom-8"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Participation in a cultural activity",
                                   digits=1)

setGraphData[["culCom-9"]] <- filter(NPFdata,Characteristic%in%c("Religion"),Indicator%in%c("Attendance at cultural events or places of culture"))  %>%
  mutate(Measure=Indicator) 
setGraphOptions[["culCom-9"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)

setGraphData[["culCom-10"]] <- filter(NPFdata,Characteristic%in%c("Religion"),Indicator%in%c("Participation in a cultural activity"))  %>%
  mutate(Measure=Indicator) 
setGraphOptions[["culCom-10"]] <- list(graphType="timeSeries1",
                                    NPFindicator="Participation in a cultural activity",
                                    digits=1)
setGraphData[["religion-5"]] <- setGraphData[["culCom-10"]]
setGraphOptions[["religion-5"]] <- setGraphOptions[["culCom-10"]]

setGraphData[["culCom-11"]] <- filter(NPFdata,Characteristic%in%c("Ethnicity"),Indicator%in%c("Attendance at cultural events or places of culture")) %>% 
  mutate(Measure=Indicator) %>%
  mutate(Breakdown=ifelse(Breakdown=="Other Ethnic","Other",Breakdown))
setGraphOptions[["culCom-11"]] <- list(graphType="timeSeries1",
                                    NPFindicator="Attendance at cultural events or places of culture",
                                    digits=1)

setGraphData[["culCom-12"]] <- filter(NPFdata,Characteristic%in%c("Ethnicity"),Indicator%in%c("Participation in a cultural activity"))  %>%
  mutate(Measure=Indicator) %>%
  mutate(Breakdown=ifelse(Breakdown=="Other ethnic","Other Minority Ethnic",Breakdown))
setGraphOptions[["culCom-12"]] <- list(graphType="timeSeries1",
                                    NPFindicator="Participation in a cultural activity",
                                    digits=1)
setGraphData[["ethnicity-5"]] <- setGraphData[["culCom-12"]]
setGraphOptions[["ethnicity-5"]] <- setGraphOptions[["culCom-12"]]

setGraphData[["culCom-111"]] <- filter(EEFlatest,Characteristic=="Age",Indicator%in%c("Discrimination"))
setGraphOptions[["culCom-111"]] <- list(graphType="barChart3",
                                     sortOrder="",
                                     digits=1)

setGraphData[["culCom-112"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("Discrimination"))
setGraphOptions[["culCom-112"]] <- list(graphType="barChart3",
                                     digits=1)
setGraphData[["disability-4"]] <- setGraphData[["culCom-112"]]
setGraphOptions[["disability-4"]] <- setGraphOptions[["culCom-112"]]

setGraphData[["culCom-113"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("Discrimination"))
setGraphOptions[["culCom-113"]] <- list(graphType="barChart3",
                                     digits=1)
setGraphData[["ethnicity-4"]] <- setGraphData[["culCom-113"]]
setGraphOptions[["ethnicity-4"]] <- setGraphOptions[["culCom-113"]]

setGraphData[["culCom-114"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("Discrimination"))
setGraphOptions[["culCom-114"]] <- list(graphType="barChart3",
                                     digits=1)

setGraphData[["culCom-115"]] <- filter(EEFlatest,Characteristic=="Religion",Indicator%in%c("Discrimination"))
setGraphOptions[["culCom-115"]] <- list(graphType="barChart3",
                                     digits=1)
setGraphData[["religion-4"]] <- setGraphData[["culCom-115"]]
setGraphOptions[["religion-4"]] <- setGraphOptions[["culCom-115"]]

setGraphData[["culCom-116"]] <- filter(EEFlatest,Characteristic=="Sexual Orientation",Indicator%in%c("Discrimination"))
setGraphOptions[["culCom-116"]] <- list(graphType="barChart3",
                                     digits=1)
setGraphData[["sexualOrientation-4"]] <- setGraphData[["culCom-116"]]
setGraphOptions[["sexualOrientation-4"]] <- setGraphOptions[["culCom-116"]]

setGraphData[["culCom-117"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("Discrimination"))
setGraphOptions[["culCom-117"]] <- list(graphType="barChart3",
                                     digits=1)

setGraphData[["culCom-121"]] <- filter(EEFlatest,Characteristic=="Age",Indicator%in%c("Strength of feeling of belonging to community"))
setGraphOptions[["culCom-121"]] <- list(graphType="barChart1",
                                     sortOrder="",
                                     digits=1,
                                     gvisOptions=list(isStacked=TRUE))
setGraphData[["age-4"]] <- setGraphData[["culCom-121"]]
setGraphOptions[["age-4"]] <- setGraphOptions[["culCom-121"]]

setGraphData[["culCom-123"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("Strength of feeling of belonging to community"))
setGraphOptions[["culCom-123"]] <- list(graphType="barChart1",
                                     digits=1,
                                     gvisOptions=list(isStacked=TRUE))

setGraphData[["culCom-124"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("Strength of feeling of belonging to community"))
setGraphOptions[["culCom-124"]] <- list(graphType="barChart1",
                                     digits=1,
                                     gvisOptions=list(isStacked=TRUE))
setGraphData[["gender-4"]] <- setGraphData[["culCom-124"]]
setGraphOptions[["gender-4"]] <- setGraphOptions[["culCom-124"]]

setGraphData[["culCom-127"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("Strength of feeling of belonging to community"))
setGraphOptions[["culCom-127"]] <- list(graphType="barChart1",
                                     digits=1,
                                     gvisOptions=list(isStacked=TRUE))
setGraphData[["socioEconomic-4"]] <- setGraphData[["culCom-127"]]
setGraphOptions[["socioEconomic-4"]] <- setGraphOptions[["culCom-127"]]

setGraphData[["culCom-201"]] <- filter(EEFdata,Indicator=="Social attitudes",Characteristic=="Transgender")
setGraphOptions[["culCom-201"]] <- list(graphType="barChart0",
                                     colourPalette="blue",
                                     sortOrder="",
                                     gvisOptions=list(chartArea="{left: 350}"))
setGraphData[["transgender-4"]] <- setGraphData[["culCom-201"]]
setGraphOptions[["transgender-4"]] <- setGraphOptions[["culCom-201"]]

####Advanced Learning and Skills####

setGraphData[["empSLL-1"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% Higher education entrants",Characteristic=="Gender")
setGraphOptions[["empSLL-1"]] <- list(graphType="pieChart0",
                                   digits=1)
setGraphData[["gender-73"]] <- setGraphData[["empSLL-1"]] 
setGraphOptions[["gender-73"]] <- setGraphOptions[["empSLL-1"]] 

setGraphData[["empSLL-2"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Entrants - All levels","Entrants - Postgraduate","Entrants - First Degree","Entrants - Sub-Degrees"),Characteristic=="Age")
setGraphOptions[["empSLL-2"]] <- list(graphType="barChart2",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)
setGraphData[["age-73"]] <- setGraphData[["empSLL-2"]]
setGraphOptions[["age-73"]] <- setGraphOptions[["empSLL-2"]]

setGraphData[["empSLL-3"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% of entrants to Scottish HEIs from a minority ethnic background",Characteristic=="Ethnicity")
setGraphOptions[["empSLL-3"]] <- list(graphType="barChart1",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)
setGraphData[["ethnicity-73"]] <- setGraphData[["empSLL-3"]]
setGraphOptions[["ethnicity-73"]] <- setGraphOptions[["empSLL-3"]]

setGraphData[["empSLL-4"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% Higher education entrants",Characteristic=="Disability")
setGraphOptions[["empSLL-4"]] <- list(graphType="timeSeries0",
                                   intervalType="Academic",
                                   digits=0)
setGraphData[["disability-73"]] <- setGraphData[["empSLL-4"]]
setGraphOptions[["disability-73"]] <- setGraphOptions[["empSLL-4"]]

setGraphData[["empSLL-5"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% Higher education entrants",Characteristic=="Socio-Economic Status")
setGraphOptions[["empSLL-5"]] <- list(graphType="barChart1",
                                   isStacked=FALSE,
                                   digits=1,
                                   sortOrder=NA)
setGraphData[["socioEconomic-73"]] <- setGraphData[["empSLL-5"]]
setGraphOptions[["socioEconomic-73"]] <- setGraphOptions[["empSLL-5"]]

setGraphData[["empSLL-6"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Qualifiers - All levels","Qualifiers - Postgraduate","Qualifiers - First Degree","Qualifiers - Sub-Degrees"),Characteristic=="Age")
setGraphOptions[["empSLL-6"]] <- list(graphType="barChart2",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)

setGraphData[["empSLL-7"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Number of qualifiers"),Characteristic=="Ethnicity")
setGraphOptions[["empSLL-7"]] <- list(graphType="barChart1",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)

setGraphData[["empSLL-8"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Qualifiers - All","Qualifiers - Part-time","Qualifiers - Full-time"),Characteristic=="Gender")
setGraphOptions[["empSLL-8"]] <- list(graphType="barChart2",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)

setGraphData[["empSLL-9"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% qualifiers",Characteristic=="Socio-Economic Status")
setGraphOptions[["empSLL-9"]] <- list(graphType="barChart1",
                                   isStacked=FALSE,
                                   digits=1,
                                   sortOrder=NA)

setGraphData[["empSLL-10"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="Number of full-time further education entrants",Characteristic=="Age")
setGraphOptions[["empSLL-10"]] <- list(graphType="barChart1",
                                    gvisOptions=list(isStacked=TRUE),
                                    digits=1,
                                    sortOrder=NA)

setGraphData[["empSLL-11"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% further education entrants",Characteristic=="Disability")
setGraphOptions[["empSLL-11"]] <- list(graphType="barChart1",
                                    graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"empSLL-11"],
                                    isStacked=FALSE,
                                    digits=1,
                                    sortOrder=NA)

setGraphData[["empSLL-12"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% further education entrants",Characteristic=="Ethnicity")
setGraphOptions[["empSLL-12"]] <- list(graphType="barChart1",
                                    isStacked=FALSE,
                                    digits=1,
                                    sortOrder=NA)

setGraphData[["empSLL-13"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",grepl("% .*further education entrants",Indicator),Characteristic=="Gender")
setGraphOptions[["empSLL-13"]] <- list(graphType="barChart2",
                                    isStacked=FALSE,
                                    digits=1,
                                    sortOrder=NA)

setGraphData[["empSLL-14"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",grepl("% .*further education entrants",Indicator),Characteristic=="Socio-Economic Status")
setGraphOptions[["empSLL-14"]] <- list(graphType="barChart3",
                                    isStacked=FALSE,
                                    digits=1,
                                    sortOrder=NA)

setGraphData[["empSLL-15"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="Number of Modern Apprenticeships",Characteristic=="Age")
setGraphOptions[["empSLL-15"]] <- list(graphType="barChart1",
                                    graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"empSLL-15"],
                                    gvisOptions=list(isStacked=TRUE),
                                    digits=0,
                                    sortOrder=NA)

setGraphData[["empSLL-16"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% of Modern Apprenticeships identifying a disability",Characteristic=="Disability") 
setGraphOptions[["empSLL-16"]] <- list(graphType="timeSeries0",
                                    intervalType="Academic",
                                    digits=1,
                                    ylabel="% of Modern Apprentices")

setGraphData[["empSLL-17"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="% of Modern Apprenticeships identifying as a minority ethnic group",Characteristic=="Ethnicity") 
setGraphOptions[["empSLL-17"]] <- list(graphType="timeSeries0",
                                    intervalType="Academic",
                                    digits=1,
                                    ylabel="% of Modern Apprentices")

setGraphData[["empSLL-18"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="Number of Modern Apprenticeships",Characteristic=="Gender") 
setGraphOptions[["empSLL-18"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=0)

setGraphData[["empSLL-19"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participation Rate"),Characteristic=="Ethnicity")
setGraphOptions[["empSLL-19"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)
setGraphData[["ethnicity-72"]] <- setGraphData[["empSLL-19"]]
setGraphOptions[["ethnicity-72"]] <- setGraphOptions[["empSLL-19"]]

setGraphData[["empSLL-20"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Ethnicity")
setGraphOptions[["empSLL-20"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

setGraphData[["empSLL-21"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participation Rate"),Characteristic=="Gender")
setGraphOptions[["empSLL-21"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)
setGraphData[["gender-72"]] <- setGraphData[["empSLL-21"]]
setGraphOptions[["gender-72"]] <- setGraphOptions[["empSLL-21"]]

setGraphData[["empSLL-22"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Gender")
setGraphOptions[["empSLL-22"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

setGraphData[["empSLL-23"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participation Rate"),Characteristic=="Socio-Economic Status")
setGraphOptions[["empSLL-23"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)
setGraphData[["socioEconomic-11"]] <- setGraphData[["empSLL-23"]]
setGraphOptions[["socioEconomic-11"]] <- setGraphOptions[["empSLL-23"]]
setGraphData[["socioEconomic-72"]] <- setGraphData[["empSLL-23"]]
setGraphOptions[["socioEconomic-72"]] <- setGraphOptions[["empSLL-23"]]

setGraphData[["empSLL-24"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Socio-Economic Status")
setGraphOptions[["empSLL-24"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

setGraphData[["empSLL-25"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participation Rate"),Characteristic=="Age")
setGraphOptions[["empSLL-25"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)
setGraphData[["age-72"]] <- setGraphData[["empSLL-25"]]
setGraphOptions[["age-72"]] <- setGraphOptions[["empSLL-25"]]

setGraphData[["empSLL-26"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Age")
setGraphOptions[["empSLL-26"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

setGraphData[["empSLL-27"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator=="Number of Modern Apprenticeships",Characteristic=="Socio-Economic Status")
setGraphOptions[["empSLL-27"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=0)

setGraphData[["empSLL-28"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participation Rate"),Characteristic=="Disability")
setGraphOptions[["empSLL-28"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)

setGraphData[["empSLL-29"]] <- filter(EEFdata,policy_area=="Advanced Learning and Skills",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Disability")
setGraphOptions[["empSLL-29"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

####Health, Social Care and Sport####

# setGraphData[["health-9"]] <- filter(ODPdata,policy_area=="Health, Social Care and Sport",Indicator=="Smoking - Scottish Survey Core Questions")
# setGraphOptions[["health-9"]] <- list(graphType="timeSeries1",
#                                    digits=1,
#                                    intervalType="Year",
#                                    query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#                                            SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
#                                            WHERE {
#                                            ?obs 
#                                            <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ;
#                                            <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
#                                            <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
#                                            
#                                            <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/smoking-sscq> ;
#                                            <http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/all>;
#                                            <http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all>;
#                                            <http://statistics.gov.scot/def/dimension/householdType> <http://statistics.gov.scot/def/concept/household-type/all>;
#                                            <http://statistics.gov.scot/def/dimension/typeOfTenure> <http://statistics.gov.scot/def/concept/type-of-tenure/all>;
#                                            <http://purl.org/linked-data/cube#dataSet>  ?i ;
#                                            <http://statistics.gov.scot/def/dimension/currentlySmokesCigarettes> ?m ;
#                                            <http://statistics.gov.scot/def/dimension/limitingLong-termPhysicalOrMentalHealthCondition> ?b ;
#                                            <http://statistics.gov.scot/def/measure-properties/percent> ?Figure.
#                                            
#                                            <http://statistics.gov.scot/data/smoking-sscq> 
#                                            <http://purl.org/dc/terms/modified> ?LastUpdated ;
#                                            <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
#                                            
#                                            ?Interval rdfs:label ?DateCode.
#                                            ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
#                                            ?d rdfs:label ?Date.
#                                            ?i rdfs:label ?Indicator.
#                                            ?m rdfs:label ?Measure.
#                                            ?b rdfs:label ?Breakdown.
#                                            ?l rdfs:label ?yLabel.
#                                            }")
#   
# setGraphData[["health-90"]] <- filter(ODPdata,policy_area=="Health, Social Care and Sport",Indicator=="Mental Wellbeing - Scottish Surveys Core Questions")
# setGraphOptions[["health-90"]] <- list(graphType="timeSeries0",
#                                    digits=1,
#                                    intervalType="Year",
#                                    query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#                                          SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
#                                          WHERE {
#                                          ?obs 
#                                          <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; 
#                                          <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
#                                          <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
#                                          
#                                          <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/mental-wellbeing-sscq> ;
#                                          <http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/all>;
#                                          <http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all>;                              
#                                          <http://statistics.gov.scot/def/dimension/householdType> <http://statistics.gov.scot/def/concept/household-type/all>;
#                                          <http://statistics.gov.scot/def/dimension/typeOfTenure> <http://statistics.gov.scot/def/concept/type-of-tenure/all>;
#                                          <http://purl.org/linked-data/cube#dataSet> ?i ;
#                                          <http://purl.org/linked-data/cube#dataSet> ?m ;
#                                          <http://statistics.gov.scot/def/dimension/limitingLong-termPhysicalOrMentalHealthCondition> ?b ;
#                                          <http://statistics.gov.scot/def/measure-properties/mean> ?Figure.
#                                          
#                                          <http://statistics.gov.scot/data/mental-wellbeing-sscq> 
#                                          <http://purl.org/dc/terms/modified> ?LastUpdated ;
#                                          <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
#                                          
#                                          ?Interval rdfs:label ?DateCode.
#                                          ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
#                                          ?d rdfs:label ?Date.
#                                          ?i rdfs:label ?Indicator.
#                                          ?m rdfs:label ?Measure.
#                                          ?b rdfs:label ?Breakdown.
#                                          ?l rdfs:label ?yLabel.
#                                          }")

# setGraphData[["health-6a"]] <- filter(NPFdata,Indicator=="Physical activity",Characteristic=="Age")
# setGraphOptions[["health-6a"]] <- list(graphType="timeSeries1",
#                                     NPFindicator="Physical Activity",
#                                     digits=1)
# setGraphData[["health-7"]] <- filter(ODPdata,policy_area=="Health, Social Care and Sport",Indicator%in%"Care Homes: Demographic Characteristics of Residents",Characteristic=="Age")
# setGraphOptions[["health-7"]] <- list(graphType="timeSeries2",
#                                    query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#                                    SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
#                                    WHERE {
#                                    ?obs 
#                                    <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
#                                    <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
#                                    <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
#                                    
#                                    <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/care-homes-demography> ;
#                                    <http://statistics.gov.scot/def/dimension/sex> <http://statistics.gov.scot/def/concept/sex/all> ;
#                                    <http://purl.org/linked-data/cube#dataSet> ?i ;
#                                    <http://statistics.gov.scot/def/dimension/mainClientGroupInCareHome> ?m ;
#                                    <http://statistics.gov.scot/def/dimension/age> ?b ;
#                                    <http://statistics.gov.scot/def/measure-properties/percent> ?Figure.
#                                    
#                                    <http://statistics.gov.scot/data/care-homes-demography> 
#                                    <http://purl.org/dc/terms/modified> ?LastUpdated ;
#                                    <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
#                                    
#                                    ?Interval rdfs:label ?DateCode.
#                                    ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
#                                    ?d rdfs:label ?Date.
#                                    ?i rdfs:label ?Indicator.
#                                    ?m rdfs:label ?Measure.
#                                    ?b rdfs:label ?Breakdown.
#                                    ?l rdfs:label ?yLabel.
#                                    }")
setGraphData[["health-7"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of long stay care home residents aged 65+",Characteristic=="Age")
setGraphOptions[["health-7"]] <- list(graphType="pieChart0")

setGraphData[["health-14"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of home care clients with a physical disability",Characteristic=="Disability")
setGraphOptions[["health-14"]] <- list(graphType="pieChart0")

setGraphData[["health-21"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%c("% of direct payment clients","% of home care clients"),Characteristic=="Ethnicity")
setGraphOptions[["health-21"]] <- list(graphType="barChart3",maxZoom=c(0,100))

# setGraphData[["health-27a"]] <- filter(NPFdata,Indicator=="Physical activity",Characteristic=="Gender")
# setGraphOptions[["health-27a"]] <- list(graphType="timeSeries1",
#                                      NPFindicator="Physical Activity",
#                                      graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"health-6a"],
#                                      digits=1)

# setGraphData[["health-28"]] <- filter(ODPdata,policy_area=="Health, Social Care and Sport",Indicator%in%"% of long stay care home residents",Characteristic=="Gender")
# setGraphOptions[["health-28"]] <- list(graphType="timeSeries2",maxZoom=c(0,100),
#                                     query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#                                     SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
#                                     WHERE {
#                                     ?obs 
#                                     <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
#                                     <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
#                                     <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
#                                     
#                                     <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/care-homes-demography> ;
#                                     <http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/18-and-over> ;
#                                     <http://purl.org/linked-data/cube#dataSet> ?i ;
#                                     <http://statistics.gov.scot/def/dimension/mainClientGroupInCareHome> ?m ;
#                                     <http://statistics.gov.scot/def/dimension/sex> ?b ;
#                                     <http://statistics.gov.scot/def/measure-properties/percent> ?Figure.
#                                     
#                                     <http://statistics.gov.scot/data/care-homes-demography> 
#                                     <http://purl.org/dc/terms/modified> ?LastUpdated ;
#                                     <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
#                                     
#                                     ?Interval rdfs:label ?DateCode.
#                                     ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
#                                     ?d rdfs:label ?Date.
#                                     ?i rdfs:label ?Indicator.
#                                     ?m rdfs:label ?Measure.
#                                     ?b rdfs:label ?Breakdown.
#                                     ?l rdfs:label ?yLabel.
#                                     }")
setGraphData[["health-28"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of long stay care home residents",Characteristic=="Gender")
setGraphOptions[["health-28"]] <- list(graphType="barChart1",maxZoom=c(0,100))



setGraphData[["health-60"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of social care at home clients aged 65+",Characteristic=="Age")
setGraphOptions[["health-60"]] <- list(graphType="pieChart0")

setGraphData[["health-66"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of social care at home clients",Characteristic=="Gender")
setGraphOptions[["health-66"]] <- list(graphType="barChart1",maxZoom=c(0,100))

####Income and Poverty####

setGraphData[["incPov-1"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="Relative poverty: single working-age adult")
setGraphOptions[["incPov-1"]] <- list(graphType="timeSeries2",
                                   intervalType="3 year",
                                   NPFindicator="Relative poverty after housing costs")
setGraphData[["gender-121"]] <- setGraphData[["incPov-1"]]
setGraphOptions[["gender-121"]] <- setGraphOptions[["incPov-1"]]
setGraphData[["gender-112"]] <- setGraphData[["incPov-1"]]
setGraphOptions[["gender-112"]] <- setGraphOptions[["incPov-1"]]

setGraphData[["incPov-3"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="Relative poverty: single pensioners")
setGraphOptions[["incPov-3"]] <- list(graphType="timeSeries2",
                                   intervalType="3 year",
                                   #xFormat="(year-3)+'-'+year.toString().substr(2,2);",
                                   NPFindicator="Relative poverty after housing costs")
setGraphData[["gender-123"]] <- setGraphData[["incPov-3"]]
setGraphOptions[["gender-123"]] <- setGraphOptions[["incPov-3"]]

setGraphData[["incPov-4"]] <- filter(EEFlatest,Characteristic=="Ethnicity",SubBreakdown%in%NA,Indicator%in%c("% in relative poverty AHC","% in relative poverty BHC"))
setGraphOptions[["incPov-4"]] <- list(graphType="barChart2",
                                   NPFindicator="Relative poverty after housing costs")
setGraphData[["ethnicity-121"]] <- setGraphData[["incPov-4"]]
setGraphOptions[["ethnicity-121"]] <- setGraphOptions[["incPov-4"]]

setGraphData[["incPov-4a"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("% in relative poverty after housing costs 2007-17","% in relative poverty before housing costs 2007-17"))
setGraphOptions[["incPov-4a"]] <- list(graphType="barChart2",
                                    NPFindicator="Relative poverty after housing costs",
                                    gvisOptions=list(chartArea="{left: '35%', width: '100%', bottom:50}"))

setGraphData[["incPov-5"]] <- filter(ODPdata,Characteristic=="Age",Indicator=="Poverty")
setGraphOptions[["incPov-5"]] <- list(graphType="timeSeries2",
                                   updateRmd=FALSE,
                                   query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                   SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
                                   WHERE {
                                   ?obs 
                                   <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ;
                                   <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/poverty> ;
                                   <http://purl.org/linked-data/cube#dataSet> ?i ;
                                   <http://statistics.gov.scot/def/dimension/housingCosts> ?m ;
                                   <http://statistics.gov.scot/def/dimension/populationGroup> ?b ;
                                   <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
                                   <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
                                   <http://statistics.gov.scot/def/measure-properties/ratio> ?Figure.
                                   
                                   <http://statistics.gov.scot/data/poverty> 
                                       <http://purl.org/dc/terms/modified> ?LastUpdated ;
                                       <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
                                   ?Interval rdfs:label ?DateCode.
                                   ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
                                   ?d rdfs:label ?Date.
                                   ?i rdfs:label ?Indicator.
                                   ?m rdfs:label ?Measure.
                                   ?b rdfs:label ?Breakdown.
                                   ?l rdfs:label ?yLabel.
                                   }",
                                   NPFindicator="Relative poverty after housing costs")
setGraphData[["age-12"]] <- setGraphData[["incPov-5"]]
setGraphOptions[["age-12"]] <- setGraphOptions[["incPov-5"]]

setGraphData[["incPov-6"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="Relative poverty")
setGraphOptions[["incPov-6"]] <- list(graphType="timeSeries2",
                                   intervalType="3 year",
                                   #xFormat="(year-3)+'-'+year.toString().substr(2,2);",
                                   NPFindicator="Relative poverty after housing costs")
setGraphData[["disability-12"]] <- setGraphData[["incPov-6"]]
setGraphOptions[["disability-12"]] <- setGraphOptions[["incPov-6"]]

setGraphData[["incPov-7"]] <- filter(EEFdata,Characteristic=="Gender",Indicator%in%c("Gender Pay Gap"))
setGraphOptions[["incPov-7"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Pay gap",
                                   digits=1,
                                   dyEvent=list(list(x=as.Date("2004-01-01"),label="Break in time series",labelLoc="bottom"),
                                                list(x=as.Date("2006-01-01")),
                                                list(x=as.Date("2011-01-01"))),
                                   axisMonth=0,
                                   intervalType="Year")


setGraphData[["incPov-7"]] <- filter(ODPdata,Indicator=="Gender Pay Gap",Characteristic=="Gender")
setGraphOptions[["incPov-7"]] <- list(graphType="timeSeries1",
                                      filterSparql=TRUE,
                                      updateQuery=TRUE,
                                      dyEvent=list(list(x=as.Date("2004-01-01"),label="Break in time series",labelLoc="bottom"),
                                                   list(x=as.Date("2006-01-01")),
                                                   list(x=as.Date("2011-01-01"))),
                                      query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                      SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
                                      WHERE {
                                      ?obs 
                                      <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
                                      <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
                                      <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
                                      
                                      <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/earnings-paygap> ;
                                      <http://purl.org/linked-data/cube#dataSet> ?i ;
                                      <http://purl.org/linked-data/cube#dataSet> ?m ;
                                      <http://statistics.gov.scot/def/dimension/workingPattern> ?b ;
                                      <http://statistics.gov.scot/def/measure-properties/ratio> ?Figure.
                                      
                                      <http://statistics.gov.scot/data/earnings-paygap> 
                                      <http://purl.org/dc/terms/modified> ?LastUpdated ;
                                      <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
                                      
                                      ?Interval rdfs:label ?DateCode.
                                      ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
                                      ?d rdfs:label ?Date.
                                      ?i rdfs:label ?Indicator.
                                      ?m rdfs:label ?Measure.
                                      ?b rdfs:label ?Breakdown.
                                      ?l rdfs:label ?yLabel.
                                      }")



setGraphData[["gender-91"]] <- setGraphData[["incPov-7"]]
setGraphOptions[["gender-91"]] <- setGraphOptions[["incPov-7"]]
setGraphData[["labSoc-10"]] <- setGraphData[["incPov-7"]]
setGraphOptions[["labSoc-10"]] <- setGraphOptions[["incPov-7"]]

setGraphData[["incPov-11"]] <- filter(NPFdata,Characteristic=="Age",Indicator%in%c("Employees on the Living wage")) %>%
  mutate(Measure=Indicator)
setGraphOptions[["incPov-11"]] <- list(graphType="timeSeries1",
                                       updateNPF=TRUE,
                                       NPFindicator="Employees on the Living Wage",
                                       ylabel="% of Employees")
setGraphData[["age-92"]] <- setGraphData[["incPov-11"]]
setGraphOptions[["age-92"]] <- setGraphOptions[["incPov-11"]]
setGraphData[["labSoc-12"]] <- setGraphData[["incPov-11"]]
setGraphOptions[["labSoc-12"]] <- setGraphOptions[["incPov-11"]]

setGraphData[["incPov-12"]] <- filter(EEFdata,Characteristic=="Age",Indicator%in%c("Managing well financially"))
setGraphOptions[["incPov-12"]] <- list(graphType="timeSeries1",
                                    digits=0)

setGraphData[["incPov-13"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="Relative poverty excluding disability living costs")
setGraphOptions[["incPov-13"]] <- list(graphType="timeSeries2",
                                    intervalType="4 year")
#xFormat="(year-3)+'-'+year.toString().substr(2,2);")
setGraphData[["disability-122"]] <- setGraphData[["incPov-13"]]
setGraphOptions[["disability-122"]] <- setGraphOptions[["incPov-13"]]

setGraphData[["incPov-17"]] <- filter(EEFdata,policy_area=="Income and Poverty",Indicator=="Asylum seekers",Characteristic=="Ethnicity")
setGraphOptions[["incPov-17"]] <- list(graphType="timeSeries0",
                                    digits=0,
                                    ylabel="Asylum Seekers")

setGraphData[["incPov-18"]] <- filter(EEFlatest,Characteristic=="Religion",Indicator%in%c("% in relative poverty AHC","% in relative poverty BHC"))
setGraphOptions[["incPov-18"]] <- list(graphType="barChart2"
)
setGraphData[["religion-121"]] <- setGraphData[["incPov-18"]]
setGraphOptions[["religion-121"]] <- setGraphOptions[["incPov-18"]]

setGraphData[["incPov-22"]] <- filter(EEFdata,Characteristic=="Gender",Indicator%in%c("Managing well financially"))
setGraphOptions[["incPov-22"]] <- list(graphType="timeSeries1",
                                    digits=0)

setGraphData[["incPov-23"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator%in%c("Managing well financially"))
setGraphOptions[["incPov-23"]] <- list(graphType="timeSeries1",
                                       #dyEvent=list(list(date=as.Date("2008-01-01"),label="2008 Financial Crisis")),
                                       digits=0
                                    )

####Labour Market and Social Security####

setGraphData[["labSoc-1"]] <- filter(ODPdata,Indicator=="Employment",Characteristic=="Age")
setGraphOptions[["labSoc-1"]] <- list(graphType="timeSeries2",
                                      filterSparql=TRUE,
                                      rescale=TRUE,
                                      query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                      SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
                                      WHERE {
                                      ?obs 
                                      <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
                                      <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
                                      <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
                                      
                                      <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/employment> ;
                                      <http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all>;
                                      <http://purl.org/linked-data/cube#dataSet> ?i ;
                                      <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?m ;
                                      <http://statistics.gov.scot/def/dimension/age> ?b ;
                                      <http://purl.org/linked-data/cube#measureType> ?dummy;
                                      ?dummy ?Figure.
                                      
                                      {?obs <http://purl.org/linked-data/cube#measureType> <http://statistics.gov.scot/def/measure-properties/ratio>} UNION {?obs <http://purl.org/linked-data/cube#measureType> <http://statistics.gov.scot/def/measure-properties/count>}
                                      
                                      <http://statistics.gov.scot/data/employment> 
                                      <http://purl.org/dc/terms/modified> ?LastUpdated ;
                                      <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
                                      
                                      ?Interval rdfs:label ?DateCode.
                                      ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
                                      ?d rdfs:label ?Date.
                                      ?i rdfs:label ?Indicator.
                                      ?m rdfs:label ?Measure.
                                      ?b rdfs:label ?Breakdown.
                                      ?l rdfs:label ?yLabel.
                                      }")

setGraphData[["labSoc-4"]] <- filter(ODPdata,Indicator=="Gender Employment Gap",Characteristic=="Gender")
setGraphOptions[["labSoc-4"]] <- list(graphType="timeSeries1",
                                      filterSparql=TRUE,
                                      query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                      SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
                                      WHERE {
                                      ?obs 
                                      <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
                                      <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
                                      <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
                                      
                                      <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/gender-employment-gap> ;
                                      <http://purl.org/linked-data/cube#dataSet> ?i ;
                                      <http://purl.org/linked-data/cube#dataSet> ?m ;
                                      <http://statistics.gov.scot/def/dimension/age> ?b ;
                                      <http://statistics.gov.scot/def/measure-properties/percentage-points> ?Figure.
                                      
                                      <http://statistics.gov.scot/data/gender-employment-gap> 
                                      <http://purl.org/dc/terms/modified> ?LastUpdated ;
                                      <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
                                      
                                      ?Interval rdfs:label ?DateCode.
                                      ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
                                      ?d rdfs:label ?Date.
                                      ?i rdfs:label ?Indicator.
                                      ?m rdfs:label ?Measure.
                                      ?b rdfs:label ?Breakdown.
                                      ?l rdfs:label ?yLabel.
                                      }",
                                   digits=1,
                                   defaultSelected="16-64")

setGraphData[["labSoc-23"]] <- filter(EEFdata,Characteristic=="Age",policy_area=="Summary",grepl("Public Sector Workforce",Indicator),Measure=="Scottish Government") %>%
  arrange(desc(Indicator))
setGraphOptions[["labSoc-23"]] <- list(graphType="barChart2",sortOrder=NA)

setGraphData[["labSoc-24"]] <- filter(EEFdata,Characteristic=="Disability",policy_area=="Summary",grepl("Public Sector Workforce",Indicator),Measure=="Scottish Government") %>%
  arrange(desc(Indicator))
setGraphOptions[["labSoc-24"]] <- list(graphType="barChart2",sortOrder=NA)

setGraphData[["labSoc-25"]] <- filter(EEFdata,Characteristic=="Ethnicity",policy_area=="Summary",grepl("Public Sector Workforce",Indicator),Measure=="Scottish Government") %>%
  arrange(desc(Indicator))
setGraphOptions[["labSoc-25"]] <- list(graphType="barChart2",sortOrder=NA)

setGraphData[["labSoc-26"]] <- filter(EEFdata,Characteristic=="Gender",policy_area=="Summary",grepl("Public Sector Workforce",Indicator),Measure=="Scottish Government") %>%
  arrange(desc(Indicator))
setGraphOptions[["labSoc-26"]] <- list(graphType="barChart2",sortOrder=NA)

setGraphData[["labSoc-27"]] <- filter(EEFdata,Characteristic=="Religion",policy_area=="Summary",grepl("Public Sector Workforce",Indicator),Measure=="Scottish Government") %>%
  arrange(desc(Indicator))
setGraphOptions[["labSoc-27"]] <- list(graphType="barChart2",sortOrder=NA)

setGraphData[["labSoc-28"]] <- filter(EEFdata,Characteristic=="Sexual Orientation",policy_area=="Summary",grepl("Public Sector Workforce",Indicator),Measure=="Scottish Government") %>%
  arrange(desc(Indicator))
setGraphOptions[["labSoc-28"]] <- list(graphType="barChart2",sortOrder=NA)

setGraphData[["labSoc-38"]] <- filter(EEFdata,Characteristic=="Gender",policy_area=="Summary",grepl("Public Sector Workforce",Indicator),Measure=="Scottish Fire and Rescue Service") %>%
  arrange(desc(Indicator))
setGraphOptions[["labSoc-38"]] <- list(graphType="timeSeries1",digits=1)

setGraphData[["labSoc-39"]] <- filter(EEFdata,Characteristic=="Age",policy_area=="Summary",grepl("Public Sector Workforce",Indicator),Measure=="Scottish Fire and Rescue Service") %>%
  arrange(desc(Indicator))
setGraphOptions[["labSoc-39"]] <- list(graphType="barChart1",sortOrder=NA,digits=1)



####Local Government & Third Sector####

setGraphData[["locgov-1"]] <- filter(NPFdata,Characteristic=="Gender",Indicator%in%c("Quality of public services","Influence over local decisions")) %>%
  mutate(Measure=Indicator,
         Breakdown=ifelse(Breakdown=="Male","Men",Breakdown),
         Breakdown=ifelse(Breakdown=="Female","Women",Breakdown))
setGraphOptions[["locgov-1"]] <- list(graphType="timeSeries2",
                                   NPFindicator=c("Quality of public services","Influence over local decisions"),
                                   digits=0)

#setGraphData[["locgov-2"]] <- filter(NPFextra,Characteristic=="Age",Measure%in%c("Quality of public services","Influence over local decisions"))
setGraphData[["locgov-2"]] <- filter(NPFdata,Characteristic=="Age",Indicator%in%c("Quality of public services","Influence over local decisions")) %>%
  mutate(Measure=Indicator)
  setGraphOptions[["locgov-2"]] <- list(graphType="timeSeries2",
                                     NPFindicator=c("Quality of public services","Influence over local decisions"),
                                     digits=0)

#setGraphData[["locgov-3"]] <- filter(NPFextra,Characteristic=="Disability",Measure%in%c("Quality of public services","Influence over local decisions"))
setGraphData[["locgov-3"]] <- filter(NPFdata,Characteristic=="Disability",Indicator%in%c("Quality of public services","Influence over local decisions"))  %>%
  mutate(Measure=Indicator)
setGraphOptions[["locgov-3"]] <- list(graphType="timeSeries2",
                                   NPFindicator=c("Quality of public services","Influence over local decisions"),
                                   digits=0)

#setGraphData[["locgov-5"]] <- filter(NPFextra,Characteristic=="Ethnicity",Measure%in%c("Quality of public services","Influence over local decisions"))
setGraphData[["locgov-5"]] <- filter(NPFdata,Characteristic=="Ethnicity",Indicator%in%c("Quality of public services","Influence over local decisions"))  %>%
  mutate(Measure=Indicator)%>%
  mutate(Breakdown=ifelse(Breakdown=="Other minority ethnic","Minority Ethnic",Breakdown))
setGraphOptions[["locgov-5"]] <- list(graphType="timeSeries2",
                                   NPFindicator=c("Quality of public services","Influence over local decisions"),
                                   digits=0)

#setGraphData[["locgov-6"]] <- filter(NPFextra,Characteristic=="Socio-Economic Status",Measure%in%c("Quality of public services","Influence over local decisions"))
setGraphData[["locgov-6"]] <- filter(NPFdata,Characteristic=="Socio-Economic Status",Indicator%in%c("Quality of public services","Influence over local decisions"))  %>%
  mutate(Measure=Indicator)
setGraphOptions[["locgov-6"]] <- list(graphType="timeSeries2",
                                   NPFindicator=c("Quality of public services","Influence over local decisions"),
                                   digits=0)


####Rural and Environment####

setGraphData[["ruralenv-1"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Age",Indicator=="% of Midyear Population Estimate")
setGraphOptions[["ruralenv-1"]] <- list(graphType="barChart1",
                                     gvisOptions=list(isStacked="percent",height=400),
                                     sortOrder=NA,
                                     colourPalette="blue")

setGraphData[["ruralenv-2"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Age",Indicator=="% Adults Visiting Outdoors at least once a week") #%>% mutate(SubBreakdown=Year)
setGraphOptions[["ruralenv-2"]] <- list(graphType="timeSeries1",
                                     NPFindicator="Visits to the outdoors",
                                     digits=1)
setGraphData[["age-81"]] <- setGraphData[["ruralenv-2"]] 
setGraphOptions[["age-81"]] <- setGraphOptions[["ruralenv-2"]] 

setGraphData[["ruralenv-3"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Age",Indicator=="% Adults Believing Climate Change is Immediate and Urgent Problem") #%>% mutate(SubBreakdown=Year)
setGraphOptions[["ruralenv-3"]] <- list(graphType="timeSeries1",
                                     digits=1)
setGraphData[["age-82"]] <- setGraphData[["ruralenv-3"]]
setGraphOptions[["age-82"]] <- setGraphOptions[["ruralenv-3"]]

setGraphData[["ruralenv-5"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Disability",Indicator=="% Adults Visiting Outdoors")
setGraphOptions[["ruralenv-5"]] <- list(graphType="barChart1",
                                     NPFindicator="Visits to the outdoors",
                                     gvisOptions=list(isStacked="percent",chartArea="{left: '35%', width: '100%', bottom:50}"),
                                     colourPalette="blue",
                                     sortOrder=NA)
setGraphData[["disability-81"]] <- setGraphData[["ruralenv-5"]]
setGraphOptions[["disability-81"]] <- setGraphOptions[["ruralenv-5"]]

setGraphData[["ruralenv-6"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Disability",Indicator=="% Adults Believing Climate Change is Immediate and Urgent Problem") #%>% mutate(SubBreakdown=Year)
setGraphOptions[["ruralenv-6"]] <- list(graphType="timeSeries1",
                                     digits=1)
setGraphData[["disability-82"]] <- setGraphData[["ruralenv-6"]]
setGraphOptions[["disability-82"]] <- setGraphOptions[["ruralenv-6"]]

setGraphData[["ruralenv-7"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Ethnicity",Indicator=="Ethnicity by Urban Rural Classification - Household Reference Person")
setGraphOptions[["ruralenv-7"]] <- list(graphType="barChart1",
                                     colourPalette="diverging",
                                     gvisOptions=list(height="1000px",isStacked="percent"))
setGraphData[["ethnicity-8"]] <- setGraphData[["ruralenv-7"]]
setGraphOptions[["ethnicity-8"]] <- setGraphOptions[["ruralenv-7"]]

setGraphData[["ruralenv-8"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Gender",Indicator=="Life Expectancy by Gender and Urban Rural")
setGraphOptions[["ruralenv-8"]] <- list(graphType="barChart3",
                                     digits=1,
                                     colourPalette="blue",
                                     sortOrder=NA)

setGraphData[["ruralenv-9"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Gender",Indicator=="% Adults Visiting Outdoors")
setGraphOptions[["ruralenv-9"]] <- list(graphType="barChart1",
                                     gvisOptions=list(isStacked="percent"),
                                     colourPalette="blue",
                                     NPFindicator="Visits to the outdoors")
setGraphData[["gender-81"]] <- setGraphData[["ruralenv-9"]]
setGraphOptions[["gender-81"]] <- setGraphOptions[["ruralenv-9"]]

setGraphData[["ruralenv-11"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Religion",Indicator=="% Adults belonging to no religion") #%>% mutate(SubBreakdown=Year)
setGraphOptions[["ruralenv-11"]] <- list(graphType="timeSeries1",
                                      digits=1)
setGraphData[["religion-8"]] <- setGraphData[["ruralenv-11"]]
setGraphOptions[["religion-8"]] <- setGraphOptions[["ruralenv-11"]]

setGraphData[["ruralenv-13"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Socio-Economic Status",Indicator=="% Adults Visiting Outdoors at least once a week") #%>% mutate(SubBreakdown=Year)
setGraphOptions[["ruralenv-13"]] <- list(graphType="timeSeries1",
                                      NPFindicator="Visits to the outdoors",
                                      digits=1)

setGraphData[["ruralenv-14"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Socio-Economic Status",Indicator=="% Adults living 11 minutes or more walk from nearest green or blue space") #%>% mutate(SubBreakdown=Year)
setGraphOptions[["ruralenv-14"]] <- list(graphType="timeSeries1",
                                      digits=1)
setGraphData[["socioEconomic-8"]] <- setGraphData[["ruralenv-14"]]
setGraphOptions[["socioEconomic-8"]] <- setGraphOptions[["ruralenv-14"]]

####School Education####

setGraphData[["sch-Edu-1"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
setGraphOptions[["sch-Edu-1"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)
setGraphData[["gender-71"]] <- setGraphData[["sch-Edu-1"]]
setGraphOptions[["gender-71"]] <- setGraphOptions[["sch-Edu-1"]]

setGraphData[["sch-Edu-2"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
setGraphOptions[["sch-Edu-2"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

setGraphData[["sch-Edu-3"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
setGraphOptions[["sch-Edu-3"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

setGraphData[["sch-Edu-4"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="% school leavers in a positive follow-up destination")
setGraphOptions[["sch-Edu-4"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

setGraphData[["sch-Edu-5"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
setGraphOptions[["sch-Edu-5"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)
setGraphData[["ethnicity-71"]] <- setGraphData[["sch-Edu-5"]]
setGraphOptions[["ethnicity-71"]] <- setGraphOptions[["sch-Edu-5"]]

setGraphData[["sch-Edu-6"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
setGraphOptions[["sch-Edu-6"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

setGraphData[["sch-Edu-7"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
setGraphOptions[["sch-Edu-7"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

setGraphData[["sch-Edu-8"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator=="% school leavers in a positive follow-up destination")
setGraphOptions[["sch-Edu-8"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

setGraphData[["sch-Edu-9"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
setGraphOptions[["sch-Edu-9"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)
setGraphData[["socioEconomic-71"]] <- setGraphData[["sch-Edu-9"]]
setGraphOptions[["socioEconomic-71"]] <- setGraphOptions[["sch-Edu-9"]]

setGraphData[["sch-Edu-10"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
setGraphOptions[["sch-Edu-10"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

setGraphData[["sch-Edu-11"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
setGraphOptions[["sch-Edu-11"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

setGraphData[["sch-Edu-12"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator=="% school leavers in a positive follow-up destination")
setGraphOptions[["sch-Edu-12"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

setGraphData[["sch-Edu-13"]] <- filter(EEFdata,Indicator=="Teachers (headcount)",Characteristic=="Age") %>% 
  mutate(DateString=paste(sep="",as.character(2000+as.numeric(Measure)),"-01-01"),Date=as.Date(DateString)) #A hacky way of displaying line graphs with integer x-axis values by converting values to a dummy date (and converting back in the axis formatter)
setGraphOptions[["sch-Edu-13"]] <- list(graphType="timeSeries1",
                                     xFormat=list(xAxisFormat="year.toString().substr(2,2);",
                                                  xValueFormat="'Aged '+(year.toString().substr(2,2));"),
                                     colourPalette="blue",
                                     ylabel="Teachers (FTE)",
                                     minZoom=c(1000,NA),
                                     digits=0)
setGraphData[["sch-Edu-14"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator=="Pupil ethnicity")
setGraphOptions[["sch-Edu-14"]] <- list(graphType="barChart1",
                                     defaultSelected="Total Pupils",
                                     digits=1)

setGraphData[["sch-Edu-15"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator=="Teacher ethnicity")
setGraphOptions[["sch-Edu-15"]] <- list(graphType="barChart1",
                                     digits=1)

setGraphData[["sch-Edu-21"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
setGraphOptions[["sch-Edu-21"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)
setGraphData[["disability-71"]] <- setGraphData[["sch-Edu-21"]]
setGraphOptions[["disability-71"]] <- setGraphOptions[["sch-Edu-21"]]

setGraphData[["sch-Edu-22"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
setGraphOptions[["sch-Edu-22"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

setGraphData[["sch-Edu-23"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
setGraphOptions[["sch-Edu-23"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

setGraphData[["sch-Edu-24"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="% school leavers in a positive follow-up destination")
setGraphOptions[["sch-Edu-24"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)


setGraphData[["sch-Edu-41"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
setGraphOptions[["sch-Edu-41"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     intervalType="Academic",
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-42"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("P4 - Reading","P4 - Writing","P4 - Listening & Talking","P4 - Numeracy"))
setGraphOptions[["sch-Edu-42"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-43"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("P7 - Reading","P7 - Writing","P7 - Listening & Talking","P7 - Numeracy"))
setGraphOptions[["sch-Edu-43"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-44"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("S3 - Reading","S3 - Writing","S3 - Listening & Talking","S3 - Numeracy"))
setGraphOptions[["sch-Edu-44"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-45"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
setGraphOptions[["sch-Edu-45"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-46"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("P4 - Reading","P4 - Writing","P4 - Listening & Talking","P4 - Numeracy"))
setGraphOptions[["sch-Edu-46"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-47"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("P7 - Reading","P7 - Writing","P7 - Listening & Talking","P7 - Numeracy"))
setGraphOptions[["sch-Edu-47"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-48"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("S3 - Reading","S3 - Writing","S3 - Listening & Talking","S3 - Numeracy"))
setGraphOptions[["sch-Edu-48"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-49"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
setGraphOptions[["sch-Edu-49"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-50"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("P4 - Reading","P4 - Writing","P4 - Listening & Talking","P4 - Numeracy"))
setGraphOptions[["sch-Edu-50"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-51"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("P7 - Reading","P7 - Writing","P7 - Listening & Talking","P7 - Numeracy"))
setGraphOptions[["sch-Edu-51"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-52"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("S3 - Reading","S3 - Writing","S3 - Listening & Talking","S3 - Numeracy"))
setGraphOptions[["sch-Edu-52"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-53"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
setGraphOptions[["sch-Edu-53"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-54"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("P4 - Reading","P4 - Writing","P4 - Listening & Talking","P4 - Numeracy"))
setGraphOptions[["sch-Edu-54"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-55"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("P7 - Reading","P7 - Writing","P7 - Listening & Talking","P7 - Numeracy"))
setGraphOptions[["sch-Edu-55"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

setGraphData[["sch-Edu-56"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("S3 - Reading","S3 - Writing","S3 - Listening & Talking","S3 - Numeracy"))
setGraphOptions[["sch-Edu-56"]] <- list(graphType="pieChart2",
                                        intervalType="Academic",
                                        updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

####Transport and Travel####

setGraphData[["TravAge-1"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Travel"))
setGraphOptions[["TravAge-1"]] <- list(graphType="barChart1",
                                    sortOrder=NA,
                                    digits=0)

setGraphData[["TravAge-1a"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",grepl("Travel to Work:",Indicator))
setGraphOptions[["TravAge-1a"]] <- list(graphType="barChart2",
                                     sortOrder=NA,
                                     digits=0)

setGraphData[["TravAge-2"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Driving"))
setGraphOptions[["TravAge-2"]] <- list(graphType="barChart1",
                                       sortOrder=NA,
                                       digits=0)

setGraphData[["TravAge-3"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Road accidents","Casualties"))
setGraphOptions[["TravAge-3"]] <- list(graphType="timeSeries2")

setGraphData[["TravAge-4"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Bus Use","Train Use"))
setGraphOptions[["TravAge-4"]] <- list(graphType="barChart3",
                                    sortOrder=NA,
                                    digits=0)

setGraphData[["TravAge-5"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Concessionary Journeys"))
setGraphOptions[["TravAge-5"]] <- list(graphType="timeSeries0",
                                    digits=0)

setGraphData[["TravAge-6"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Concessionary Pass Use"))
setGraphOptions[["TravAge-6"]] <- list(graphType="barChart1",
                                       sortOrder=NA,
                                       colourPalette="blue",
                                       gvisOptions=list(isStacked=TRUE))

setGraphData[["TravAge-7"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Walking","Cycling"))
setGraphOptions[["TravAge-7"]] <- list(graphType="barChart2",
                                       sortOrder=NA,
                                       colourPalette="blue",
                                       maxZoom=c(0,100),
                                       gvisOptions=list(isStacked=TRUE))



setGraphData[["TravDis-1"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Driving"))
setGraphOptions[["TravDis-1"]] <- list(graphType="barChart1",
                                       sortOrder=NA,
                                       digits=0,
                                       colourPalette="blue",
                                       maxZoom=c(0,100),
                                       gvisOptions=list(isStacked=TRUE))

setGraphData[["TravDis-2"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Bus Use"))
setGraphOptions[["TravDis-2"]] <- list(graphType="barChart1",
                                    sortOrder=NA,
                                    colourPalette="blue",
                                    gvisOptions=list(isStacked=TRUE))

# setGraphData[["TravDis-3"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Measure%in%c("Number of blue badges on issue","Concessionary fare passes issued to disabled people","Journeys made under the concessionary fare schemes (millions)","Percentage of journeys by concessionary passengers"))
# setGraphOptions[["TravDis-3"]] <- list(graphType="timeSeries2",
#                                        forceRescale=TRUE,
#                                        digits=0)

setGraphData[["TravDis-3a"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Blue badges"))
setGraphOptions[["TravDis-3a"]] <- list(graphType="timeSeries0",
                                     digits=0)

setGraphData[["TravDis-3b"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Concessionary Passes"))
setGraphOptions[["TravDis-3b"]] <- list(graphType="timeSeries1",
                                     digits=0)
setGraphData[["TravDis-3c"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Concessionary Journeys"))
setGraphOptions[["TravDis-3c"]] <- list(graphType="timeSeries2",
                                        rescale=TRUE,
                                     digits=0)

setGraphData[["TravDis-5"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Accessibility"))
setGraphOptions[["TravDis-5"]] <- list(graphType="timeSeries0",
                                    maxZoom=c(0,100),
                                    ylabel="Percentage of Buses",
                                    digits=0)

setGraphData[["TravEth-2"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Ethnicity",Indicator%in%c("Walking","Cycling"))
setGraphOptions[["TravEth-2"]] <- list(graphType="barChart2",
                                       sortOrder=NA,
                                       colourPalette="blue",
                                       maxZoom=c(0,100),
                                       gvisOptions=list(isStacked=TRUE))

setGraphData[["TravEth-3"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Ethnicity",Indicator%in%c("Bus Use","Train Use"))
setGraphOptions[["TravEth-3"]] <- list(graphType="barChart2",
                                       sortOrder=NA,
                                       colourPalette="blue")

setGraphData[["TravGen-1a"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Gender",grepl("Travel to Work:",Indicator))
setGraphOptions[["TravGen-1a"]] <- list(graphType="barChart2",
                                        sortOrder=NA,
                                        digits=0)

setGraphData[["TravGen-3"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Gender",Indicator%in%c("Road accidents","Casualties"))
setGraphOptions[["TravGen-3"]] <- list(graphType="timeSeries1")

setGraphData[["TravGen-4"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Gender",Indicator%in%c("Walking","Cycling"))
setGraphOptions[["TravGen-4"]] <- list(graphType="barChart2",
                                    sortOrder=NA,
                                    colourPalette="blue",
                                    gvisOptions=list(isStacked=TRUE))


setGraphData[["TravGen-5"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Gender",Indicator%in%c("Bus Use","Train Use"))
setGraphOptions[["TravGen-5"]] <- list(graphType="barChart2",
                                       sortOrder=NA,
                                       colourPalette="blue",
                                       gvisOptions=list(isStacked=TRUE))


setGraphData[["TravSoc-1"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Socio-Economic Status",Indicator%in%c("Driving"))
setGraphOptions[["TravSoc-1"]] <- list(graphType="barChart1",
                                       sortOrder=NA,
                                       digits=0,
                                       colourPalette="blue",
                                       maxZoom=c(0,100),
                                       gvisOptions=list(isStacked=TRUE))

setGraphData[["TravSoc-1a"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Socio-Economic Status",grepl("Travel to Work:",Indicator))
setGraphOptions[["TravSoc-1a"]] <- list(graphType="barChart2",
                                        sortOrder=NA,
                                        digits=0)

setGraphData[["TravSoc-2"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Socio-Economic Status",Indicator%in%c("Walking","Cycling"))
setGraphOptions[["TravSoc-2"]] <- list(graphType="barChart2",
                                       sortOrder=NA,
                                       colourPalette="blue",
                                       gvisOptions=list(isStacked=TRUE))

setGraphData[["TravSoc-3"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Socio-Economic Status",Indicator%in%c("Bus Use","Train Use"))
setGraphOptions[["TravSoc-3"]] <- list(graphType="barChart2",
                                       sortOrder=NA,
                                       colourPalette="blue",
                                       gvisOptions=list(isStacked=TRUE))
