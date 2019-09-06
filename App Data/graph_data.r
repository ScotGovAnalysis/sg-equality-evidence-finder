#Graph data
filterData <- list()
graphOptions <- list()

####NPF Data####

graphOptions[["npf"]] <- list(ylabel="",graphType="npfDataExplorer")
filterData[["npf"]] <- NPFdata

####Summary####

filterData[["disability-1"]] <- filter(EEFlatest,policy_area=="Summary",Characteristic=="Disability",Indicator=="% of adult population who were disabled")
graphOptions[["disability-1"]] <- list(graphType="barChart1",
                                       graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"disability-1"],
                                       digits=0)

filterData[["disability-2"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Disability",Indicator=="% of adult population who were disabled") 
graphOptions[["disability-2"]] <- list(graphType="timeSeries1",
                                       graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"disability-2"],
                                       digits=0)
filterData[["Demog-46"]] <- filterData[["disability-2"]]
graphOptions[["Demog-46"]] <- graphOptions[["disability-2"]]

filterData[["disability-13"]] <- filter(EEFlatest,policy_area=="Summary",Characteristic=="Disability",Indicator=="% of children who were disabled") 
graphOptions[["disability-13"]] <- list(graphType="barChart1",
                                        graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"disability-13"],
                                        digits=0)
filterData[["chifam-13"]] <- filterData[["disability-13"]]
graphOptions[["chifam-13"]] <- graphOptions[["disability-13"]]

filterData[["disability-14"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Disability",Indicator=="% of children who were disabled") 
graphOptions[["disability-14"]] <- list(graphType="timeSeries1",
                                        graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"disability-14"],
                                        digits=0)
filterData[["chifam-14"]] <- filterData[["disability-14"]]
graphOptions[["chifam-14"]] <- graphOptions[["disability-14"]]

filterData[["ethnicity-1"]] <- filter(EEFlatest,policy_area=="Summary",Characteristic=="Ethnicity",Indicator=="% of Adult Population") 
graphOptions[["ethnicity-1"]] <- list(graphType="pieChart0",
                                      colourPalette="full",
                                      digits=1)

filterData[["ethnicity-2"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Ethnicity",Indicator=="% of Population")
graphOptions[["ethnicity-2"]] <- list(graphType="barChart0",
                                      colourPalette="full",
                                      gvisOptions=list(isStacked="percent"))

filterData[["religion-1"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Religion",Indicator=="% of Adult Population") 
graphOptions[["religion-1"]] <- list(graphType="pieChart0",
                                     colourPalette="full",
                                     graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"religion-1"],
                                     digits=1)
filterData[["Demog-18"]] <- filterData[["religion-1"]]
graphOptions[["Demog-18"]] <- graphOptions[["religion-1"]]

filterData[["sexualOrientation-1"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Sexual Orientation",Indicator=="Overview") 
graphOptions[["sexualOrientation-1"]] <- list(graphType="pieChart0",
                                              colourPalette="full",
                                              graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"sexualOrientation-1"],
                                              digits=1)
filterData[["Demog-11"]] <- filterData[["sexualOrientation-1"]]
graphOptions[["Demog-11"]] <- graphOptions[["sexualOrientation-1"]]


filterData[["sexualOrientation-2"]] <- filter(EEFdata,policy_area=="Summary",Characteristic=="Sexual Orientation",Indicator=="% of Adult Population")
graphOptions[["sexualOrientation-2"]] <- list(graphType="barChart1",
                                              digits=1,
                                              #gvisOptions=list(isStacked=TRUE),
                                              colourPalette="full")
filterData[["Demog-12"]] <- filterData[["sexualOrientation-2"]]
graphOptions[["Demog-12"]] <- graphOptions[["sexualOrientation-2"]]
filterData[["Demog-41"]] <- filterData[["sexualOrientation-2"]]
graphOptions[["Demog-41"]] <- graphOptions[["sexualOrientation-2"]]


####Business, Enterprise and Tourism####

filterData[["busEnt-1"]] <- filter(NPFdata,Indicator=="Entrepreneurial activity",Characteristic=="Age",Year>=2010)
graphOptions[["busEnt-1"]] <- list(graphType="timeSeries1",
                                   ylabel="% of working-age population",
                                   NPFindicator="Entrepreneurial activity",
                                   digits=1)
filterData[["age-91"]] <- filterData[["busEnt-11"]]
graphOptions[["age-91"]] <- graphOptions[["busEnt-11"]]


filterData[["busEnt-3"]] <- filter(ODPdata,Indicator=="Self-Employment",Characteristic=="Age")
graphOptions[["busEnt-3"]] <- list(graphType="timeSeries1",
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
filterData[["busEnt-3"]] <- filter(EEFlatest,Indicator=="Self-employment",Characteristic=="Age")
graphOptions[["busEnt-3"]] <- list(graphType="barChart3",
                                   ylabel="% of working-age population",
                                   digits=1)


filterData[["busEnt-5"]] <- filter(EEFlatest,Indicator=="Self-employment",Characteristic=="Disability")
graphOptions[["busEnt-5"]] <- list(graphType="barChart3",
                                   ylabel="% of working-age population",
                                   digits=1)

filterData[["busEnt-7"]] <- filter(EEFlatest,Indicator=="Self-employment",Characteristic=="Ethnicity")
graphOptions[["busEnt-7"]] <- list(graphType="barChart3",
                                   ylabel="% of working-age population",
                                   digits=1)

filterData[["busEnt-9"]] <- filter(ODPdata,Indicator=="Self-employment",Characteristic=="Gender")
graphOptions[["busEnt-9"]] <- list(graphType="timeSeries1",
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
filterData[["busEnt-9"]] <- filter(EEFdata,Indicator=="Self-employment",Characteristic=="Gender")
graphOptions[["busEnt-9"]] <- list(graphType="timeSeries1",
                                   ylabel="% of working-age population",
                                   digits=1)


filterData[["busEnt-11"]] <- filter(NPFdata,Indicator=="Entrepreneurial activity",Characteristic=="Gender")
graphOptions[["busEnt-11"]] <- list(graphType="timeSeries1",
                                    ylabel="% of working-age population",
                                    NPFindicator="Entrepreneurial activity",
                                    digits=1)
filterData[["gender-92"]] <- filterData[["busEnt-1"]]
graphOptions[["gender-92"]] <- graphOptions[["busEnt-1"]]

####Children and Families####

filterData[["chifam-1"]] <- filter(EEFdata,Measure=="% of young people aged 17 and under looked after 31st July",Characteristic=="Age")
graphOptions[["chifam-1"]] <- list(graphType="timeSeries0",
                                   ylabel="% Young People",
                                   digits=1)

filterData[["chifam-2"]] <- filter(EEFdata,Measure=="Count of young people looked after, 31st July",Characteristic=="Age")
graphOptions[["chifam-2"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of Young People",
                                   digits=0)
filterData[["age-3"]] <- filterData[["chifam-2"]]
graphOptions[["age-3"]] <- graphOptions[["chifam-2"]]

filterData[["chifam-3"]] <- filter(EEFdata,Measure=="Count of young people on the child protection register, 31st July",Characteristic=="Age")
graphOptions[["chifam-3"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of Young People",
                                   digits=0)

filterData[["chifam-4"]] <- filter(EEFdata,Measure=="Count of Early Learning and Childcare registrations, September",Characteristic=="Age")
graphOptions[["chifam-4"]] <- list(graphType="timeSeries0",
                                   ylabel="Number of Young People",
                                   digits=0)

filterData[["chifam-5"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% young people looked after with a disability, 31st July")
graphOptions[["chifam-5"]] <- list(graphType="timeSeries0",
                                   digits=1)
filterData[["disability-3"]] <- filterData[["chifam-5"]]
graphOptions[["disability-3"]] <- graphOptions[["chifam-5"]]

filterData[["chifam-6"]] <- filter(EEFdata,Measure=="% young people looked after by ethnic group, 31st July",Characteristic=="Ethnicity")
graphOptions[["chifam-6"]] <- list(graphType="timeSeries1",
                                   ylabel="% Young People",
                                   digits=1)

filterData[["chifam-7"]] <- filter(EEFdata,Measure=="% young people on the child protection register by ethnic group, 31st July",Characteristic=="Ethnicity")
graphOptions[["chifam-7"]] <- list(graphType="timeSeries1",
                                   ylabel="% Young People",
                                   digits=1)

filterData[["chifam-8"]] <- filter(EEFdata,Measure=="% Early Learning and Childcare registrations with a home language other than English, September",Characteristic=="Ethnicity")
graphOptions[["chifam-8"]] <- list(graphType="timeSeries0",
                                   ylabel="% Young People",
                                   digits=1)
filterData[["ethnicity-3"]] <- filterData[["chifam-8"]]
graphOptions[["ethnicity-3"]] <- graphOptions[["chifam-8"]]

filterData[["chifam-9"]] <- filter(EEFdata,Measure=="% young people looked after by gender, 31st July",Characteristic=="Gender")
graphOptions[["chifam-9"]] <- list(graphType="timeSeries1",
                                   ylabel="% Young People",
                                   digits=1)
filterData[["gender-3"]] <- filterData[["chifam-9"]]
graphOptions[["gender-3"]] <- graphOptions[["chifam-9"]]

filterData[["chifam-10"]] <- filter(EEFdata,Measure=="% young people on the child protection register by gender, 31st July",Characteristic=="Gender")
graphOptions[["chifam-10"]] <- list(graphType="timeSeries1",
                                    ylabel="% Young People",
                                    digits=1)

filterData[["chifam-11"]] <- filter(EEFdata,Measure=="% young people looked after by religion, 31st July",Characteristic=="Religion")
graphOptions[["chifam-11"]] <- list(graphType="timeSeries1",
                                    ylabel="% Young People",
                                    digits=1)
filterData[["religion-3"]] <- filterData[["chifam-11"]]
graphOptions[["religion-3"]] <- graphOptions[["chifam-11"]]

filterData[["chifam-12"]] <- filter(EEFdata,Measure=="% young people on the child protection register by religion, 31st July",Characteristic=="Religion")
graphOptions[["chifam-12"]] <- list(graphType="timeSeries1",
                                    ylabel="% Young People",
                                    digits=1)

####Culture, Communities and Society####

filterData[["culCom-1"]] <- filter(NPFdata,Characteristic%in%c("Total","Gender"),Indicator%in%c("Attendance at cultural events or places of culture")) %>%
  mutate(Breakdown=ifelse(Breakdown=="Male","Men",Breakdown),
         Breakdown=ifelse(Breakdown=="Female","Women",Breakdown)
  )
graphOptions[["culCom-1"]] <- list(graphType="timeSeries2",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)

filterData[["culCom-2"]] <- filter(NPFdata,Characteristic%in%c("Total","Gender"),Indicator%in%c("Participation in a cultural activity")) %>%
  mutate(Breakdown=ifelse(Breakdown=="Male","Men",Breakdown),
         Breakdown=ifelse(Breakdown=="Female","Women",Breakdown)
  )
graphOptions[["culCom-2"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Participation in a cultural activity",
                                   digits=1)
graphOptions[["gender-5"]] <- graphOptions[["culCom-2"]]
filterData[["gender-5"]] <- filterData[["culCom-2"]]

filterData[["culCom-3"]] <- filter(NPFdata,Characteristic%in%c("Total","Age"),Indicator%in%c("Attendance at cultural events or places of culture")) 
graphOptions[["culCom-3"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)
filterData[["age-5"]] <- filterData[["culCom-3"]]
graphOptions[["age-5"]] <- graphOptions[["culCom-3"]]

filterData[["culCom-4"]] <- filter(NPFdata,Characteristic%in%c("Total","Age"),Indicator%in%c("Participation in a cultural activity")) 
graphOptions[["culCom-4"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Participation in a cultural activity",
                                   digits=1)

filterData[["culCom-5"]] <- filter(NPFdata,Characteristic%in%c("Total","Socio-Economic Status"),Indicator%in%c("Attendance at cultural events or places of culture")) 
graphOptions[["culCom-5"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)
filterData[["socioEconomic-51"]] <- filterData[["culCom-5"]]
graphOptions[["socioEconomic-51"]] <- graphOptions[["culCom-5"]]

filterData[["culCom-6"]] <- filter(NPFdata,Characteristic%in%c("Total","Socio-Economic Status"),Indicator%in%c("Participation in a cultural activity")) 
graphOptions[["culCom-6"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Participation in a cultural activity",
                                   digits=1)
filterData[["socioEconomic-52"]] <- filterData[["culCom-6"]]
graphOptions[["socioEconomic-52"]] <- graphOptions[["culCom-6"]]

filterData[["culCom-7"]] <- filter(NPFdata,Characteristic%in%c("Total","Disability"),Indicator%in%c("Attendance at cultural events or places of culture")) 
graphOptions[["culCom-7"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)

filterData[["disability-5"]] <- filterData[["culCom-7"]]
graphOptions[["disability-5"]] <- graphOptions[["culCom-7"]]

filterData[["culCom-8"]] <- filter(NPFdata,Characteristic%in%c("Total","Disability"),Indicator%in%c("Participation in a cultural activity")) 
graphOptions[["culCom-8"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Participation in a cultural activity",
                                   digits=1)

filterData[["culCom-9"]] <- filter(NPFdata,Characteristic%in%c("Total","Religion"),Indicator%in%c("Attendance at cultural events or places of culture")) 
graphOptions[["culCom-9"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Attendance at cultural events or places of culture",
                                   digits=1)

filterData[["culCom-10"]] <- filter(NPFdata,Characteristic%in%c("Total","Religion"),Indicator%in%c("Participation in a cultural activity")) 
graphOptions[["culCom-10"]] <- list(graphType="timeSeries1",
                                    NPFindicator="Participation in a cultural activity",
                                    digits=1)
filterData[["religion-5"]] <- filterData[["culCom-10"]]
graphOptions[["religion-5"]] <- graphOptions[["culCom-10"]]

filterData[["culCom-11"]] <- filter(NPFdata,Characteristic%in%c("Total","Ethnicity"),Indicator%in%c("Attendance at cultural events or places of culture")) %>%
  mutate(Breakdown=ifelse(Breakdown=="Other Ethnic","Other",Breakdown))
graphOptions[["culCom-11"]] <- list(graphType="timeSeries1",
                                    NPFindicator="Attendance at cultural events or places of culture",
                                    digits=1)

filterData[["culCom-12"]] <- filter(NPFdata,Characteristic%in%c("Total","Ethnicity"),Indicator%in%c("Participation in a cultural activity")) %>%
  mutate(Breakdown=ifelse(Breakdown=="Other ethnic","Other Minority Ethnic",Breakdown))
graphOptions[["culCom-12"]] <- list(graphType="timeSeries1",
                                    NPFindicator="Participation in a cultural activity",
                                    digits=1)
filterData[["ethnicity-5"]] <- filterData[["culCom-12"]]
graphOptions[["ethnicity-5"]] <- graphOptions[["culCom-12"]]

filterData[["culCom-111"]] <- filter(EEFdata,Characteristic=="Age",Indicator%in%c("Discrimination"))
graphOptions[["culCom-111"]] <- list(graphType="barChart3",
                                     sortOrder="",
                                     digits=1)

filterData[["culCom-112"]] <- filter(EEFdata,Characteristic=="Disability",Indicator%in%c("Discrimination"))
graphOptions[["culCom-112"]] <- list(graphType="barChart3",
                                     digits=1)
filterData[["disability-4"]] <- filterData[["culCom-112"]]
graphOptions[["disability-4"]] <- graphOptions[["culCom-112"]]

filterData[["culCom-113"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator%in%c("Discrimination"))
graphOptions[["culCom-113"]] <- list(graphType="barChart3",
                                     digits=1)
filterData[["ethnicity-4"]] <- filterData[["culCom-113"]]
graphOptions[["ethnicity-4"]] <- graphOptions[["culCom-113"]]

filterData[["culCom-114"]] <- filter(EEFdata,Characteristic=="Gender",Indicator%in%c("Discrimination"))
graphOptions[["culCom-114"]] <- list(graphType="barChart3",
                                     digits=1)

filterData[["culCom-115"]] <- filter(EEFdata,Characteristic=="Religion",Indicator%in%c("Discrimination"))
graphOptions[["culCom-115"]] <- list(graphType="barChart3",
                                     digits=1)
filterData[["religion-4"]] <- filterData[["culCom-115"]]
graphOptions[["religion-4"]] <- graphOptions[["culCom-115"]]

filterData[["culCom-116"]] <- filter(EEFdata,Characteristic=="Sexual Orientation",Indicator%in%c("Discrimination"))
graphOptions[["culCom-116"]] <- list(graphType="barChart3",
                                     digits=1)
filterData[["sexualOrientation-4"]] <- filterData[["culCom-116"]]
graphOptions[["sexualOrientation-4"]] <- graphOptions[["culCom-116"]]

filterData[["culCom-117"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator%in%c("Discrimination"))
graphOptions[["culCom-117"]] <- list(graphType="barChart3",
                                     digits=1)

filterData[["culCom-121"]] <- filter(EEFdata,Characteristic=="Age",Indicator%in%c("Strength of feeling of belonging to community"))
graphOptions[["culCom-121"]] <- list(graphType="barChart1",
                                     sortOrder="",
                                     digits=1,
                                     gvisOptions=list(isStacked=TRUE))
filterData[["age-4"]] <- filterData[["culCom-121"]]
graphOptions[["age-4"]] <- graphOptions[["culCom-121"]]

filterData[["culCom-123"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator%in%c("Strength of feeling of belonging to community"))
graphOptions[["culCom-123"]] <- list(graphType="barChart1",
                                     digits=1,
                                     gvisOptions=list(isStacked=TRUE))

filterData[["culCom-124"]] <- filter(EEFdata,Characteristic=="Gender",Indicator%in%c("Strength of feeling of belonging to community"))
graphOptions[["culCom-124"]] <- list(graphType="barChart1",
                                     digits=1,
                                     gvisOptions=list(isStacked=TRUE))
filterData[["gender-4"]] <- filterData[["culCom-124"]]
graphOptions[["gender-4"]] <- graphOptions[["culCom-124"]]

filterData[["culCom-127"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator%in%c("Strength of feeling of belonging to community"))
graphOptions[["culCom-127"]] <- list(graphType="barChart1",
                                     digits=1,
                                     gvisOptions=list(isStacked=TRUE))
filterData[["socioEconomic-4"]] <- filterData[["culCom-127"]]
graphOptions[["socioEconomic-4"]] <- graphOptions[["culCom-127"]]

filterData[["culCom-201"]] <- filter(EEFdata,Indicator=="Social attitudes",Characteristic=="Transgender")
graphOptions[["culCom-201"]] <- list(graphType="barChart0",
                                     colourPalette="blue",
                                     sortOrder="",
                                     gvisOptions=list(chartArea="{left: 350}"))
filterData[["transgender-4"]] <- filterData[["culCom-201"]]
graphOptions[["transgender-4"]] <- graphOptions[["culCom-201"]]

####Employability, Skills and Lifelong Learning####

filterData[["empSLL-1"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="% Higher education entrants",Characteristic=="Gender")
graphOptions[["empSLL-1"]] <- list(graphType="pieChart0",
                                   digits=1)
filterData[["gender-73"]] <- filterData[["empSLL-1"]] 
graphOptions[["gender-73"]] <- graphOptions[["empSLL-1"]] 

filterData[["empSLL-2"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Entrants - All levels","Entrants - Postgraduate","Entrants - First Degree","Entrants - Sub-Degrees"),Characteristic=="Age")
graphOptions[["empSLL-2"]] <- list(graphType="barChart2",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)
filterData[["age-73"]] <- filterData[["empSLL-2"]]
graphOptions[["age-73"]] <- graphOptions[["empSLL-2"]]

filterData[["empSLL-3"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="% of entrants to Scottish HEI from a BME background",Characteristic=="Ethnicity")
graphOptions[["empSLL-3"]] <- list(graphType="barChart1",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)
filterData[["ethnicity-73"]] <- filterData[["empSLL-3"]]
graphOptions[["ethnicity-73"]] <- graphOptions[["empSLL-3"]]

filterData[["empSLL-4"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="% Higher education entrants",Characteristic=="Disability")
graphOptions[["empSLL-4"]] <- list(graphType="timeSeries0",
                                   intervalType="Academic",
                                   digits=0)
filterData[["disability-73"]] <- filterData[["empSLL-4"]]
graphOptions[["disability-73"]] <- graphOptions[["empSLL-4"]]

filterData[["empSLL-5"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="% Higher education entrants",Characteristic=="Socio-Economic Status")
graphOptions[["empSLL-5"]] <- list(graphType="barChart1",
                                   isStacked=FALSE,
                                   digits=1,
                                   sortOrder=NA)
filterData[["socioEconomic-73"]] <- filterData[["empSLL-5"]]
graphOptions[["socioEconomic-73"]] <- graphOptions[["empSLL-5"]]

filterData[["empSLL-6"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Qualifiers - All levels","Qualifiers - Postgraduate","Qualifiers - First Degree","Qualifiers - Sub-Degrees"),Characteristic=="Age")
graphOptions[["empSLL-6"]] <- list(graphType="barChart2",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)

filterData[["empSLL-7"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Number of qualifiers"),Characteristic=="Ethnicity")
graphOptions[["empSLL-7"]] <- list(graphType="barChart1",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)

filterData[["empSLL-8"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Qualifiers - All","Qualifiers - Part-time","Qualifiers - Full-time"),Characteristic=="Gender")
graphOptions[["empSLL-8"]] <- list(graphType="barChart2",
                                   gvisOptions=list(isStacked=TRUE),
                                   digits=1,
                                   sortOrder=NA)

filterData[["empSLL-9"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="% qualifiers",Characteristic=="Socio-Economic Status")
graphOptions[["empSLL-9"]] <- list(graphType="barChart1",
                                   isStacked=FALSE,
                                   digits=1,
                                   sortOrder=NA)

filterData[["empSLL-10"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="Number of full-time further education entrants",Characteristic=="Age")
graphOptions[["empSLL-10"]] <- list(graphType="barChart1",
                                    gvisOptions=list(isStacked=TRUE),
                                    digits=1,
                                    sortOrder=NA)

filterData[["empSLL-11"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="% further education entrants",Characteristic=="Disability")
graphOptions[["empSLL-11"]] <- list(graphType="barChart1",
                                    graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"empSLL-11"],
                                    isStacked=FALSE,
                                    digits=1,
                                    sortOrder=NA)

filterData[["empSLL-12"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="% further education entrants",Characteristic=="Ethnicity")
graphOptions[["empSLL-12"]] <- list(graphType="barChart1",
                                    isStacked=FALSE,
                                    digits=1,
                                    sortOrder=NA)

filterData[["empSLL-13"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",grepl("% .*further education entrants",Indicator),Characteristic=="Gender")
graphOptions[["empSLL-13"]] <- list(graphType="barChart2",
                                    isStacked=FALSE,
                                    digits=1,
                                    sortOrder=NA)

filterData[["empSLL-14"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",grepl("% .*further education entrants",Indicator),Characteristic=="Socio-Economic Status")
graphOptions[["empSLL-14"]] <- list(graphType="barChart3",
                                    isStacked=FALSE,
                                    digits=1,
                                    sortOrder=NA)

filterData[["empSLL-15"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="Number of Modern Apprenticeships",Characteristic=="Age")
graphOptions[["empSLL-15"]] <- list(graphType="barChart1",
                                    graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"empSLL-15"],
                                    gvisOptions=list(isStacked=TRUE),
                                    digits=0,
                                    sortOrder=NA)

filterData[["empSLL-16"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="Number of Modern Apprenticeships",Characteristic=="Disability") 
graphOptions[["empSLL-16"]] <- list(graphType="timeSeries0",
                                    intervalType="Academic",
                                    digits=0)

filterData[["empSLL-17"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="Number of Modern Apprenticeships",Characteristic=="Ethnicity") 
graphOptions[["empSLL-17"]] <- list(graphType="timeSeries0",
                                    intervalType="Academic",
                                    digits=0)

filterData[["empSLL-18"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="Number of Modern Apprenticeships",Characteristic=="Gender") 
graphOptions[["empSLL-18"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=0)

filterData[["empSLL-19"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participation Rate"),Characteristic=="Ethnicity")
graphOptions[["empSLL-19"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)
filterData[["ethnicity-72"]] <- filterData[["empSLL-19"]]
graphOptions[["ethnicity-72"]] <- graphOptions[["empSLL-19"]]

filterData[["empSLL-20"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Ethnicity")
graphOptions[["empSLL-20"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

filterData[["empSLL-21"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participation Rate"),Characteristic=="Gender")
graphOptions[["empSLL-21"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)
filterData[["gender-72"]] <- filterData[["empSLL-21"]]
graphOptions[["gender-72"]] <- graphOptions[["empSLL-21"]]

filterData[["empSLL-22"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Gender")
graphOptions[["empSLL-22"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

filterData[["empSLL-23"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participation Rate"),Characteristic=="Socio-Economic Status")
graphOptions[["empSLL-23"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)
filterData[["socioEconomic-11"]] <- filterData[["empSLL-23"]]
graphOptions[["socioEconomic-11"]] <- graphOptions[["empSLL-23"]]
filterData[["socioEconomic-72"]] <- filterData[["empSLL-23"]]
graphOptions[["socioEconomic-72"]] <- graphOptions[["empSLL-23"]]

filterData[["empSLL-24"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Socio-Economic Status")
graphOptions[["empSLL-24"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

filterData[["empSLL-25"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participation Rate"),Characteristic=="Age")
graphOptions[["empSLL-25"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)
filterData[["age-72"]] <- filterData[["empSLL-25"]]
graphOptions[["age-72"]] <- graphOptions[["empSLL-25"]]

filterData[["empSLL-26"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Age")
graphOptions[["empSLL-26"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

filterData[["empSLL-27"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator=="Number of Modern Apprenticeships",Characteristic=="Socio-Economic Status")
graphOptions[["empSLL-27"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=0)

filterData[["empSLL-28"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participation Rate"),Characteristic=="Disability")
graphOptions[["empSLL-28"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    digits=1)

filterData[["empSLL-29"]] <- filter(EEFdata,policy_area=="Employability, Skills and Lifelong Learning",Indicator%in%c("Participating in Employment","Participating in Education"),Characteristic=="Disability")
graphOptions[["empSLL-29"]] <- list(graphType="timeSeries2",
                                    intervalType="Academic",
                                    digits=1)

####Health, Social Care and Sport####

# filterData[["health-9"]] <- filter(ODPdata,policy_area=="Health, Social Care and Sport",Indicator=="Smoking - Scottish Survey Core Questions")
# graphOptions[["health-9"]] <- list(graphType="timeSeries1",
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
# filterData[["health-90"]] <- filter(ODPdata,policy_area=="Health, Social Care and Sport",Indicator=="Mental Wellbeing - Scottish Surveys Core Questions")
# graphOptions[["health-90"]] <- list(graphType="timeSeries0",
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

filterData[["health-6a"]] <- filter(NPFdata,Indicator=="Physical activity",Characteristic=="Age")
graphOptions[["health-6a"]] <- list(graphType="timeSeries1",
                                    NPFindicator="Physical Activity",
                                    digits=1)
#filterData[["health-7"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of long stay care home residents aged 65+",Characteristic=="Age")
#graphOptions[["health-7"]] <- list(graphType="pieChart0")
filterData[["health-7"]] <- filter(ODPdata,policy_area=="Health, Social Care and Sport",Indicator%in%"Care Homes: Demographic Characteristics of Residents",Characteristic=="Age")
graphOptions[["health-7"]] <- list(graphType="timeSeries2",
                                   query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                   SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
                                   WHERE {
                                   ?obs 
                                   <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
                                   <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
                                   <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
                                   
                                   <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/care-homes-demography> ;
                                   <http://statistics.gov.scot/def/dimension/sex> <http://statistics.gov.scot/def/concept/sex/all> ;
                                   <http://purl.org/linked-data/cube#dataSet> ?i ;
                                   <http://statistics.gov.scot/def/dimension/mainClientGroupInCareHome> ?m ;
                                   <http://statistics.gov.scot/def/dimension/age> ?b ;
                                   <http://statistics.gov.scot/def/measure-properties/percent> ?Figure.
                                   
                                   <http://statistics.gov.scot/data/care-homes-demography> 
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

filterData[["health-14"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of home care clients with a physical disability",Characteristic=="Disability")
graphOptions[["health-14"]] <- list(graphType="pieChart0")

filterData[["health-21"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%c("% of direct payment clients","% of home care clients"),Characteristic=="Ethnicity")
graphOptions[["health-21"]] <- list(graphType="barChart3",maxZoom=c(0,100))

filterData[["health-27a"]] <- filter(NPFdata,Indicator=="Physical activity",Characteristic=="Gender")
graphOptions[["health-27a"]] <- list(graphType="timeSeries1",
                                     NPFindicator="Physical Activity",
                                     graphTitle=EEFindex$subtitle[EEFindex$tabUID%in%"health-6a"],
                                     digits=1)

#filterData[["health-28"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of long stay care home residents",Characteristic=="Gender")
#graphOptions[["health-28"]] <- list(graphType="barChart1",maxZoom=c(0,100))
filterData[["health-28"]] <- filter(ODPdata,policy_area=="Health, Social Care and Sport",Indicator%in%"% of long stay care home residents",Characteristic=="Gender")
graphOptions[["health-28"]] <- list(graphType="timeSeries2",maxZoom=c(0,100),
                                    query="PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                    SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
                                    WHERE {
                                    ?obs 
                                    <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
                                    <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
                                    <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
                                    
                                    <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/care-homes-demography> ;
                                    <http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/18-and-over> ;
                                    <http://purl.org/linked-data/cube#dataSet> ?i ;
                                    <http://statistics.gov.scot/def/dimension/mainClientGroupInCareHome> ?m ;
                                    <http://statistics.gov.scot/def/dimension/sex> ?b ;
                                    <http://statistics.gov.scot/def/measure-properties/percent> ?Figure.
                                    
                                    <http://statistics.gov.scot/data/care-homes-demography> 
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



filterData[["health-60"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of social care at home clients aged 65+",Characteristic=="Age")
graphOptions[["health-60"]] <- list(graphType="pieChart0")

filterData[["health-66"]] <- filter(EEFlatest,policy_area=="Health, Social Care and Sport",Indicator%in%"% of social care at home clients",Characteristic=="Gender")
graphOptions[["health-66"]] <- list(graphType="barChart1",maxZoom=c(0,100))

####Income and Poverty####

filterData[["incPov-1"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="Relative poverty: single working-age adult")
graphOptions[["incPov-1"]] <- list(graphType="timeSeries2",
                                   intervalType="3 year",
                                   NPFindicator="Relative poverty after housing costs")
filterData[["gender-121"]] <- filterData[["incPov-1"]]
graphOptions[["gender-121"]] <- graphOptions[["incPov-1"]]
filterData[["gender-112"]] <- filterData[["incPov-1"]]
graphOptions[["gender-112"]] <- graphOptions[["incPov-1"]]

filterData[["incPov-3"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="Relative poverty: single pensioners")
graphOptions[["incPov-3"]] <- list(graphType="timeSeries2",
                                   intervalType="3 year",
                                   #xFormat="(year-3)+'-'+year.toString().substr(2,2);",
                                   NPFindicator="Relative poverty after housing costs")
filterData[["gender-123"]] <- filterData[["incPov-3"]]
graphOptions[["gender-123"]] <- graphOptions[["incPov-3"]]

filterData[["incPov-4"]] <- filter(EEFlatest,Characteristic=="Ethnicity",SubBreakdown%in%NA,Indicator%in%c("% in relative poverty AHC","% in relative poverty BHC"))
graphOptions[["incPov-4"]] <- list(graphType="barChart2",
                                   NPFindicator="Relative poverty after housing costs")
filterData[["ethnicity-121"]] <- filterData[["incPov-4"]]
graphOptions[["ethnicity-121"]] <- graphOptions[["incPov-4"]]

filterData[["incPov-4a"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("% in relative poverty after housing costs 2007-17","% in relative poverty before housing costs 2007-17"))
graphOptions[["incPov-4a"]] <- list(graphType="barChart2",
                                    NPFindicator="Relative poverty after housing costs",
                                    gvisOptions=list(chartArea="{left: '35%', width: '100%', bottom:50}"))

filterData[["incPov-5"]] <- filter(ODPdata,Characteristic=="Age",Indicator=="Poverty")
graphOptions[["incPov-5"]] <- list(graphType="timeSeries2",
                                   #updateRmd=T,
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
filterData[["age-12"]] <- filterData[["incPov-5"]]
graphOptions[["age-12"]] <- graphOptions[["incPov-5"]]

filterData[["incPov-6"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="Relative poverty")
graphOptions[["incPov-6"]] <- list(graphType="timeSeries2",
                                   intervalType="3 year",
                                   #xFormat="(year-3)+'-'+year.toString().substr(2,2);",
                                   NPFindicator="Relative poverty after housing costs")
filterData[["disability-12"]] <- filterData[["incPov-6"]]
graphOptions[["disability-12"]] <- graphOptions[["incPov-6"]]

filterData[["incPov-7"]] <- filter(EEFdata,Characteristic=="Gender",Indicator%in%c("Gender Pay Gap"))
graphOptions[["incPov-7"]] <- list(graphType="timeSeries1",
                                   NPFindicator="Pay gap",
                                   digits=1,
                                   axisMonth=0)
filterData[["gender-91"]] <- filterData[["incPov-7"]]
graphOptions[["gender-91"]] <- graphOptions[["incPov-7"]]
filterData[["labSoc-10"]] <- filterData[["incPov-7"]]
graphOptions[["labSoc-10"]] <- graphOptions[["incPov-7"]]

filterData[["incPov-11"]] <- filter(EEFdata,Characteristic=="Age",Indicator%in%c("Living Wage"))
graphOptions[["incPov-11"]] <- list(graphType="timeSeries1",
                                    digits=0,
                                    axisMonth=0,
                                    ylabel="% of Employees")
filterData[["age-92"]] <- filterData[["incPov-11"]]
graphOptions[["age-92"]] <- graphOptions[["incPov-11"]]
filterData[["labSoc-12"]] <- filterData[["incPov-11"]]
graphOptions[["labSoc-12"]] <- graphOptions[["incPov-11"]]

filterData[["incPov-12"]] <- filter(EEFdata,Characteristic=="Age",Indicator%in%c("Managing well financially"))
graphOptions[["incPov-12"]] <- list(graphType="timeSeries1",
                                    digits=0)

filterData[["incPov-13"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="Relative poverty excluding disability living costs")
graphOptions[["incPov-13"]] <- list(graphType="timeSeries2",
                                    intervalType="4 year")
#xFormat="(year-3)+'-'+year.toString().substr(2,2);")
filterData[["disability-122"]] <- filterData[["incPov-13"]]
graphOptions[["disability-122"]] <- graphOptions[["incPov-13"]]

filterData[["incPov-17"]] <- filter(EEFdata,policy_area=="Income and Poverty",Indicator=="Asylum seekers",Characteristic=="Ethnicity")
graphOptions[["incPov-17"]] <- list(graphType="timeSeries0",
                                    digits=0,
                                    ylabel="Asylum Seekers")

filterData[["incPov-18"]] <- filter(EEFlatest,Characteristic=="Religion",Indicator%in%c("% in relative poverty AHC","% in relative poverty BHC"))
graphOptions[["incPov-18"]] <- list(graphType="barChart2"
)
filterData[["religion-121"]] <- filterData[["incPov-18"]]
graphOptions[["religion-121"]] <- graphOptions[["incPov-18"]]

filterData[["incPov-22"]] <- filter(EEFdata,Characteristic=="Gender",Indicator%in%c("Managing well financially"))
graphOptions[["incPov-22"]] <- list(graphType="timeSeries1",
                                    digits=0)

filterData[["incPov-23"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator%in%c("Managing well financially"))
graphOptions[["incPov-23"]] <- list(graphType="timeSeries1",
                                    digits=0)

####Local Government & Third Sector####

filterData[["locgov-1"]] <- filter(NPFdata,Characteristic=="Gender",Indicator%in%c("Quality of public services","Influence over local decisions")) %>%
  mutate(Breakdown=ifelse(Breakdown=="Male","Men",Breakdown),
         Breakdown=ifelse(Breakdown=="Female","Women",Breakdown))
graphOptions[["locgov-1"]] <- list(graphType="timeSeries2",
                                   NPFindicator=c("Quality of public services","Influence over local decisions"),
                                   digits=0)

#filterData[["locgov-2"]] <- filter(NPFextra,Characteristic=="Age",Measure%in%c("Quality of public services","Influence over local decisions"))
filterData[["locgov-2"]] <- filter(NPFdata,Characteristic=="Age",Indicator%in%c("Quality of public services","Influence over local decisions"))
  graphOptions[["locgov-2"]] <- list(graphType="timeSeries2",
                                     NPFindicator=c("Quality of public services","Influence over local decisions"),
                                     digits=0)

#filterData[["locgov-3"]] <- filter(NPFextra,Characteristic=="Disability",Measure%in%c("Quality of public services","Influence over local decisions"))
filterData[["locgov-3"]] <- filter(NPFdata,Characteristic=="Disability",Indicator%in%c("Quality of public services","Influence over local decisions")) 
graphOptions[["locgov-3"]] <- list(graphType="timeSeries2",
                                   NPFindicator=c("Quality of public services","Influence over local decisions"),
                                   digits=0)

#filterData[["locgov-5"]] <- filter(NPFextra,Characteristic=="Ethnicity",Measure%in%c("Quality of public services","Influence over local decisions"))
filterData[["locgov-5"]] <- filter(NPFdata,Characteristic=="Ethnicity",Indicator%in%c("Quality of public services","Influence over local decisions")) %>%
  mutate(Breakdown=ifelse(Breakdown=="Other minority ethnic","Minority Ethnic",Breakdown))
graphOptions[["locgov-5"]] <- list(graphType="timeSeries2",
                                   NPFindicator=c("Quality of public services","Influence over local decisions"),
                                   digits=0)

#filterData[["locgov-6"]] <- filter(NPFextra,Characteristic=="Socio-Economic Status",Measure%in%c("Quality of public services","Influence over local decisions"))
filterData[["locgov-6"]] <- filter(NPFdata,Characteristic=="Socio-Economic Status",Indicator%in%c("Quality of public services","Influence over local decisions")) 
graphOptions[["locgov-6"]] <- list(graphType="timeSeries2",
                                   NPFindicator=c("Quality of public services","Influence over local decisions"),
                                   digits=0)


####Rural and Environment####

filterData[["ruralenv-1"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Age",Indicator=="% of Midyear Population Estimate")
graphOptions[["ruralenv-1"]] <- list(graphType="barChart1",
                                     gvisOptions=list(isStacked="percent",height=400),
                                     colourPalette="diverging")

filterData[["ruralenv-2"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Age",Indicator=="% Adults Visiting Outdoors at least once a week") #%>% mutate(SubBreakdown=Year)
graphOptions[["ruralenv-2"]] <- list(graphType="timeSeries1",
                                     NPFindicator="Visits to the outdoors",
                                     digits=1)
filterData[["age-81"]] <- filterData[["ruralenv-2"]] 
graphOptions[["age-81"]] <- graphOptions[["ruralenv-2"]] 

filterData[["ruralenv-3"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Age",Indicator=="% Adults Believing Climate Change is Immediate and Urgent Problem") #%>% mutate(SubBreakdown=Year)
graphOptions[["ruralenv-3"]] <- list(graphType="timeSeries1",
                                     digits=1)
filterData[["age-82"]] <- filterData[["ruralenv-3"]]
graphOptions[["age-82"]] <- graphOptions[["ruralenv-3"]]

filterData[["ruralenv-5"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Disability",Indicator=="% Adults Visiting Outdoors")
graphOptions[["ruralenv-5"]] <- list(graphType="barChart1",
                                     NPFindicator="Visits to the outdoors",
                                     gvisOptions=list(isStacked="percent",chartArea="{left: '35%', width: '100%', bottom:50}"),
                                     colourPalette="blue",
                                     sortOrder=NA)
filterData[["disability-81"]] <- filterData[["ruralenv-5"]]
graphOptions[["disability-81"]] <- graphOptions[["ruralenv-5"]]

filterData[["ruralenv-6"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Disability",Indicator=="% Adults Believing Climate Change is Immediate and Urgent Problem") #%>% mutate(SubBreakdown=Year)
graphOptions[["ruralenv-6"]] <- list(graphType="timeSeries1",
                                     digits=1)
filterData[["disability-82"]] <- filterData[["ruralenv-6"]]
graphOptions[["disability-82"]] <- graphOptions[["ruralenv-6"]]

filterData[["ruralenv-7"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Ethnicity",Indicator=="Ethnicity by Urban Rural Classification - Household Reference Person")
graphOptions[["ruralenv-7"]] <- list(graphType="barChart1",
                                     colourPalette="diverging",
                                     gvisOptions=list(height="1000px",isStacked="percent"))
filterData[["ethnicity-8"]] <- filterData[["ruralenv-7"]]
graphOptions[["ethnicity-8"]] <- graphOptions[["ruralenv-7"]]

filterData[["ruralenv-8"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Gender",Indicator=="Life Expectancy by Gender and Urban Rural")
graphOptions[["ruralenv-8"]] <- list(graphType="barChart3",
                                     digits=1,
                                     colourPalette="blue",
                                     sortOrder=NA)

filterData[["ruralenv-9"]] <- filter(EEFlatest,policy_area=="Rural and Environment",Characteristic=="Gender",Indicator=="% Adults Visiting Outdoors")
graphOptions[["ruralenv-9"]] <- list(graphType="barChart1",
                                     gvisOptions=list(isStacked="percent"),
                                     colourPalette="blue",
                                     NPFindicator="Visits to the outdoors")
filterData[["gender-81"]] <- filterData[["ruralenv-9"]]
graphOptions[["gender-81"]] <- graphOptions[["ruralenv-9"]]

filterData[["ruralenv-11"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Religion",Indicator=="% Adults belonging to no religion") #%>% mutate(SubBreakdown=Year)
graphOptions[["ruralenv-11"]] <- list(graphType="timeSeries1",
                                      digits=1)
filterData[["religion-8"]] <- filterData[["ruralenv-11"]]
graphOptions[["religion-8"]] <- graphOptions[["ruralenv-11"]]

filterData[["ruralenv-13"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Socio-Economic Status",Indicator=="% Adults Visiting Outdoors at least once a week") #%>% mutate(SubBreakdown=Year)
graphOptions[["ruralenv-13"]] <- list(graphType="timeSeries1",
                                      NPFindicator="Visits to the outdoors",
                                      digits=1)

filterData[["ruralenv-14"]] <- filter(EEFdata,policy_area=="Rural and Environment",Characteristic=="Socio-Economic Status",Indicator=="% Adults living 11 minutes or more walk from nearest green or blue space") #%>% mutate(SubBreakdown=Year)
graphOptions[["ruralenv-14"]] <- list(graphType="timeSeries1",
                                      digits=1)
filterData[["socioEconomic-8"]] <- filterData[["ruralenv-14"]]
graphOptions[["socioEconomic-8"]] <- graphOptions[["ruralenv-14"]]

####School Education####

filterData[["sch-Edu-1"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
graphOptions[["sch-Edu-1"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)
filterData[["gender-71"]] <- filterData[["sch-Edu-1"]]
graphOptions[["gender-71"]] <- graphOptions[["sch-Edu-1"]]

filterData[["sch-Edu-2"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
graphOptions[["sch-Edu-2"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

filterData[["sch-Edu-3"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
graphOptions[["sch-Edu-3"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

filterData[["sch-Edu-4"]] <- filter(EEFdata,Characteristic=="Gender",Indicator=="% school leavers in a positive follow-up destination")
graphOptions[["sch-Edu-4"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

filterData[["sch-Edu-5"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
graphOptions[["sch-Edu-5"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)
filterData[["ethnicity-71"]] <- filterData[["sch-Edu-5"]]
graphOptions[["ethnicity-71"]] <- graphOptions[["sch-Edu-5"]]

filterData[["sch-Edu-6"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
graphOptions[["sch-Edu-6"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

filterData[["sch-Edu-7"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
graphOptions[["sch-Edu-7"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

filterData[["sch-Edu-8"]] <- filter(EEFdata,Characteristic=="Ethnicity",Indicator=="% school leavers in a positive follow-up destination")
graphOptions[["sch-Edu-8"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)

filterData[["sch-Edu-9"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
graphOptions[["sch-Edu-9"]] <- list(graphType="timeSeries1",
                                    intervalType="Academic",
                                    ylabel="% School Leavers",
                                    digits=1)
filterData[["socioEconomic-71"]] <- filterData[["sch-Edu-9"]]
graphOptions[["socioEconomic-71"]] <- graphOptions[["sch-Edu-9"]]

filterData[["sch-Edu-10"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
graphOptions[["sch-Edu-10"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

filterData[["sch-Edu-11"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
graphOptions[["sch-Edu-11"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

filterData[["sch-Edu-12"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Indicator=="% school leavers in a positive follow-up destination")
graphOptions[["sch-Edu-12"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

filterData[["sch-Edu-13"]] <- filter(EEFdata,Indicator=="Teachers (headcount)",Characteristic=="Age") %>% 
  mutate(DateString=paste(sep="",as.character(2000+as.numeric(Measure)),"-01-01"),Date=as.Date(DateString)) #A hacky way of displaying line graphs with integer x-axis values by converting values to a dummy date (and converting back in the axis formatter)
graphOptions[["sch-Edu-13"]] <- list(graphType="timeSeries1",
                                     xFormat=list(xAxisFormat="year.toString().substr(2,2);",
                                                  xValueFormat="'Aged '+(year.toString().substr(2,2));"),
                                     colourPalette="blue",
                                     ylabel="Teachers (FTE)",
                                     minZoom=c(1000,NA),
                                     digits=0)
filterData[["sch-Edu-14"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator=="Pupil ethnicity")
graphOptions[["sch-Edu-14"]] <- list(graphType="barChart1",
                                     defaultSelected="Total Pupils",
                                     digits=1)

filterData[["sch-Edu-15"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator=="Teacher ethnicity")
graphOptions[["sch-Edu-15"]] <- list(graphType="barChart1",
                                     digits=1)

filterData[["sch-Edu-21"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
graphOptions[["sch-Edu-21"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)
filterData[["disability-71"]] <- filterData[["sch-Edu-21"]]
graphOptions[["disability-71"]] <- graphOptions[["sch-Edu-21"]]

filterData[["sch-Edu-22"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
graphOptions[["sch-Edu-22"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

filterData[["sch-Edu-23"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
graphOptions[["sch-Edu-23"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)

filterData[["sch-Edu-24"]] <- filter(EEFdata,Characteristic=="Disability",Indicator=="% school leavers in a positive follow-up destination")
graphOptions[["sch-Edu-24"]] <- list(graphType="timeSeries1",
                                     intervalType="Academic",
                                     ylabel="% School Leavers",
                                     digits=1)


filterData[["sch-Edu-41"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
graphOptions[["sch-Edu-41"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-42"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("P4 - Reading","P4 - Writing","P4 - Listening & Talking","P4 - Numeracy"))
graphOptions[["sch-Edu-42"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-43"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("P7 - Reading","P7 - Writing","P7 - Listening & Talking","P7 - Numeracy"))
graphOptions[["sch-Edu-43"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-44"]] <- filter(EEFlatest,Characteristic=="Gender",Indicator%in%c("S3 - Reading","S3 - Writing","S3 - Listening & Talking","S3 - Numeracy"))
graphOptions[["sch-Edu-44"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-45"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
graphOptions[["sch-Edu-45"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-46"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("P4 - Reading","P4 - Writing","P4 - Listening & Talking","P4 - Numeracy"))
graphOptions[["sch-Edu-46"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-47"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("P7 - Reading","P7 - Writing","P7 - Listening & Talking","P7 - Numeracy"))
graphOptions[["sch-Edu-47"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-48"]] <- filter(EEFlatest,Characteristic=="Disability",Indicator%in%c("S3 - Reading","S3 - Writing","S3 - Listening & Talking","S3 - Numeracy"))
graphOptions[["sch-Edu-48"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-49"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
graphOptions[["sch-Edu-49"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-50"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("P4 - Reading","P4 - Writing","P4 - Listening & Talking","P4 - Numeracy"))
graphOptions[["sch-Edu-50"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-51"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("P7 - Reading","P7 - Writing","P7 - Listening & Talking","P7 - Numeracy"))
graphOptions[["sch-Edu-51"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-52"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Indicator%in%c("S3 - Reading","S3 - Writing","S3 - Listening & Talking","S3 - Numeracy"))
graphOptions[["sch-Edu-52"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-53"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
graphOptions[["sch-Edu-53"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-54"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("P4 - Reading","P4 - Writing","P4 - Listening & Talking","P4 - Numeracy"))
graphOptions[["sch-Edu-54"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-55"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("P7 - Reading","P7 - Writing","P7 - Listening & Talking","P7 - Numeracy"))
graphOptions[["sch-Edu-55"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

filterData[["sch-Edu-56"]] <- filter(EEFlatest,Characteristic=="Socio-Economic Status",Indicator%in%c("S3 - Reading","S3 - Writing","S3 - Listening & Talking","S3 - Numeracy"))
graphOptions[["sch-Edu-56"]] <- list(graphType="pieChart2",
                                     updateRmd=TRUE,
                                     icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"),
                                     digits=1)

####Transport and Travel####

filterData[["TravAge-1"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Percentage travelling"))
graphOptions[["TravAge-1"]] <- list(graphType="barChart1",
                                    sortOrder=NA,
                                    digits=0)

filterData[["TravAge-1a"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",grepl("Travel to Work:",Indicator))
graphOptions[["TravAge-1a"]] <- list(graphType="barChart2",
                                     sortOrder=NA,
                                     digits=0)

filterData[["TravAge-3"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Road accidents","Casualties"))
graphOptions[["TravAge-3"]] <- list(graphType="timeSeries2")

filterData[["TravAge-4"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Bus Use","Train Use"))
graphOptions[["TravAge-4"]] <- list(graphType="barChart3",
                                    sortOrder=NA,
                                    digits=0)

filterData[["TravAge-5"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Concessionary Journeys"))
graphOptions[["TravAge-5"]] <- list(graphType="timeSeries0",
                                    digits=0)

filterData[["TravAge-6"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Age",Indicator%in%c("Concessionary Pass Use"))
graphOptions[["TravAge-6"]] <- list(graphType="barChart1",
                                    sortOrder=NA,
                                    colourPalette="blue",
                                    gvisOptions=list(isStacked="percent"))

filterData[["TravDis-2"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Bus Use"))
graphOptions[["TravDis-2"]] <- list(graphType="barChart1",
                                    sortOrder=NA,
                                    colourPalette="blue",
                                    gvisOptions=list(isStacked="percent"))

filterData[["TravDis-3a"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Blue badges"))
graphOptions[["TravDis-3a"]] <- list(graphType="timeSeries0",
                                     digits=0)

filterData[["TravDis-3b"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Concessionary Passes"))
graphOptions[["TravDis-3b"]] <- list(graphType="timeSeries1",
                                     digits=0)
filterData[["TravDis-3c"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Concessionary Journeys"))
graphOptions[["TravDis-3c"]] <- list(graphType="timeSeries0",
                                     digits=0)

filterData[["TravDis-5"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Indicator%in%c("Accessibility"))
graphOptions[["TravDis-5"]] <- list(graphType="timeSeries0",
                                    maxZoom=c(0,100),
                                    ylabel="Percentage of Buses",
                                    digits=0)

filterData[["TravGen-3"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Gender",Indicator%in%c("Road accidents","Casualties"))
graphOptions[["TravGen-3"]] <- list(graphType="timeSeries1",
                                    digits=0)

filterData[["TravGen-4"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Gender",Indicator%in%c("Walking","Cycling"))
graphOptions[["TravGen-4"]] <- list(graphType="barChart2",
                                    sortOrder=NA,
                                    colourPalette="blue",
                                    gvisOptions=list(isStacked="percent"))



filterData[["TravDis-3"]] <- filter(EEFdata,policy_area=="Transport and Travel",Characteristic=="Disability",Measure%in%c("Number of blue badges on issue","Concessionary fare passes issued to disabled people","Journeys made under the concessionary fare schemes (millions)","Percentage of journeys by concessionary passengers"))
graphOptions[["TravDis-3"]] <- list(graphType="timeSeries2",
                                    forceRescale=TRUE,
                                    digits=0)

