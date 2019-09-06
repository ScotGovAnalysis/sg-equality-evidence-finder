eefColours <- function(colour) {
  defaultColours <- c("blue","orange","green","magenta","purple","light-green")
  tolower(colour) %>%
    ifelse(is.na(.),defaultColours,.) %>%
    ifelse(.=="blue","#0068b3",.) %>%
    ifelse(.=="very-light-blue","#cceaff",.) %>%
    ifelse(.=="orange","#b34b00",.) %>%
    ifelse(.=="green","#68b300",.) %>%
    ifelse(.=="magenta","#b30068",.) %>%
    ifelse(.=="purple","#4b00b3",.) %>%
    ifelse(.=="light-green","#00b34b",.)
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
                                   empSLL="Employability, Skills and Lifelong Learning",
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
                                   "Summary")
                                   
#modified sliderInput with discrete categories
sliderInputLabels <- function(inputId,label,tickLabels,value,min=NULL,max=NULL,step=NULL,...) {
  args <- list(inputId=inputId,label=label,value=match(value,tickLabels)-1,min=1,max=length(tickLabels),step=1,...)
  html <- do.call('sliderInput', args)
  html$children[[2]]$attribs[['data-values']] <- paste0(tickLabels, collapse=',')
  return(html)
}

equalityCharacteristics <- c("Age","Disability","Ethnicity","Gender","Religion","Sexual Orientation","Transgender","Socio-Economic Status")
equalityCharacteristicsID <- c("age","disability","ethnicity","gender","religion","sexualOrientation","transgender","socioEconomicStatus")
allPolicyAreasID <- c("summ","busEnt","chiFam","criJus","cult","dem","empSLL","health","houReg","incPov","labMar","locGov","rurEnv","schEdu","thiSec","transp")
allPolicyAreasID <- c("summ","busEnt","chiFam","criJus","culCom","dem","empSLL","health","houReg","incPov","labSoc","locThi","rurEnv","schEdu","transp")
allPagesID <- c("home2","Approach","equality")


oldEEF <- list(
  busEnt=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeBusEntEn",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabBusEntEn",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthBusEntEn",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenBusEntEn",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelBusEntEn",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationBusEntEn",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgBusEntEn",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  chiFam=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeChYP",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabChYP",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthChYP",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenChYP",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelChYP",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationChYP",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgChYP",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  criJus=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeCrimeJust",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabCrimeJust",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthCrimeJust",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenCrimeJust",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelCrimeJust",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationCrimeJust",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgCrimeJust",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  culCom=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeTCS",
            "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabTCS",
            "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthTCS",
            "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenTCS",
            "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelTCS",
            "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationTCS",
            "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgTCS",
            "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  dem=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgePopMig",
           "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabPopMig",
           "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthPopMig",
           "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenPopMig",
           "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelPopMig",
           "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationPopMig",
           "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgPopMig",
           "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  empSLL=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeESLL",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabESLL",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthESLL",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenESLL",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelESLL",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationESLL",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgESLL",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  health=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeHealth",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabHealth",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthHealth",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenHealth",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelHealth",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationHealth",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgHealth",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  houReg=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeHousRegen",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabHousRegen",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthHousRegen",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenHousRegen",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelHousRegen",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationHousRegen",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgHousRegen",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  incPov=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgePov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabPov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthPov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenPov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelPov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationPov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgPov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  labSoc=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeLab",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabLab",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthLab",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenLab",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelLab",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationLab",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgLab",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  locThi=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeLocalGov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabLocalGov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthLocalGov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenLocalGov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelLocalGov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationLocalGov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgLocalGov",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  rurEnv=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeRuralEnv",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabRuralEnv",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthRuralEnv",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenRuralEnv",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelRuralEnv",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationRuralEnv",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgRuralEnv",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  schEdu=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeSchEd",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabSchEd",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthSchEd",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenSchEd",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelSchEd",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationSchEd",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgSchEd",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  ),
  
  transp=c("http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Age/AgeTransport",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Disability/DisabTransport",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Ethnicity/EthTransport",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Gender/GenTransport",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Religion/RelTransport",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/SexualOrientation/SOrientationTransport",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/Transgender/TransgTransport",
              "http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid/"
  )
  
)



#shinyjs bugfix - overwrites the shinyjs functions with the shinyjs 1.01 version
if(packageVersion("shinyjs") <= "1.0"){
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


# fileName="EEF Links Final.xlsx"
# init <- function() with(.GlobalEnv,{
#   EEFsources <- read_excel(fileName,skip=1,sheet=1) %>%
#     mutate(published_date=as.Date(published_date,origin="1900-1-1"),
#            published_year=ifelse(published_year%in%c(NA,"",0),as.numeric(substr(published_date,1,4)),published_year),
#            sort_date=ifelse(published_date%in%c(NA,"",0),ymd(paste0(published_year,"-1-1"),quiet=TRUE),published_date))
#   
#   EEFlatestSource <- 
#     EEFsources %>%
#     group_by(series_name) %>%
#     summarise(sort_date=max(sort_date,na.rm=TRUE)) %>%
#     merge(EEFsources)
#   
#   EEFpublished <- merge(EEFlatestSource,read_excel(fileName,skip=1,sheet=2))
#   
#   EEFdataLinks <- read_excel(fileName,skip=1,sheet=3)
# 
#   EEFexternal <- read_excel(fileName,skip=1,sheet=4)
# })

eefContact <- function(policy)  div(class="w3-row  w3-content",style="width:100%;",
                                    div(id=NS("equality","contact"),
                                        class="w3-content eef-text",
                                        includeMarkdown(file.path("EEF",policy,"Contact.md"))
                                    )
)

updatePublications <- function(theme,characteristic) {
  html <- character(0)
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

updateData <- function(theme,characteristic) {
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
    html_header <- "\n\n<h3>External Links</h3>\n\n<p class=\"info\">Please note that you will leave the Scottish Government web site by clicking on any of the following links, and that the Scottish Government and its staff are not responsible for content external to this web site. The research below has been carried out independently of the Scottish Government and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p>\n\n"
    html_text <- htmlData(match_data4)
    html <- c(html,html_header,html_text)
  }
  html <- paste(html,collapse="")
  return(html)
}

updateExternal <- function(theme,characteristic) {
  html <- character(0)
  
  match_external_pub <- EEFpublished[which(EEFpublished$policy_area==theme &
                                         EEFpublished$internal_external=="External" & 
                                         EEFpublished[,characteristic]>0),]
  
  match_external_pub <- arrange_(match_external_pub,paste0("`",characteristic,"`"),"desc(sort_date)","name")
  
  match_external_org <- EEFexternal[which(EEFexternal$policy_area==theme &
                                            EEFexternal[,characteristic]>0),]
  
 match_external_org <- arrange_(match_external_org,paste0("`",characteristic,"`"),"name")
  
  if(nrow(match_external_pub)>0|nrow(match_external_org)>0) {
    html <- c(html,"<p class=\"info\">Please note that you will leave the Scottish Government web site by clicking on any of the following links, and that the Scottish Government and its staff are not responsible for content external to this web site. The research below has been carried out independently of the Scottish Government and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p>")
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

htmlPublications <- function(eef_data) {
  eef_data <- mutate(eef_data,description=gsub("&(amp;)?","&amp;",description))
  eef_data <- mutate(eef_data,link=gsub("&(amp;)?","&amp;",link))
  eef_data <- mutate(eef_data,published_by=gsub("&","&amp;",published_by))
  eef_data <- mutate(eef_data,html_link=paste("<a target=\"_blank\" href=\"",link,"\">",name,"</a>",sep=""))
  eef_data <- mutate(eef_data,text_date=ifelse(published_date%in%c("",0,NA),published_year,format(as.Date(published_date,origin="1900-1-1"),"%B %Y")))
  eef_data <- mutate(eef_data,html_details=ifelse(published_by%in%c("",NA)&text_date%in%c("",NA)," - ",paste(" (",ifelse(published_by%in%c("",NA),"",published_by),ifelse(published_by%in%c("",NA)|text_date%in%c("",NA),"",", "),ifelse(text_date%in%c("",NA),"",text_date),") ",sep="")))
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

htmlData <- function(eef_data) {
  eef_data <- mutate(eef_data,description=gsub("&(amp;)?","&amp;",description))
  eef_data <- mutate(eef_data,link=gsub("&(amp;)?","&amp;",link))
  eef_data <- mutate(eef_data,published_by=gsub("&","&amp;",published_by))
  eef_data <- mutate(eef_data,html_link=paste("<a target=\"_blank\" href=\"",link,"\">",name,"</a>",sep=""))
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


htmlExternal <- function(eef_data) {
  eef_data <- mutate(eef_data,description=gsub("&(amp;)?","&amp;",description))
  eef_data <- mutate(eef_data,link=gsub("&(amp;)?","&amp;",link))
  eef_data <- mutate(eef_data,html_link=paste("<a target=\"_blank\" href=\"",link,"\">",name,"</a>",sep=""))
  eef_data <- mutate(eef_data,text_date=ifelse(published_date%in%c("",0,NA),published_year,format(as.Date(published_date,origin="1900-1-1"),"%B %Y")))
  eef_data <- mutate(eef_data,html_details=ifelse(published_by%in%c("",NA)&text_date%in%c("",NA)," - ",paste(" (",ifelse(published_by%in%c("",NA),"",published_by),ifelse(published_by%in%c("",NA)|text_date%in%c("",NA),"",", "),ifelse(text_date%in%c("",NA),"",text_date),") ",sep="")))
  eef_data <- mutate(eef_data,html=paste("<p>",html_link,html_details,ifelse(description%in%c("",NA),"",description),"</p>",sep=""))    
  html_text <- paste(eef_data$html,sep="",collapse="\n\n")
  return(html_text)
}



pubSource <- function(x,extra_details="",notes="") {
  html <- character(0)
  # pattern = paste("^(",paste(x,collapse=")|("),")$",sep="")
  # 
  # match_source <- filter(EEFlatestSource,grepl(pattern,series_name,ignore.case = TRUE))
  # if(nrow(match_source)==0) {
  #   match_source <- filter(EEFsources,grepl(pattern,name,ignore.case = TRUE))
  # }
  match_source <- filter(EEFlatestSource,series_name %in% x)
  if(nrow(match_source)==0) {
    match_source <- filter(EEFSources,name %in% x)
  }
  
  match_internal <- filter(match_source,internal_external=="Internal")
  
  if(nrow(match_internal)>0) {
    html_header <- "<p><strong>Source: </strong>"
    html_text <- htmlSource(match_internal,ifelse(tolower(match_internal$routine_adhoc)%in%"ad hoc","Published: ","Last updated: "),extra_details)
    if(!notes%in%c(NA,"")) {
      html_text <- c(html_text,paste0("\n\n<p><strong>Note: </strong>",notes,"</p>"))
    }
    html_footer <- "</p>\n<div>&nbsp;</div>"
    html <- c(html,html_header,html_text)
  }
  
  match_external <- filter(match_source,internal_external=="External")
  
  if(nrow(match_external)>0) {
    html_header <- "<p><strong>External Source: </strong>"
    html_text <- htmlSource(match_external,"Published: ",extra_details)
    html_footer <- "</p>\n<p class=\"info\">This research has been carried out independently of the Scottish Government, the results are hosted on an external website and the findings do not necessarily represent the views of the Scottish Government or Scottish Ministers</p><div>&nbsp;</div>"
    html <- c(html,html_header,html_text,html_footer)
  }

  html <- paste(html,collapse="")
  return(html)
}

htmlSource <- function(eef_data,prefix_date="Last updated: ",extra_details) {
  eef_data$extra_details <- extra_details
  eef_data$prefix_date <- prefix_date
  eef_data <- mutate(eef_data,link=gsub("&","&amp;",link))
  eef_data <- mutate(eef_data,published_by=gsub("&","&amp;",published_by))
  eef_data <- mutate(eef_data,html_links=paste("<a target='_blank' href=\"",link,"\">",name,"</a>",sep=""))
  eef_data <- mutate(eef_data,text_date=ifelse(published_date%in%c("",0,NA),published_year,format(as.Date(published_date,origin="1900-1-1"),"%B %Y")))
  eef_data <- mutate(eef_data,html_updated=paste(ifelse(text_date%in%c(NA,""),"",paste(prefix_date,text_date,sep="")),ifelse(published_by%in%c(NA,"")|text_date%in%c(NA,""),"",", "),ifelse(published_by%in%c(NA,""),"",published_by),sep=""))
  eef_data <- mutate(eef_data,details_updated=paste(ifelse(html_updated%in%c("",NA),"",html_updated),ifelse(html_updated%in%c(NA,"")|extra_details%in%c(NA,""),"",". "),ifelse(extra_details%in%c(NA,""),"",extra_details),sep=""))
  eef_data <- mutate(eef_data,html_details=ifelse(details_updated%in%c("",NA),"",paste(" (",details_updated,")",sep="")))
  
  html_text <- paste(ifelse(eef_data$link%in%c(NA,""),eef_data$name,eef_data$html_links),eef_data$html_details,sep="",collapse="<strong>, </strong>")
  return(html_text)
}

#init()

arrangeBreakdown <- function(dataset,col,...) {
  o <- order(tolower(dataset$Breakdown)%in%c("total","all"),dataset[[col]],...)
  dataset[o,]
}
  
