###################################################################################
#                                                                                 #
# SECTION CONTENT / DATA SOURCE LOOKUP                                            #
#                                                                                 #
###################################################################################
#                                                                                 #
# Author: Jay Ware                                                                #
# Created: 01/08/2018                                                             #
# Last updated: 14/01/2021                                                        #
#                                                                                 #
# Purpose: These functions are used to generate content for various sections of   #
#          the Evidence Finder. Currently used for contacts, and links sections   #
#                                                                                 #
# Functions:                                                                      #
#                                                                                 #
#  uiOuputLoading(): wrapper function for uiOutput to add a loading spinner       #
#  eefTooltip(): used in the grid to provide a tooltip listing the available      #
#                content                                                          #
#  eefContact(): reads in the contacts md file for each policy area               #
#  eefContactList(): content for the contacts section. Includes the Equality      #
#                    Analysis contact details, and a section for the specific     #
#                    policy area (all policy areas are included and we hide/show  #
#                    them depending on the "page" being viewed)                   #
#  updateGuidance(): updates the guidance section content based on policy area    #
#                    and equality characteristic                                  #
#  updateGlossary(): updates the glossary section content based on policy area    #
#                    and equality characteristic - NOT CURRENTLY USED             #
#  updatePublications(): updates the publication links section content based on   #
#                        policy area and equality characteristic                  #
#  htmlPublications(): generates the html from the filtered list of relevant      #
#                      publications                                               #
#  updateData(): updates the data links section content based on policy area and  #
#                equality characteristic                                          #
#  htmlData(): generates the html from the filtered list of relevant data links   #
#  updateExternal(): updates the external links section content based on policy   #
#                    area and equality characteristic                             #
#  htmlExternal(): generates the html from the filtered list of relevant external #
#                  data links                                                     #
#  getSource(): looks up a publication, first checking for a match in the series  #
#               name column. If it doesn't find a match, it then checks the name  #
#               column                                                            #
#  pubSource(): generates the data source text for the panels sections. Includes  #
#               data source name and link, last published date, external source   #
#               disclaimer and data source notes (as applicable). The data source #
#               text changes depending on the data source type                    #
#               (routine/ad hoc; internal/external; single/multiple sources)      #
#  htmlSource(): generates the html used in pubSource()                           #
#                                                                                 #
###################################################################################

#a hacky alternative to using shinycssloaders as couldn't get anything else to work...
uiOutputLoading <- function (outputId, inline = FALSE, container = if (inline) span else div,  ...) {
  options(warn=-1) #temporarily disable R warnings as Shiny will warn that the spinner gif will be replaced by content (when it is loaded)
  #html <- uiOutput(outputId,inline,container,style="position:relative;min-height:66px",img(src="icons/spinner.gif",style="position:absolute;margin-left:-33px;margin-top:-33px;left:50%;top:50%"))
  html <- uiOutput(outputId,inline,container,div(class="panel-load-container load1",div(class="loader","Loading")))

  options(warn=0)
  return(html)
}

#uiOutputLoading <- function(x) {withSpinner(uiOutput(x))}

eefTooltip <- function(policyArea,char) {
  contents <- filter(EEFindex,policy_area==gsub("&","and",policyArea),characteristic==char)$topic %>%
    unique %>%
    c("Publications & Data")
  paste0(policyArea,", ",char,paste0("\n - ",contents,collapse=""))
}


# Contacts panel ----------------------------------------------------------

eefContact <- function(p) {
  options(warn=-1) #temporarily disable R warnings as R will moan that we are not enforcing an obselete convention from decades ago regarding the "proper" way of ending a text file...
  #mdFile <- includeMarkdown(file.path("EEF",policyLabel(p),"Contact.md"))
  mdFile <- contact[[policyLabel(p)]]
  options(warn=0)
  column(6,id=NS(policyCSS(p),"contact"),
         class=paste0("eef-text eef eef-",p),
         mdFile)
}

eefContactList <- function(policy) {
  options(warn=-1)
  # mdFile <- includeMarkdown(file.path("EEF","Summary","Contact.md"))
  mdFile <- contact[["Summary"]]
  options(warn=0)
  fluidRow(class="w3-content",style="width:100%",
           column(6,id=NS("eef-main","contact"),class="eef-text",mdFile),
           shinyjs::hidden(lapply(policy,eefContact))
  )
}


# Guidance panel ----------------------------------------------------------

#filter EEFadditional dataset for "Data Collection" publication type and particular theme/characteristic selected
updateGuidance <- function(theme,characteristic) {
  if(theme=="") theme <- "Summary"
  html <- character(0)
  match_guidance <- EEFadditional[which(EEFadditional$publication_type=="Data Collection Guidance" & 
                                          EEFadditional[,characteristic]>0),]
  
  #match_guidance <- arrange_(match_guidance,paste0("`",characteristic,"`"),"name")
  match_guidance <- arrange(match_guidance, .data[[characteristic]], name)
  
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
  
  #match_glossary <- arrange_(match_glossary,paste0("`",characteristic,"`"),"name")
  match_glossary <- arrange(match_glossary, .data[[characteristic]], name)
  
  if(nrow(match_glossary)>0) {
    readLines("data/glossary.rmd",warn=FALSE) %>%
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


# Publications panel ------------------------------------------------------

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
  
  #match_internal <- arrange_(match_internal,paste0("`",characteristic,"`"),"desc(sort_date)","name")
  match_internal <- arrange(match_internal, .data[[characteristic]], desc(sort_date), name)
  
  if(nrow(match_internal)>0) {
    html_header <- "<h3>Publications and Outputs</h3>\n\n"
    html_text <- htmlPublications(match_internal)
    html <- c(html,html_text)
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

# Data panel --------------------------------------------------------------

#filter EEFdataLinks dataset for particular theme/characteristic selected
#raw html is used here - using R markdown would have been simpler
updateData <- function(theme,characteristic) {
  if(theme=="") theme <- "Summary"
  html <- character(0)
  match_data <- EEFdataLinks[which(EEFdataLinks$policy_area==theme &
                                     EEFdataLinks[,characteristic]>0),]
  
  #match_data <- arrange_(match_data,characteristic,"name")
  match_data <- arrange(match_data, .data[[characteristic]], name)
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

# External panel ----------------------------------------------------------

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
  
  #match_external_pub <- arrange_(match_external_pub,paste0("`",characteristic,"`"),"desc(sort_date)","name")
  match_external_pub <- arrange(match_external_pub, .data[[characteristic]], desc(sort_date), name)
  
  match_external_org <- EEFexternal[which(EEFexternal$policy_area==theme &
                                            EEFexternal[,characteristic]>0),]
  
  #match_external_org <- arrange_(match_external_org,paste0("`",characteristic,"`"),"name")
  match_external_org <- arrange(match_external_org, .data[[characteristic]], name)
  
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


# Data source links -------------------------------------------------------

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
  if(!is.null(notes)) notes <- paste0("Additional analysis and methodology details can be found in the source publication\n",notes[!notes%in%c("",NA)])
  
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
    
    if(!is.null(notes)) if(!notes%in%c(NA,"")) {
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
    
    if(!is.null(notes)) if(!notes%in%c(NA,"")) {
      notes <- sub("^\\s*Notes?:?\\s?","",notes) #ignore "Note:" if entered in the spreadsheet text
      notes <- strsplit(notes,"\n|\r")[[1]] #treat new lines in excel cell as seperate (numbered) notes
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
    if(!is.null(notes)) if(!notes%in%c(NA,"")) {
      notes <- sub("^\\s*Notes?:?\\s?","",notes) #ignore "Note:" if entered in the spreadsheet text
      notes <- strsplit(notes,"\n|\r")[[1]] #treat new lines in excel cell as seperate (numbered) notes
      notes <- notes[grepl("\\S",notes)] #ignore blank lines
      if(length(notes)>1) html_text <- c(html_text,paste0("\n\n<p><strong>Notes: </strong>",
                                                          "<ol>",paste0("<li>",notes,"</li>",collapse=""),"</ol>",
                                                          "</p>"))
      if(length(notes)==1) html_text <- c(html_text,paste0("\n\n<p><strong>Note: </strong>",notes,"</p>"))
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


# Link to scotxed ---------------------------------------------------------

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

