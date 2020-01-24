##EEF main page
eefHomePage <-  function(id="equality") {
  ns <- NS(id)
   tagList(
          eefHomeHeader(id),
          eefGrid(id),
          br(),
          eefHeaderImage(ns("publications"),"Publications & Data","EEF/parallax-publications2.jpg",active=2),
          br(),
          eefSection(ns("publications"),"Publications and Outputs",tabs=equalityButtons9(ns("publications")),content=uiOutput(NS(ns("publications"),"panel"))),
          eefSection(ns("data"),"Data",tabs=equalityButtons9(ns("data")),content=uiOutput(NS(ns("data"),"panel"))),
          eefSection(ns("external"),"External Links",tabs=equalityButtons9(ns("external")),content=uiOutput(NS(ns("external"),"panel"))),
          br(),
          eefHeaderImage(ns("contact"),"Contact","EEF/parallax-contact.jpg",active=3),
          br(),
          eefContact("Summary"),
          br(),br(),
          eefFooter(ns("top"))
  )
}


###EEF Page

eefPage <- function(id,icon,title,policy,equality,oldEEFLink) {
  ns <- NS(id)
  char <- c("overview",equalityCharacteristicsID)
  activePub <- filter(EEFpublished,policy_area==policy) %>% 
    summarise_at(char,sum,na.rm=TRUE) %>%
    select_if(.>0) %>%
    names
  activeDat <- filter(EEFdataLinks,policy_area==policy) %>% 
    summarise_at(char,sum,na.rm=TRUE) %>%
    select_if(.>0) %>%
    names
  activeExt <- filter(EEFexternal,policy_area==policy) %>% 
    summarise_at(char,sum,na.rm=TRUE) %>%
    select_if(.>0) %>%
    names
  tagList(
           eefHeader(id,icon,title),
           div(class="eef-text w3-content",
               p("The new Equality Evidence Finder website is currently under development. The full range of equality statistics for ",
                 strong(policy),
                 " and ",
                 strong(equality),
                 " is available on the old ",
               a(href=oldEEFLink,"Equality Evidence Finder website"))
           ),
           br(),
           eefContent(id,policy,equality),
           br(),
           eefHeaderImage(ns("publications"),"Publications & Data","EEF/parallax-publications2.jpg",active=2),
           br(),
           eefSection(ns("publications"),"Publications and Outputs",
                      tabs=equalityButtons9(ns("publications"),active=char %in% activePub,selected=equality),
                      content=uiOutput(NS(ns("publications"),"panel"))),
           eefSection(ns("data"),"Data",
                      tabs=equalityButtons9(ns("data"),active=char %in% activeDat,selected=equality),
                      content=uiOutput(NS(ns("data"),"panel"))),
           eefSection(ns("external"),"External Links",
                      tabs=equalityButtons9(ns("external"),active=char %in% activeExt,selected=equality),
                      content=uiOutput(NS(ns("external"),"panel"))),
           br(),
           eefHeaderImage(ns("contact"),"Contact","EEF/parallax-contact.jpg",active=3),
           br(),
           #eefSection(ns("contact"),"Contact",
           #           content=eefContact(policy)),
           eefContact(policy),
           br(),br(),
           eefFooter(ns("top"))
  )
}

###EEF Grid

eefGrid <- function(id) {
  ns <- NS(id)
  div(id=ns("grid-section"),
      div(class="eef-grid-container eef-text",
          #h1("Equality Evidence Finder"),
          p("The new Equality Evidence Finder website is currently under development. The full range of equality statistics is available at ",
            a(href="http://www.gov.scot/Topics/People/Equality/Equalities/DataGrid","www.gov.scot/Topics/People/Equality/Equalities/DataGrid")),
          br(),
          p("Scottish Government and its Agencies collect, analyse and publish equality evidence across a wide 
                                range of policy areas. Using the ",strong("Evidence Finder")," below you can find evidence by:"),
          tags$ul(tags$li(class="eef","by equality characteristic - click on the characteristic you are interested in"),
                  tags$li(class="eef","by policy area and equality characteristic - for example",
                          strong(" religion and culture "),"by clicking on the relevant intersection below")),
          p(strong("Note: "),"Clicking on the grid will take you to the old Equality Evidence Finder website whilst this new website is under development")
      ),
      div(class="eef-grid-wrapper",
          tagList(
            #div(class="eef-text",id="eef-grid-title",tags$p(class="eef-grid00","Equality Evidence Finder")),
            div(class="eef-text",id="eef-grid-title",img(src="EEF/eefLogo.svg",alt="LOGO",width="95%")),
            div(class="eef-grid-equality",
                tags$a(id=NS("summ","grid1"),title="Age",class="eef-grid20",href="javascript:void(0)",tags$img(src="EEF/info90.svg",class="eef-grid-info90"),"Age"),
                tags$a(id=NS("summ","grid2"),title="Disability",class="eef-grid21",href="javascript:void(0)",tags$img(src="EEF/info90.svg",class="eef-grid-info90"),"Disability"),
                tags$a(id=NS("summ","grid3"),title="Ethnicity",class="eef-grid20",href="javascript:void(0)",tags$img(src="EEF/info90.svg",class="eef-grid-info90"),"Ethnicity"),
                tags$a(id=NS("summ","grid4"),title="Gender",class="eef-grid21",href="javascript:void(0)",tags$img(src="EEF/info90.svg",class="eef-grid-info90"),"Gender"),
                tags$a(id=NS("summ","grid5"),title="Religion",class="eef-grid20",href="javascript:void(0)",tags$img(src="EEF/info90.svg",class="eef-grid-info90"),"Religion"),
                tags$a(id=NS("summ","grid6"),title="Sexual Orientation",class="eef-grid21",href="javascript:void(0)",tags$img(src="EEF/info90.svg",class="eef-grid-info90"),"Sexual Orientation"),
                tags$a(id=NS("summ","grid8"),title="Socio-Economic Status",class="eef-grid20",href="javascript:void(0)",tags$img(src="EEF/info90.svg",class="eef-grid-info90"),"Socio-Economic Status"),
                tags$a(id=NS("summ","grid7"),title="Transgender",class="eef-grid21",href="javascript:void(0)",tags$img(src="EEF/info90.svg",class="eef-grid-info90"),"Transgender"))
          ),div(class="eef-grid-content",
                
                gridRowOdd("busEnt"),
                gridRowEven("chiFam"),
                gridRowOdd("criJus"),
                gridRowEven("culCom"),
                #gridRowEven("cult"),
                gridRowOdd("dem"),
                gridRowEven("empSLL"),
                gridRowOdd("health"),
                gridRowEven("houReg"),
                gridRowOdd("incPov"),
                gridRowEven("labSoc"),
                #gridRowEven("labMar"),
                gridRowOdd("locThi"),
                #gridRowOdd("locGov"),
                gridRowEven("rurEnv"),
                gridRowOdd("schEdu"),
                #gridRowEven("thiSec"),
                gridRowEven("transp")
          )
      )
  )
}

gridRowOdd <- function(id,policyArea=gsub("and","&",policyLabel(id)),img=paste0("EEF/",id,".svg"),link=rep("javascript:void(0)",8)) {
  ns <- NS(id)
  tagList(tags$p(id=ns(0),class="eef-grid-policy eef-grid20",tags$img(src=img,class="eef-grid-icon"),policyArea),
          tags$a(id=ns("grid1"),title=paste0(policyArea,", Age"),class="eef-grid-square eef-grid00",href=link[1],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid2"),title=paste0(policyArea,", Disability"),class="eef-grid-square eef-grid01",href=link[2],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid3"),title=paste0(policyArea,", Ethnicity"),class="eef-grid-square eef-grid00",href=link[3],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid4"),title=paste0(policyArea,", Gender"),class="eef-grid-square eef-grid01",href=link[4],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid5"),title=paste0(policyArea,", Religion"),class="eef-grid-square eef-grid00",href=link[5],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid6"),title=paste0(policyArea,", Sexual Orientation"),class="eef-grid-square eef-grid01",href=link[6],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid8"),title=paste0(policyArea,", Socio-Economic Status"),class="eef-grid-square eef-grid00",href=link[8],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid7"),title=paste0(policyArea,", Transgender"),class="eef-grid-square eef-grid01",href=link[7],tags$img(src="EEF/info.svg",class="eef-grid-info"))
  )
}

gridRowEven <- function(id,policyArea=gsub("and","&",policyLabel(id)),img=paste0("EEF/",id,".svg"),link=rep("javascript:void(0)",8)) {
  policyArea <- gsub("and","&",policyLabel(id))
  img <- paste0("EEF/",id,".svg")
  ns <- NS(id)
  tagList(tags$p(id=ns(0),class="eef-grid-policy eef-grid21",tags$img(src=img,class="eef-grid-icon"),policyArea),
          tags$a(id=ns("grid1"),title=paste0(policyArea,", Age"),class="eef-grid-square eef-grid10",href=link[1],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid2"),title=paste0(policyArea,", Disability"),class="eef-grid-square eef-grid11",href=link[2],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid3"),title=paste0(policyArea,", Ethnicity"),class="eef-grid-square eef-grid10",href=link[3],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid4"),title=paste0(policyArea,", Gender"),class="eef-grid-square eef-grid11",href=link[4],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid5"),title=paste0(policyArea,", Religion"),class="eef-grid-square eef-grid10",href=link[5],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid6"),title=paste0(policyArea,", Sexual Orientation"),class="eef-grid-square eef-grid11",href=link[6],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid8"),title=paste0(policyArea,", Socio-Economic Status"),class="eef-grid-square eef-grid10",href=link[8],tags$img(src="EEF/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid7"),title=paste0(policyArea,", Transgender"),class="eef-grid-square eef-grid11",href=link[7],tags$img(src="EEF/info.svg",class="eef-grid-info"))
  )
}

eefHomeHeader <- function(id) {
  ns=NS(id)
  tagList(
    div(class="w3-display-container eef-header-box eef-clearfix",
        id="equality-top",
        div(class="eef-header",
            #    uiOutput("eefHeaderTitle")
            div(class="e3-display-middle w3-center",img(src="EEF/eefLogoWhite.svg",height="200"))
        ),
        div(class="eef-header-menu",
            a(id=ns("menu1"),href="javascript:void(0)",img(src="EEF/eefLogoWhite.svg",height="42px"),class="eef-page-links2"),
            a(id=ns("menu2"),href="javascript:void(0)","Equality Evidence",class="eef-page-links2 eef-page-links-active"),
            a(id=ns("menu3"),href="javascript:void(0)","Publications & Data",class="eef-page-links2"),
            #a(id=ns(NS("data","menu")),href="javascript:void(0)","Data",class="eef-page-links2"),
            #a(id=ns(NS("external","menu")),href="javascript:void(0)","External Links",class="eef-page-links2"),
            a(id=ns("menu4"),href="javascript:void(0)","Contact",class="eef-page-links2")
        )
    ),
    br(),br()
  )
}

eefHeader <- function(id,icon,title) {
  ns=NS(id)
  div(class="w3-display-container eef-header-box eef-clearfix",
     # id="equality-top",
     id=ns("top"),
      div(class="eef-header",
          div(class="eef-header-left",img(src=icon,height="200")),
          div(class="e3-display-middle eef-header-centre",
              div(class="w3-center",tags$span(class="eef-header-title w3-text-white w3-wide",title))
          ),
          div(class="eef-header-right msie11",img(src="EEF/eefLogoWhite.svg",height="100"))
      ),
      div(class="eef-header-menu",
          a(id=ns("menu1"),href="javascript:void(0)",img(src="EEF/eefLogoWhite.svg",height="42px"),class="eef-page-links2"),
          a(id=ns("menu2"),href="javascript:void(0)","Equality Evidence",class="eef-page-links2 eef-page-links-active"),
          a(id=ns("menu3"),href="javascript:void(0)","Publications & Data",class="eef-page-links2"),
          #a(id=ns(NS("data","menu")),href="javascript:void(0)","Data",class="eef-page-links2"),
          #a(id=ns(NS("external","menu")),href="javascript:void(0)","External Links",class="eef-page-links2"),
          a(id=ns("menu4"),href="javascript:void(0)","Contact",class="eef-page-links2")
      )
  )
}


#not currently used - header containing a parallax image 
eefHeaderImage <- function(uid,title,image="",imageColourClass="eef-blue",active=1) {
  ns=NS(uid)
  tagList(br(id=ns("top")),br(),
  div(class="w3-display-container eefParallax",
      style=paste("background-image: url(",image,");"),
      #id=ns("top"),
      div(class="w3-display-middle", style="white-space:nowrap;",
          tags$span(class=paste("w3-center w3-padding-large ",imageColourClass,"w3-xlarge w3-wide w3-animate-opacity"),
                    title)
      )#,
      #div(class="eef-header-image-menu",
      #    a(id=ns("menu1"),href="javascript:void(0)",img(src="EEF/eefLogoWhite.svg",height="42px"),class="eef-page-links2"),
      #    a(id=ns("menu2"),href="javascript:void(0)","Equality Evidence",class=paste("eef-page-links2",ifelse(1 %in% active,"eef-page-links-active",""))),
      #    a(id=ns("menu3"),href="javascript:void(0)","Publications",class=paste("eef-page-links2",ifelse(2 %in% active,"eef-page-links-active",""))),
      #    a(id=ns("menu4"),href="javascript:void(0)","Contact",class=paste("eef-page-links2",ifelse(3 %in% active,"eef-page-links-active","")))
      #)
      
  )#,br(),br()
  )
}

eefFooter = function(id)  tags$footer(class="w3-center eef-footer eef-blue w3-padding-64",
                                      tags$a(href="javascript:scroll(0,0);",class="w3-button e3-opacity-blue w3-hover-white", tags$i(class="fa fa-arrow-up w3-margin-right"),"To the top")
                                      # ,div(class="w3-xlarge w3-section",
                                      # tags$i(class="fa fa-facebook-official w3-hover-opacity"),
                                      # tags$i(class="fa fa-instagram w3-hover-opacity"),
                                      # tags$i(class="fa fa-snapchat w3-hover-opacity"),
                                      # tags$i(class="fa fa-pinterest-p w3-hover-opacity"),
                                      # tags$i(class="fa fa-twitter w3-hover-opacity"),
                                      # tags$i(class="fa fa-linkedin w3-hover-opacity")
                                      #)
)

#EEF Section Template
eefSection <- function(id,title,colour="eef-blue",tabs=NULL,content=NULL) {
  ns <- NS(id)
  div(
    id=ns("section"),
    class="w3-row  w3-content",style="width:100%;",
    br(),
    br(),
    tags$button(
      id = ns("header"),
      title,
      #      href = "javascript:void(0)",
      class=paste("eef-section-header",colour), 
      tags$a(class="e3-white fa fa-sort-down",style="margin-left:1ch")
    ),
    shinyjs::hidden(
      div(id=ns("content"),
          class="w3-content eef-text",
          tabs,
          br(),
          content
      )
    )
  )
  
}

#EEF Shiny Graph Template
eefGraph <- function(ns,graph,column1=NULL,column2=NULL,column3=NULL,zoom=TRUE) {
  if(zoom) buttons <- tagList( tags$button(id=ns("zoomIn"),class="btn eef-btn float",img(src="EEF/zoom-in.svg",width=24)),
                               tags$button(id=ns("zoomOut"),class="btn eef-btn float",img(src="EEF/zoom-out.svg",width=24)),
                               tags$button(id=ns("info"),class="btn eef-btn float",img(src="EEF/information.svg",width=24))
  )
  if(!zoom) buttons <- tags$button(id=ns("info"),class="eef-options-zoom float",img(src="EEF/information.svg",width=24))
  
  fixedRow(class="w3-content",
           div(class="float eef-plot",graph),
           div(class="float eef-options-container",
               div(class="float eef-options",buttons,
                   tags$a(id=ns("download-chart"),href="javascript:void(0)",class="btn eef-btn float",icon("download"),"download chart"),
                   tags$a(id=ns("download-data"),href="javascript:void(0)",class="btn eef-btn float",icon("download"),"download data"),
                   tags$button(id = ns("toggledash"), "Show/Hide", href = "#", class="float eef-options-toggle greyscalebutton")),
               div(id=ns("column1"),class="float eef-options",column1),
               div(id=ns("column2"),class="float eef-options",column2),
               div(id=ns("column3"),class="float eef-options",column3)
           ))
}

#Equality buttons (8 button version)
equalityButtons8 <- function(id,active=rep(TRUE,8),selected="Overview") {
  ns <- NS(id)
  div(class="eef-clearfix",
      tags$button(
        id = ns("age"),
        "Age",
        href = "#",
        class=paste("eef-equality-buttons8",ns("buttons"),
                    ifelse(selected=="Age","selected",""),
                    ifelse(active[1],"eef-equality-buttons-light","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("disability"),
        "Disability",
        href = "#",
        class=paste("eef-equality-buttons8",ns("buttons"),
                    ifelse(selected=="Disability","selected",""),
                    ifelse(active[2],"eef-equality-buttons-light","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("ethnicity"),
        "Ethnicity",
        href = "#",
        class=paste("eef-equality-buttons8",ns("buttons"),
                    ifelse(selected=="Ethnicity","selected",""),
                    ifelse(active[3],"eef-equality-buttons-light","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("gender"),
        "Gender",
        href = "#",
        class=paste("eef-equality-buttons8",ns("buttons"),
                    ifelse(selected=="Gender","selected",""),
                    ifelse(active[4],"eef-equality-buttons-light","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("religion"),
        "Religion",
        href = "#",
        class=paste("eef-equality-buttons8",ns("buttons"),
                    ifelse(selected=="Religion","selected",""),
                    ifelse(active[5],"eef-equality-buttons-light","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("sexualOrientation"),
        "Sexual Orientation",
        href = "#",
        class=paste("eef-equality-buttons8",ns("buttons"),
                    ifelse(selected=="Sexual Orientation","selected",""),
                    ifelse(active[6],"eef-equality-buttons-light","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("socioEconomicStatus"),
        "Socio-Economic Status",
        href = "#",
        class=paste("eef-equality-buttons8",ns("buttons"),
                    ifelse(selected=="Socio-Economic Status","selected",""),
                    ifelse(active[8],"eef-equality-buttons-light","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("transgender"),
        "Transgender",
        href = "#",
        class=paste("eef-equality-buttons8",ns("buttons"),
                    ifelse(selected=="Transgender","selected",""),
                    ifelse(active[7],"eef-equality-buttons-light","eef-equality-buttons-inactive")))
  )
}



#Equality buttons (9 button version)
equalityButtons9 <- function(id,active=rep(TRUE,9),selected="Overview") {
  ns <- NS(id)
  div(class="eef-clearfix",
      tags$button(
        id = ns("overview"),
        "Overview",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Overview","selected",""),
                    ifelse(active[1],"eef-equality-buttons-dark","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("age"),
        "Age",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Age","selected",""),
                    ifelse(active[2],"eef-equality-buttons-dark","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("disability"),
        "Disability",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Disability","selected",""),
                    ifelse(active[3],"eef-equality-buttons-dark","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("ethnicity"),
        "Ethnicity",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Ethnicity","selected",""),
                    ifelse(active[4],"eef-equality-buttons-dark","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("gender"),
        "Gender",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Gender","selected",""),
                    ifelse(active[5],"eef-equality-buttons-dark","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("religion"),
        "Religion",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Religion","elected",""),
                    ifelse(active[6],"eef-equality-buttons-dark","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("sexualOrientation"),
        "Sexual Orientation",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Sexual Orientation","selected",""),
                    ifelse(active[7],"eef-equality-buttons-dark","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("socioEconomicStatus"),
        "Socio-Economic Status",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Socio-Economic Status","selected",""),
                    ifelse(active[9],"eef-equality-buttons-dark","eef-equality-buttons-inactive"))),
      tags$button(
        id = ns("transgender"),
        "Transgender",
        href = "#",
        class=paste("eef-equality-buttons9",ns("buttons"),
                    ifelse(selected=="Transgender","selected",""),
                    ifelse(active[8],"eef-equality-buttons-dark","eef-equality-buttons-inactive")))
  )
}


###Page Layout Elements###

##load summary sections from the modules.csv spreadsheet

eefContent <- function(id,policy,equality) {
  ns <- NS(id)
  content <- filter(EEFindex,policy_area==policy)
  topic <- unique(content$topic)
  if(length(topic)==0) return(NULL)
  
  sectionContent <- function(id,topic) {
    char <- unique(content$characteristic[content$topic%in%topic])
    tagList(eefSection(id=id,
               title=topic,
               colour=ifelse(equality %in% char,"eef-mid-blue","eef-gray"),
               tabs=equalityButtons8(id,active=equalityCharacteristics %in% char,selected=equality),
               content=uiOutput(NS(id,"panel"))))
               #content=tabsetPanel(id=ns("tabs"))))
  }
  mapply(FUN=sectionContent,topic=as.character(topic),id=ns(1:length(topic)))
}

##load tab panels for each of the summary section from the modules.csv spreadsheet
panelContent <- function(equality,policy,id) {
  content <- filter(EEFindex,policy_area==policy,topicID==id,characteristic==equality)
  if(nrow(content)==0) return(NULL)
  
  panel <- list()
  for(r in 1:nrow(content)) {
    panelInner <- list()
    if(!content$headline[r] %in% c("",NA)) panelInner <- c(panelInner,list(h2(content$headline[r])))
    if(!content$subtitle[r] %in% c("",NA)) panelInner <- c(panelInner,list(p(strong(content$subtitle[r]))))
    if(tolower(content$graphID[r]) %in% c("yes","y")) {
      panelInner <- c(panelInner,list(uiOutput(NS(content$tabUID[r],"graph"))))
    } else {
      if(!content$image[r] %in% c("",NA)) panelInner <- c(panelInner,list(img(src=content$image[r],alt=content$subtitle[[r]],title=content$subtitle[r],width="680")))
    }
    if(!content$markdownFile[r] %in% c("",NA)) if(file.exists(as.character(content$markdownFile[r]))) panelInner <- c(panelInner,list(includeMarkdown(as.character(content$markdownFile[r]))))
    if(!content$dataSource[r] %in% c("",NA)) panelInner <- c(panelInner,list(HTML(pubSource(content$dataSource[r],notes=content$dataSourceNotes[r]))))
    panelInner <- div(class="eef-text",panelInner)
    panel[[r]] <- tabPanel(title=content$tab[r],value=NS("equality",NS("panel",content$tabUID[r])),panelInner)
  }
  
  if(length(panel)==1) return(panelInner)
  if(length(panel)==2) return(tabsetPanel(panel[[1]],panel[[2]]))
  if(length(panel)==3) return(tabsetPanel(panel[[1]],panel[[2]],panel[[3]]))
  if(length(panel)==4) return(tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]]))
  if(length(panel)==5) return(tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]]))
  if(length(panel)==6) return(tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]]))
  if(length(panel)==7) return(tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]],panel[[7]]))
  if(length(panel)>=8) return(tabsetPanel(panel[[1]],panel[[2]],panel[[3]],panel[[4]],panel[[5]],panel[[6]],panel[[7]],panel[[8]]))
}
