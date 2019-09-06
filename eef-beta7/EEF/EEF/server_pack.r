#Graph data
# filterData <- list()
# filterData[["incPov-1"]] <- filter(EEFdata,Characteristic=="Gender",Measure%in%c("% in relative poverty AHC"))
# filterData[["incPov-4"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure%in%c("% in relative poverty AHC","% in relative poverty BHC"))
# filterData[["incPov-8"]] <- filter(EEFlatest,Characteristic=="Religion",Measure%in%c("% in relative poverty AHC","% in relative poverty BHC"))
# filterData[["incPov-6"]] <- filter(EEFdata,Characteristic=="Disability",Measure=="% in relative poverty AHC")
# filterData[["sch-Edu-1"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
# filterData[["sch-Edu-2"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
# filterData[["sch-Edu-3"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
# filterData[["sch-Edu-4"]] <- filter(EEFdata,Characteristic=="Gender",Measure=="% school leavers in a positive follow-up destination")
# filterData[["sch-Edu-5"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
# filterData[["sch-Edu-6"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
# filterData[["sch-Edu-7"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
# filterData[["sch-Edu-8"]] <- filter(EEFdata,Characteristic=="Ethnicity",Measure=="% school leavers in a positive follow-up destination")
# filterData[["sch-Edu-9"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 4 or better")
# filterData[["sch-Edu-10"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 5 or better")
# filterData[["sch-Edu-11"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers achieving 1 or more passes at SCQF Level 6 or better")
# filterData[["sch-Edu-12"]] <- filter(EEFdata,Characteristic=="Socio-Economic Status",Measure=="% school leavers in a positive follow-up destination")
# filterData[["sch-Edu-14"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure=="Pupil ethnicity")
# filterData[["sch-Edu-15"]] <- filter(EEFlatest,Characteristic=="Ethnicity",Measure=="Teacher ethnicity")
# filterData[["sch-Edu-41"]] <- filter(EEFlatest,Characteristic=="Gender",Measure%in%c("P1 - Reading","P1 - Writing","P1 - Listening & Talking","P1 - Numeracy"))
# filterData[["sch-Edu-13"]] <- filter(EEFdata,Measure=="Teachers (headcount)",Characteristic=="Age")
#NOTE: redundant code deleted but can be found in equality-evidence-finder-alpha3

eefServer <- function(serv_env) with(serv_env, {
  
  state <- reactiveValues(equality='Overview',equalityID='',policyID='',policy="Summary")
  
  #callModule(eefPageLinksServer,NS("equality","grid"))
  state <- callModule(eefPageLinksServer,"equality",state)
  state <- callModule(eefPageLinksServer,NS("equality","publications"),state)
  state <- callModule(eefPageLinksServer,NS("equality","data"),state)
  state <- callModule(eefPageLinksServer,NS("equality","external"),state)
  state <- callModule(eefPageLinksServer,NS("equality","contact"),state)
  
  #EEF grid
  state <- callModule(gridRowServer,"summ",state)
  state <- callModule(gridRowServer,"busEnt",state)
  state <- callModule(gridRowServer,"chiFam",state)
  state <- callModule(gridRowServer,"criJus",state)
  state <- callModule(gridRowServer,"culCom",state)
  #state <- callModule(gridRowServer,"cult",state)
  state <- callModule(gridRowServer,"dem",state)
  state <- callModule(gridRowServer,"empSLL",state)
  state <- callModule(gridRowServer,"health",state)
  state <- callModule(gridRowServer,"houReg",state)
  state <- callModule(gridRowServer,"incPov",state)
  state <- callModule(gridRowServer,"labSoc",state)
  #state <- callModule(gridRowServer,"labMar",state)
  #state <- callModule(gridRowServer,"locGov",state)
  state <- callModule(gridRowServer,"locThi",state)
  state <- callModule(gridRowServer,"rurEnv",state)
  state <- callModule(gridRowServer,"schEdu",state)
  #state <- callModule(gridRowServer,"thiSec",state)
  state <- callModule(gridRowServer,"transp",state)
  
  #EEF sections
  state <- callModule(eefSectionServer,"equality-publications",state,"publications")
  state <- callModule(eefSectionServer,"equality-data",state,"data")
  state <- callModule(eefSectionServer,"equality-external",state,"external")
  state <- callModule(eefSectionServer,"equality-contact",state,"contact")
  state <- callModule(eefSectionServer,"equality-1",state,"summary",1)
  state <- callModule(eefSectionServer,"equality-2",state,"summary",2)
  state <- callModule(eefSectionServer,"equality-3",state,"summary",3)
  state <- callModule(eefSectionServer,"equality-4",state,"summary",4)
  state <- callModule(eefSectionServer,"equality-5",state,"summary",5)
  state <- callModule(eefSectionServer,"equality-6",state,"summary",6)
  state <- callModule(eefSectionServer,"equality-7",state,"summary",7)
  state <- callModule(eefSectionServer,"equality-8",state,"summary",8)
  state <- callModule(eefSectionServer,"equality-9",state,"summary",9)
  state <- callModule(eefSectionServer,"equality-10",state,"summary",10)
  state <- callModule(eefSectionServer,"equality-11",state,"summary",11)
  state <- callModule(eefSectionServer,"equality-12",state,"summary",12)
  state <- callModule(eefSectionServer,"equality-13",state,"summary",13)
  state <- callModule(eefSectionServer,"equality-14",state,"summary",14)
  state <- callModule(eefSectionServer,"equality-15",state,"summary",15)
  
  #EEF graphs
  
  callModule(lineChartServer2,"incPov-1",filterData[["incPov-1"]])
  #callModule(timeSeriesServer2,"incPov-1",filterData[["incPov-1"]])
  callModule(barChartServer2,"incPov-4",filterData[["incPov-4"]])
  callModule(barChartServer1,"incPov-8",filterData[["incPov-8"]])
  callModule(timeSeriesServer1,"incPov-6",filterData[["incPov-6"]])
  callModule(timeSeriesServer1,"sch-Edu-1",filterData[["sch-Edu-1"]],ylabel="% School Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-2",filterData[["sch-Edu-2"]],ylabel="% School Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-3",filterData[["sch-Edu-3"]],ylabel="% School Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-4",filterData[["sch-Edu-4"]],ylabel="% School Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-5",filterData[["sch-Edu-5"]],ylabel="% School Leavers",defaultSelected="All Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-6",filterData[["sch-Edu-6"]],ylabel="% School Leavers",defaultSelected="All Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-7",filterData[["sch-Edu-7"]],ylabel="% School Leavers",defaultSelected="All Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-8",filterData[["sch-Edu-8"]],ylabel="% School Leavers",defaultSelected="All Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-9",filterData[["sch-Edu-9"]],ylabel="% School Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-10",filterData[["sch-Edu-10"]],ylabel="% School Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-11",filterData[["sch-Edu-11"]],ylabel="% School Leavers",digits=1)
  callModule(timeSeriesServer1,"sch-Edu-12",filterData[["sch-Edu-12"]],ylabel="% School Leavers",digits=1)
  callModule(barChartServer1,"sch-Edu-14",filterData[["sch-Edu-14"]],defaultSelected="Total Pupils",digits=1)
  callModule(barChartServer1,"sch-Edu-15",filterData[["sch-Edu-15"]],digits=1)
  callModule(pieChartServer2,"sch-Edu-41",filterData[["sch-Edu-41"]],digits=1,icon=c("EEF/School Education/open-book.png","EEF/School Education/edit.png","EEF/School Education/listeningtalking.png","EEF/School Education/calculator.png"))
  #callModule(pieChartServer2,"sch-Edu-41",filterData[["sch-Edu-41"]],digits=1)
  callModule(lineChartServer1,"sch-Edu-13",filterData[["sch-Edu-13"]],digits=0)

  #EEF page
  output$eefHome <- renderUI({
#    shinyjs::runjs("location.href='#';")
    title <- ifelse(state$policyID=="summ",
                    toupper(equalityLabel(state$equalityID)),
                    toupper(gsub("and","&",policyLabel(state$policyID)))
                    )
    
    #show grid if policy and characteristic not selected
#    if(state$policyID=="summ" & state$equalityID=="") {
    if(state$equalityID=="") {
      eefHomePage("equality")
    } else {
      eefPage(id="equality",icon=paste0("EEF/",state$policyID,".svg"),title=title,policy=policyLabel(state$policyID),equality=equalityLabel(state$equalityID),oldEEFLink=oldEEF[[state$policyID]][switch(state$equalityID,age=1,disability=2,ethnicity=3,gender=4,religion=5,sexualOrientation=6,transgender=7,8)]
      )
    }
  })
  
})

eefPageLinksServer <- function(input,output,session,state) {
  ns <- session$ns
  shinyjs::onclick("menu1", {
    state$policyID <- ""
    state$equalityID <- ""
    shinyjs::runjs(paste0("location.hash='equality-top';"))
    #shinyjs::show(id = "content", anim = FALSE)
    #shinyjs::runjs(paste0("location.hash='",ns("top"),"';"))
    #removeClass(class="eefPageLinks-active",selector=".eefPageLinks") #requires shinyjs 1.01 - doesn't work on Scots!
    #shinyjs::addClass(class="eefPageLinks-active",id="menu")
    #  shinyjs::runjs(paste0("setTimeout(function() {location.href='#",ns("section"),"';},1000);"))
  })  
  shinyjs::onclick("menu2", {
    shinyjs::runjs(paste0("location.hash='equality-top';"))
  })  
  shinyjs::onclick("menu3", {
    shinyjs::runjs(paste0("location.hash='equality-publications-top';"))
  })  
  shinyjs::onclick("menu4", {
    shinyjs::runjs(paste0("location.hash='equality-contact-top';"))
  }) 
  return(state)
}

###Section Modules###

eefSectionServer <- function(input,output,session,state,type,topicID=NULL) {
  ns <- session$ns
  section_state <- reactiveValues(equalityID="overview",equality="Overview")
  shinyjs::onclick("overview",{
    section_state$equalityID <- "overview"
  })
  shinyjs::onclick("age",{
    section_state$equalityID <- "age"
  })
  shinyjs::onclick("disability",{
    section_state$equalityID <- "disability"
  })
  shinyjs::onclick("ethnicity",{
    section_state$equalityID <- "ethnicity"
  })
  shinyjs::onclick("gender",{
    section_state$equalityID <- "gender"
  })
  shinyjs::onclick("religion",{
    section_state$equalityID <- "religion"
  })
  shinyjs::onclick("sexualOrientation",{
    section_state$equalityID <- "sexualOrientation"
  })
  shinyjs::onclick("transgender",{
    section_state$equalityID <- "transgender"
  })
  shinyjs::onclick("socioEconomicStatus",{
    section_state$equalityID <- "socioEconomicStatus"
  })
  
  shinyjs::onclick("header", {shinyjs::toggle(id = "content", anim = TRUE)}) 
  
  observeEvent(state$equalityID, {
    section_state$equalityID <- state$equalityID
    if(section_state$equalityID=="") section_state$equalityID <- "overview"
  })
  
  observeEvent(section_state$equalityID, {
    removeClass(class="selected",selector=paste0(".",ns("buttons"))) #requires shinyjs 1.01 - doesn't work on Scots!
    shinyjs::addClass(class="selected",id=section_state$equalityID)
  })
  
  
    if(type=="publications") output$panel <- renderText({updatePublications(policyLabel(state$policyID),section_state$equalityID)})
    if(type=="data") output$panel <- renderText({updateData(policyLabel(state$policyID),section_state$equalityID)})
    if(type=="external") output$panel <- renderText({updateExternal(policyLabel(state$policyID),section_state$equalityID)})
    if(type=="summary") output$panel <- renderUI({
      panelContent(equality=equalityLabel(section_state$equalityID),policy=policyLabel(state$policyID),id=topicID)
    })
      
    # if(type=="contact") output$panel <- renderText({
    #   if(state$policyID=="") {
    #     includeMarkdown("equalityContact.md")
    #   } else {
    #     includeMarkdown(paste(state$policyID,"contact.md",sep="/"))
    #}
    #})
  
  
  return(state)
}

###Navigation###
navigation <- function(serv_env) with(serv_env, {

  initialNav <- reactiveVal(TRUE)

  observeEvent(session$clientData$url_search,{
    query <- parseQueryString(session$clientData$url_search)
    if(is.null(query$page)) query$page <- "Home"
      
    if(!is.null(query$page)) {
      # if(query$page %in% allPagesID) { only change page if the url is a valid page
        if(!query$page %in% nav$page) { #if find a match and it doesn't already match the internal state tracker, update the internal state tracker, and change the panel - the state tracker will match if the url change was triggered by a change of panel - this avoids  an infinite loop
          nav$page <- query$page
          nav$query <- session$clientData$url_search
          updateNavbarPage(session,"dashboard",nav$page)
        #} 
      } 
      
      #EEF navigation
      if(query$page=="equality"){
        if(is.null(query$policy)) state$policyID <- ""
        if(!is.null(query$policy)) if(query$policy %in% allPolicyAreasID) state$policyID <- query$policy 
        if(is.null(query$equality)) state$equalityID <- ""
        if(!is.null(query$equality)) if(query$equality %in% equalityCharacteristicsID) state$equalityID <- query$equality 
      }
      
    }    
  },priority=99)

  #EEF navigation
  observe({
    if(!is.null(input$dashboard)) {
      if(nav$page=="equality"&input$dashboard=="equality") {
        str <- c(nav$page,state$equalityID,state$policyID)
        str <- paste0(c("page=","equality=","policy="),str)[str!=""]
        nav$query <- paste0("?",paste(str,collapse="&"))
        
        nav$hash <- NS(nav$page,"top")
      }
    }
  })

  #run whenever the page changes
  observeEvent(input$dashboard, {
    if(!initialNav()) {#don't run on first initialising input$dashboard
      if(!input$dashboard %in% nav$page) {#if tab has changed and no longer matches the internal state traker, update it - the state tracker will match if the tab change was triggered by an url update - this avoids  an infinite loop
        nav$page <- input$dashboard
        if(nav$page!="") {
          nav$query <- paste0("?page=",nav$page)
          nav$query <- gsub(" ","%20",nav$query)
          nav$query <- gsub("&","%26",nav$query)
        }
        # if(nav$page=="equality") {
        #   str <- c(nav$page,state$equalityID,state$policyID)
        # } else {
        #   str <- c(nav$page,"","")
        # }
        # str <- paste0(c("page=","equality=","policy="),str)[str!=""]
        # nav$query <- paste0("?",paste(str,collapse="&"))
        nav$hash <- NS(nav$page,"top")
      }
    }
    initialNav(FALSE)
  },priority=99) 

  #run whenever the browser url needs updating
    observeEvent(nav$query,{
     # if(session$clientData$url_search!=nav$query) updateQueryString(paste0(nav$query,"#",nav$hash),mode="push")
      if(session$clientData$url_search!=nav$query) updateQueryString(nav$query,mode="push")
      shinyjs::runjs("scroll(0,0);") #scroll to top of page - needed as location.hash isn't working in chrome
      },priority=99)
    
    #Commented out - Hash navigation not reliable - trying to move to anchor before page fully loaded
    # observe({
    #   temp <- gsub("#","",session$clientData$url_hash)
    #   if(session$clientData$url_search==nav$query & temp!="") shinyjs::runjs(paste0("location.hash='",session$clientData$url_hash,"';"))
    # })
    
})



gridRowServer <- function(input,output,session,state) {
  ns <- session$ns
  id <- substr(ns(""),1,nchar(ns(""))-length(ns.sep))
  shinyjs::onclick("grid1",{
    state$equalityID <- "age"
    state$policyID <- id
  })
  shinyjs::onclick("grid2",{
    state$equalityID <- "disability"
    state$policyID <- id
  })
  shinyjs::onclick("grid3",{
    state$equalityID <- "ethnicity"
    state$policyID <- id
  })
  shinyjs::onclick("grid4",{
    state$equalityID <- "gender"
    state$policyID <- id
  })
  shinyjs::onclick("grid5",{
    state$equalityID <- "religion"
    state$policyID <- id
  })
  shinyjs::onclick("grid6",{
    state$equalityID <- "sexualOrientation"
    state$policyID <- id
  })
  shinyjs::onclick("grid7",{
    state$equalityID <- "transgender"
    state$policyID <- id
  })
  shinyjs::onclick("grid8",{
    state$equalityID <- "socioEconomicStatus"
    state$policyID <- id
  })
  return(state)
}
