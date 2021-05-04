###################################################################################
#                                                                                 #
# UI COMPONENTS - TOPIC BOXES / GRID / HEADERS / HELP BOXES                       #
#                                                                                 #
###################################################################################
#                                                                                 #
# Author: Jay Ware                                                                #
# Created: 01/08/2018                                                             #
# Last updated: 10/01/2021                                                        #
#                                                                                 #
# Purpose: creates the UI functions (except graphs) for use in the app. The app   #
#          is built up of a number of these UI components, which are used in the  #
#          main app code (app.R) and various section modules (server_modules.R)   #
#                                                                                 #
#          See server_modules.R and app.R for where these are inserted into the   #
#          app                                                                    #
#                                                                                 #
# Functions:                                                                      #
#                                                                                 #
# eefHelp(): adds the pop-up boxes for the help pages, the gray surrounding box   #
#            and the x for users to close the pop-up                              #
# eefHeader(): creates the top header box. There is currently only one box added  #
#              with javascript used to hide/show text and images for individual   #
#              policy "pages". It is a module and takes the standard module id    #
#              pararameter. id="equality" is used here and throughout this        #
#              script for components on the EEF page (currently everything)       #
#              [parameters: id]                                                   #
# eefGrid(): creates the Equality Evidence Finder grid. This function generates   #
#            a list of links which are then styled to look like a grid using css. #
#            There is a second mobile version of the "grid" included as well with #
#            css used to specify which version is displayed depending on screen   #
#            size. Note: this function takes an optional parameter to set the     #
#            namespace, *but* this was never fully implemented and using more     #
#            than one grid in an app will cause errors                            #
# gridRowOdd(), gridRowEven(): functions used within eefGrid() to add the policy  #
#                              area rows. These are the same except for the box   #
#                              colour. Alternate odd and even for the EEF pattern #
# eefContent(): function to generate all the evidence topic sections used in the  #
#               app. Loops through all topics in EEFindex and calls the           #
#               eefSection() function                                             #
# eefSection(): creates the drop down boxes containing the 8/9 equality tab       #
#               buttons and the content.                                          #
#               [parameters: id - unique identifier (namespace);                  #
#                title - text to appear in box header; class - css classes        #
#                specifying the policy area(s) ("page") and equality              #
#                characteristic(s) where the box is visible (technically all boxes#
#                are present all the time - we just hide the ones not for the     #
#                current "page"); colour - css class used to specify the box      #
#                header background colour; tabs - see below function;             #
#                content - used to insert panel content]                          #
# equalityButtons(): creates the 8/9 equality tab buttons used in eefSection()    #
# equalityButtons8(), equalityButtons9(): wrappers for equalityButtons() to       #
#                create the 8/9 button tab versions. Sets css classes used to set #
#                the correct tab widths, and specifies the equality characteristic#
#                ids to be included in the tabs                                   #
# eefHeaderImage(): function to create the image headers used to break up pages   #
# npfText(): reads in markdown text used in the NPF section                       #
# npfCog(): creates the NPF indicator text included on some panels                #
# eefFooter(): footer used at the bottom of the page - largely redundant now      #
#                                                                                 #
###################################################################################

# Accessibility Statement -------------------------------------------------

accessibilityStatement <- function(id = "accessibility") {
  div(class = paste("w3-content eef eef-text", paste0("eef-", id, collapse = " ")),
      includeMarkdown("data/accessibility.md")
  )
}

# Help section ------------------------------------------------------------

#see EEFextra.css for styles for the popups
#see EEF.js for javascript to close the popups
#see server_graphs.r for the buttons that display the popups
eefHelp <- function() {
  tagList(
    div(id="eef-help-blank"), #related styles in EEFextra.css for id: #eef-help-blank (boundary box)
    div(class="eef-help eef-text",id="eef-help-navigation",
        div(id="eef-help-navigation-top",tabindex="0"),
        includeMarkdown("data/Help/navigation.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-timeSeries0",
        div(id="eef-help-timeSeries0-top",tabindex="0"),
        includeMarkdown("data/Help/timeSeries0.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-timeSeries1",
        div(id="eef-help-timeSeries1-top",tabindex="0"),
        includeMarkdown("data/Help/timeSeries1.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-timeSeries2",
        div(id="eef-help-timeSeries2-top",tabindex="0"),
        includeMarkdown("data/Help/timeSeries2.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-lineChart0",
        div(id="eef-help-lineChart0-top",tabindex="0"),
        includeMarkdown("data/Help/lineChart0.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-lineChart1",
        div(id="eef-help-lineChart1-top",tabindex="0"),
        includeMarkdown("data/Help/lineChart1.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-lineChart2",
        div(id="eef-help-lineChart2-top",tabindex="0"),
        includeMarkdown("data/Help/lineChart2.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-barChart0",
        div(id="eef-help-barChart0-top",tabindex="0"),
        includeMarkdown("data/Help/barChart0.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-barChart1",
        div(id="eef-help-barChart1-top",tabindex="0"),
        includeMarkdown("data/Help/barChart1.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-barChart2",
        div(id="eef-help-barChart2-top",tabindex="0"),
        includeMarkdown("data/Help/barChart2.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-barChart3",
        div(id="eef-help-barChart3-top",tabindex="0"),
        includeMarkdown("data/Help/barChart3.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-pieChart0",
        div(id="eef-help-pieChart0-top",tabindex="0"),
        includeMarkdown("data/Help/pieChart0.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-pieChart1",
        div(id="eef-help-pieChart1-top",tabindex="0"),
        includeMarkdown("data/Help/pieChart1.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    div(class="eef-help eef-text",id="eef-help-pieChart2",
        div(id="eef-help-pieChart2-top",tabindex="0"),
        includeMarkdown("data/Help/pieChart2.md") #related styles in EEFextra.css for class: .eef-help, .eef-text (pop up)
    ),
    # div(id="eef-help-close",img(src="icons/cross.svg",height=24,width=24)) #related styles in EEFextra.css for id: #eef-help-close  (close pop up)
    div(id="eef-help-close",a(href="javascript:void(0)",img(src="icons/cross.svg",height=24,width=24))) #related styles in EEFextra.css for id: #eef-help-close  (close pop up)
  )
}

# Main header -------------------------------------------------------------

#Class "eef-<policyID>" associates an element with a particular policy/characteristic page. Any elements with classes "eef" AND "eef-<policyID>" will be shown/hid depending on what policy/characteristic is being viewed
eefHeader <- function(id="equality") {
  ns=NS(id)
  policyCSS <- paste0("eef-",allPolicyAreasID,collapse=" ") #creates the CSS classes used to control what is displayed.
  
  div(class="w3-display-container eef-header-box eef-clearfix",#related styles in EEFextra.css for class: .eef-header-box (invisible box containing the header)
      id=ns("top"),
      div(class="eef-header", role="banner",
          lapply(allPolicyAreasID,FUN=function(x) shinyjs::hidden(div(img(src=paste0("icons/",x,".svg")),class=paste0("hidden-xs eef eef-header-left eef-",x)))), #policy icons with
          div(class="e3-display-middle eef-header-centre",
              div(class="w3-center",
                  lapply(allPolicyAreasID[allPolicyAreasID!="summ"],
                         FUN=function(x) shinyjs::hidden(span(toupper(gsub("and","&",policyLabel(x))),class=paste0("eef eef-header-title w3-text-white w3-wide eef-",x)))),
                  lapply(equalityCharacteristicsID,FUN=function(x) shinyjs::hidden(span(toupper(gsub("and","&",equalityLabel(x))),class=paste0("eef eef-header-title w3-text-white w3-wide eef-summ-",x))))
              )
          ),
          div(id="eef-header-main-logo",class="eef-header-left msie11 eef eef-main eef-accessibility",img(src="icons/eefLogoWhite.svg")),
          div(id="eef-header-sg-logo",class="eef-header-right eef eef-main eef-accessibility",img(src="icons/Scottish Government logo.svg")),
          shinyjs::hidden(div(class=paste("hidden-xs eef-header-right msie11 eef",policyCSS,paste0("eef-summ-",equalityCharacteristicsID,collapse=" ")),img(src="icons/eefLogoWhite.svg")))
      ),
      div(class="eef-header-menu", role="navigation", `aria-label`="Primary", 
          a(id=ns("menu1"),href="javascript:void(0)",`data-value`="equality",img(src="icons/eefLogoWhite.svg",height="42px",width="103px", alt = "Home"),class=paste("eef-page-links2 eef-page-links20")),
          a(id=ns("menu2"),href="javascript:void(0)","National Performance Framework",class="eef-page-links2 eef-page-links25"),#class "eef eef-XXXX" shows/hides components when on/not on page XXXX 
          a(id=ns("menu3"),href="javascript:void(0)","Publications & Data",class="eef-page-links2 eef-page-links25"),
          a(id=ns("menu4"),href="javascript:void(0)","Contact",class="eef-page-links2 eef-page-links15"),
          a(id=ns("menu5"),href="javascript:void(0)","Take a Tour",class="eef-page-links2 eef-page-links15")
      )
  )
}

# EEF grid ----------------------------------------------------------------

eefGrid <- function(id) {
  ns <- NS(id)
  div(id=ns("grid-section"),class="eef-section eef-main",
      role="navigation", `aria-label`="Equality Evidence", 
      
      div(class="w3-content eef-text",
          h1("Equality Evidence"),
          p("Scottish Government and its Agencies collect, analyse and publish equality evidence across a wide 
            range of policy areas. By clicking on the ",strong("Evidence Finder")," below you can find evidence by:"),
          tags$ul(tags$li(#class="eef",
                          "by equality characteristic - click on the characteristic you are interested in"),
                  tags$li(#class="eef",
                          "by policy area and equality characteristic - for example",
                          strong(" religion and demographics "),"by clicking on the relevant intersection below"))
          ),
      tags$div(class="hidden-sm hidden-md hidden-lg", 
               lapply(equalityCharacteristicsID,function(e) {#equalityCharacteristicsID are all the characteristic ID values, and is a variable created in helper_funcs.r
                 tagList(
                   tags$a(class="eef-mobile-grid main",style="color:white",#equalityLabel is a function created in helper_funcs.r that converts characteristic IDs into the corresponding label
                            equalityLabel(e),href="javascript:void(0)", 
                            tags$p(class="e3-white",style="margin-left:1ch"),
                               tags$img(src="icons/plus.svg",class="eef-section-header-expand",title="Click to show section"),
                               shinyjs::hidden(tags$img(src="icons/minus.svg",class="eef-section-header-collapse",title="Click to hide section"))
                   ),
                   shinyjs::hidden(
                     div(lapply(allPolicyAreasID,function(x) {
                         # div(
                         #   div(tags$img(src=paste0("icons/",x,".svg"),class="eef-grid-icon"),class="float eef-mobile-grid-icon"),
                         #   tags$a(href="javascript:void(0)", gsub("and","&",policyLabel(x))),
                         #   class="eef-mobile-grid sub",`data-pol`=x,`data-char`=e)
                         tags$a(href="javascript:void(0)",
                                class="eef-mobile-grid sub",`data-pol`=x,`data-char`=e, 
                                div(tags$img(src=paste0("icons/",x,".svg"),class="eef-grid-icon"),class="float eef-mobile-grid-icon", alt = "", `aria-hidden`="true"),
                                gsub("and","&",policyLabel(x)))
                         
                       })
                     )
                   )
                 )
               })
      ),
      
      div(class="eef-grid-wrapper hidden-xs",#eef-grid-wrapper is a container primarily used as a reference point for the positioning and sizing the following elements
          tagList(
            #div(class="eef-text",id="eef-grid-title",tags$p(class="eef-grid00","Equality Evidence Finder")),
            div(class="eef-text",id="eef-grid-title",img(src="icons/eefLogo.svg",alt="LOGO",width="95%")),
            div(class="eef-grid-equality",#eef-grid-equality is rotated 90 degrees in the css file. Note that the rotation complicates the positioning and sizing of the elements - height and width are flipped round for example
                tags$a(id=NS("summ","grid1"),title=eefTooltip("Summary","Age"),`data-pol`="summ",`data-char`="age",class="eef-grid-summ eef-grid20",href="javascript:void(0)",tags$img(src="icons/info90.svg",class="eef-grid-info90"),"Age"),
                tags$a(id=NS("summ","grid2"),title=eefTooltip("Summary","Disability"),`data-pol`="summ",`data-char`="disability",class="eef-grid-summ eef-grid21",href="javascript:void(0)",tags$img(src="icons/info90.svg",class="eef-grid-info90"),"Disability"),
                tags$a(id=NS("summ","grid3"),title=eefTooltip("Summary","Ethnicity"),`data-pol`="summ",`data-char`="ethnicity",class="eef-grid-summ eef-grid20",href="javascript:void(0)",tags$img(src="icons/info90.svg",class="eef-grid-info90"),"Ethnicity"),
                tags$a(id=NS("summ","grid4"),title=eefTooltip("Summary","Gender"),`data-pol`="summ",`data-char`="gender",class="eef-grid-summ eef-grid21",href="javascript:void(0)",tags$img(src="icons/info90.svg",class="eef-grid-info90"),"Gender"),
                tags$a(id=NS("summ","grid5"),title=eefTooltip("Summary","Religion"),`data-pol`="summ",`data-char`="religion",class="eef-grid-summ eef-grid20",href="javascript:void(0)",tags$img(src="icons/info90.svg",class="eef-grid-info90"),"Religion"),
                tags$a(id=NS("summ","grid6"),title=eefTooltip("Summary","Sexual Orientation"),`data-pol`="summ",`data-char`="sexualOrientation",class="eef-grid-summ eef-grid21",href="javascript:void(0)",tags$img(src="icons/info90.svg",class="eef-grid-info90"),"Sexual Orientation"),
                tags$a(id=NS("summ","grid8"),title=eefTooltip("Summary","Socio-Economic Status"),`data-pol`="summ",`data-char`="socioEconomicStatus",class="eef-grid-summ eef-grid20",href="javascript:void(0)",tags$img(src="icons/info90.svg",class="eef-grid-info90"),"Socio-Economic Status"),
                tags$a(id=NS("summ","grid7"),title=eefTooltip("Summary","Transgender"),`data-pol`="summ",`data-char`="transgender",class="eef-grid-summ eef-grid21",href="javascript:void(0)",tags$img(src="icons/info90.svg",class="eef-grid-info90"),"Transgender"))
          ),div(class="eef-grid-content",
                
                # gridRowOdd("busEnt"),
                # gridRowEven("chiFam"),
                # gridRowOdd("criJus"),
                # gridRowEven("culCom"),
                # gridRowOdd("dem"),
                # gridRowEven("empSLL"),
                gridRowOdd("empSLL"),
                gridRowEven("busEnt"),
                gridRowOdd("chiFam"),
                gridRowEven("covid"),
                gridRowOdd("criJus"),
                gridRowEven("culCom"),
                gridRowOdd("dem"),
                gridRowEven("health"),
                gridRowOdd("houReg"),
                gridRowEven("incPov"),
                #gridRowEven("labSoc"),
                gridRowOdd("labMar"),
                gridRowEven("locThi"),
                #gridRowOdd("locGov"),
                gridRowOdd("rurEnv"),
                gridRowEven("schEdu"),
                gridRowOdd("socSec"),
                #gridRowEven("thiSec"),
                gridRowEven("transp")
          )
      )
      )
}
gridRowOdd <- function(id,policyArea=gsub("and","&",policyLabel(id)),img=paste0("icons/",id,".svg"),link=rep("javascript:void(0)",8)) {
  ns <- NS(id)
  tagList(tags$p(id=ns(0),class="eef-grid-policy eef-grid20",tags$img(src=img,class="eef-grid-icon"),policyArea),
          tags$a(id=ns("grid1"),title=eefTooltip(policyArea,"Age"),`data-pol`=id,`data-char`="age",class="eef-grid-square eef-grid00",onclick="",href=link[1],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid2"),title=eefTooltip(policyArea,"Disability"),`data-pol`=id,`data-char`="disability",class="eef-grid-square eef-grid01",href=link[2],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid3"),title=eefTooltip(policyArea,"Ethnicity"),`data-pol`=id,`data-char`="ethnicity",class="eef-grid-square eef-grid00",href=link[3],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid4"),title=eefTooltip(policyArea,"Gender"),`data-pol`=id,`data-char`="gender",class="eef-grid-square eef-grid01",href=link[4],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid5"),title=eefTooltip(policyArea,"Religion"),`data-pol`=id,`data-char`="religion",class="eef-grid-square eef-grid00",href=link[5],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid6"),title=eefTooltip(policyArea,"Sexual Orientation"),`data-pol`=id,`data-char`="sexualOrientation",class="eef-grid-square eef-grid01",href=link[6],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid8"),title=eefTooltip(policyArea,"Socio-Economic Status"),`data-pol`=id,`data-char`="socioEconomicStatus",class="eef-grid-square eef-grid00",href=link[8],tags$img(src="icons/info.svg",class="eef-grid-info")),#this has id grid8 as it was added later than the other characteristics
          tags$a(id=ns("grid7"),title=eefTooltip(policyArea,"Transgender"),`data-pol`=id,`data-char`="transgender",class="eef-grid-square eef-grid01",href=link[7],tags$img(src="icons/info.svg",class="eef-grid-info"))
  )
}

gridRowEven <- function(id,policyArea=gsub("and","&",policyLabel(id)),img=paste0("icons/",id,".svg"),link=rep("javascript:void(0)",8)) {
  policyArea <- gsub("and","&",policyLabel(id))
  img <- paste0("icons/",id,".svg")
  ns <- NS(id)
  tagList(tags$p(id=ns(0),class="eef-grid-policy eef-grid21",tags$img(src=img,class="eef-grid-icon"),policyArea),
          tags$a(id=ns("grid1"),title=eefTooltip(policyArea,"Age"),`data-pol`=id,`data-char`="age",class="eef-grid-square eef-grid10",href=link[1],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid2"),title=eefTooltip(policyArea,"Disability"),`data-pol`=id,`data-char`="disability",class="eef-grid-square eef-grid11",href=link[2],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid3"),title=eefTooltip(policyArea,"Ethnicity"),`data-pol`=id,`data-char`="ethnicity",class="eef-grid-square eef-grid10",href=link[3],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid4"),title=eefTooltip(policyArea,"Gender"),`data-pol`=id,`data-char`="gender",class="eef-grid-square eef-grid11",href=link[4],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid5"),title=eefTooltip(policyArea,"Religion"),`data-pol`=id,`data-char`="religion",class="eef-grid-square eef-grid10",href=link[5],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid6"),title=eefTooltip(policyArea,"Sexual Orientation"),`data-pol`=id,`data-char`="sexualOrientation",class="eef-grid-square eef-grid11",href=link[6],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid8"),title=eefTooltip(policyArea,"Socio-Economic Status"),`data-pol`=id,`data-char`="socioEconomicStatus",class="eef-grid-square eef-grid10",href=link[8],tags$img(src="icons/info.svg",class="eef-grid-info")),
          tags$a(id=ns("grid7"),title=eefTooltip(policyArea,"Transgender"),`data-pol`=id,`data-char`="transgender",class="eef-grid-square eef-grid11",href=link[7],tags$img(src="icons/info.svg",class="eef-grid-info"))
  )
}

# Equality evidence sections wrapper --------------------------------------

eefContent <- function(id) {
  ns <- NS(id)
  content <- distinct(EEFindex,class,topic,topicID)
  if(nrow(content)==0) return(NULL)
  
  sectionContent <- function(id,topic,class) {
    char <- unique(EEFindex$characteristic[EEFindex$topic%in%topic & EEFindex$class%in%class])
    tagList(eefSection(id=id,
                       title=topic,
                       class=paste(class,paste(equalityCSS(char),collapse=" ")),
                       colour="eef-section-main",
                       tabs=equalityButtons8(NS(id,"section"),title=topic,active=equalityCharacteristics %in% char),
                       content=uiOutputLoading(NS(id,"panel"))))
    #content=tabsetPanel(id=ns("tabs"))))
  }
  mapply(FUN=sectionContent,topic=as.character(content$topic),id=ns(content$topicID),class=as.character(content$class))
}


# EEF Section Template ----------------------------------------------------

eefSection <- function(id,title,class="",colour="eef-mid-blue",tabs=NULL,content=NULL) {
  ns <- NS(id)
  tags$section(
    id=ns("section"),
    class=paste("w3-row  w3-content eef eef-section",class),style="width:100%;",
    br(),
    br(),
    tags$button(
      id = ns("header"),
      `data-section`=title,
      title,
      tags$img(src="icons/plus.svg",class="eef-section-header-expand",title="Click to show section"),
      shinyjs::hidden(tags$img(src="icons/minus.svg",class="eef-section-header-collapse",title="Click to hide section")),
      class=paste("eef-section-header active",colour), 
      tags$a(class="e3-white",style="margin-left:1ch")
    ),
    shinyjs::hidden(
      div(id=ns("content"),
          class="w3-content eef-section-content eef-text",
          tabs,
          br(),
          content
      )
    )
  )
  
}

equalityButtons <- function(id,equalityID,class="eef-equality-buttons8",title="",active=NULL,selected="Overview") {
  ns <- NS(id)
  if(is.null(active)) active <- rep(TRUE,length(equalityID))
  div(class="eef-clearfix hidden-xs", role = "navigation", `aria-label` = "Equality Groups",
      pmap(list(equalityID,active,class),
          ~tags$button(
            id = ns(..1),
            equalityLabel(..1),
            href = "#",
            `data-id`=id, #used in the javascript code to track which section's tab was clicked
            `data-section`=title, #the title is added as the `data-section` attribute, so that it can be used in the google analytics code
            `data-char`=..1, #used in the javascript code to track which equality tab was clicked
            class=paste(..3,ns("buttons"),
                        ifelse(selected==equalityLabel(..1),"selected",""),
                        ifelse(..2,"","inactive"))
            )
      )
  )
}
equalityButtons8 <- function(id,title="",active=rep(TRUE,8),selected="Overview",buttonClass=rep("",8))
  equalityButtons(id=id,
                  title=title,
                  selected=selected,
                  equalityID=c("age","disability","ethnicity","gender","religion","sexualOrientation","socioEconomicStatus","transgender"),
                 class=paste("eef-equality-buttons8 eef-equality-buttons-light",buttonClass[c(1,2,3,4,5,6,8,7)]),
                 active=active[c(1,2,3,4,5,6,8,7)]) #not in numeric order, the 8th equality characteristic is socioEconomicStatus as it was added to the app after the others
equalityButtons9 <- function(id,title="",active=rep(TRUE,9),selected="Overview",buttonClass=rep("",9))
  equalityButtons(id=id,
                  title=title,
                  selected=selected,
                  equalityID=c("overview","age","disability","ethnicity","gender","religion","sexualOrientation","socioEconomicStatus","transgender"),
                 class=paste("eef-equality-buttons9 eef-equality-buttons-dark",buttonClass[c(1,2,3,4,5,6,7,9,8)]),
                 active=active[c(1,2,3,4,5,6,7,9,8)]) #not in numeric order, the 9th equality characteristic is socioEconomicStatus as it was added to the app after the others



# Banner images -----------------------------------------------------------

eefHeaderImage <- function(uid,title,image="",imageColourClass="eef-blue",active=1,class="") {
  ns=NS(uid)
  tagList(br(id=ns("top-spacer")),br(),
          div(class=paste("w3-display-container eefParallax",class),
              style=paste("background-image: url(",image,");"),
              tabindex="-1",
              id=ns("top"),
              div(class="w3-display-middle eef-subheader-text",
                  tags$h2(class=paste("w3-center w3-padding-large ",imageColourClass,"w3-xlarge w3-wide w3-animate-opacity"),
                            title)
              )
          )#,br(),br()
  )
}


# NPF ---------------------------------------------------------------------

npfText <- function(class) div(class=paste("eef-text w3-content",class),
                               includeMarkdown("data/NPF text.md")
)

#added to some panels that contain NPF indicators
npfCog <- function(id,ind) {
  ns <- NS(id)
  if(length(ind)>1) indText <- paste("These are the",paste0(ind[-length(ind)],collapse=", "),"and",ind[length(ind)],"National Indicators.") else indText <- paste("This is the",ind,"National Indicator.")
  fluidRow(column(1,img(src="icons/NPFlogo.png",width=50,height=50)),
           column(11,
                  p(paste(indText,"Explore the full range of National Indicators on the "),
                    a(href="javascript:void(0)",id=ns("npf-link"),"interactive dashboard."),
                    tags$script(paste0("$('#",ns("npf-link"),"').click(function(){$('#equality-menu2')[0].click();gtag('event', 'click', {'event_category': policyEquality, 'event_label': $(this).attr('id') });});")),
                    "Visit the",
                    a(href="http://www.nationalperformance.gov.scot",target="blank_","National Performance Framework website"),
                    "for more information.")))
}

# Footer ------------------------------------------------------------------

eefFooter = function(id)  {
  ns <- NS(id)
  tags$footer(class="w3-center eef-footer eef-blue w3-padding-64",
                                      a(id=ns("menu6"),href="javascript:void(0)","Accessibility Statement",class="w3-button e3-opacity-blue w3-hover-white",
                                        `data-pol` = "accessibility", `data-char` = "overview"),#class "eef eef-XXXX" shows/hides components when on/not on page XXXX 
                                      tags$a(class="eef-footer-top",href="javascript:void(0);",class="w3-button e3-opacity-blue w3-hover-white", tags$i(class="fa fa-arrow-up w3-margin-right"),"To the top")
                                      # ,div(class="w3-xlarge w3-section",
                                      # tags$i(class="fa fa-facebook-official w3-hover-opacity"),
                                      # tags$i(class="fa fa-instagram w3-hover-opacity"),
                                      # tags$i(class="fa fa-snapchat w3-hover-opacity"),
                                      # tags$i(class="fa fa-pinterest-p w3-hover-opacity"),
                                      # tags$i(class="fa fa-twitter w3-hover-opacity"),
                                      # tags$i(class="fa fa-linkedin w3-hover-opacity")
                                      #)
)
}