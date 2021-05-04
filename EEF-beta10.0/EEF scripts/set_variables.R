###################################################################################
#                                                                                 #
# SET VARIABLES - COLOUR PALETTE / POLICY AREA & EQUALITY IDS & LABELS            #
#                                                                                 #
###################################################################################
#                                                                                 #
# Author: Jay Ware                                                                #
# Created: 01/08/2018                                                             #
# Last updated: 14/01/2021                                                        #
#                                                                                 #
# Purpose: Set up variables listing the policy areas and equality characteristic  #
#          labels and IDs. Labels appear in the Evidence Finder Site, whereas IDs #
#          must be valid R (and HTML) variable names - i.e. contain no spaces.    #
#                                                                                 #
# Variables:                                                                      #
#                                                                                 #
# allPolicyAreasID: vector containing all policy area IDs currently used          #
# equalityCharacteristics: vector containing all equality characteristic labels   #
# equalityCharacteristicsID: vector containing all equality characteristic IDs    #
#                                                                                 #
# Functions:                                                                      #
#                                                                                 #
# eefColours(): Function returning the HTML colour for each of the EEF colours.   #
#               The valid EEF colours are listed in defaultColours. There are     #
#               several colour palettes coded for. The main ones used are         # 
#               "restricted" (mostly colour-blind friendly) and "blue"            #
# policyLabel(): Converts a policy area ID into the corresponding label           #
# policyCSS(): Converts a policy area (label or ID) into the relevant CSS class.  #
#              CSS classes use the policy area ID prefixed with "eef-"            #
# equalityLabel(): Converts a equality characteristic ID into the corresponding   #
#                  label                                                          #
# equalityCSS(): Converts a equality characteristic (label or ID) into the        #
#                relevant CSS class. CSS classes use the characteristic ID        #
#                prefixed with "eef-"                                             #
#                                                                                 #
###################################################################################


# Colour palette ----------------------------------------------------------

#17-colour pallete (including 6 shades of "eef" blue and 4 shades of "eef" orange, with other colours chosen to be colour-blind friendly based on 15-colour pallete from mkweb.bcgsc.ca/biovis2012 - with blue and orange modified slightly to fit eef scheme)
eefColours <- function(colour,colourPalette="restricted") {
  defaultColours <- c("blue","dark-red","purple","dark-green","orange","light-blue","grey","lilac","lime","brown",
                      "very-dark-blue","dark-orange","mid-red","black","mid-purple","red","mid-green","very-dark-orange",
                      "dark-yellow","dark-blue","yellow","green","dark-grey","mid-blue","light-orange","light-lilac",
                      "light-green","light-grey","very-light-blue","dark-brown","olive","dark-purple","very-dark-grey")
  
  
  #if(length(colour)<3)
  if(colourPalette=="neutral") defaultColours <- c("black","tundora","gray","silver","white")
  if(colourPalette=="diverging" & length(colour)<12) defaultColours <- brewer.pal(length(colour),"RdYlBu")
  if(colourPalette=="blue" & length(colour)<12) defaultColours <- c("very-dark-blue","mid-dark-blue","dark-blue","light-dark-blue","blue","dark-mid-blue","mid-blue","light-mid-blue","light-blue","mid-light-blue","very-light-blue")
  if(colourPalette=="blue" & length(colour)<7) defaultColours <- c("very-dark-blue","dark-blue","blue","mid-blue","light-blue","very-light-blue")
  if(colourPalette=="blue" & length(colour)==4)  defaultColours <- c("very-dark-blue","dark-blue","blue","light-blue")
  if(colourPalette=="blue" & length(colour)<4)  defaultColours <- c("very-dark-blue","blue","light-blue")
  if(colourPalette=="orange" & length(colour)<5) defaultColours <- c("very-dark-orange","dark-orange","orange","light-orange")
  if(colourPalette=="restricted" & length(colour)<11) defaultColours <- c("blue","dark-red","purple","dark-green","orange","light-blue","grey","lilac","lime","brown")
  if(colourPalette=="not-blue" & length(colour)<9) defaultColours <- c("dark-red","purple","dark-green","orange","grey","lilac","lime","brown")
  if(colourPalette=="not-blue" & length(colour)>=9) defaultColours <- c("dark-red","purple","dark-green","orange","grey","lilac","lime","brown",
                                                                        "dark-orange","mid-red","black","mid-purple","red","mid-green","very-dark-orange",
                                                                        "dark-yellow","yellow","green","dark-grey","light-orange","light-lilac",
                                                                        "light-green","light-grey","very-light-blue","dark-brown","olive","dark-purple","very-dark-grey")
  unusedColours <- setdiff(defaultColours,colour)
  
  palette <- ifelse(grepl("#",colour),toupper(colour),tolower(colour)) %>%
    ifelse(.%in%c(defaultColours,"dark-gray","gray","light-gray","very-dark-gray","very-light-blue"),.,unusedColours) %>% #"dark-gray","gray","light-gray","very-dark-gray","very-light-blue" will always be available as valid colours, but won't be automatically selected in some of the palettes.
    ifelse(.=="black","#000000",.) %>%
    ifelse(.=="tundora","#4B4B4B",.) %>%
    ifelse(.=="gray","#828282",.) %>%
    ifelse(.=="grey","#828282",.) %>%
    ifelse(.=="silver","#b9b9b9",.) %>%
    ifelse(.=="white","#FFFFFF",.) %>%
    ifelse(.=="blue","#0080db",.) %>%
    ifelse(.=="dark-red","#920000",.) %>%
    ifelse(.=="purple","#490092",.) %>%
    ifelse(.=="dark-green","#004949",.) %>%
    ifelse(.=="orange","#ff8021",.) %>%
    ifelse(.=="light-blue","#6bc1ff",.) %>%
    ifelse(.=="lilac","#b66dff",.) %>%
    ifelse(.=="lime","#24ff24",.) %>%
    ifelse(.=="brown","#924900",.) %>%
    ifelse(.=="very-dark-blue","#002d4d",.) %>%
    ifelse(.=="dark-orange","#db5f00",.) %>%
    ifelse(.=="mid-red","#ff0000",.) %>%
    ifelse(.=="black","#000000",.) %>%
    ifelse(.=="mid-purple","#8000ff",.) %>%
    ifelse(.=="red","#cc0000",.) %>%
    ifelse(.=="mid-green","#00adad",.) %>%
    ifelse(.=="very-dark-orange","#a84900",.) %>%
    ifelse(.=="dark-yellow","#cccc00",.) %>%
    ifelse(.=="dark-blue","#005999",.) %>%
    ifelse(.=="mid-dark-blue","#004475",.) %>%
    ifelse(.=="dark-mid-blue","#0095ff",.) %>%
    ifelse(.=="mid-light-blue","#8fd0ff",.) %>%
    ifelse(.=="light-dark-blue","#006bb8",.) %>%
    ifelse(.=="yellow","#ffff33",.) %>%
    ifelse(.=="green","#009900",.) %>%
    ifelse(.=="dark-grey","#666666",.) %>%
    ifelse(.=="dark-gray","#666666",.) %>%
    ifelse(.=="mid-blue","#24a4ff",.) %>%
    ifelse(.=="light-mid-blue","#47b3ff",.) %>%
    ifelse(.=="light-orange","#ffa35c",.) %>%
    ifelse(.=="light-lilac","#d4a8ff",.) %>%
    ifelse(.=="light-green","#80ff80",.) %>%
    ifelse(.=="light-grey","#aaaaaa",.) %>%
    ifelse(.=="light-gray","#aaaaaa",.) %>%
    ifelse(.=="very-light-blue","#b8e1ff",.) %>%
    ifelse(.=="dark-brown","#4d2600",.) %>%
    ifelse(.=="olive","#666600",.) %>%
    ifelse(.=="dark-purple","#4d004d",.) %>%
    ifelse(.=="very-dark-grey","#3f3f3f",.) %>%
    ifelse(.=="very-dark-gray","#3f3f3f",.)
  names(palette) <- names(colour)
  return(palette)
}


# Page IDs ----------------------------------------------------------------

#Don't think this is used anymore. All ui components are now in the "equality" namespace
allPagesID <- c("home2","Approach","equality")

# Policy areas ------------------------------------------------------------
allPolicyAreasID <- c("summ","busEnt","chiFam","covid","criJus","culCom","dem","empSLL","health","houReg","incPov","labMar","locThi","rurEnv","schEdu","socSec","transp")

policyLabel <- function(id) {
  case_when(id == "summ" ~ "Summary",
            id == "busEnt" ~ "Business, Enterprise and Tourism",
            id == "chiFam" ~ "Children and Families",
            id == "covid" ~ "COVID-19",
            id == "criJus" ~ "Crime and Justice",
            id == "culCom" ~ "Culture, Communities and Society",
            id == "cult" ~ "Culture",#not currently used
            id == "dem" ~ "Demographics",
            id == "empSLL" ~ "Advanced Learning and Skills",
            id == "health" ~ "Health, Social Care and Sport",
            id == "houReg" ~ "Housing and Regeneration",
            id == "incPov" ~ "Income and Poverty",
            id == "labMar" ~ "Labour Market",
            id == "labSoc" ~ "Labour Market and Social Security",#not currently used
            id == "locGov" ~ "Local Government",#not currently used
            id == "locThi" ~ "Local Government and Third Sector",
            id == "rurEnv" ~ "Rural and Environment",
            id == "schEdu" ~ "School Education",
            id == "socSec" ~ "Social Security",
            id == "thiSec" ~ "Third Sector",#not currently used
            id == "transp" ~ "Transport and Travel",
            TRUE ~ "")
}

policyCSS <- function(policy_area) {
  case_when(policy_area=="Summary" ~ "summ",
            policy_area=="Business, Enterprise and Tourism" ~ "busEnt",
            policy_area=="Children and Families" ~ "chiFam", 
            policy_area=="COVID-19" ~ "covid", 
            policy_area=="Crime and Justice" ~ "criJus", 
            policy_area=="Culture, Communities and Society" ~ "culCom", 
            policy_area=="Demographics" ~ "dem", 
            policy_area=="Employability, Skills and Lifelong Learning" ~ "empSLL", 
            policy_area=="Advanced Learning and Skills" ~ "empSLL", 
            policy_area=="Health, Social Care and Sport" ~ "health", 
            policy_area=="Housing and Regeneration" ~ "houReg", 
            policy_area=="Income and Poverty" ~ "incPov", 
            policy_area=="Labour Market and Social Security" ~ "labSoc", 
            policy_area=="Labour Market" ~ "labMar", 
            policy_area=="Local Government and Third Sector" ~ "locThi", 
            policy_area=="Rural and Environment" ~ "rurEnv", 
            policy_area=="School Education" ~ "schEdu", 
            policy_area=="Social Security" ~ "socSec", 
            policy_area=="Transport and Travel" ~ "transp",
            policy_area=="National Performance Framework" ~ "npf",
            TRUE ~ policy_area) %>%
    paste0("eef-", .)
}


# Equality characteristics ------------------------------------------------

equalityCharacteristics <- c("Age","Disability","Ethnicity","Gender","Religion","Sexual Orientation","Transgender","Socio-Economic Status")
equalityCharacteristicsID <- c("age","disability","ethnicity","gender","religion","sexualOrientation","transgender","socioEconomicStatus")

equalityLabel <- function(id) {
  case_when(id == "age" ~ "Age",
            id == "disability" ~ "Disability",
            id == "ethnicity" ~ "Ethnicity",
            id == "gender" ~ "Gender",
            id == "religion" ~ "Religion",
            id == "sexualOrientation" ~ "Sexual Orientation",
            id == "socioEconomicStatus" ~ "Socio-Economic Status",
            id == "transgender" ~ "Transgender",
            TRUE ~ "Overview")
}

equalityCSS <- function(char) {
  case_when(char == "Socio-Economic Status" ~ "socioEconomicStatus",
            char == "Sexual Orientation" ~ "sexualOrientation",
            char %in% equalityCharacteristics ~ tolower(char),
            TRUE ~ char) %>%
  paste0("eef-", .)
}


