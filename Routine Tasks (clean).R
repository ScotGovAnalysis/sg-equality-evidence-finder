###################################################################################
#                                                                                 #
# Author: Jaye Ware                                                               #
# Last updated: 07/01/2019                                                        #
#                                                                                 #
# Purpose: collate code routinely used to update the EEF app.                     #
#                                                                                 #
###################################################################################

###WARNING: do not run this entire script - this script contains commonly run statements. Select the lines to run individually as needed.
###

#STEP 1: this always needs running - specify folders to use and load the source code 
#for version control, multiple different versions are currently stored on the datashare - including "main", "test" and "experimental" apps
mainApp <- list(updatesDir = "Content Updates",
                appDir = "EEF-beta10.0",
                reportsDir = "Update Reports",
                appDataDir = "App Data",
                scriptDir = "Pre-processing Scripts")
source(file.path(mainApp$scriptDir,"update_EEF4.r"), local = TRUE)

###STEP 2: generating reports for lead analysts

#generates a report detailing everything up to and including the specified date. 
#This uses the R date format (YYYY-MM-DD)
# - these are saved in the Reports folder
# - second parameter specifies the policy area(s) to report. allPolicyAreas is a variable set
#   in publish_EEF.r containing the full list of policy areas
generateReports(mainApp,endDate="2020-04-30",deadline=as.Date("2020-04-29"),reportPolicyArea=allPolicyAreas)

#generates a report summarising everything to be updated over the coming year
# - doesn't produce policy area specific reports
generateReports(mainApp,endDate="2021-12-31",reportPolicyArea=character(0))

###STEP 3: add content to the app folder

#don't run these in order! - run the set policy Area and update command lines individually as needed
updatePolicyArea <- "Summary"
updatePolicyArea <- "Business, Enterprise and Tourism"
updatePolicyArea <- "Children and Families"
updatePolicyArea <- "COVID-19"
updatePolicyArea <- "Crime and Justice"
updatePolicyArea <- "Culture, Communities and Society"
updatePolicyArea <- "Demographics"
updatePolicyArea <- "Advanced Learning and Skills"
updatePolicyArea <- "Health, Social Care and Sport"
updatePolicyArea <- "Housing and Regeneration"
updatePolicyArea <- "Income and Poverty"
updatePolicyArea <- "Labour Market"
updatePolicyArea <- "Social Security"
updatePolicyArea <- "Local Government and Third Sector"
updatePolicyArea <- "Rural and Environment"
updatePolicyArea <- "School Education"
updatePolicyArea <- "Transport and Travel"

update_sources(updatePolicyArea, mainApp)
update_index(updatePolicyArea, mainApp)
update_publication_links(updatePolicyArea, mainApp)
update_data_links(updatePolicyArea, mainApp)
update_external_links(updatePolicyArea, mainApp)
update_additional_links(updatePolicyArea, mainApp)
update_contacts(updatePolicyArea, mainApp)
update_EEF_data(updatePolicyArea, mainApp)
update_ODP_data(mainApp)
update_panels(updatePolicyArea, mainApp)
runApp(mainApp$appDir)

####STAGE 4: deploy to server####

#deploy test app
rsconnect::deployApp(appName = "sg-equality-evidence-finder",
                     appDir  = mainApp$appDir, 
                     account = "scotland")


