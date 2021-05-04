# Equality Evidence Finder

## License

You may use or re-use this source code (not including logos, icons, or images) and documentation free of charge in any format or medium, under the terms of the Open Government Licence. See: [www.nationalarchives.gov.uk/doc/open-government-licence](www.nationalarchives.gov.uk/doc/open-government-licence). Icons used on the Equality Evidence Finder (not included within this repository) require a seperate license from <https://www.flaticon.com> 

## About

The [Equality Evidence Finder Shiny App](www.equalityevidence.scot) summarises the equality evidence produced by the Scottish Government and its Agencies. Note that the data contained in the repository is not kept up to date. The latest available data can be found on the [Equality Evidence Finder](www.equalityevidence.scot).

Please contact the [Scottish Government Equality Analysis Team](mailto:social-justice-analysis@gov.scot) for further information.

The code for this app is split into two parts: -

* Pre-processing: Processes the raw data and knits R markdown files before deploying the app. This is used for data and text that is updated infrequently on an ad hoc basis, and doesn't need to be done whilst the app is running. Code and data is found in the App data directory
* App: Source code for the app.

The folders used for the Evidence Finder are:
*	Content Updates – folder containing the content provided by analysts
*	EEF-beta10 – folder for the actual app itself
*	Update Reports – folder where any reports are saved
*	App Data – data that is used during the update process. This contains the input data that is added to the app.
*	Pre-processing Scripts – contains the R scripts used for updating the app (the app has a separate folder containing app scripts)


## 1. Pre-processing

The routine tasks.R script contains code used to routinely update the app

### Step 1: Preliminary tasks

List(s) are set up specifing the various directories used for the app. The update functions script is then sourced.

### Step 2: Generate reports

Reports can be generated aid identifying content which needs updating, based on previous year's updates. Reports are generated using Rmarkdown, which looks at the next update columns on the EEF datasets, and identifies anything that is due to be updated. The functions for these are in update_EEF4.r, maintain_EEF_reports.Rmd and maintain_EEF_summary.Rmd. However these only need modifying if you want to change the report templates.

Reports are saved in the Reports folder

### Step 3: Updating the app

Various functions are used to update each part of the app content. These are usually run for each policy area individually as new content is provided

* __update_sources()__: Reads in the sources spreadsheet for specified policy area(s) and adds to the App Data dataset. Updates EEFsources and EEFlatestSource in App folder.                                
* __update_index()__: Reads in the modules spreadsheet for specified policy area(s) and adds to the App Data dataset. Adds topic ID and CSS classes to EEFindex and updates EEFindex in App folder                  
* __update_publication_links()__: Reads in the publications spreadsheet for specified policy area(s) and adds to the App Data dataset. Updates EEFpublished and EEFpublishedIndex in App folder                                              
* __update_data_links()__: Reads in the publications spreadsheet for specified policy area(s) and adds to the App Data dataset. Updates EEFdataLinks and EEFdataLinksIndex in App folder           
* __update_external_links()__: Reads in the external spreadsheet for specified policy area(s) and adds to the App Data dataset. Updates EEFexternal and EEFexternalIndex in App folder 
* update_additional_links(): Reads in the additional links spreadsheet for specified policy area(s) and adds to the App Data dataset. Updates EEFadditional in App folder         
* __update_contacts()__: Reads in the contacts markdown file for specified policy area(s) and updates contact in App folder                    
* __update_EEF_data()__: Reads in the data spreadsheet for specified policy area(s) and adds to the App Data dataset. Adds interval type based on graphOptions (recommended) or trys to guess from data spreadsheet.                                                 
* __update_ODP_data()__: Reads in the NPF spreadsheet from the open data platform and adds to the App Data dataset. Adds dateCode (label) and interval type based on best guess from NPF spreadsheet (requires NPF spreadsheet to use reasonably sensible date naming conventions)                                          
* __update_panels()__: Reads in the data from the App data folder for specified tabUID(s) and updates graph, graphOptions, panel, commentary, NPFdataIndex and SPARQLindex in the App folder. Copies images from the updates folder. Reads in (and knits) markdown files. [Policy areas can be specified instead of individual tabUIDs]  
* __update_all()__: Wrapper function that will run all of the above update functions or specified policy area(s)                                      

#### Adding interactive charts

The code needs adding to the graph_data.r script (in the pre-processing folder).

To set up interactive charts, various parameters need setting which filter the data to be used in the chart, and control how it is displayed. This step can be simplified by finding a similar chart and copying the code.

The app uses two lists, which contain all the filtered data, and graph parameters. In R, lists are collections containing various R variables with variables usually being named. The app uses the lists setGraphData and setGraphOptions, containing variables with names corresponding to the tab panel id that the chart appears on (see the analysts modules spreadsheet – column A – to find this tab panel id). setGraphData contains the filtered data, and setGraphOptions contains the various graph parameters.

The data for a particular tab panel id is assigned to the corresponding variable in the setGraphData list. For example for the “disability-1” tab panel ID:

```setGraphData[["disability-1"]]```

Data can be read in from one of the datasets in the app data folder. Usually it's necessary to filter these datasets to produce the relevant data for the panel's chart. The available datasets are: -
*	EEFdata – contains all the data from analysts’ data spreadsheets
*	EEFlatest – a filtered version of EEFdata, that only shows data for the latest year
*	SHSdata – contains all the data from the SHS tables provided by the SHS team (same datasets that are used on their data explorer though transformed into the standard data structure we use)
*	NPFdata – a copy of the dataset from the open data platform (see automated updates section)
*	ODPdata – a copy of the sparql.xlsx spreadsheet (see automated updates section)

Usually filtering by Characteristic, Measure and policy_area is sufficient to select the relevent data.

The second variable that needs setting is in the setGraphOptions list. E.g.

```setGraphOptions[["disability-1"]] = list(graphType = "timeSeries1")```

This variable should be set to a list() containing the graphType, and any other parameters. The parameters used vary between different graphs – the only necessary parameter is graphType.

## 2. Shiny App

> To add

### Reference

* [Shiny Modules](http://shiny.rstudio.com/articles/modules.html)
* [Dygraphs options documentation](http://dygraphs.com/options.html)
* [Google charts - bar charts documentation](https://developers.google.com/chart/interactive/docs/gallery/barchart)
* [Google charts - pie charts documentation](https://developers.google.com/chart/interactive/docs/gallery/piechart)
* [jquery tutorial](https://www.w3schools.com/jquery/default.asp)
* [css tutorial](https://www.w3schools.com/Css/)