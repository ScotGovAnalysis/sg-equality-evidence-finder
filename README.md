# Equality Evidence Finder

> You may use or re-use this source code (not including logos or icons) and documentation free of charge in any format or medium, under the terms of the Open Government Licence. See: [www.nationalarchives.gov.uk/doc/open-government-licence](www.nationalarchives.gov.uk/doc/open-government-licence)

## About

The [Equality Evidence Finder Shiny App](www.equalityevidence.scot) summarises the equality evidence produced by the Scottish Government and its Agencies. Note that the data contained in the repository is not kept up to date. The latest available data can be found on the [Equality Evidence Finder](www.equalityevidence.scot).

Please contact the [Scottish Government Equality Analysis Team](mailto:social-justice-analysis@gov.scot) for further information.

The code for this app is split into two parts: -

* Pre-processing: Processes the raw data and knits R markdown files before deploying the app. This is used for data and text that is updated infrequently on an ad hoc basis, and doesn't need to be done whilst the app is running. Code and data is found in the App data directory
* App: Source code for the app. Currently in the EEF-beta7 directory

## 1. Pre-processing

The raw app data is found in App Data/EEF.rData. This contains several datasets used for managing the content on the app: -

* **EEFsources:** contains the details for each of the data sources used in the app. Each has a unique series name, along with publication name, date and links
* **EEFpublished:** contains the list of publication series to be included in the links sections, along with flags specifying which characteristic(s) the link should appear under
* **EEFdataLinks:** is similar to EEFpublished, but contains the list of data included in the data links section
* **EEFexternal:** is similar to EEFpublished, but contains the list of external organisations included in the external links section (external publications still are found on the EEFpublished dataset)
* **EEFindex:** contains the data for the content to be included in each of the policy area topic sections, along with the filepath of images and markdown files to be included. There is one row for each panel displayed on the Evidence Finder, containing a headline, graph title, graph, commentary (in markdown) and the data source series (see EEFsources)
* **EEFdata:** contains the data for interactive charts. The data for all charts is stored under the same set of variables, with Characteristic, Indicator, Measure, and Breakdown specifying what the data value represents (Characteristic is the equality characteristic that the data relates to; Indicator roughly corresponds to the panel the chart appears on; Measure corresponds to different chart views users can select between; and Breakdown is the various series to be displayed - though different chart templates use these slightly differently)
* **NPFdata:** Similar to EEFdata but contains a copy of the NPF indicator dataset. NPFdata has an additional variable for NPF Outcome
* **ODPdata:** A lookup table for the Characteristic, Indicator, Measure, Breakdowns for the data pulled in from the open data platform using sparql. The data on the open data platform isn't ordered, so this lookup table is used to specify the order (e.g. age bands are in ascending order). This lookup table also allows extra parameters to be specified including the graph title and series colour.
* **EEFadditional:** is similar to EEFpublished, but contains a list of links that don't fit into any of the other categories (currently used for data collection guidance and glossary).

The app will read in the processed master data files and will automatically generate the app based on these. Any step that doesn't need to be run every time a user visits the website is done at this stage to try to reduce the loading time of the app. This includes: -
* knitting R markdown files
* tidying up variables and data types
* specifying the data to be used for interactive graphs

The pre-processing scripts are found in the app data folder: -
* **update_app.r:** main script to be run when performing updates
* **pre_process_EEF.r:** various misc. tidying up tasks are performed here.
* **graph_data.r:** interactive charts are specified here. These are stored in two lists, with the contents tagged with the id of the panel the chart relates to. filterData  is a list of the various data frames used by the charts; and graphOptions is a list containing the various options for how the interactive chart is displayed. The full list of graph options is listed below.

Example:

``` r
filterData[["PANEL_ID"]] <- filter(EEFdata,policy_area=="SOME POLICY AREA",Indicator=="SOME INDICATOR")
graphOptions[["PANEL_ID"]] <- list(graphType="timeSeries1",#timeSeries1 is a line chart with user options for breakdowns
                                   digits=1)#digits is number of digits the chart rounds values to
```
### Interactive Graph Options

Interactive charts are built based on one of the app's chart templates. This avoids the need to build new charts from scratch each time, though  

* **$graphType** Specifies the graph template to be used for the interactive chart. Currently graph templates have been created for time series, bar charts and pie/donought charts. Each of the templates has several flavours containing different user options/inputs
    * timeSeries0: Time series chart using the dygraphs package, with no user options
    * timeSeries1: Time series chart using the dygraphs package, with checkbox group to select which breakdowns to display
    * timeSeries2: Time series chart using the dygraphs package, with dropdown menu to select the Measure, and checkbox group to select which breakdowns to display
    * barChart0: Bar chart using the google charts package, with no user options
    * barChart1: Bar chart using the google charts package, with checkbox group to select which breakdowns to display
    * barChart2: Bar chart using the google charts package, with dropdown menu to select the Indicator, and checkbox group to select which breakdowns to display
    * barChart3: Bar chart using the google charts package, with checkbox group to select the Measure, and checkbox group to select which breakdowns to display
    * pieChart0: Pie/Donought chart using the google charts package, with no user options

> TO ADD: full list of options

## 2. Shiny App

The shiny app has been built using [Shiny Modules](http://shiny.rstudio.com/articles/modules.html). This allows the same code to be used multiple times in the app without naming conflicts, and is used for organising the code. The code for each of the Evidence Finder app components is split between a

* module UI function - found in EEF scripts/ui_pack.r or EEF scripts/ui_graphs.r
* module Server function - found in EEF scripts/server_pack.r or EEF scripts/server_graphs.r
* CSS code for styling - found in EEF scripts/EEFextra.css
* Javascript code for showing/hiding components and google analytics events - found in www/javascript/EEF.js

> **Note:** Some components don't have a server function, css code and/or javascript code

Miscelaneous code can be found  in EEF scripts/helper_funcs.r. This includes functions for reading data from [statistics.gov.scot], the label formatter used from formatting dates in dygraphs, and the Evidence Finder colour palette.

###  app.r

app.r contains the code for loading the pre-processed app data (stored in EEF/index.rData and EEF/graph_data.rData), and calls the various module components.


### App Components - UI functions

* **eefHelp():** contains the help screens for each graph. The text for these are written in markdown, found in the EEF/help folder. This starts hidden
* feedback banner - not placed in a function as only one line - this places the beta banner at the top of the page
* **eefHeader():** the main header banner. This contains the text and images for all the pages, with only the relevant text/image displayed at any one time. Also contains the menu links.
* **eefGrid():** wrapper function containing the grid components. 
* **eefSection():** creates a drop down section for the NPF data explorer, publications, data and external links sections. eefSection() is also run by the eefContent() function to add the topic sections.
* **eefContent():** wrapper function that adds the topic sections for each policy area. These are all hidden to start with, and only become visible when the user clicks on the relevant grid square. This function runs the eefSection() UI component for each of the ~60 sections. The function reads in the topic data from the EEFindex (modules.csv) dataset and automatically populates the sections.
* **eefHeaderImage():** banner image and text that are used to break up the page. Currently these display on all pages, except NPF which is on the main page only
* **npfText():** reads in a markdown file containing the NPF text (EEF/ NPF text.md). Currently this only appears on the main page
* **eefContactList():** function that reads in 1 or 2 markdown files displaying the contact details for the Equality Stats team, and policy area (if relevant). Currently this is temporarily overwritten when the test app is updated so that it can read in the policy area details from the sandbox markdown file instead of the main app file.
* **eefFooter():** adds the footer at the bottom.

### App Components - Server functions

* **eefPublicationsSectionServer():** updates the links sections depending on the policy area and characteristic selected. Note that the app only has one publication links section that appears on all pages, rather than separate ones for each policy page.
* **npfSectionServer():** displays the NPF dashboard for the selected equality characteristic (or a message saying breakdowns aren't available)
* **npfServer3():** graph server for the npf dashboard
* **eefSummarySectionServer():** displays the single-panel content (or panel tabs) for the selected equality characteristic. This is looped through for each topic section using lapply
* **timeSeriesServer0(),...:** graph servers for each of the chart templates. This is run for each panel containing a graph, and the app uses lapply to select the correct server based on the graph type in graphOptions.

### App Components - Javascript

Javascript written in the jquery syntax is mostly used for google analytics, and for showing/hiding components. The Evidence Finder uses some custom javascript to handle navigation instead of navbarPage due to the large (120) number of "subpages". Instead the Evidence Finder keeps track of the "subpage" a user is on in a javascript variable and shows/hides the components as needed.

Any component that is page specific is tagged with the class "eef" or "eef-main". An additional class is used to denote the page(s) it is visible on. For example:

* "eef-main": visible on home page only
* "eef eef-main eef-summ": visible on home page and equality characteristic summary pages
* "eef eef-criJus": visible on the Crime and Justice pages only

## Reference

* [Shiny Modules](http://shiny.rstudio.com/articles/modules.html)
* [Dygraphs options documentation](http://dygraphs.com/options.html)
* [Google charts - bar charts documentation](https://developers.google.com/chart/interactive/docs/gallery/barchart)
* [Google charts - pie charts documentation](https://developers.google.com/chart/interactive/docs/gallery/piechart)
* [jquery tutorial](https://www.w3schools.com/jquery/default.asp)
* [css tutorial](https://www.w3schools.com/Css/)
