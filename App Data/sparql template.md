#SPARQL Query Template

SPARQL queries are added to the Evidence Finder graph_data.r script. The query should be entered
as a parameter in the graphOptions list e.g.
``` r
graphOptions[["INSERT ID"]] <- list(query="INSERT QUERY")
```
The app code requires the sparql query to create a dataset including the standard EEF data variables with some of the additional metadata (Indicator, Measure, Breakdown, Date, Figure, DateCode yLabel,...). Queries can be written by

1. copying the below template into the <https://statistics.gov.scot/sparql> query editor
2. adding the relevant URIs. These can be obtained by clicking the api tab for the dataset on the open data platform
    * the datacube URI is in the URI section (note: this is only visible **before** you lock any dimensions)
    * everything else is found in the Dimension-locked value pairs (you need to copy the URIs rather than the labels for this)
    * Indicator/Measure/Breakdown - only the left hand (Dimension) is needed - no values are being locked
    * For all other dimensions, the dimension and locked values need specifying. You can lock all these dimensions on the data tab
    * note: the measure type URI is only visible after you have locked the relevant measure (Ratio/count/etc.) - or see below for the common types
    * Reference area and reference period can usually be ignored. The template has the Scotland level geography pre-filled.
3. run the query. check that it runs successfully and returns the dataset you expected at the bottom
4. Currently sparql can't be run on scots machines. However if the query works on the query editor, it should be fine to copy it into the Evidence Finder graph options.
5. A lookup table needs adding to the data tab of the sandbox spreadsheet. This is used to determine the order that e.g. Breakdowns appear. The columns that need completing are: Indicator, Measure, Breakdown, graphTitle (and seriesColour if used). These need to match exactly the Indicator, Measure and Breakdown values from the sparql queries (case-sensitive).
5. If a new type of date interval is used, the app code may need updating so that it knows how to deal with it:
    * eefSparql() [helper_funcs.r] <- intervalType, Year, Date need to be correctly derived from the sparql input
    * eefDygraphFormatter() [helper_funcs.r] <- dygraph formatter may need updating to specify how to format values and axis labels (needs to be in javascript, but can usually copy from a similar interval type)
    * Update TestApp.r / update_app.r <- How to construct DateCode may need specifying for the particular intervalType (DateCode is the string version of the date as used in the csv download buttons)
6. There are additional graphOptions parameters to control how the app treats the query
    * updateQuery: set to TRUE if you want the dataset to be read into the app everytime the app is viewed by a user (the default is for the update to only be done when the updateApp() script is run before deploying the app)
    * filterSparql: by default the app will only include sparql query results that appear in the look-up table in the main spreadsheets (see step 5). Set filterSparql to FALSE to include everything that the query returns

##Add URIs for

* geography (Scotland level has been pre-filled. Change this for any other levels)
    * Scotland: <http://statistics.gov.scot/id/statistical-geography/S92000003>
* datacube
* Locked dimensions (include the locked dimension URI followed by the locked value URI)
* Indicator dimension (only used for NPF Indicators - otherwise use the datacube predicate <http://purl.org/linked-data/cube#dataSet>)
* Measure dimension (only used for where user can select what is plotted on a line chart, or bar charts with multiple series - otherwise use the datacube predicate <http://purl.org/linked-data/cube#dataSet>)
* Breakdown dimension (always needed - usually an equality characteristic)
    * Age: <http://statistics.gov.scot/def/dimension/age>
    * Gender: <http://statistics.gov.scot/def/dimension/gender>
* Measure type (is the value the ratio or count?)
    * Count: <http://statistics.gov.scot/def/measure-properties/count>
    * Ratio: <http://statistics.gov.scot/def/measure-properties/ratio>

##Template

```
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
WHERE {
  ?obs 
    <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; #SCOTLAND LEVEL GEOGRAPHY
    <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
    <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
    
    <http://purl.org/linked-data/cube#dataSet> <INSERT DATACUBE URI> ;
    <INSERT LOCKED DIMENSION URI> <INSERT LOCKED VALUE URI>;
    <INSERT INDICATOR DIMENSION URI> ?i ;
    <INSERT MEASURE DIMENSION URI> ?m ;
    <INSERT BREAKDOWN DIMENSION URI> ?b ;
    <INSERT INDICATOR MEASURE TYPE (RATIO/COUNT/ETC) URI> ?Figure.
  
  <INSERT DATACUBE URI> 
    <http://purl.org/dc/terms/modified> ?LastUpdated ;
    <http://publishmydata.com/def/dataset#nextUpdateDue> ?NextUpdated.
    
  ?Interval rdfs:label ?DateCode.
  ?Interval <http://www.w3.org/2006/time#hasEnd> ?d.
  ?d rdfs:label ?Date.
  ?i rdfs:label ?Indicator.
  ?m rdfs:label ?Measure.
  ?b rdfs:label ?Breakdown.
  ?l rdfs:label ?yLabel.
}
```

##Example Query

```
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?Indicator ?Measure ?Breakdown ?Date ?DateCode ?Figure ?yLabel ?LastUpdated ?NextUpdated ?Interval
WHERE {
  ?obs 
    <http://purl.org/linked-data/sdmx/2009/dimension#refArea> <http://statistics.gov.scot/id/statistical-geography/S92000003> ; 
    <http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure> ?l ;
    <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?Interval ;
    
    <http://purl.org/linked-data/cube#dataSet> <http://statistics.gov.scot/data/poverty> ;
    <http://purl.org/linked-data/cube#dataSet> ?i ;
    <http://statistics.gov.scot/def/dimension/housingCosts> ?m ;
    <http://statistics.gov.scot/def/dimension/populationGroup> ?b ;
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
}
```

