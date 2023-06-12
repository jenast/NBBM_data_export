Data export from the Norwegian Bumblebee and Butterfly Monitoring
program
================
Jens Åström
12 June, 2023

**With data formatting for the European Butterfly Monitoring Scheme**

**Verified to work with the 2023 published version of the data
(exporting data from 2009 - 2022 to the eBMS format). But eBMS only
request data including 2021, so we filter out the 2022 data.**

``` r
#We also load tidyverse, which doesn't format well in markdown. Not shown.
require(DBI)
require(RPostgres)
require(sf)
require(maps)
require(tidyjson)
require(xml2)
```

# Introduction

This script downloads observation records from the Norwegian national
bumblebee and butterfly monitoring program, and arranges the data to
suit the formatting needs of the European Butterfly Monitoring Scheme
(BMS). The data is stored within GBIF, and the first section shows how
to download the data from there. The rest of the script deals with
rearranging the data, from the GBIF format to the BMS format. Portions
of the script can be used as an example for projects with other data
formatting preferences. The data is freely available, given that you
site the source appropriately. See “Citation and meta-data” below.

## Data structure

The data has a hierarchical nature, where we have 1) observations of the
individual counts of each bumblebee and butterfly species within a 50
meter transect, 2) several such transects within each survey square (20
transects that of in total 1000m), and 3) several visits to each survey
square throughout the years (typically 3 visits each year). The data
contains a lot of individual counts of zero, as we include also
zero-records of all the species found earlier in the project, plus
potential additional species that think could have been recorded. The
list of species might expand in the future as new species are recorded.

The hierarchical structure is handled within the GBIF structure through
the Darwin Event Core-standard. Here, the observation records of species
are located in an “occurrence”-table, which is linked to an
“event”-table. Within the event table, there can be multiple levels of
“parent-events”. In our case, the 50 meter transect walks represent the
base-level event. The centroid of the transects are used as the point
geometry for the individual observations (latitude, longitude), although
the observations could have been performed along the whole 50 m
transect. The linestring-geometry of the whole 50m section is also
included as a “footprintWKT” for each event. These events are linked to
a parent-event that represents the whole visit to the entire survey
square, thus containing 20 such transect walks. By this design of GBIF,
you can have an arbitrary number of parent events (hierarchical levels)
within the same table, instead of normalizing the data across several
tables. The downside is that you need to keep track of the eventID’s and
the parentEventID’s to recreate the original data structure.

# Finding the raw data and how to cite it

The dataset can be found at <http://gbif.org> by searching for
“Bumblebees and butterflies in Norway”. This will take you to the
webpage of the dataset:
<https://www.gbif.org/dataset/aea17af8-5578-4b04-b5d3-7adf0c5a1e60> Here
you can access the meta-data of the dataset. This data is freely
available, but please cite the source. The citation is visible at the
dataset’s webpage at GBIF. The citation can be fetched programmatically
this way.

``` r
datasetID <- "aea17af8-5578-4b04-b5d3-7adf0c5a1e60" ##From the webpage URL
# Suggested citation: Take the citation as from downloaded from GBIF website, replace "via GBIF.org" by endpoint url. 
tmp <- tempfile()
download.file(paste0("http://api.gbif.org/v1/dataset/",datasetID,"/document"),tmp) # get medatadata from gbif api
meta <- read_xml(tmp) %>% as_list() # create list from xml schema
gbif_citation <- meta$eml$additionalMetadata$metadata$gbif$citation[[1]] # extract citation
```

The citation to use is then:

- Åström S, Åström J (2023). Bumblebees and butterflies in Norway.
  Version 1.5. Norwegian Institute for Nature Research. Sampling event
  dataset <https://doi.org/10.15468/mpsa4g> accessed via GBIF.org on
  2023-06-12.

# Fetching the raw-data from GBIF

We need an “endpoint_url” to retrieve the actual data set. This is found
at the bottom of the webpage for the dataset. This can also be found
programatically as shown here.

``` r
datasetURL <-  paste0("http://api.gbif.org/v1/dataset/",datasetID,"/endpoint")
dataset <- RJSONIO::fromJSON(datasetURL)
endpoint_url <- dataset[[1]]$url # extracting URL from API call result
```

The endpoint for our dataset looks like this: (Also visible on the
webpage for the dataset)

- <https://ipt.nina.no/archive.do?r=butterflies_bumblebees2020>

With the endpoint, we can download the data in a zipped format.

``` r
# Download from dwc-a from IPT  putting into data folder
system("mkdir -p GBIF_data")
download.file(endpoint_url, destfile="GBIF_data/gbif_download.zip", mode="wb")
```

We unzip it in the same folder.

``` r
unzip("GBIF_data/gbif_download.zip",
      exdir = "GBIF_data")
```

## Read in the raw GBIF files

The data is a simple tab delimited text file. I use `read_delim` here,
which unfortunately parses some columns incorrectly, probably since the
file starts with lots of null values. So here we specify the data types
manually.

``` r
eventRaw <- read_delim("GBIF_data/event.txt", 
                       delim = "\t",
                       locale = locale(encoding = "UTF-8"),
                       col_types = cols(id = col_character(),
  type = col_character(),
  modified = col_datetime(format = ""),
  ownerInstitutionCode = col_character(),
  dynamicProperties = col_character(),
  eventID = col_character(),
  parentEventID = col_character(),
  samplingProtocol = col_character(),
  sampleSizeValue = col_double(),
  sampleSizeUnit = col_character(),
  eventDate = col_datetime(format = ""),
  eventTime = col_character(),
  eventRemarks = col_character(),
  locationID = col_character(),
  country = col_character(),
  countryCode = col_character(),
  stateProvince = col_character(),
  municipality = col_character(),
  locality = col_character(),
  locationRemarks = col_character(),
  decimalLatitude = col_double(),
  decimalLongitude = col_double(),
  geodeticDatum = col_character(),
  coordinateUncertaintyInMeters = col_double(),
  footprintWKT = col_character(),
  footprintSRS = col_character()
))
```

The occurrence data reads in easier.

``` r
occurrenceRaw <- read_delim("GBIF_data/occurrence.txt", 
                       delim = "\t",
                       locale = locale(encoding = "UTF-8")
                       )
```

    ## Rows: 33767 Columns: 19
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr  (16): id, basisOfRecord, occurrenceID, lifeStage, occurrenceStatus, eve...
    ## dbl   (1): individualCount
    ## lgl   (1): sex
    ## dttm  (1): modified
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## A note on terminology

The terminology of the BMS differ from the original dataset in an
important way. In the BMS format, the 20 50 meter transects within the
same 1.5x1.5km survey square are combined, and is referred to as one
“transect”. The original 50 meter transects are referred to as
“sections” within each transect. From here on, we will use the BMS
nomenclature.

# Extracting the data from the GBIF raw format

We have to split the event data into section events (with parent ID),
and transect events (without parent ID’s). This is because GBIF combines
all hierarchical events into one table. If we would have more
hierarchical levels, this would have been more convoluted.

``` r
transectEventRaw <- eventRaw %>% 
  filter(is.na(parentEventID))

sectionEventRaw <- eventRaw %>% 
  filter(!is.na(parentEventID))
```

## Subset the butterflies

The original data also contains bumblebees, but the BMS only wants
butterflies, so we subset the data. This is of course optional. This
subset could also have been performed at a later stage, but it is more
efficient to do it at this stage.

``` r
lepiRaw <- occurrenceRaw %>% 
  filter(order == "Lepidoptera") %>% 
  collect()

lepiRaw
```

    ## # A tibble: 15,620 × 19
    ##    id          modified            basis…¹ occur…² indiv…³ sex   lifeS…⁴ occur…⁵
    ##    <chr>       <dttm>              <chr>   <chr>     <dbl> <lgl> <chr>   <chr>  
    ##  1 d9f05488-5… 2023-06-07 14:06:40 HumanO… d4d105…       1 NA    Adult   present
    ##  2 868a1db2-0… 2023-06-07 14:06:40 HumanO… 72afc9…       1 NA    Adult   present
    ##  3 4581bbc3-8… 2023-06-07 14:06:40 HumanO… 5033c2…       1 NA    Adult   present
    ##  4 d9a5d096-5… 2023-06-07 14:06:40 HumanO… 93e029…       1 NA    Adult   present
    ##  5 cfdbcf58-7… 2023-06-07 14:06:40 HumanO… 79b11b…       1 NA    Adult   present
    ##  6 bc1167c6-9… 2023-06-07 14:06:40 HumanO… a76a96…       5 NA    Adult   present
    ##  7 265b35be-0… 2023-06-07 14:06:40 HumanO… 6c42b4…       1 NA    Adult   present
    ##  8 24152952-3… 2023-06-07 14:06:40 HumanO… 2737c1…       1 NA    Adult   present
    ##  9 2cbdf3ff-5… 2023-06-07 14:06:40 HumanO… 9738b3…       3 NA    Adult   present
    ## 10 2cbdf3ff-5… 2023-06-07 14:06:40 HumanO… 8d29e8…       1 NA    Adult   present
    ## # … with 15,610 more rows, 11 more variables: eventID <chr>, taxonID <chr>,
    ## #   scientificName <chr>, kingdom <chr>, phylum <chr>, class <chr>,
    ## #   order <chr>, family <chr>, genus <chr>, specificEpithet <chr>,
    ## #   vernacularName <chr>, and abbreviated variable names ¹​basisOfRecord,
    ## #   ²​occurrenceID, ³​individualCount, ⁴​lifeStage, ⁵​occurrenceStatus

## Extract the data that didn’t fit into the GBIF standard

We have additional information that don’t fit into the GBIF data format.
These are bundled together in to a JSON-string in a GBIF column named
“dynamicProperties”. We need to spread this content into separate
columns.

``` r
sectionEventRaw %>% 
  select(dynamicProperties) %>% 
  head(1)
```

    ## # A tibble: 1 × 1
    ##   dynamicProperties                                                             
    ##   <chr>                                                                         
    ## 1 "{\"observerID\" : \"f35304ca-96ed-434f-9a3b-8b9ccf804127\", \"habitatType\" …

Here, we split out the json string (dynamicProperties) into a separate
table, and join it back to the original data afterwards. This takes
about 30 seconds on a pretty fast machine. I tried various json-packages
earlier with varying success, but this works and should be stable.

``` r
system.time({
sectionEvent <- sectionEventRaw %>% 
  collect() %>% 
  mutate(`document.id` = 1:nrow(.))

tempDynamic <- sectionEventRaw %>% 
  #head() %>% 
  select(dyn = dynamicProperties) %>% 
  collect() %>% 
  unlist() %>% 
  spread_all()  %>% 
  as_tibble()


sectionEvent <- sectionEvent %>% 
  left_join(tempDynamic,
            by = c("document.id" = "document.id"))
})
```

    ##    user  system elapsed 
    ##  21.614   4.316  18.591

The new columns contain a habitat type classification, cloud cover in %,
temperature, and a 4 level classification of the total flower cover for
each transect. A sample of the extracted data:

``` r
sectionEvent %>% 
  select(observerID,
         habitatType,
         `cloudCover%`,
         temperatureCelsius,
         flowerCover0123) %>% 
  head()
```

    ## # A tibble: 6 × 5
    ##   observerID                           habitatType `cloudCover%` tempe…¹ flowe…²
    ##   <chr>                                <chr>               <dbl>   <dbl>   <dbl>
    ## 1 f35304ca-96ed-434f-9a3b-8b9ccf804127 forest                 50    17.5       1
    ## 2 9b98f4cf-9e65-41ea-8601-3461032583e9 forest                 10    18         1
    ## 3 ef323094-1f26-48d2-8e97-fe3a305b981c grassland               0    27         1
    ## 4 a6495896-d41b-4998-93c1-5fd85b35d1f1 forest                 30    20         1
    ## 5 e67031d2-1c93-44d3-b597-c47242aab90b grassland               0    23         3
    ## 6 ef323094-1f26-48d2-8e97-fe3a305b981c forest                 40    21         1
    ## # … with abbreviated variable names ¹​temperatureCelsius, ²​flowerCover0123

## Split the event times into start and end times

The GBIF format has a single column for event time, where we have stored
both the start and the end times. These need to be split into separate
columns. First the “section” times, i.e. the survey times for each 50m
section, which range between 2 minutes and 30 minutes depending on the
number of individuals handled. Note that the effective search time
should be the same, irrespective of the handling time for
identification, since the observers are instructed to walk at a set pace
along the transects and stop when handling an individual.

``` r
sectionEvent <- sectionEvent %>% 
    separate(eventTime, 
           c("start_time", "end_time"), 
           sep = "/") %>% 
  mutate(start_time = as.POSIXct(start_time),
         end_time = as.POSIXct(end_time))


sectionEvent %>% 
  select(start_time,
         end_time)
```

    ## # A tibble: 36,974 × 2
    ##    start_time          end_time           
    ##    <dttm>              <dttm>             
    ##  1 2016-06-28 15:12:00 2016-06-28 15:18:00
    ##  2 2011-05-10 11:49:00 2011-05-10 11:52:00
    ##  3 2021-06-02 16:26:00 2021-06-02 16:28:00
    ##  4 2018-06-23 15:12:00 2018-06-23 15:18:00
    ##  5 2009-06-23 15:53:00 2009-06-23 15:57:00
    ##  6 2012-07-27 14:54:00 2012-07-27 14:59:00
    ##  7 2017-08-13 13:40:00 2017-08-13 13:42:00
    ##  8 NA                  NA                 
    ##  9 2018-06-24 15:35:00 2018-06-24 15:43:00
    ## 10 NA                  NA                 
    ## # … with 36,964 more rows

We split the times for the parent events the same way (“transects” in
BMS lingo). Surveying an entire “transect” of 1000m can take anywhere
between 20 minutes and several hours, depending on the amount of
individuals handled, and potential weather breaks. Note that we need to
collect the data from the database and not use lazy queries for many
operations. Also, later joins between lazy queries and tibbles doesn’t
seem to work so we need to collect everything before joining.

``` r
transectEvent <- transectEventRaw %>% 
    collect() %>% 
      separate(eventTime, 
           c("start_time", "end_time"), 
           sep = "/") %>% 
  mutate(start_time = as.POSIXct(start_time),
         end_time = as.POSIXct(end_time)) 

transectEvent %>% 
  select(start_time,
         end_time)
```

    ## # A tibble: 1,873 × 2
    ##    start_time          end_time           
    ##    <dttm>              <dttm>             
    ##  1 2011-06-30 14:55:00 2011-06-30 16:14:00
    ##  2 2013-07-20 12:15:00 2013-07-20 13:20:00
    ##  3 2016-06-09 13:00:00 2016-06-09 17:30:00
    ##  4 2021-05-17 12:45:00 2021-05-17 14:51:00
    ##  5 2011-08-03 13:10:00 2011-08-03 14:30:00
    ##  6 2014-05-22 12:30:00 2014-05-22 14:30:00
    ##  7 2021-06-02 14:30:00 2021-06-02 15:12:00
    ##  8 2014-07-10 14:54:00 2014-07-10 16:38:00
    ##  9 2019-06-29 13:30:00 2019-06-29 14:30:00
    ## 10 2015-06-12 11:00:00 2015-06-12 14:11:00
    ## # … with 1,863 more rows

## Join the tables

We can now join the occurrence, section_event, and transect_event tables
into a single large table. This could be a suitable endpoint for other
uses, that don’t need to split up the data into the tables used by the
BMS. Just note that this join contains a lot of columns.

``` r
occTrans <- lepiRaw %>%
  left_join(sectionEvent, 
             by = c("eventID" = "eventID"),
             suffix = c(".occurr", ".section")) %>% 
  left_join(transectEvent,
            by = c("parentEventID" = "eventID"),
            suffix = c(".section", ".transect"))
  
#occTrans %>% 
#  print(n = 1, width = Inf)
```

# Further arranging of the data to the BMS format

The following section only pertains to the BMS format, but could be used
as an example for other uses, where you want to normalize the data
(split it into separate tables with unique records for each row).

## Subset the data according to time

This time around, the BMS wants all data including 2021.

``` r
occTrans <- occTrans %>% 
  filter(eventDate.section < '2022-01-01')
```

## Arrange a butterfly count data table.

Here we want the species names, amount of individuals, and enough
identifying information to link these to the transect walks.

``` r
#colnames(occTrans)

lepiCountTab <- occTrans %>% 
  select(visit_ID = parentEventID,
         transect_ID = locationID.transect,
         section_ID = locationID.section, #Note that we use unique identifiers here, not ids that are nested within transects (1, 2, 3, ..).
         date = eventDate.transect, 
         species_name = scientificName,
         count = individualCount
         ) %>% 
  arrange(visit_ID, transect_ID, section_ID, date, species_name)
```

We need to alter some species names to fit the BMS nomenclature. It
turned out to be only one species names that was slightly different.
Here are all the butterfly species in the dataset.

``` r
lepiCountTab %>% 
  select(species_name) %>% 
  distinct() %>% 
  arrange(species_name) %>% 
  print(n = Inf)
```

    ## # A tibble: 60 × 1
    ##    species_name             
    ##    <chr>                    
    ##  1 Aglais io                
    ##  2 Aglais urticae           
    ##  3 Agriades optilete        
    ##  4 Anthocharis cardamines   
    ##  5 Aphantopus hyperantus    
    ##  6 Argynnis paphia          
    ##  7 Aricia artaxerxes        
    ##  8 Boloria aquilonaris      
    ##  9 Boloria eunomia          
    ## 10 Boloria euphrosyne       
    ## 11 Boloria selene           
    ## 12 Brenthis ino             
    ## 13 Callophrys rubi          
    ## 14 Carterocephalus palaemon 
    ## 15 Carterocephalus silvicola
    ## 16 Celastrina argiolus      
    ## 17 Coenonympha arcania      
    ## 18 Coenonympha pamphilus    
    ## 19 Coenonympha tullia       
    ## 20 Colias palaeno           
    ## 21 Cupido minimus           
    ## 22 Cyaniris semiargus       
    ## 23 Erebia ligea             
    ## 24 Erynnis tages            
    ## 25 Eumedonia eumedon        
    ## 26 Fabriciana adippe        
    ## 27 Glaucopsyche alexis      
    ## 28 Gonepteryx rhamni        
    ## 29 Hesperia comma           
    ## 30 Hipparchia semele        
    ## 31 Issoria lathonia         
    ## 32 Lasiommata maera         
    ## 33 Lasiommata megera        
    ## 34 Lasiommata petropolitana 
    ## 35 Limenitis populi         
    ## 36 Lycaena hippothoe        
    ## 37 Lycaena phlaeas          
    ## 38 Lycaena virgaureae       
    ## 39 Maniola jurtina          
    ## 40 Melitaea athalia         
    ## 41 Nymphalis antiopa        
    ## 42 Ochlodes sylvanus        
    ## 43 Papilio machaon          
    ## 44 Pararge aegeria          
    ## 45 Pieris brassicae         
    ## 46 Pieris napi              
    ## 47 Pieris rapae             
    ## 48 Plebejus argus/idas      
    ## 49 Polygonia c-album        
    ## 50 Polyommatus amandus      
    ## 51 Polyommatus icarus       
    ## 52 Pontia edusa             
    ## 53 Pyrgus malvae            
    ## 54 Satyrium w-album         
    ## 55 Speyeria aglaja          
    ## 56 Thymelicus lineola       
    ## 57 Vanessa atalanta         
    ## 58 Vanessa cardui           
    ## 59 Zygaena exulans          
    ## 60 Zygaena filipendulae

``` r
lepiCountTab <- lepiCountTab %>% 
  mutate(species_name = ifelse(species_name == 'Colias croceus', 'Colias crocea', species_name))
```

The butterfly count table is now ready.

``` r
lepiCountTab
```

    ## # A tibble: 13,859 × 6
    ##    visit_ID                    trans…¹ secti…² date                speci…³ count
    ##    <chr>                       <chr>   <chr>   <dttm>              <chr>   <dbl>
    ##  1 00161ef1-3471-4aba-9b0d-b6… 20cb41… 154dfc… 2011-06-30 00:00:00 Lasiom…     1
    ##  2 00161ef1-3471-4aba-9b0d-b6… 20cb41… 4921b3… 2011-06-30 00:00:00 Lasiom…     1
    ##  3 00161ef1-3471-4aba-9b0d-b6… 20cb41… 8def04… 2011-06-30 00:00:00 Agriad…     1
    ##  4 00161ef1-3471-4aba-9b0d-b6… 20cb41… 8def04… 2011-06-30 00:00:00 Argynn…     1
    ##  5 00161ef1-3471-4aba-9b0d-b6… 20cb41… 8def04… 2011-06-30 00:00:00 Lasiom…     1
    ##  6 00161ef1-3471-4aba-9b0d-b6… 20cb41… 8def04… 2011-06-30 00:00:00 Ochlod…     2
    ##  7 00161ef1-3471-4aba-9b0d-b6… 20cb41… 9deaa4… 2011-06-30 00:00:00 Argynn…     2
    ##  8 00161ef1-3471-4aba-9b0d-b6… 20cb41… 9deaa4… 2011-06-30 00:00:00 Lasiom…     3
    ##  9 00161ef1-3471-4aba-9b0d-b6… 20cb41… 9deaa4… 2011-06-30 00:00:00 Plebej…     1
    ## 10 00161ef1-3471-4aba-9b0d-b6… 20cb41… aa351f… 2011-06-30 00:00:00 Lasiom…     3
    ## # … with 13,849 more rows, and abbreviated variable names ¹​transect_ID,
    ## #   ²​section_ID, ³​species_name

## Arrange a monitoring visit table

Here we summarize the characteristics for the individual transect walks.

In the original data, we have specific data on time, temperature, and
cloud cover for each “section”. In rare occurrences, the “transects” can
also be split over two days, so that there are two dates for a single
“transect”-visit. We therefore have information that is unique to
individual transect sections (50m) that might be worth including. This
isn’t strictly compatible with the BMS format, which expects only set of
conditions for the whole “transect”.

We have two options here. Either provide the data on the original
resolution, with unique values per section-visit, or aggregate the
values to the “transect” (square-visit) resolution. We will show a
solution for both approaches, arranging the data on the “section” level
(our 50m transects), in addition to the “transect” level, which the BMS
asks for.

First we take the section level arrangement.

``` r
monVisTabSection <-  occTrans %>% 
  mutate(wind = NA, #We don't have wind data
         completed = 1, #All recorded transects are implicitly completed
         start_time = strftime(start_time.section, format = "%H:%M:%S"), #Remove the date part of the datetimes
         end_time = strftime(end_time.section, format = "%H:%M:%S")) %>%
  select(visit_ID = parentEventID,
         recorder_id = observerID,
         transect_ID = locationID.transect,
         section = locationID.section,
         date = eventDate.section,
         start_time,
         end_time,
         temperature = temperatureCelsius,
         cloud = `cloudCover%`,
         wind,
         completed) %>% 
  distinct()
    
monVisTabSection
```

    ## # A tibble: 9,264 × 11
    ##    visit_ID  recor…¹ trans…² section date                start…³ end_t…⁴ tempe…⁵
    ##    <chr>     <chr>   <chr>   <chr>   <dttm>              <chr>   <chr>     <dbl>
    ##  1 6c705183… 2f0ccd… d53aef… dfb927… 2013-07-23 00:00:00 11:45:… 11:56:…      25
    ##  2 cdb21341… 2f0ccd… d53aef… dfb927… 2014-08-27 00:00:00 12:10:… 12:15:…      17
    ##  3 30792c0e… 2f0ccd… d53aef… dfb927… 2015-07-04 00:00:00 12:00:… 12:05:…      14
    ##  4 730f2f1c… 2f0ccd… d53aef… dfb927… 2018-07-10 00:00:00 12:21:… 12:25:…      16
    ##  5 46ef2c75… 2f0ccd… d53aef… dfb927… 2021-08-21 00:00:00 15:43:… 15:47:…      17
    ##  6 5a5062fa… 2f0ccd… d53aef… efc706… 2019-08-26 00:00:00 16:18:… 16:24:…      25
    ##  7 8b2bcc9e… 2f0ccd… d53aef… efc706… 2021-07-01 00:00:00 15:47:… 15:52:…      19
    ##  8 6c705183… 2f0ccd… d53aef… fff25b… 2013-07-23 00:00:00 12:07:… 12:18:…      25
    ##  9 5a5062fa… 2f0ccd… d53aef… fff25b… 2019-08-26 00:00:00 16:23:… 16:29:…      25
    ## 10 eec203f8… 2f0ccd… d53aef… fff25b… 2020-07-15 00:00:00 16:25:… 16:32:…      17
    ## # … with 9,254 more rows, 3 more variables: cloud <dbl>, wind <lgl>,
    ## #   completed <dbl>, and abbreviated variable names ¹​recorder_id, ²​transect_ID,
    ## #   ³​start_time, ⁴​end_time, ⁵​temperature

Then the corresponding operation on the transect level (survey square).
We first select the columns of interest.

``` r
#Select the data of interest
monVisTabTransect <-  occTrans %>% 
  mutate(wind = NA, #We don't have wind data
         completed = 1, #All recorded transects are implicitly completed
         start_time = strftime(start_time.transect, format = "%H:%M:%S"), #Remove the date part of the datetimes
         end_time = strftime(end_time.transect, format = "%H:%M:%S")) %>%
  select(visit_ID = parentEventID,
         recorder_id = observerID,
         transect_ID = locationID.transect,
         date = eventDate.transect,
         start_time,
         end_time,
         temperature = temperatureCelsius,
         cloud = `cloudCover%`,
         wind,
         completed) %>% 
  distinct()
```

Then we aggregate the data to the transect (survey square) level.

``` r
monVisTabTransect <- monVisTabTransect %>% 
  group_by(visit_ID) %>% 
  summarise(recorder_id = min(recorder_id), #Use the first recorder, if several has divided the sections up between them
            transect_ID = min(transect_ID), #Has only one value per survey event
            date = min(date),
            start_time = min(start_time),
            end_time = max(end_time),
            temperature = mean(temperature),
            cloud = mean(cloud),
            wind = mean(wind),
            completed = mean(completed))
   
monVisTabTransect
```

    ## # A tibble: 1,431 × 10
    ##    visit_ID    recor…¹ trans…² date                start…³ end_t…⁴ tempe…⁵ cloud
    ##    <chr>       <chr>   <chr>   <dttm>              <chr>   <chr>     <dbl> <dbl>
    ##  1 00161ef1-3… 9b98f4… 20cb41… 2011-06-30 00:00:00 14:55:… 16:14:…    24    20  
    ##  2 005b7cff-0… fc6b9e… e0a3c1… 2016-06-09 00:00:00 13:00:… 17:30:…    20.8  18.8
    ##  3 0087893a-9… e8bd88… 97cf7d… 2021-05-17 00:00:00 12:45:… 14:51:…    14.3  26.7
    ##  4 008d7abe-c… a939d9… e3756f… 2011-08-03 00:00:00 13:10:… 14:30:…    24    46.7
    ##  5 009cf41c-d… 6a4780… 911293… 2014-05-22 00:00:00 12:30:… 14:30:…    25    15  
    ##  6 00c6d260-b… ef3230… 33db66… 2021-06-02 00:00:00 14:30:… 15:12:…    26     0  
    ##  7 00ef24d4-c… 99c120… 7bb929… 2014-07-10 00:00:00 14:54:… 16:38:…    27     0  
    ##  8 013b0437-4… 2b780a… a2834b… 2019-06-29 00:00:00 13:30:… 14:30:…    25    40  
    ##  9 018d499b-e… c2f4a0… ccf555… 2015-06-12 00:00:00 11:00:… 14:11:…    17    30  
    ## 10 01a22c7a-7… c2f4a0… ccf555… 2019-06-11 00:00:00 12:00:… 14:31:…    15    10  
    ## # … with 1,421 more rows, 2 more variables: wind <dbl>, completed <dbl>, and
    ## #   abbreviated variable names ¹​recorder_id, ²​transect_ID, ³​start_time,
    ## #   ⁴​end_time, ⁵​temperature

## Arrange a site geographical information table

This output table contains the geographical information for the
individual survey sections, i.e. the starting points, ending points, and
the centroid for the 50m sections.

``` r
siteGeoInfo <- occTrans %>% 
  select(transect_ID = locationID.transect,
         section = locationID.section,
         decimalLatitude.section,
         decimalLongitude.section,
         footprintWKT.section) %>% 
  distinct()

##Register the geometry as SF object
siteGeoInfoSF <- siteGeoInfo %>% 
  st_as_sf(wkt = "footprintWKT.section",
          crs = 4326) 
#%>% 
 # st_transform(crs = 25833)
```

We can plot the locations of the transects on a simple map for a simple
quality check. We can at least see that the transects lie within Norway,
which rules out transformation and projection errors.

``` r
world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
norway <- world1 %>% filter(ID == "Norway") 
#%>% 
 # st_transform(crs = 25833) #To be able to use st_cast

plot(norway,
     main = NULL,
     reset = F,
     col = "seagreen")

plot(siteGeoInfoSF["transect_ID"], 
     lwd = 4,
     add = T)
```

![Transect locations of the Norwegian bumblebee and butterfly
monitoring](figure/transect_loc-1.png)

We now extract the start and end points from the linestring that
represents the section. The eBMS just want the start and endpoints, so
the meandering of transects isn’t captured. It seems you have to jump
through some hoops to get the start and end points from a linestring in
`sf`. When we extract the end points from the linestrings, these are
first stored in separate rows instead of columns, and we have to spread
them into separate columns manually.

!! use this instead to get end points If you only want the endpoints,
then st_line_sample is what you are looking for:

ptns = st_line_sample(ls, sample = 1)

Also get the centroid at this stage.

``` r
# points <- siteGeoInfoSF %>% 
#   st_transform(crs = 25833) %>% 
#   st_cast("POINT",
#           ) %>% 
#   st_transform(4326) %>% 
#   select(section) %>% 
#   as_tibble()

points <- siteGeoInfoSF %>% 
  st_transform(crs = 25833) %>% 
  mutate(points = st_line_sample(footprintWKT.section, sample = c(0, 1))) %>% 
  st_set_geometry("points") %>% 
     st_cast("POINT",
          ) %>% 
  st_transform(4326) %>% 
  select(section) %>% 
  as_tibble()
```

    ## Warning in st_cast.sf(., "POINT", ): repeating attributes for all
    ## sub-geometries for which they may not be constant

``` r
points <- points %>% 
  mutate(which = rep(c("start", "end"), nrow(.)/2)) %>% 
  group_by(section, which) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(values_from = points,
              names_from = which) %>% 
  select(-row)

#Join the points to the original data.
siteGeoInfoTab <- siteGeoInfoSF %>% 
  as_tibble() %>% 
  right_join(points, by = c("section" = "section"))
```

We can plot a transect as a sanity check. Starting points are here shown
in blue, endpoints in red, centroids in black. Notice that the order of
the start and end point of the individual sections doesn’t necessarily
follow the transect in a continuous way. Meaning, some sections go left
to right, while some go right to left. We would need to sort this out in
the source database.

``` r
##Sanity check plot
##Seems to work, but the start and endpoints of the sections seem to be random.

#Register the centroid points as a geometry
tempSection <- siteGeoInfoTab %>% 
  st_as_sf(coords = c("decimalLongitude.section", "decimalLatitude.section"),
           crs = 4326)

tempSection
```

    ## Simple feature collection with 1571 features and 2 fields
    ## Active geometry column: footprintWKT.section
    ## Geometry type: LINESTRING
    ## Dimension:     XY
    ## Bounding box:  xmin: 5.221364 ymin: 58.0589 xmax: 13.18196 ymax: 64.9271
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 1,571 × 6
    ##    transect_ID       section      footprintWKT.section               start
    ##  * <chr>             <chr>            <LINESTRING [°]>         <POINT [°]>
    ##  1 d53aef1f-7502-47… dfb927… (5.221583 59.39742, 5.22… (5.221583 59.39742)
    ##  2 d53aef1f-7502-47… efc706… (5.221583 59.39742, 5.22… (5.221583 59.39742)
    ##  3 d53aef1f-7502-47… fff25b… (5.221911 59.39699, 5.22… (5.221911 59.39699)
    ##  4 d53aef1f-7502-47… 316c21… (5.223379 59.39645, 5.22… (5.223379 59.39645)
    ##  5 d53aef1f-7502-47… e07338… (5.224109 59.39617, 5.22… (5.224109 59.39617)
    ##  6 d53aef1f-7502-47… 34a6ae… (5.224726 59.39583, 5.22… (5.224726 59.39583)
    ##  7 d53aef1f-7502-47… 17696e… (5.225234 59.39545, 5.22… (5.225234 59.39545)
    ##  8 d53aef1f-7502-47… cc8b58… (5.225234 59.39545, 5.22… (5.225234 59.39545)
    ##  9 d53aef1f-7502-47… b191a5… (5.226234 59.39467, 5.22… (5.226234 59.39467)
    ## 10 d53aef1f-7502-47… 0628de… (5.226706 59.39428, 5.22… (5.226706 59.39428)
    ## # … with 1,561 more rows, and 2 more variables: end <POINT [°]>,
    ## #   geometry <POINT>

``` r
#Plot the linestring
tempSection %>% 
  filter(transect_ID == 'd53aef1f-7502-4724-9be7-dd49f44dce59') %>% 
  select(transect_ID) %>% 
  plot(reset = F,
       key.pos = NULL,
       main = NULL)

#Plot the start points
st_geometry(tempSection) <- "start"
tempSection %>% 
  filter(transect_ID == 'd53aef1f-7502-4724-9be7-dd49f44dce59') %>% 
  select(transect_ID) %>% 
  plot(add = T,
       col = "blue")

#Plot the end points
st_geometry(tempSection) <- "end"
tempSection %>% 
  filter(transect_ID == 'd53aef1f-7502-4724-9be7-dd49f44dce59') %>% 
  select(transect_ID) %>% 
  plot(add = T,
       col = "red")

#Plot the centroids
st_geometry(tempSection) <- "geometry"
tempSection %>% 
  filter(transect_ID == 'd53aef1f-7502-4724-9be7-dd49f44dce59') %>% 
  select(transect_ID) %>% 
  plot(add = T,
       col = "black")
```

![Example of a few sections of one
transect.](figure/plot_section_points-1.png) We see that we have the
correct start, end, and centroid points of the transect line, although
the order of the start and end points are a little off. We can then
rename the colums and extract the columns of interest.

``` r
siteGeoInfoTab <- siteGeoInfoTab %>% 
  mutate(lon_start_point = st_coordinates(start)[,1],
         lat_start_point = st_coordinates(start)[,2],
         lon_end_point = st_coordinates(end)[,1],
         lat_end_point = st_coordinates(end)[,2],
         SRID_for_all_coordinates = 4326,
         monitoring_type = 2) %>% 
  select(transect_ID,
         section_ID = section,
         lon_start_point,
         lat_start_point,
         lon_end_point,
         lat_end_point,
         lon_centroid = decimalLongitude.section,
         lat_centroid = decimalLatitude.section,
         SRID_for_all_coordinates,
         monitoring_type
         )

siteGeoInfoTab
```

    ## # A tibble: 1,571 × 10
    ##    transect_ID   secti…¹ lon_s…² lat_s…³ lon_e…⁴ lat_e…⁵ lon_c…⁶ lat_c…⁷ SRID_…⁸
    ##    <chr>         <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 d53aef1f-750… dfb927…    5.22    59.4    5.22    59.4    5.22    59.4    4326
    ##  2 d53aef1f-750… efc706…    5.22    59.4    5.22    59.4    5.22    59.4    4326
    ##  3 d53aef1f-750… fff25b…    5.22    59.4    5.22    59.4    5.22    59.4    4326
    ##  4 d53aef1f-750… 316c21…    5.22    59.4    5.22    59.4    5.22    59.4    4326
    ##  5 d53aef1f-750… e07338…    5.22    59.4    5.22    59.4    5.22    59.4    4326
    ##  6 d53aef1f-750… 34a6ae…    5.22    59.4    5.22    59.4    5.22    59.4    4326
    ##  7 d53aef1f-750… 17696e…    5.23    59.4    5.22    59.4    5.22    59.4    4326
    ##  8 d53aef1f-750… cc8b58…    5.23    59.4    5.23    59.4    5.23    59.4    4326
    ##  9 d53aef1f-750… b191a5…    5.23    59.4    5.23    59.4    5.23    59.4    4326
    ## 10 d53aef1f-750… 0628de…    5.23    59.4    5.23    59.4    5.23    59.4    4326
    ## # … with 1,561 more rows, 1 more variable: monitoring_type <dbl>, and
    ## #   abbreviated variable names ¹​section_ID, ²​lon_start_point, ³​lat_start_point,
    ## #   ⁴​lon_end_point, ⁵​lat_end_point, ⁶​lon_centroid, ⁷​lat_centroid,
    ## #   ⁸​SRID_for_all_coordinates

## Arrange a habitat type table

This contains information on the habitat types for each section. BMS
asks for the habitat type for both sides of a transect. In the national
monitoring program, we haven’t differentiated between the sides of the
transect, but only characterized the individual sections to be in either
“grassland” or “forest”. All sections are located in open/low
vegetation, and most often align with roads or trails.

“Grassland” refers to habitats that are dominated by continuous
clearing/grazing/plowing. A substantial part is lowland semi-natural
grasslands, but the sections also span more intensively managed
farmland.

“Forest” refers to open but otherwise forested areas, which is dominated
by managed forests but can include natural forests. These sections
typically align with smaller gravel roads in conifer forests (mostly
Norway spruce and Pine), but can include powerline clearings.

``` r
habTypeTab <- occTrans %>% 
  select(transect_ID = locationID.transect,
         section_ID = locationID.section,
         habitat_Type = habitatType
) %>% 
  distinct()

habTypeTab
```

    ## # A tibble: 1,572 × 3
    ##    transect_ID                          section_ID                       habit…¹
    ##    <chr>                                <chr>                            <chr>  
    ##  1 d53aef1f-7502-4724-9be7-dd49f44dce59 dfb9272e-be2a-4a7e-b882-4cc5a79… grassl…
    ##  2 d53aef1f-7502-4724-9be7-dd49f44dce59 efc706a7-9cef-4199-b5bf-5f5b975… grassl…
    ##  3 d53aef1f-7502-4724-9be7-dd49f44dce59 fff25b5b-7a7c-4fcc-a927-cb6047a… grassl…
    ##  4 d53aef1f-7502-4724-9be7-dd49f44dce59 316c2185-30c1-4077-9f39-52cfe5b… grassl…
    ##  5 d53aef1f-7502-4724-9be7-dd49f44dce59 e0733813-696e-4ec5-9986-d98fe09… grassl…
    ##  6 d53aef1f-7502-4724-9be7-dd49f44dce59 34a6aebd-a193-4851-a46b-98417fe… grassl…
    ##  7 d53aef1f-7502-4724-9be7-dd49f44dce59 17696e5a-5f7a-4a2b-b879-b3d6e19… grassl…
    ##  8 d53aef1f-7502-4724-9be7-dd49f44dce59 cc8b5806-0a52-4d58-8a6c-73c9022… grassl…
    ##  9 d53aef1f-7502-4724-9be7-dd49f44dce59 b191a51d-6385-43d5-98a0-238c5e2… grassl…
    ## 10 d53aef1f-7502-4724-9be7-dd49f44dce59 0628debb-a252-4e14-ba19-0ef29a6… grassl…
    ## # … with 1,562 more rows, and abbreviated variable name ¹​habitat_Type

## Arrange a species name table

The only discrepancy between the locally used latin names and tha Fauna
Europea latin names is “Colias Croceus”/“Colias crocea”. Note that some
taxa are not distinguished to species. We also include the local
vernacular name (Norwegian).

``` r
speciesNameTab <- occTrans %>% 
  select(national_species_latin = scientificName,
         fauna_europea_species_latin = scientificName,
         local_species_norwegian = vernacularName) %>% 
  mutate(national_species_latin = ifelse(national_species_latin == "Colias croceus", "Colias crocea", national_species_latin)) %>% 
  distinct()

speciesNameTab
```

    ## # A tibble: 60 × 3
    ##    national_species_latin   fauna_europea_species_latin local_species_norwegian 
    ##    <chr>                    <chr>                       <chr>                   
    ##  1 Pieris brassicae         Pieris brassicae            Stor Kålsommerfugl      
    ##  2 Polyommatus icarus       Polyommatus icarus          Tiriltungeblåvinge      
    ##  3 Pieris napi              Pieris napi                 Rapssommerfugl          
    ##  4 Vanessa cardui           Vanessa cardui              Tistelsommerfugl        
    ##  5 Plebejus argus/idas      Plebejus argus/idas         Argusblåvinge/Idasblåvi…
    ##  6 Speyeria aglaja          Speyeria aglaja             Aglajaperlemorvinge     
    ##  7 Gonepteryx rhamni        Gonepteryx rhamni           Sitronsommerfugl        
    ##  8 Callophrys rubi          Callophrys rubi             Grønnstjertvinge        
    ##  9 Boloria selene           Boloria selene              Brunflekket Perlemorvin…
    ## 10 Lasiommata petropolitana Lasiommata petropolitana    Bergringvinge           
    ## # … with 50 more rows

# Export the BMS data into CSV-files

We are now ready to export the tables. Here, we use the csv-format. We
use the default encoding (UTF-8).

``` r
write_csv(lepiCountTab,
          path = "out/ButterflyCountDataTable.csv")
```

    ## Warning: The `path` argument of `write_csv()` is deprecated as of readr 1.4.0.
    ## ℹ Please use the `file` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
write_csv(monVisTabTransect,
          path = "out/MonitoringVisitTableTransectLevel.csv")

write_csv(monVisTabSection,
          path = "out/MonitoringVisitTableSectionLevel.csv")

write_csv(siteGeoInfoTab,
          path = "out/SiteGeographicalInformationTable.csv")

write_csv(habTypeTab,
          path = "out/HabitatTypeTable.csv")

write_csv(speciesNameTab,
          path = "out/SpeciesNameTable.csv")
```

## Impute zero counts

As of the GBIF data export in 2023 (dataset version 1.5), we don’t
include zeroes (absences) in our dataset. These can be imputed by
filling in zeroes of all the on recorded species.

We can reuse the existing method for this, when we had zeroes in the
dataset.

``` r
uniqueVisits <- lepiCountTab %>% 
  select(visit_ID,
         transect_ID,
         section_ID,
         date) %>% 
  distinct()

#superfluous in the current version
lepiCountTabNoZeroes  <- lepiCountTab %>% 
  filter(count > 0)
```

Instead of storing 2.4 million rows, we are suddenly down to 23.5
thousand individual surveys, and a little over 9 thousand non zero
observation, with no loss of information. We store this in separate
csv-files. We already have all species names stored in the species
table.

``` r
write_csv(uniqueVisits,
          path = "out/uniqueVisits.csv")

write_csv(lepiCountTabNoZeroes,
          path = "out/ButterflyCountDataTableNoZeroes.csv")
```

## Recreate the zero counts

Here is how to restore the full data, with zeroes. We start with reading
in the csv files for completeness. Note that we also read in the full
table of species names.

``` r
uniqueVisits <- read_csv("out/uniqueVisits.csv")
```

    ## Rows: 9264 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): visit_ID, transect_ID, section_ID
    ## dttm (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
lepiCountTabNoZeroes <- read_csv("out/ButterflyCountDataTableNoZeroes.csv")
```

    ## Rows: 13859 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): visit_ID, transect_ID, section_ID, species_name
    ## dbl  (1): count
    ## dttm (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
speciesNameTab <- read_csv("out/SpeciesNameTable.csv")
```

    ## Rows: 60 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): national_species_latin, fauna_europea_species_latin, local_species_...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

We then expand all combinations of species and survey times.

``` r
zeroesToJoin <- speciesNameTab %>% 
  select(species_name = national_species_latin) %>% 
  cross_join(uniqueVisits)
```

And join these to the records with non-zero counts.

``` r
lepiCountTabRecreatedZeroes <- zeroesToJoin %>% 
  left_join(lepiCountTabNoZeroes) %>% 
  mutate(count = coalesce(count, 0)) %>% 
  select(visit_ID,
         transect_ID,
         section_ID,
         date,
         species_name,
         count) %>% 
    arrange(visit_ID, transect_ID, section_ID, date, species_name)
```

    ## Joining with `by = join_by(species_name, visit_ID, transect_ID, section_ID,
    ## date)`
