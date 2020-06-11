Data export from the Norwegian Bumblebee and Butterfly Monitoring
program
================
Jens Åström
11 June, 2020

``` r
#We also load tidyverse, which doesn't format well in markdown. Not shown.
require(DBI)
```

    ## Loading required package: DBI

``` r
require(RPostgres)
```

    ## Loading required package: RPostgres

``` r
require(sf)
```

    ## Loading required package: sf

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 7.0.0

``` r
require(maps)
```

    ## Loading required package: maps

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
require(tidyjson)
```

    ## Loading required package: tidyjson

    ## 
    ## Attaching package: 'tidyjson'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

**With data formatting for the European Butterfly Monitoring Scheme**

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
“event”-table. Within the event table, there can be multiple levels
of “parent-events”. In our case, the 50 meter transect walks represent
the base-level event. The centroid of the transects are used as the
point geometry for the individual observations (latitude, longitude),
although the observations could have been performed along the whole 50 m
transect. The linestring-geometry of the whole 50m section is also
included as a “footprintWKT” for each event. These events are linked to
a parent-event that represents the whole visit to the entire survey
square, thus containing 20 such transect walks. By this design of GBIF,
you can have an arbitrary number of parent events (hierarchical levels)
within the same table, instead of normalizing the data across several
tables. The downside is that you need to keep track of the eventID’s and
the parentEventID’s to recreate the original data structure.

# Fetching the data from GBIF

Here we show how to get the source data from GBIF.

``` r
datasetID <- "4f829580-180d-46a9-9c87-ed8ec959b545"
datasetURL <-  paste0("http://api.gbif.org/v1/dataset/",datasetID,"/endpoint")
dataset <- RJSONIO::fromJSON()
endpoint_url <- dataset[[1]]$url # extracting URL from API call result
```

The endpoint for our dataset looks like this: \*
<http://data.nina.no:8080/ipt/archive.do?r=butterflies_bumblebees2020>

With the endpoint, we can download the data in a zipped
format.

``` r
endpoint_url <- "http://data.nina.no:8080/ipt/archive.do?r=butterflies_bumblebees2020"
# Download from dwc-a from IPT  putting into data folder
download.file(endpoint_url, destfile="GBIF_data/gbif_download.zip", mode="wb")
```

We unzip it in the same folder.

``` r
unzip("GBIF_data/gbif_download.zip",
      exdir = "GBIF_data")
```

# Citation and meta-data

This data is freely available, but please cite the source. The citation
can be fetched programmatically this
way.

``` r
# Suggested citation: Take the citation as from downloaded from GBIF website, replace "via GBIF.org" by endpoint url. 
tmp <- tempfile()
download.file(paste0("http://api.gbif.org/v1/dataset/",datasetID,"/document"),tmp) # get medatadata from gbif api
meta <- read_xml(tmp) %>% as_list() # create list from xml schema
gbif_citation <- meta$eml$additionalMetadata$metadata$gbif$citation[[1]] # extract citation
citation <- gsub("GBIF.org", paste(endpoint_url), gbif_citation) # replace "gbif.org" with endpoint url
```

``` r
citation <- "to_come"
```

The citation to use is then: - to\_come

## Read in the raw GBIF files

Read\_delim parses some columns incorrectly, probably since the file
starts with lots of null values. We here specify the data types
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

We then split the event data into section events (with parent ID), and
transect events (without parent ID’s). If we would have more
hierarchical levels, this would have been more convoluted.

``` r
transectEventRaw <- eventRaw %>% 
  filter(is.na(parentEventID))

sectionEventRaw <- eventRaw %>% 
  filter(!is.na(parentEventID))
```

The occurrence data reads in easier.

``` r
occurrenceRaw <- read_delim("GBIF_data/occurrence.txt", 
                       delim = "\t",
                       locale = locale(encoding = "UTF-8")
                       )
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_character(),
    ##   modified = col_datetime(format = ""),
    ##   basisOfRecord = col_character(),
    ##   occurrenceID = col_character(),
    ##   individualCount = col_double(),
    ##   sex = col_logical(),
    ##   lifeStage = col_character(),
    ##   occurrenceStatus = col_character(),
    ##   eventID = col_character(),
    ##   taxonID = col_character(),
    ##   scientificName = col_character(),
    ##   kingdom = col_character(),
    ##   phylum = col_character(),
    ##   class = col_character(),
    ##   order = col_character(),
    ##   family = col_character(),
    ##   genus = col_character(),
    ##   specificEpithet = col_character(),
    ##   vernacularName = col_character()
    ## )

# Temporary export from source database

Until the GBIF/IPT source is set up, we can get the data directly from
the source database. Note that this is not publicly available.

The data is exported through three (materialized) views in the database.
Here events (transect visits) and parent events (survey square visits)
are accessed through separate views.

``` r
con <- dbConnect(Postgres(),
                 host = "ninradardata01.nina.no", 
                 dbname = "humlesommerf", 
                 user = username, 
                 password = password)
```

## A note on terminology

The terminology of the BMS differ from the original dataset in an
important way. In the BMS format, the 20 50 meter transects within the
same 1.5x1.5km survey square are combined, and is referred to as one
“transect”. The original 50 meter transects are referred to as
“sections” within each transect. From here on, we will use the BMS
nomenclature.

# Extracting the data from the GBIF raw format

In the GBIF data, the two event tables will be combined into one.

``` r
transectEventRaw <- tbl(con, in_schema("views", "parent_event_flate")) 
sectionEventRaw <- tbl(con, in_schema("views", "event_transect")) 
occurrenceRaw <- tbl(con, in_schema("views", "occurrence")) 
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

    ## # A tibble: 2,736,195 x 19
    ##    id    modified            basisOfRecord occurrenceID individualCount sex  
    ##    <chr> <dttm>              <chr>         <chr>                  <dbl> <lgl>
    ##  1 c57a… 2020-06-10 11:28:40 HumanObserva… 4ce65989-d9…               0 NA   
    ##  2 c57a… 2020-04-20 11:53:51 HumanObserva… a6383b77-d2…               0 NA   
    ##  3 c57a… 2020-04-20 11:53:51 HumanObserva… bb9e2dec-0d…               0 NA   
    ##  4 c57a… 2020-04-20 11:53:51 HumanObserva… e4631b50-a6…               0 NA   
    ##  5 c31e… 2020-06-10 11:28:40 HumanObserva… 3885fb31-8a…               0 NA   
    ##  6 c31e… 2020-06-10 11:28:40 HumanObserva… c601da8d-85…               0 NA   
    ##  7 c31e… 2020-04-20 11:53:51 HumanObserva… 3b973f0d-be…               0 NA   
    ##  8 c31e… 2020-06-05 13:22:50 HumanObserva… f3162934-26…               0 NA   
    ##  9 c31e… 2020-06-10 11:28:40 HumanObserva… 7040bab4-40…               0 NA   
    ## 10 c31e… 2020-04-20 11:53:51 HumanObserva… 08f3c86d-13…               0 NA   
    ## # … with 2,736,185 more rows, and 13 more variables: lifeStage <chr>,
    ## #   occurrenceStatus <chr>, eventID <chr>, taxonID <chr>, scientificName <chr>,
    ## #   kingdom <chr>, phylum <chr>, class <chr>, order <chr>, family <chr>,
    ## #   genus <chr>, specificEpithet <chr>, vernacularName <chr>

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

    ## # A tibble: 1 x 1
    ##   dynamicProperties                                                             
    ##   <chr>                                                                         
    ## 1 "{\"observerID\" : \"3347a291-889e-46b2-8c84-9c1480370eed\", \"habitatType\" …

The following solution is a bit cumbersome but the best I’ve found so
far. The `tidyjson` package seem to have pretty poor performance on
large datasets. I had a go at using `purrr::map` and `rjson::fromJSON`
but I don’t seem to get this to work at the moment (although it did work
at some point).

Here, we split out the json string (dynamicProperties) into a separate
table, and join it back to the original data afterwards. This takes
about 30 seconds on a pretty fast machine.

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
    ##  41.849   0.424  42.273

The new columns contain a habitat type classification, cloud cover in %,
temperature, and a 4 level classification of the total flower cover for
each transect. A sample of the extacted data:

``` r
sectionEvent %>% 
  select(observerID,
         habitatType,
         `cloudCover%`,
         temperatureCelsius,
         flowerCover0123) %>% 
  head()
```

    ## # A tibble: 6 x 5
    ##   observerID          habitatType `cloudCover%` temperatureCels… flowerCover0123
    ##   <chr>               <chr>               <dbl>            <dbl>           <dbl>
    ## 1 3347a291-889e-46b2… forest                  0               15               1
    ## 2 3347a291-889e-46b2… forest                  0               15               1
    ## 3 3347a291-889e-46b2… forest                  0               15               1
    ## 4 3347a291-889e-46b2… forest                  0               15               1
    ## 5 3347a291-889e-46b2… forest                  0               15               1
    ## 6 3347a291-889e-46b2… grassland               0               13               2

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

    ## # A tibble: 26,565 x 2
    ##    start_time          end_time           
    ##    <dttm>              <dttm>             
    ##  1 2009-05-12 11:50:00 2009-05-12 11:57:00
    ##  2 2009-05-12 11:57:00 2009-05-12 12:04:00
    ##  3 2009-05-12 12:04:00 2009-05-12 12:11:00
    ##  4 2009-05-12 12:11:00 2009-05-12 12:18:00
    ##  5 2009-05-12 12:18:00 2009-05-12 12:25:00
    ##  6 2009-05-12 09:40:00 2009-05-12 09:58:00
    ##  7 2009-05-12 09:58:00 2009-05-12 10:16:00
    ##  8 2009-05-12 10:16:00 2009-05-12 10:34:00
    ##  9 2009-05-12 10:34:00 2009-05-12 10:52:00
    ## 10 2009-05-12 10:52:00 2009-05-12 11:10:00
    ## # … with 26,555 more rows

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

    ## # A tibble: 1,350 x 2
    ##    start_time          end_time           
    ##    <dttm>              <dttm>             
    ##  1 2013-05-18 11:06:00 2013-05-18 11:46:00
    ##  2 2013-05-28 10:45:00 2013-05-28 14:45:00
    ##  3 2013-05-24 11:53:00 2013-05-24 12:46:00
    ##  4 2013-06-08 10:50:00 2013-06-08 12:31:00
    ##  5 2013-06-04 12:00:00 2013-06-04 15:51:00
    ##  6 2013-06-07 11:00:00 2013-06-07 15:20:00
    ##  7 2013-06-08 13:05:00 2013-06-08 14:11:00
    ##  8 2013-06-06 12:05:00 2013-06-06 13:45:00
    ##  9 2013-06-06 10:00:00 2013-06-06 12:00:00
    ## 10 2013-06-06 14:00:00 2013-06-06 16:30:00
    ## # … with 1,340 more rows

## Join the tables

We can now join the occurrence, section\_event, and transect\_event
tables into a single large table. This could be a suitable endpoint for
other uses, that don’t need to split up the data into the tables used by
the BMS. Just note that this join contains a lot of columns.

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

At this moment, the BMS only wants data up until including 2018, so
we’ll subset the joined table, here performed on the event dates.

``` r
occTrans <- occTrans %>% 
  filter(eventDate.section < '2019-01-01')
```

## Arrange the butterfly count data table.

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

    ## # A tibble: 103 x 1
    ##     species_name             
    ##     <chr>                    
    ##   1 Adscita statices         
    ##   2 Aglais io                
    ##   3 Aglais urticae           
    ##   4 Agriades aquilo          
    ##   5 Agriades optilete        
    ##   6 Agriades orbitulus       
    ##   7 Anthocharis cardamines   
    ##   8 Aphantopus hyperantus    
    ##   9 Aporia crataegi          
    ##  10 Argynnis paphia          
    ##  11 Aricia artaxerxes        
    ##  12 Aricia nicias            
    ##  13 Boloria aquilonaris      
    ##  14 Boloria chariclea        
    ##  15 Boloria eunomia          
    ##  16 Boloria euphrosyne       
    ##  17 Boloria freija           
    ##  18 Boloria frigga           
    ##  19 Boloria improba          
    ##  20 Boloria napaea           
    ##  21 Boloria polaris          
    ##  22 Boloria selene           
    ##  23 Boloria thore            
    ##  24 Brenthis ino             
    ##  25 Callophrys rubi          
    ##  26 Carterocephalus palaemon 
    ##  27 Carterocephalus silvicola
    ##  28 Celastrina argiolus      
    ##  29 Coenonympha arcania      
    ##  30 Coenonympha hero         
    ##  31 Coenonympha pamphilus    
    ##  32 Coenonympha tullia       
    ##  33 Colias croceus           
    ##  34 Colias hecla             
    ##  35 Colias palaeno           
    ##  36 Colias tyche             
    ##  37 Cupido minimus           
    ##  38 Cyaniris semiargus       
    ##  39 Erebia disa              
    ##  40 Erebia embla             
    ##  41 Erebia ligea             
    ##  42 Erebia pandrose          
    ##  43 Erebia polaris           
    ##  44 Erynnis tages            
    ##  45 Eumedonia eumedon        
    ##  46 Euphydryas iduna         
    ##  47 Fabriciana adippe        
    ##  48 Fabriciana niobe         
    ##  49 Favonius quercus         
    ##  50 Glaucopsyche alexis      
    ##  51 Gonepteryx rhamni        
    ##  52 Hesperia comma           
    ##  53 Hipparchia hermione      
    ##  54 Hipparchia semele        
    ##  55 Issoria lathonia         
    ##  56 Lasiommata maera         
    ##  57 Lasiommata megera        
    ##  58 Lasiommata petropolitana 
    ##  59 Leptidea sinapis/reali   
    ##  60 Limenitis populi         
    ##  61 Lycaena helle            
    ##  62 Lycaena hippothoe        
    ##  63 Lycaena phlaeas          
    ##  64 Lycaena virgaureae       
    ##  65 Maniola jurtina          
    ##  66 Melitaea athalia         
    ##  67 Melitaea cinxia          
    ##  68 Melitaea diamina         
    ##  69 Nymphalis antiopa        
    ##  70 Nymphalis polychloros    
    ##  71 Ochlodes sylvanus        
    ##  72 Oeneis bore              
    ##  73 Oeneis jutta             
    ##  74 Oeneis norna             
    ##  75 Papilio machaon          
    ##  76 Pararge aegeria          
    ##  77 Parnassius apollo        
    ##  78 Parnassius mnemosyne     
    ##  79 Pieris brassicae         
    ##  80 Pieris napi              
    ##  81 Pieris rapae             
    ##  82 Plebejus argus/idas      
    ##  83 Plebejus argyrognomon    
    ##  84 Polygonia c-album        
    ##  85 Polyommatus amandus      
    ##  86 Polyommatus icarus       
    ##  87 Pontia daplidice         
    ##  88 Pyrgus alveus            
    ##  89 Pyrgus andromedae        
    ##  90 Pyrgus centaureae        
    ##  91 Pyrgus malvae            
    ##  92 Satyrium w-album         
    ##  93 Scolitantides orion      
    ##  94 Speyeria aglaja          
    ##  95 Thecla betulae           
    ##  96 Thymelicus lineola       
    ##  97 Vanessa atalanta         
    ##  98 Vanessa cardui           
    ##  99 Zygaena exulans          
    ## 100 Zygaena filipendulae     
    ## 101 Zygaena lonicerae        
    ## 102 Zygaena osterodensis     
    ## 103 Zygaena viciae

``` r
lepiCountTab <- lepiCountTab %>% 
  mutate(species_name = ifelse(species_name == 'Colias croceus', 'Colias crocea', species_name))
```

The butterfly count table is now ready.

``` r
lepiCountTab
```

    ## # A tibble: 2,426,474 x 6
    ##    visit_ID    transect_ID   section_ID   date                species_name count
    ##    <chr>       <chr>         <chr>        <dttm>              <chr>        <dbl>
    ##  1 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Adscita sta…     0
    ##  2 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Aglais io        0
    ##  3 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Aglais urti…     0
    ##  4 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Agriades aq…     0
    ##  5 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Agriades op…     0
    ##  6 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Agriades or…     0
    ##  7 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Anthocharis…     0
    ##  8 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Aphantopus …     0
    ##  9 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Aporia crat…     0
    ## 10 00161ef1-3… 20cb4183-069… 04901d87-48… 2011-06-30 00:00:00 Argynnis pa…     0
    ## # … with 2,426,464 more rows

## Arrange monitoring visit table

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

    ## # A tibble: 23,558 x 11
    ##    visit_ID recorder_id transect_ID section date                start_time
    ##    <chr>    <chr>       <chr>       <chr>   <dttm>              <chr>     
    ##  1 facd2b5… 6a478010-e… 2287ad7a-0… 15c2e7… 2011-05-29 00:00:00 13:38:00  
    ##  2 ca07bcc… 6a478010-e… 2287ad7a-0… 15c2e7… 2011-07-16 00:00:00 16:25:00  
    ##  3 5c2e6d8… 6a478010-e… 2287ad7a-0… 15c2e7… 2011-08-20 00:00:00 16:05:00  
    ##  4 a7d7a6e… 6a478010-e… 2287ad7a-0… 15c2e7… 2012-05-23 00:00:00 16:10:00  
    ##  5 9d03032… 6a478010-e… 2287ad7a-0… 15c2e7… 2012-07-14 00:00:00 14:45:00  
    ##  6 6392e5f… 6a478010-e… 2287ad7a-0… 15c2e7… 2012-09-07 00:00:00 15:15:00  
    ##  7 d8aa3d0… 6a478010-e… 2287ad7a-0… 15c2e7… 2013-06-20 00:00:00 17:06:00  
    ##  8 9a48474… 6a478010-e… 2287ad7a-0… 15c2e7… 2013-07-23 00:00:00 16:12:00  
    ##  9 be8d871… 6a478010-e… 2287ad7a-0… 15c2e7… 2013-08-13 00:00:00 14:51:00  
    ## 10 e12e945… 6a478010-e… 2287ad7a-0… 15c2e7… 2014-05-25 00:00:00 13:36:00  
    ## # … with 23,548 more rows, and 5 more variables: end_time <chr>,
    ## #   temperature <dbl>, cloud <dbl>, wind <lgl>, completed <dbl>

The the corresponding operation on the transect level (survey square).
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
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
monVisTabTransect
```

    ## # A tibble: 1,199 x 10
    ##    visit_ID recorder_id transect_ID date                start_time end_time
    ##    <chr>    <chr>       <chr>       <dttm>              <chr>      <chr>   
    ##  1 00161ef… 9b98f4cf-9… 20cb4183-0… 2011-06-30 00:00:00 14:55:00   16:14:00
    ##  2 004d789… 76839443-4… cce42c93-e… 2013-07-20 00:00:00 12:15:00   13:20:00
    ##  3 005b7cf… fc6b9ecf-7… e0a3c18a-3… 2016-06-09 00:00:00 13:00:00   17:30:00
    ##  4 008d7ab… a939d930-1… e3756fff-2… 2011-08-03 00:00:00 13:10:00   14:30:00
    ##  5 009cf41… 6a478010-e… 911293b2-2… 2014-05-22 00:00:00 12:30:00   14:30:00
    ##  6 00ef24d… 99c120bf-8… 7bb92922-4… 2014-07-10 00:00:00 14:54:00   16:38:00
    ##  7 018d499… c2f4a043-e… ccf55543-4… 2015-06-12 00:00:00 11:00:00   14:11:00
    ##  8 01a6668… a6495896-d… df94d08e-b… 2018-05-18 00:00:00 15:30:00   17:11:00
    ##  9 01ce94c… ef323094-1… 5e85d80f-f… 2016-08-05 00:00:00 13:26:00   14:40:00
    ## 10 0207a1e… 6a478010-e… 2287ad7a-0… 2014-08-02 00:00:00 13:30:00   16:01:00
    ## # … with 1,189 more rows, and 4 more variables: temperature <dbl>, cloud <dbl>,
    ## #   wind <dbl>, completed <dbl>

## Arrange the site geographical information table

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

We can plot the locatons of the transects on a simple map for a simple
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
represents the section. It seems you have to jump through some hoops to
get the start and end points from a linestring in `sf`. When we extract
the end points from the linestrings, these are first stored in separate
rows instead of columns, and we have to spread them into separate
columns manually.

``` r
points <- siteGeoInfoSF %>% 
  st_transform(crs = 25833) %>% 
  st_cast("POINT",
          ) %>% 
  st_transform(4326) %>% 
  select(section) %>% 
  as_tibble()
```

    ## Warning in st_cast.sf(., "POINT", ): repeating attributes for all sub-geometries
    ## for which they may not be constant

``` r
points <- points %>% 
  mutate(which = rep(c("start", "end"), nrow(.)/2)) %>% 
  group_by(section, which) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(values_from = footprintWKT.section,
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

    ## Simple feature collection with 1831 features and 2 fields
    ## Active geometry column: footprintWKT.section
    ## geometry type:  LINESTRING
    ## dimension:      XY
    ## bbox:           xmin: 5.221364 ymin: 58.0589 xmax: 13.18196 ymax: 64.9271
    ## geographic CRS: WGS 84
    ## # A tibble: 1,831 x 6
    ##    transect_ID section      footprintWKT.section                     start
    ##    <chr>       <chr>            <LINESTRING [°]>               <POINT [°]>
    ##  1 2287ad7a-0… 15c2e7… (10.95253 59.36785, 10.9…       (10.95253 59.36785)
    ##  2 2287ad7a-0… 840562… (10.95212 59.36882, 10.9…       (10.95212 59.36882)
    ##  3 553685f8-0… ee199d… (10.12944 62.90943, 10.1…       (10.12944 62.90943)
    ##  4 303753c0-e… 5a9b1c… (10.0312 59.01691, 10.03…        (10.0312 59.01691)
    ##  5 303753c0-e… 7f5dce… (10.03208 59.01696, 10.0…       (10.03208 59.01696)
    ##  6 303753c0-e… 68f4d7… (10.03297 59.01698, 10.0…       (10.03297 59.01698)
    ##  7 303753c0-e… 85148d… (10.03385 59.01703, 10.0…       (10.03385 59.01703)
    ##  8 553685f8-0… c87088… (10.13028 62.90968, 10.1…       (10.13028 62.90968)
    ##  9 553685f8-0… 435506… (10.13113 62.90993, 10.1…       (10.13113 62.90993)
    ## 10 553685f8-0… 1a0530… (10.10346 62.89997, 10.1…       (10.10346 62.89997)
    ## # … with 1,821 more rows, and 2 more variables: end <POINT [°]>,
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

    ## # A tibble: 1,831 x 10
    ##    transect_ID section_ID lon_start_point lat_start_point lon_end_point
    ##    <chr>       <chr>                <dbl>           <dbl>         <dbl>
    ##  1 2287ad7a-0… 15c2e737-…            11.0            59.4          11.0
    ##  2 2287ad7a-0… 8405622a-…            11.0            59.4          11.0
    ##  3 553685f8-0… ee199d4c-…            10.1            62.9          10.1
    ##  4 303753c0-e… 5a9b1c68-…            10.0            59.0          10.0
    ##  5 303753c0-e… 7f5dce12-…            10.0            59.0          10.0
    ##  6 303753c0-e… 68f4d708-…            10.0            59.0          10.0
    ##  7 303753c0-e… 85148d68-…            10.0            59.0          10.0
    ##  8 553685f8-0… c870881b-…            10.1            62.9          10.1
    ##  9 553685f8-0… 435506ac-…            10.1            62.9          10.1
    ## 10 553685f8-0… 1a0530bf-…            10.1            62.9          10.1
    ## # … with 1,821 more rows, and 5 more variables: lat_end_point <dbl>,
    ## #   lon_centroid <dbl>, lat_centroid <dbl>, SRID_for_all_coordinates <dbl>,
    ## #   monitoring_type <dbl>

## Arrange the habitat type table

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

    ## # A tibble: 1,810 x 3
    ##    transect_ID                      section_ID                      habitat_Type
    ##    <chr>                            <chr>                           <chr>       
    ##  1 2287ad7a-008e-433e-92ee-db69cab… 15c2e737-ce42-4d07-9baa-d95ba3… forest      
    ##  2 2287ad7a-008e-433e-92ee-db69cab… 8405622a-2a61-4f46-a933-864e5a… forest      
    ##  3 553685f8-0303-4f49-bcbb-9f6ca5d… ee199d4c-f1a1-4180-a4f3-b02180… grassland   
    ##  4 303753c0-e2c6-4fdd-8a7a-b74f625… 5a9b1c68-699c-4995-8526-0c9652… grassland   
    ##  5 303753c0-e2c6-4fdd-8a7a-b74f625… 7f5dce12-e549-4459-bf34-c15af3… grassland   
    ##  6 303753c0-e2c6-4fdd-8a7a-b74f625… 68f4d708-ee23-4ac9-995c-d98657… grassland   
    ##  7 303753c0-e2c6-4fdd-8a7a-b74f625… 85148d68-5fb1-4923-a796-5df968… grassland   
    ##  8 553685f8-0303-4f49-bcbb-9f6ca5d… c870881b-124e-49f3-8f5c-74a578… grassland   
    ##  9 553685f8-0303-4f49-bcbb-9f6ca5d… 435506ac-d60f-4e20-bc42-dc956e… grassland   
    ## 10 553685f8-0303-4f49-bcbb-9f6ca5d… 1a0530bf-ab57-480a-ba49-408598… grassland   
    ## # … with 1,800 more rows

## Arrange the species name table

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

    ## # A tibble: 103 x 3
    ##    national_species_latin   fauna_europea_species_latin local_species_norwegian
    ##    <chr>                    <chr>                       <chr>                  
    ##  1 Zygaena viciae           Zygaena viciae              Liten BloddrÃ¥pesvermer
    ##  2 Erebia disa              Erebia disa                 Disas Ringvinge        
    ##  3 Lasiommata petropolitana Lasiommata petropolitana    Bergringvinge          
    ##  4 Fabriciana adippe        Fabriciana adippe           Adippeperlemorvinge    
    ##  5 Boloria improba          Boloria improba             Dvergperlemorvinge     
    ##  6 Satyrium w-album         Satyrium w-album            Almestjertvinge        
    ##  7 Nymphalis antiopa        Nymphalis antiopa           SÃ¸rgekÃ¥pe            
    ##  8 Pieris brassicae         Pieris brassicae            Stor KÃ¥lsommerfugl    
    ##  9 Melitaea diamina         Melitaea diamina            MÃ¸rk Rutevinge        
    ## 10 Callophrys rubi          Callophrys rubi             GrÃ¸nnstjertvinge      
    ## # … with 93 more rows

# Export the BMS data into CSV-files

We are now ready to export the tables. Here, we use the csv-format. We
use the default encoding (UTF-8).

``` r
write_csv(lepiCountTab,
          path = "out/ButterflyCountDataTable.csv")

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

## Extract zeroes to avoid large sample size

Since we include count data for all species for all surveys, the count
table contains a huge amount of zeroes. This results in a file that is
too large to store on github.

However, it is possible to exlude the rows with zero individuals and
recreate these in R afterwards. To do that, we can store all unique
surveys, and the records with non zero counts.

``` r
uniqueVisits <- lepiCountTab %>% 
  select(visit_ID,
         transect_ID,
         section_ID,
         date) %>% 
  distinct()

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

    ## Parsed with column specification:
    ## cols(
    ##   visit_ID = col_character(),
    ##   transect_ID = col_character(),
    ##   section_ID = col_character(),
    ##   date = col_datetime(format = "")
    ## )

``` r
lepiCountTabNoZeroes <- read_csv("out/ButterflyCountDataTableNoZeroes.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   visit_ID = col_character(),
    ##   transect_ID = col_character(),
    ##   section_ID = col_character(),
    ##   date = col_datetime(format = ""),
    ##   species_name = col_character(),
    ##   count = col_double()
    ## )

``` r
speciesNameTab <- read_csv("out/SpeciesNameTable.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   national_species_latin = col_character(),
    ##   fauna_europea_species_latin = col_character(),
    ##   local_species_norwegian = col_character()
    ## )

We then expand all combinations of species and survey times.

``` r
zeroesToJoin <- speciesNameTab %>% 
  select(species_name = national_species_latin) %>% 
  left_join(uniqueVisits,
            by = character())
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

    ## Joining, by = c("species_name", "visit_ID", "transect_ID", "section_ID", "date")

We can check that the recreated data matches the original.

``` r
all(lepiCountTab == lepiCountTabRecreatedZeroes)
```

    ## [1] TRUE
