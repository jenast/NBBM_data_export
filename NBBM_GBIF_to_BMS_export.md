Data export of bumblebees and butterflies from the NINA IPT and
arrangement for BMS
================
Jens Åström
08 June, 2020

To-do: Figure captions doesn’t work in github\_documents.

# Intro

This is…

## Data structure

The data has a hierarchical nature, in that we have 1) several
observations within a 50 meter transect, 2) several transect walks
within each survey square, and 3) several visits to each survey square
throughout the years. This structure is handled within the GBIF
structure through the Darwin Event Core-standard. Here, the observation
records are located in a “occurrence”-table, which is linked to an
“event”-table. Within the event table, there can be multiple levels of
“parent-events”. In our case, the 50 meter transect walks represent the
base-level event. They are linked to a parent-event that is the whole
visit to the entire survey square, containing 20 such transect walks. By
this design, you can have an arbitrary number of parent events
(hierarchical levels) within the same table, instead of normalizing the
data across several tables. The catch is that you have to keep track of
the eventID’s and the parentEventID’s to recreate the original data
structure.

# Fetching the data from the NINA IPT (A GBIF NODE)

To come…

# Temporary export from source database

Before the GBIF/IPT source is set up, we can get the data directly from
the source database.

The data is exported through three (materialized) views in the database.

``` r
con <- dbConnect(Postgres(),
                 host = "ninradardata01.nina.no", 
                 dbname = "humlesommerf", 
                 user = username, 
                 password = password)
```

``` r
parentEventRaw <- tbl(con, in_schema("views", "parent_event_flate")) 
transectEventRaw <- tbl(con, in_schema("views", "event_transect")) 
occurrenceRaw <- tbl(con, in_schema("views", "occurrence")) 
```

# Arrange the data to the BMS format (Butterfly monitoring )

Note that in this format, the 1.5x1.5km survey squares are named
“transects”, and the 20, 50m transects within these are named
“sections”. In the original data, we have specific data on time,
temperature, and cloud cover for each “section”. In rare occurrences,
the “transects” can also be split over two days, so that there is two
dates for a single “transect”-visit.

We have two options here. Either provide the data on the original
resolution, with unique values per section-visit, or aggregate the
values somehow to the “transect” (square-visit) resolution. We will
provide both solutions.

## Subset the butterflies

The original data also contains bumblebees, so here we subset the
lepidoptera.

``` r
lepiRaw <- occurrenceRaw %>% 
  filter(order == "Lepidoptera") %>% 
  collect()

lepiRaw
```

    ## # A tibble: 2,736,195 x 19
    ##    id    modified            basisOfRecord occurrenceID individualCount
    ##    <chr> <dttm>              <chr>         <chr>                  <int>
    ##  1 7734… 2020-06-05 13:22:50 HumanObserva… ebf2e6bc-2a…               0
    ##  2 7734… 2020-04-20 11:53:51 HumanObserva… fa3d5449-42…               0
    ##  3 7734… 2020-04-20 11:53:51 HumanObserva… e3c39fad-3f…               0
    ##  4 7734… 2020-04-20 11:53:51 HumanObserva… adf10c23-22…               0
    ##  5 7734… 2020-06-06 08:13:15 HumanObserva… a51bf204-06…               0
    ##  6 7734… 2020-06-06 08:13:15 HumanObserva… c28db937-3c…               0
    ##  7 7734… 2020-06-06 08:13:15 HumanObserva… 4d085fc3-43…               0
    ##  8 7734… 2020-06-05 13:26:53 HumanObserva… 4c9a2b9a-f1…               0
    ##  9 7734… 2020-06-06 08:13:15 HumanObserva… 50149092-87…               0
    ## 10 7734… 2020-06-06 08:13:15 HumanObserva… 452919fe-e7…               0
    ## # … with 2,736,185 more rows, and 14 more variables: occurrenceStatus <chr>,
    ## #   sex <chr>, lifeStage <chr>, eventID <chr>, taxonID <chr>,
    ## #   scientificName <chr>, vernacularName <chr>, kingdom <chr>, phylum <chr>,
    ## #   class <chr>, order <chr>, family <chr>, genus <chr>, specificEpithet <chr>

## Split the information in the dynamic properties column into separate columns

The GBIF standard doesn’t accept all the column names we have, and these
are bundled together in to a JSON-string.

``` r
transectEventRaw %>% 
  select(dynamicProperties) %>% 
  head(1)
```

    ## # Source:   lazy query [?? x 1]
    ## # Database: postgres [jens.astrom@ninradardata01.nina.no:5432/humlesommerf]
    ##   dynamicProperties                                                             
    ##   <pq_json>                                                                     
    ## 1 {"observerID" : "3347a291-889e-46b2-8c84-9c1480370eed", "habitatType" : "fore…

This is a bit cumbersome but the best I’ve found so far. The `tidyjson`
package seem to have pretty poor performance on large datasets. I had a
go at `map` and `fromJSON` but that seems broken at the moment.

``` r
system.time({
transectEvent <- transectEventRaw %>% 
  collect() %>% 
  mutate(`document.id` = 1:nrow(.))

tempDynamic <- transectEventRaw %>% 
  #head() %>% 
  select(dyn = dynamicProperties) %>% 
  collect() %>% 
  unlist() %>% 
  spread_all()  %>% 
  as_tibble()


transectEvent <- transectEvent %>% 
  left_join(tempDynamic,
            by = c("document.id" = "document.id"))
})
```

    ##    user  system elapsed 
    ##  33.337   0.584  33.923

A sample of the extacted data:

``` r
transectEvent %>% 
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
the start and the end times. These need to be split into separate
columns. First the transect

``` r
transectEvent <- transectEvent %>% 
    separate(eventTime, 
           c("start_time", "end_time"), 
           sep = "/") %>% 
  mutate(start_time = as.POSIXct(start_time),
         end_time = as.POSIXct(end_time))
```

Then the parent events (“transects” in BMS lingo). Note that we need to
collect the data from the database and not use lazy queries.

``` r
parentEvent <- parentEventRaw %>% 
    collect() %>% 
      separate(eventTime, 
           c("start_time", "end_time"), 
           sep = "/") %>% 
  mutate(start_time = as.POSIXct(start_time),
         end_time = as.POSIXct(end_time)) 
```

## Join occurrence, event\_transect, and parent\_event\_rute tables

``` r
occTrans <- lepiRaw %>%
  left_join(transectEvent, 
             by = c("eventID" = "eventID"),
             suffix = c(".occurr", ".section")) %>% 
  left_join(parentEvent,
            by = c("parentEventID" = "eventID"),
            suffix = c(".section", ".transect"))
  
#occTrans %>% 
#  print(n = 1, width = Inf)
```

## Subset the data according to time

At this moment, the BMS only wants data up until including 2018, so
we’ll subset the joined table on the event dates.

``` r
occTrans <- occTrans %>% 
  filter(eventDate.section < '2019-01-01')
```

## Arrange butterfly count data table

``` r
#colnames(occTrans)

lepiCountTab <- occTrans %>% 
  select(visit_ID = parentEventID,
         transect_ID = locationID.transect,
         section = locationID.section, #Note that we use unique identifiers here, not ids that needs to be nested within transects.
         date = eventDate.transect, 
         species_name = scientificName,
         count = individualCount
         )

lepiCountTab
```

    ## # A tibble: 2,426,474 x 6
    ##    visit_ID       transect_ID      section       date       species_name   count
    ##    <chr>          <chr>            <chr>         <date>     <chr>          <int>
    ##  1 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Fabriciana ad…     0
    ##  2 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Vanessa cardui     0
    ##  3 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Boloria selene     0
    ##  4 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Carterocephal…     0
    ##  5 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Boloria eunom…     0
    ##  6 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Zygaena viciae     0
    ##  7 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Thecla betulae     0
    ##  8 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Polygonia c-a…     0
    ##  9 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Erebia polaris     0
    ## 10 e5b7974c-9df0… d53aef1f-7502-4… dfb9272e-be2… 2013-05-26 Boloria impro…     0
    ## # … with 2,426,464 more rows

Alter some species names to fit BMS nomenclature

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

## Arrange monitoring visit table

As mentioned above, we will arrange the data also on the “section” (50m
transect) level, in addition to the “transect” (survey square) that the
BMS asks for. This is because we have some information that is unique to
individual transect sections (50m) that might be worth including. The
transect level data simply takes the average values of the individual
section level data.

Section level arrangement.

``` r
monVisTabSection <-  occTrans %>% 
  mutate(wind = NA, #We don't have wind data
         completed = 1, #All recorded transects are completed
         start_time = strftime(start_time.section, format = "%H:%M:%S"), #Remove the date part of the starting time
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
    ##    visit_ID recorder_id transect_ID section date       start_time end_time
    ##    <chr>    <chr>       <chr>       <chr>   <date>     <chr>      <chr>   
    ##  1 e5b7974… 2f0ccd4f-a… d53aef1f-7… dfb927… 2013-05-26 11:20:00   11:25:00
    ##  2 6c70518… 2f0ccd4f-a… d53aef1f-7… dfb927… 2013-07-23 11:45:00   11:56:00
    ##  3 6c81a4a… 2f0ccd4f-a… d53aef1f-7… dfb927… 2014-06-01 17:36:00   17:42:00
    ##  4 cac20b4… 2f0ccd4f-a… d53aef1f-7… dfb927… 2014-07-12 14:50:00   15:02:00
    ##  5 cdb2134… 2f0ccd4f-a… d53aef1f-7… dfb927… 2014-08-27 12:10:00   12:15:00
    ##  6 30792c0… 2f0ccd4f-a… d53aef1f-7… dfb927… 2015-07-04 12:00:00   12:05:00
    ##  7 4f44d6d… 2f0ccd4f-a… d53aef1f-7… dfb927… 2015-08-13 14:30:00   14:36:00
    ##  8 2689da3… 2f0ccd4f-a… d53aef1f-7… dfb927… 2016-05-27 15:55:00   15:57:00
    ##  9 d413064… 2f0ccd4f-a… d53aef1f-7… dfb927… 2016-07-20 15:09:00   15:15:00
    ## 10 3f292a1… 2f0ccd4f-a… d53aef1f-7… dfb927… 2017-06-27 15:37:00   15:41:00
    ## # … with 23,548 more rows, and 4 more variables: temperature <dbl>,
    ## #   cloud <dbl>, wind <lgl>, completed <dbl>

The corresponding operation on the transect level (survey square)

``` r
monVisTabTransect <-  occTrans %>% 
  mutate(wind = NA, #We don't have wind data
         completed = 1, #All recorded transects are completed
         start_time = strftime(start_time.transect, format = "%H:%M:%S"), #Remove the date part of the starting time
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


##Aggregate the data on the transect (survey square) level
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
    ##    visit_ID recorder_id transect_ID date       start_time end_time temperature
    ##    <chr>    <chr>       <chr>       <date>     <chr>      <chr>          <dbl>
    ##  1 00161ef… 9b98f4cf-9… 20cb4183-0… 2011-06-30 14:55:00   16:14:00        24  
    ##  2 004d789… 76839443-4… cce42c93-e… 2013-07-20 12:15:00   13:20:00        22  
    ##  3 005b7cf… fc6b9ecf-7… e0a3c18a-3… 2016-06-09 13:00:00   17:30:00        20.8
    ##  4 008d7ab… a939d930-1… e3756fff-2… 2011-08-03 13:10:00   14:30:00        24  
    ##  5 009cf41… 6a478010-e… 911293b2-2… 2014-05-22 12:30:00   14:30:00        25  
    ##  6 00ef24d… 99c120bf-8… 7bb92922-4… 2014-07-10 14:54:00   16:38:00        27  
    ##  7 018d499… c2f4a043-e… ccf55543-4… 2015-06-12 11:00:00   14:11:00        17  
    ##  8 01a6668… a6495896-d… df94d08e-b… 2018-05-18 15:30:00   17:11:00        19.5
    ##  9 01ce94c… ef323094-1… 5e85d80f-f… 2016-08-05 13:26:00   14:40:00        19  
    ## 10 0207a1e… 6a478010-e… 2287ad7a-0… 2014-08-02 13:30:00   16:01:00        25  
    ## # … with 1,189 more rows, and 3 more variables: cloud <dbl>, wind <dbl>,
    ## #   completed <dbl>

## Arrange the site geographical information table

This contains the geographical information of the individual survey
sections.

``` r
siteGeoInfo <- occTrans %>% 
  select(transect_ID = locationID.transect,
         section = locationID.section,
         decimalLatitude.section,
         decimalLongitude.section,
         footprintwkt.section) %>% 
  distinct()

##Register the geometry as SF object
siteGeoInfoSF <- siteGeoInfo %>% 
  st_as_sf(wkt = "footprintwkt.section",
          crs = 4326) 
#%>% 
 # st_transform(crs = 25833)
```

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

It seems you have to jump through some hoops to get the start and end
points from a linestring in `sf`.

``` r
points <- siteGeoInfoSF %>% 
  st_transform(crs = 25833) %>% 
  st_cast("POINT",
          ) %>% 
  st_transform(4326) %>% 
  select(section)
```

    ## Warning in st_cast.sf(., "POINT", ): repeating attributes for all sub-geometries
    ## for which they may not be constant

``` r
#names(points)[names(points) == "footprintwkt.section"] <- "start_end_points"
#st_geometry(points) <- "start_end_points"

points <- as_tibble(points)

points <- points %>% 
  mutate(which = rep(c("start", "end"), nrow(.)/2)) %>% 
  group_by(section, which) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(values_from = footprintwkt.section,
              names_from = which) %>% 
  select(-row)


siteGeoInfoTab <- siteGeoInfoSF %>% 
  as_tibble() %>% 
  right_join(points, by = c("section" = "section"))
```

``` r
##Sanity check plot
##Seems to work, but the start and endpoints of the sections seem to be random.
tt <- siteGeoInfoTab %>% 
  st_as_sf(coords = c("decimalLongitude.section", "decimalLatitude.section"),
           crs = 4326)


tt %>% 
  head() %>% 
  select(transect_ID) %>% 
  plot(reset = F)

st_geometry(tt) <- "start"

tt %>% 
  head() %>% 
  select(transect_ID) %>% 
  plot(add = T,
       col = "blue")

st_geometry(tt) <- "end"

tt %>% 
  head() %>% 
  select(transect_ID) %>% 
  plot(add = T,
       col = "red")

st_geometry(tt) <- "geometry"

tt %>% 
  head() %>% 
  select(transect_ID) %>% 
  plot(add = T,
       col = "black")
```

![Example of a few sections of one transect. Starting points in blue,
endpoints in red, centroids in black. Notice that the order of the start
and end point of the individual sections doesn’t necessarily follow the
transect in a continuous way. We would need to sort this out in the
source database.](figure/section_points-1.png)

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
    ##  1 d53aef1f-7… dfb9272e-…            5.22            59.4          5.22
    ##  2 d53aef1f-7… efc706a7-…            5.22            59.4          5.22
    ##  3 d53aef1f-7… fff25b5b-…            5.22            59.4          5.22
    ##  4 d53aef1f-7… 316c2185-…            5.22            59.4          5.22
    ##  5 d53aef1f-7… e0733813-…            5.22            59.4          5.22
    ##  6 d53aef1f-7… 34a6aebd-…            5.22            59.4          5.22
    ##  7 d53aef1f-7… 17696e5a-…            5.23            59.4          5.22
    ##  8 d53aef1f-7… cc8b5806-…            5.23            59.4          5.23
    ##  9 d53aef1f-7… b191a51d-…            5.23            59.4          5.23
    ## 10 d53aef1f-7… 0628debb-…            5.23            59.4          5.23
    ## # … with 1,821 more rows, and 5 more variables: lat_end_point <dbl>,
    ## #   lon_centroid <dbl>, lat_centroid <dbl>, SRID_for_all_coordinates <dbl>,
    ## #   monitoring_type <dbl>

## Add habitat type table

We haven’t differentiated between the sides of the transect, but only
characterized the individual sections to be in either “grassland” or
“forest”. All sections are located in open/low vegetation, and most
often align with roads or trails.

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
```

## Species name table

The only discrepancy between the locally used latin names and tha Fauna
Europea latin names is “Colias Croceus”/“Colias crocea”. Note that some
taxa are not distinguished to species.

``` r
speciesNameTab <- occTrans %>% 
  select(national_species_latin = scientificName,
         fauna_europea_species_latin = scientificName,
         local_species_norwegian = vernacularName) %>% 
  mutate(national_species_latin = ifelse(national_species_latin == "Colias croceus", "Colias crocea", national_species_latin))

speciesNameTab
```

    ## # A tibble: 2,426,474 x 3
    ##    national_species_latin    fauna_europea_species_lat… local_species_norwegian 
    ##    <chr>                     <chr>                      <chr>                   
    ##  1 Fabriciana adippe         Fabriciana adippe          Adippeperlemorvinge     
    ##  2 Vanessa cardui            Vanessa cardui             Tistelsommerfugl        
    ##  3 Boloria selene            Boloria selene             Brunflekket Perlemorvin…
    ##  4 Carterocephalus silvicola Carterocephalus silvicola  Svartflekksmyger        
    ##  5 Boloria eunomia           Boloria eunomia            Ringperlemorvinge       
    ##  6 Zygaena viciae            Zygaena viciae             Liten Bloddråpesvermer  
    ##  7 Thecla betulae            Thecla betulae             Slåpetornstjertvinge    
    ##  8 Polygonia c-album         Polygonia c-album          Hvit C                  
    ##  9 Erebia polaris            Erebia polaris             Polarringvinge          
    ## 10 Boloria improba           Boloria improba            Dvergperlemorvinge      
    ## # … with 2,426,464 more rows

# Export the data into CSV-files

``` r
write_csv(lepiCountTab,
          path = out/ButterflyCountDataTable.csv)

write_csv(monVisTabTransect,
          path = out/MonitoringVisitTableTransectLevel.csv)

write_csv(monVisTabSection,
          path = out/MonitoringVisitTableSectionLevel.csv)

write_csv(siteGeoInfoTab,
          path = out/SiteGeographicalInformationTable.csv)

write_csv(habTypeTab,
          path = out/HabitatTypeTable.csv)

write_csv(speciesNameTab,
          path = out/SpeciesNameTable.csv)
```
