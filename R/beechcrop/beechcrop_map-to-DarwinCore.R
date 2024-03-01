### Map beech crop data to Darwin Core & create DwC-A files ###

# Author: Cherine Jantzen
# Created: 2024-02-29
# Last updated: 2024-03-01

# Part I: Retrieve data ---------------------------------------------------

# load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(here)

# retrieve data
source(here::here("R", "beechcrop", "beechcrop_retrieveData-SQL-Server.R"))


# II. Event table ---------------------------------------------------------

# join tree related tables
trees <- d_tree %>%  
  dplyr::select(!c("SysDate", "SysUser", "Budburst", "Frass", "Wintermoth_Selection")) %>%  
  dplyr::left_join(d_area %>% 
                     dplyr::select("AreaID", "AreaName"), 
                   by = "AreaID") %>% 
  dplyr::left_join(d_species %>% 
                     dplyr::select("TreeSpeciesID", "TreeSpeciesName"),
                   by = "TreeSpeciesID") %>% 
  dplyr::left_join(d_site %>% 
                     dplyr::select(!c("SysDate", "SysUser")),
                   by = c("TreeID", "TreeNumber", "SiteNumber"))


# create level 2 events: collection of nuts in one plot of an individual tree
collect_events <- d_sample %>% 
  dplyr::select("BeechSampleID", "WinterYear", "YearCollect", "MonthCollect", "DayCollect", "TreeID", "Position", "CollectObserverID", "Remarks") %>% 
  dplyr::mutate(eventDate = as.character(lubridate::make_date(YearCollect, MonthCollect, DayCollect)),
                doy = lubridate::yday(eventDate),
                parentEventID = paste(YearCollect, doy, TreeID, sep = "-"),
                eventID = paste(parentEventID, paste0("P", Position), sep = "_"),
                sampleSizeValue = 1,
                sampleSizeUnit = "plot",
                verbatimLocality = trees$AreaName[match(.$TreeID, trees$TreeID)]) %>% 
  dplyr::rename("year" = "YearCollect",
                "month" = "MonthCollect",
                "day" = "DayCollect",
                "recordedByID" = "CollectObserverID")                


# create level 1 events: sampling one individual tree on one day in a year
events_level1 <- collect_events %>% 
  dplyr::distinct(parentEventID, .keep_all = TRUE) %>% 
  dplyr::mutate(sampleSizeValue = 1,
                sampleSizeUnit = "tree",
                verbatimLocality = trees$AreaName[match(.$TreeID, trees$TreeID)]) %>% 
  dplyr::select(!"eventID") %>% 
  dplyr::rename("eventID" = "parentEventID")

# create level 3 events: analysing collected nuts of one plot in the lab
measure_events <- d_sample %>% 
  dplyr::select(!c("MonthCollect", "DayCollect", "CollectObserverID", "SysUser", "SysDate")) %>%
  dplyr::left_join(collect_events %>%  
                     dplyr::select("BeechSampleID", "eventID"), 
                   by = "BeechSampleID") %>% 
  dplyr::rename("parentEventID" = "eventID",
                "year" = "YearCollect",
                "month" = "MonthWeight",
                "day" = "DayWeight",
                "recordedByID" = "AnalysticObserverID") %>% 
  dplyr::mutate(eventDate = as.character(lubridate::make_date(year, month, day)),
                eventID = paste(parentEventID, "analysis", sep = "_"),
                sampleSizeValue = 1,
                sampleSizeUnit = "plot",
                verbatimLocality = "NIOO") 

# combine all event level to event file
event <- dplyr::bind_rows(events_level1, collect_events, measure_events) %>% 
  dplyr::select("eventID", "parentEventID", "eventDate", "year", "month", "day",
                "sampleSizeValue", "sampleSizeUnit", "TreeID", "verbatimLocality", "recordedByID") %>% 
  dplyr::mutate(samplingProtocol = "Perdeck, A. C., Visser, M. E., & Van Balen, J. H. (2000). Great tit Parus major survival and the beech-crop. Ardea, 88, 99-106.",
                decimalLatitude = d_tree$Longitude[match(.$TreeID, d_tree$TreeID)],
                decimalLongitude = d_tree$Latitude[match(.$TreeID, d_tree$TreeID)],
                geodeticDatum = dplyr::case_when(!is.na(decimalLatitude) ~ "EPSG:4326",
                                                 TRUE ~ NA_character_),
                language = "en",
                country = "Netherlands",
                countryCode = "NL",
                institutionID = "https://ror.org/01g25jp36",
                institutionCode = "NIOO",
                type = "Event") 

# III. Occurrence table ---------------------------------------------------
species_names <- trees %>% 
  dplyr::filter(TreeID %in% unique(d_sample$TreeID)) %>% 
  dplyr::mutate(canonicalname = dplyr::case_when(TreeSpeciesName == "Beech" ~ "Fagus sylvatica"))

# Query for all species
tax <- taxize::get_gbifid_(sci = unique(species_names$canonicalname)) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == "EXACT") %>%
  tidyr::separate(canonicalname, c("Genus", "specificEpithet"), remove = FALSE) %>%
  dplyr::select("canonicalname", "scientificName" = "scientificname", "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet") %>% 
  dplyr::left_join(species_names %>% 
                     dplyr::select(TreeID, canonicalname),
                   by = "canonicalname")

# create help file with occurrence and measurement ID 
h2 <- d_weight %>% 
  dplyr::left_join(measure_events %>% 
                     dplyr::select("eventID", "BeechSampleID"), 
                   by = "BeechSampleID") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, 1:dplyr::n(), sep = "_"), .by = eventID) %>%  
  dplyr::select(!c("SysUser", "SysDate", "Remarks")) %>% 
  tidyr::pivot_longer(cols = "GrossWeight":"NetWeight", names_to = "variable", values_to = "measurementValue") %>%
  dplyr::mutate(measurementID = paste(occurrenceID, 1:dplyr::n(), sep = "_"), .by = occurrenceID)

occurrence_L3 <- h2 %>%
  dplyr::select("eventID", "occurrenceID") %>%
  dplyr::distinct(occurrenceID, .keep_all = TRUE) %>% 
  dplyr::mutate(organismQuantity = "several",
                organismQuantityType = "beechnuts",
                basisOfRecord = "HumanObservation",
                occurrenceStatus = "present",
                occurrenceRemarks = NA) %>%  # TODO tbf?
  dplyr::left_join(measure_events %>% 
                     dplyr::select("eventID", "eventDate", "TreeID", "recordedByID"),
                   by = "eventID", relationship = "many-to-many")

occurrence_L1 <- events_level1 %>% 
  dplyr::select("eventID", "eventDate", "TreeID") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, 1, sep = "_"), # FIXME does not really fit our naming schema
                organismQuantity = "1",
                organismQuantityType = "tree",
                basisOfRecord = "HumanObservation",
                occurrenceStatus = "present",
                occurrenceRemarks = NA) # TODO tbf?

occurrence <- dplyr::bind_rows(occurrence_L3, occurrence_L1) %>% 
  dplyr::left_join(tax %>% 
                     dplyr::select(!canonicalname), 
                   by = "TreeID") %>% 
  dplyr::rename("organismID" = "TreeID")

# III. Measurement or fact -----------------------------------------------------

# Transfer measurements to measurement or fact table
measures_1 <- measure_events %>% 
  dplyr::select("eventID", "NbrWhole", "TotalGrossWeightWhole", "NbrEaten", "NbrWithCaterpillars", "NbrRotten", "NbrRemainder", "NbrEmpty") %>% 
  tidyr::pivot_longer(cols = "NbrWhole":"NbrEmpty", names_to = "variable", values_to = "measurementValue")

measures_2 <- h2 %>% 
dplyr::select(!c("occurrenceID", "WeightID", "BeechSampleID"))

measures <- dplyr::bind_rows(measures_1, measures_2) %>% 
  dplyr::mutate(measurementType = dplyr::case_when(variable == "NbrWhole" ~ "Number of whole (PATO:0001446) beechnuts (FOODON:00003627)", 
                                                   variable == "TotalGrossWeightWhole" ~ "Total mass (PATO:0000125) with pericarp (PO:0009084) of all whole (PATO:0001446) beechnuts (FOODON:00003627)",
                                                   variable == "NbrEaten" ~ "Number of beechnuts (FOODON:00003627) that have been fed on",
                                                   variable == "NbrWithCaterpillars" ~ "Number of beechnuts (FOODON:00003627) with caterpillar (?)",
                                                   variable == "NbrRotten" ~ "Number of rotten beechnuts (FOODON:00003627)",
                                                   variable == "NbrRemainder" ~ "Number of beechnuts (FOODON:00003627) belonging to no other category",
                                                   variable == "NbrEmpty" ~ "Number of empty (SIO:001339) beechnuts (FOODON:00003627)",
                                                   variable == "GrossWeight" ~ "Mass (PATO:0000125) with pericarp (PO:0009084) of individual whole (PATO:0001446) beechnuts (FOODON:00003627)",
                                                   variable == "NetWeight" ~ "Mass (PATO:0000125) without pericarp (PO:0009084) of individual whole (PATO:0001446) beechnuts (FOODON:00003627)",
                                                   variable == "NbrNuts" ~ "Number of beechnuts (FOODON:00003627)?????"), # TODO what to do with this?
                measurementUnit = dplyr::case_when(stringr::str_detect(string = measurementType, pattern = "mass") ~ "milligrams"),
                # measurementID = paste(eventID, 1:n(), sep = "_"), .by = "eventID",
                measurementRemarks = NA)


# IV. Finalise files ------------------------------------------------------

event <- event %>% 
  dplyr::select(!c("TreeID", "recordedByID"))

# V. Save DwC-A files -----------------------------------------------------

write.csv(measurement_or_fact, file = here::here("data", "beechcrop_extendedmeasurementorfact.csv"), row.names = FALSE)
write.csv(event, file = here::here("data", "beechcrop_event.csv"), row.names = FALSE)
write.csv(occurrence, file = here::here("data", "beechcrop_occurrence.csv"), row.names = FALSE)


# VI. Create meta.xml for cricket DwC-A -------------------------------------

# fetch functions to create meta.xml file from according script and create meta.xml file for cricket DwC-A
source(here::here("R", "create-meta-xml-of-DwCA.R"))

create_meta_xml(core = c("Event" = here::here("data", "beechcrop_event.csv")),
                extensions = c("ExtendedMeasurementOrFact" = here::here("data", "beechcrop_extendedmeasurementorfact.csv"),
                               "Occurrence" = here::here("data", "beechcrop_occurrence.csv")),
                file = here::here("data", "beechcrop_meta.xml"))

