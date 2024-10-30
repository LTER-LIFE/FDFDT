### Map beech crop data to Darwin Core & create DwC-A files ###

# Author: Cherine Jantzen
# Created: 2024-02-29
# Last updated: 2024-10-30

# Part I: Retrieve data ---------------------------------------------------

# load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(stringr)
library(taxize)

# retrieve data
source(here::here("R", "beechcrop", "beechcrop_retrieveData-SQL-Server.R"))


# II. Deal with missing years ---------------------------------------------
missing_years <- setdiff(seq(min(d_sample$WinterYear), max(d_sample$WinterYear), 1), unique(d_sample$WinterYear))

average_samplingDay <- d_sample %>% 
  dplyr::filter(dplyr::between(YearCollect, 2000, 2020)) %>% 
  dplyr::mutate(date = lubridate::make_date(YearCollect, MonthCollect, DayCollect),
                doy = lubridate::yday(date)) %>% 
  dplyr::summarise(meanDoy = mean(doy, na.rm = TRUE)) %>% 
  dplyr::pull(meanDoy)

eventDate_missingYears <- tibble::tibble(year = missing_years, eventDate = (round(average_samplingDay) + lubridate::make_date(missing_years, 1, 1) - 1)) %>% 
  dplyr::mutate(DayCollect = lubridate::day(eventDate),
                MonthCollect = lubridate::month(eventDate))

trees_1981_1996 <- d_sample %>%
  dplyr::filter(WinterYear == missing_years - 1) %>%
  dplyr::group_by(WinterYear) %>%
  dplyr::distinct(TreeID, .keep_all = TRUE) %>%
  dplyr::select("WinterYear", "TreeID") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(YearCollect = WinterYear + 1,
                WinterYear = YearCollect)

trees_missingYears <- trees_1981_1996 %>%
  dplyr::filter(WinterYear == 1981) %>%
  dplyr::mutate(YearCollect = WinterYear + 1,
                WinterYear = YearCollect) %>%
  dplyr::bind_rows(trees_1981_1996) %>% 
  dplyr::mutate(BeechSampleID = c((max(d_sample$BeechSampleID) + 1):(length(.$YearCollect) + max(d_sample$BeechSampleID))),
                Position = 1234,
                NbrWhole = 0,
                NbrEaten = 0, 
                NbrWithCaterpillars = 0, 
                NbrRotten = 0, 
                NbrRemainder = 0, 
                NbrEmpty = 0) %>% 
  dplyr::left_join(eventDate_missingYears, by = c("WinterYear" = "year"))

d_sample <- d_sample %>%
  dplyr::bind_rows(trees_missingYears)

# III. Event table ---------------------------------------------------------

# combine all tree related information
trees <- d_tree %>%
  dplyr::select(!c("SysDate", "SysUser", "Budburst", "Frass", "Wintermoth_Selection")) %>%  
  dplyr::left_join(d_area %>% 
                     dplyr::select("AreaID", "AreaName"), 
                   by = "AreaID") %>% 
  dplyr::left_join(d_species %>% 
                     dplyr::select("TreeSpeciesID", "TreeSpeciesName"),
                   by = "TreeSpeciesID")


# create level 2 events: collection of nuts in one plot of an individual tree
events_level2 <- d_sample %>% 
  dplyr::select("BeechSampleID", "WinterYear", "YearCollect", "MonthCollect", "DayCollect", "TreeID", "Position", "CollectObserverID", "Remarks") %>% 
  dplyr::mutate(eventDate = lubridate::make_date(YearCollect, MonthCollect, DayCollect),
                parentEventID = paste(WinterYear, paste0(MonthCollect, DayCollect), TreeID, sep = "-"),
                eventID = paste(parentEventID, paste0("P", Position), sep = "_"),
                samplingProtocol = "ground-based collection of nuts in mobile quadrat",
                sampleSizeValue = 0.09,
                sampleSizeUnit = "square metre",
                verbatimLocality = trees$AreaName[match(.$TreeID, trees$TreeID)]) %>% 
  dplyr::rename("year" = "YearCollect",
                "month" = "MonthCollect",
                "day" = "DayCollect",
                "recordedByID" = "CollectObserverID")                


# create level 1 events: sampling one individual tree on one day in a year
events_level1 <- events_level2 %>% 
  dplyr::distinct(parentEventID, .keep_all = TRUE) %>% 
  dplyr::mutate(samplingProtocol = "Perdeck, A. C., Visser, M. E., & Van Balen, J. H. (2000). Great tit Parus major survival and the beech-crop. Ardea, 88, 99-106.",
                sampleSizeValue = 1,
                sampleSizeUnit = "tree",
                verbatimLocality = trees$AreaName[match(.$TreeID, trees$TreeID)]) %>% 
  dplyr::select(!"eventID") %>% 
  dplyr::rename("eventID" = "parentEventID")


# create level 3 events: individual nuts per plot
events_level3 <- d_sample %>% 
  dplyr::left_join(d_weight %>% 
                     dplyr::select(!c("SysUser", "SysDate")), 
                   by = "BeechSampleID") %>%
  dplyr::filter(!is.na(WeightID)) %>% 
  dplyr::rename("year" = "YearCollect",
                "month" = "MonthWeight",
                "day" = "DayWeight",
                "recordedByID" = "AnalysticObserverID") %>% 
  dplyr::mutate(eventID = paste(paste(WinterYear, paste0(MonthCollect, DayCollect), TreeID, sep = "-"), paste0("P", Position), paste0("N", 1:dplyr::n()), sep = "_"), .by = "BeechSampleID",
                parentEventID = paste(paste(WinterYear, paste0(MonthCollect, DayCollect), TreeID, sep = "-"), paste0("P", Position), sep = "_"),
                eventDate = lubridate::make_date(year, month, day),
                samplingProtocol = "weighing of individual nuts",
                sampleSizeValue = 1,
                sampleSizeUnit = "nut",
                verbatimLocality = "NIOO-KNAW")

# combine all event level to event file & add general terms
event <- dplyr::bind_rows(events_level1, events_level2, events_level3) %>% 
  dplyr::select("eventID", "parentEventID", "eventDate", "year", "month", "day",
                "sampleSizeValue", "sampleSizeUnit", "TreeID", "verbatimLocality", "recordedByID") %>% 
  dplyr::mutate(decimalLatitude = d_tree$Longitude[match(.$TreeID, d_tree$TreeID)],
                decimalLongitude = d_tree$Latitude[match(.$TreeID, d_tree$TreeID)],
                geodeticDatum = dplyr::case_when(!is.na(decimalLatitude) ~ "EPSG:4326",
                                                 TRUE ~ NA_character_),
                language = "en",
                country = "Netherlands",
                countryCode = "NL",
                institutionID = "https://ror.org/01g25jp36",
                institutionCode = "NIOO-KNAW",
                type = "Event") %>% 
  dplyr::arrange(eventID) %>% 
  dplyr::select(!c("TreeID", "recordedByID"))


# III. Occurrence table ---------------------------------------------------

# add scientific species name(s)
species_names <- trees %>% 
  dplyr::filter(TreeID %in% unique(d_sample$TreeID)) %>% 
  dplyr::mutate(canonicalname = dplyr::case_when(TreeSpeciesName == "Beech" ~ "Fagus sylvatica"))

# query GBIFs taxonomic information for all species
tax <- taxize::get_gbifid_(sci = unique(species_names$canonicalname)) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == "EXACT") %>%
  tidyr::separate(canonicalname, c("Genus", "specificEpithet"), remove = FALSE) %>%
  dplyr::select("canonicalname", "scientificName" = "scientificname", "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet") %>% 
  dplyr::left_join(species_names %>% 
                     dplyr::select(TreeID, canonicalname),
                   by = "canonicalname")

# create occurrence table for level 1 events
occurrence_L1 <- events_level1 %>% 
  dplyr::select("eventID", "TreeID") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, paste0("o", 1), sep = "_"), 
                organismQuantity = 1,
                organismQuantityType = "tree",
                organismID = TreeID)

# create occurrence table for level 2 events
occurrence_L2 <- events_level2 %>% 
  dplyr::left_join(d_sample %>% 
                     dplyr::rowwise() %>% 
                     dplyr::mutate(sumNuts = sum(NbrWhole, NbrEaten, NbrWithCaterpillars, NbrRotten, NbrEmpty, NbrRemainder, na.rm = TRUE)) %>% 
                     dplyr::select("BeechSampleID", "sumNuts"), 
                   by = "BeechSampleID") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, paste0("o", 1:dplyr::n()), sep = "_"), .by = eventID,
                organismQuantity = sumNuts,
                organismQuantityType = "nuts")

# create helper file for creating occurrence and measurement table of level 3 events
h1 <- events_level3 %>% 
  dplyr::select("eventID", "GrossWeight", "NetWeight", "NbrNuts", "recordedByID", "TreeID", "eventDate") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, paste0("o", 1:dplyr::n()), sep = "_"), .by = eventID) %>% 
  tidyr::pivot_longer(cols = "GrossWeight":"NetWeight", names_to = "variable", values_to = "measurementValue") %>%
  dplyr::mutate(measurementID = paste(stringr::str_remove(string = occurrenceID, pattern = "o"), paste0("m", 1:dplyr::n()), sep = "_"), .by = occurrenceID,
                measurementDeterminedDate = eventDate)

# create occurrence table for level 3 events
occurrence_L3 <- h1 %>%
  dplyr::select("eventID", "occurrenceID", "NbrNuts", "TreeID") %>%
  dplyr::distinct(occurrenceID, .keep_all = TRUE) %>% 
  dplyr::mutate(organismQuantity = NbrNuts,
                organismQuantityType = "nuts") 

# bind occurrence files together and add general terms
occurrence <- dplyr::bind_rows(occurrence_L1, occurrence_L2, occurrence_L3) %>% 
  dplyr::left_join(tax %>% 
                     dplyr::select(!canonicalname), 
                   by = "TreeID") %>%
  dplyr::mutate(basisOfRecord = "HumanObservation",
                occurrenceStatus = "present") %>% 
  dplyr::select("eventID", "occurrenceID", "organismID", "recordedByID", "organismQuantity", "organismQuantityType", "occurrenceStatus", 
                "scientificName", "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet")


# III. Measurement or fact -----------------------------------------------------

# measurements and counts on plot level (level 2 events)
measures_1 <- events_level2 %>% 
  dplyr::left_join(d_sample %>% 
                     dplyr::select("BeechSampleID", "NbrWhole", "TotalGrossWeightWhole", "NbrEaten", "NbrWithCaterpillars", 
                                   "NbrRotten", "NbrRemainder", "NbrEmpty", "MonthWeight", "DayWeight", "AnalysticObserverID"), 
                   by = "BeechSampleID") %>% 
  tidyr::pivot_longer(cols = "NbrWhole":"NbrEmpty", names_to = "variable", values_to = "measurementValue") %>% 
  dplyr::left_join(occurrence_L2 %>% 
                     dplyr::select("BeechSampleID", "occurrenceID"),
                   by = "BeechSampleID") %>% 
  dplyr::mutate(measurementDeterminedDate = lubridate::make_date(year, MonthWeight, DayWeight),
                measurementDeterminedBy = AnalysticObserverID,
                measurementID = paste(stringr::str_remove(string = occurrenceID, pattern = "o"), paste0("m", 1:dplyr::n()), sep = "_"), .by = occurrenceID) 

# measurements on individual nut level (level 3 events)
measures_2 <- h1 %>% 
  dplyr::select(c("eventID", "occurrenceID", "variable", "measurementValue", "measurementID", "measurementDeterminedDate", "measurementDeterminedBy" = "recordedByID")) 

# bind different measurements together and add remaining terms
measurement_or_fact <- dplyr::bind_rows(measures_1, measures_2) %>% 
  dplyr::mutate(measurementType = dplyr::case_when(variable == "NbrWhole" ~ "Number of whole (PATO:0001446) nut fruits (PO:0030102)", 
                                                   variable == "TotalGrossWeightWhole" ~ "Nut fruit weight (TO:0001093) with pericarp (PO:0009084) of all whole (PATO:0001446) nut fruits (PO:0030102)",
                                                   variable == "NbrEaten" ~ "Number of nut fruits (PO:0030102) that have been fed on",
                                                   variable == "NbrWithCaterpillars" ~ "Number of nut fruits (PO:0030102) with signs of caterpillar usage",
                                                   variable == "NbrRotten" ~ "Number of rotten nut fruits (PO:0030102)",
                                                   variable == "NbrRemainder" ~ "Number of nut fruits (PO:0030102) belonging to no other category",
                                                   variable == "NbrEmpty" ~ "Number of empty (SIO:001339) nut fruits (PO:0030102)",
                                                   variable == "GrossWeight" ~ "Nut fruit weight (TO:0001093) with pericarp (PO:0009084) of individual whole (PATO:0001446) nut fruits (PO:0030102)",
                                                   variable == "NetWeight" ~ "Nut fruit weight (TO:0001093) without pericarp (PO:0009084) of individual whole (PATO:0001446) nut fruits (PO:0030102)"),
                measurementUnit = dplyr::if_else((stringr::str_detect(string = measurementType, pattern = "weight") & !is.na(measurementValue)), "milligram", NA),
                measurementMethod = dplyr::case_when(variable == "NbrWhole" ~ "Hand count all shiny and firm whole nut fruits", 
                                                     variable == "TotalGrossWeightWhole" ~ "Weigh all whole nut fruits with pericarp",
                                                     variable == "NbrEaten" ~ "Hand count nut fruits with frayed wholes (usually) at thick side or corner of nut fruit",
                                                     variable == "NbrWithCaterpillars" ~ "Hand count nut fruits with small round wholes and caterpillar droppings inside",
                                                     variable == "NbrRotten" ~ "Hand count nut fruits that are light in weight and contain smaller balck nut fruits inside",
                                                     variable == "NbrRemainder" ~ "Hand count nut fruits belonging to no other category",
                                                     variable == "NbrEmpty" ~ "Hand count nut fruits that are completely empfty and can easily be squashed",
                                                     variable == "GrossWeight" ~ "Weigh individual nut fruit with pericarp",
                                                     variable == "NetWeight" ~ "Weigh individual nut fruit without pericarp"),
                measurementRemarks = NA) %>% 
  dplyr::select("eventID", "occurrenceID", "measurementID", "measurementType", "measurementValue", "measurementUnit", "measurementDeterminedDate", "measurementDeterminedBy", "measurementMethod", "measurementRemarks")


# IV. Save DwC-A files -----------------------------------------------------

write.csv(event, file = here::here("data", "beechcrop_event.csv"), row.names = FALSE)
write.csv(occurrence, file = here::here("data", "beechcrop_occurrence.csv"), row.names = FALSE)
write.csv(measurement_or_fact, file = here::here("data", "beechcrop_extendedmeasurementorfact.csv"), row.names = FALSE)

# V. Create meta.xml for beech crop DwC-A -----------------------------------

# fetch functions to create meta.xml file from according script
source(here::here("R", "create-meta-xml-of-DwCA.R"))

# create meta.xml file for beech crop DwC-A
create_meta_xml(core = c("Event" = here::here("data", "beechcrop_event.csv")),
                extensions = c("ExtendedMeasurementOrFact" = here::here("data", "beechcrop_extendedmeasurementorfact.csv"),
                               "Occurrence" = here::here("data", "beechcrop_occurrence.csv")),
                file = here::here("data", "beechcrop_meta.xml"))

