### Filter original beech crop data for publication & map beech to Darwin Core ###

# Author: Cherine C. Jantzen
# Created: 2024-02-29
# Last updated: 2025-11-07

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


# II. Check contents of data and remove unnecessary information -----------

## check tree table
names(d_tree)

# compare TreeNumber and TreeNumber_1976_1998
d_tree %>% 
  dplyr::mutate(compare_treeNumber = TreeNumber - TreeNumber_1976_1998) %>% 
  dplyr::filter(compare_treeNumber != 0)
# no rows remaining, i.e., both numbers are identical and TreeNumber_1976_1998 can go

tree <- d_tree %>%
  dplyr::select(!c("SysDate", "SysUser", "Budburst", "Frass", "Wintermoth_Selection", 
                   "TreeNumber_1976_1998", "UserPlaceName"))

## check area table
names(d_area)
# AreaShortName, AreaID_GT, SysUser, SysDate can go

unique(d_area$Remarks) 
# no relevant remarks

area <- d_area %>% 
  dplyr::select(!c("AreaShortName", "AreaID_GT", "SysUser", "SysDate", "Remarks"))

## check species table
names(d_species)
# SysUser, SysDate can go

unique(d_species$Remarks) 
# no remarks

species <- d_species %>% 
  dplyr::select(!c("SysUser", "SysDate", "Remarks"))

## check sampletype table
names(d_sampletype)
# SysUser, SysDate can go

unique(d_sampletype$Remark)
# remarks relevant

sampletype <- d_sampletype %>% 
  dplyr::select(!c("SysUser", "SysDate")) %>% 
  dplyr::rename("sampletype_Remark" = "Remark")

## check weight table
names(d_weight)
# SysUser, SysDate can go

unique(d_weight$Remarks)
# remarks relevant

# remove Remarks that are not suitable for external use
weight <- d_weight %>% 
  dplyr::mutate(weight_Remarks = dplyr::case_when(Remarks %in% c("Gross weight was estimated") ~  Remarks,
                                                  Remarks == "Record replaces previously combined positions, JR 20250513;no net weight, value not reliable" ~ "no net weight, value not reliable",
                                                  TRUE ~ NA_character_)) %>% 
  dplyr::select(!c("SysUser", "SysDate"))

unique(weight$weight_Remarks)


## d_sample_view is the homogenized and processed version of d_sample (i.e., only use this) 
names(d_sample_view)
# SysUser, SysDate can go

unique(d_sample_view$Remarks)
# Remarks relevant

samples <- d_sample_view %>% 
  dplyr::mutate(sample_Remarks = dplyr::case_when(Remarks %in% c("Possibly false count",  "possible false count") ~ "Possibly false count",
                                                  Remarks %in% c("outer skin missing, no gross weight", "4th position extrapolated from positions 1 to 3;Possibly false count") ~ Remarks,
                                                  Remarks %in% c("4th position extrapolated from positions 1 to 3;E1 in book, recoded to E2 as used interchangeably over time",
                                                                 "4th position extrapolated from positions 1 to 3;Record replaces previously combined positions, JR 20250513;",
                                                                 "4th position extrapolated from positions 1 to 3;Record replaces previously combined positions, JR 20250513;E1 in book, recoded to E2 as used interchangeably over time",
                                                                 "4th position extrapolated from positions 1 to 3;Sample without nuts, zeros added April 2024") ~ "4th position extrapolated from positions 1 to 3", 
                                                  TRUE ~ NA_character_)) %>% 
  dplyr::select(!c("SysUser", "SysDate")) %>% 
  dplyr::rename("SampleTypeID" = "SampleType")

## check position table
names(d_position)
# SysUser, SysDate can go

position <- d_position %>% 
  dplyr::select(!c("SysUser", "SysDate"))

# II. Event table ---------------------------------------------------------

# combine all tree related information
tree_info <- tree %>%
  dplyr::left_join(area, by = "AreaID") %>% 
  dplyr::left_join(species, by = "TreeSpeciesID")

## event and eventID structure ####
# level 1: individual tree on a certain date -- eventID: YearMonthDay-TreeID (e.g., 20240920-875)
# level 2: individual plot of a tree on a date -- eventID: YearMonthDay-TreeID_PPositionTSampleType (e.g., 20240920-875_P1T1)
# level 3: individual weights of one (ore more nuts together) in a plot of a tree on a certain date -- eventID: YearMonthDay-TreeID_PPositionTSampleType_NNumberofMeasurement (e.g., 20240920-875_P1T1_N5 for the fifth nut that's been weighed for this plot)
### ###

# create level 2 events: collection of nuts in one plot of an individual tree
events_level2 <- samples %>% 
  dplyr::left_join(sampletype, by = "SampleTypeID") %>% 
  dplyr::mutate(eventDate = lubridate::make_date(YearCollect, MonthCollect, DayCollect),
                verbatimEventDate = paste("Season", WinterYear, sep =" "),
                parentEventID = paste(paste0(YearCollect, 
                                             substring(eventDate, 6, 7), 
                                             substring(eventDate, 9, 10)), 
                                      TreeID, sep = "-"),
                eventID = paste(parentEventID, paste0("P", Position, "T", SampleTypeID), sep = "_"),
                samplingProtocol = "ground-based collection of nuts in mobile quadrat",
                sampleSizeValue = 0.0625,
                sampleSizeUnit = "square metre",
                verbatimLocality = tree_info$AreaName[match(.$TreeID, tree_info$TreeID)],
                fieldNumber = paste0("BeechSampleID_", BeechSampleID),
                recordedByID = CollectObserverID,
                year = YearCollect,
                month = MonthCollect,
                day = DayCollect) 

# create level 1 events: sampling one individual tree on one day in a year
events_level1 <- events_level2 %>% 
  dplyr::distinct(parentEventID, .keep_all = TRUE) %>% 
  dplyr::mutate(samplingProtocol = "Perdeck, A. C., Visser, M. E., & Van Balen, J. H. (2000). Great tit Parus major survival and the beech-crop. Ardea, 88, 99-106.", #TODO: change to our paper 
                sampleSizeValue = 1,
                sampleSizeUnit = "tree",
                verbatimLocality = tree_info$AreaName[match(.$TreeID, tree_info$TreeID)],
                fieldNumber = NA,
                recordedByID = NA) %>% 
  dplyr::select(!"eventID") %>% 
  dplyr::rename("eventID" = "parentEventID")


level2_eventIDs <- events_level2 %>% 
  select("BeechSampleID", "Position", "parentEventID" = "eventID")

# create level 3 events: weights of individual nuts per plot
events_level3 <- samples %>% 
  dplyr::left_join(level2_eventIDs, by = c("BeechSampleID", "Position")) %>% 
  dplyr::left_join(weight, by = "BeechSampleID", relationship = "many-to-many") %>% # BeechSampleID is not unique because of corrections, and several weights belong to the same plot (i.e., BeechSampleID)
  dplyr::filter(!is.na(WeightID)) %>% 
  dplyr::mutate(eventID = paste(parentEventID, paste0("N", 1:dplyr::n()), sep = "_"), 
                .by = c(BeechSampleID, Position),
                year = YearCollect,
                month = MonthWeight,
                day = DayWeight,
                eventDate = lubridate::make_date(year, month, day),
                samplingProtocol = "weighing of individual nuts",
                sampleSizeValue = 1,
                sampleSizeUnit = "nut",
                verbatimLocality = "NIOO-KNAW",
                recordedByID = AnalysticObserverID,
                fieldNumber = paste0("weightID_", WeightID))

# combine all event level to event file & add general terms
event <- dplyr::bind_rows(events_level1, events_level2, events_level3) %>% 
  dplyr::select("eventID", "parentEventID", "eventDate", "verbatimEventDate", "year", "month", "day", "samplingProtocol",
                "sampleSizeValue", "sampleSizeUnit", "TreeID", "verbatimLocality", "recordedByID", "fieldNumber") %>% 
  dplyr::mutate(decimalLatitude = tree_info$Longitude[match(.$TreeID, tree_info$TreeID)],
                decimalLongitude = tree_info$Latitude[match(.$TreeID, tree_info$TreeID)],
                geodeticDatum = dplyr::case_when(!is.na(decimalLatitude) ~ "EPSG:4326",
                                                 TRUE ~ NA_character_),
                minimumElevationInMeters = tree_info$Elevation[match(.$TreeID, tree_info$TreeID)],
                maximumElevationInMeters = tree_info$Elevation[match(.$TreeID, tree_info$TreeID)],
                verticalDatum = "metres above sea level",
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
species_names <- tree_info %>% 
  dplyr::filter(TreeID %in% unique(samples$TreeID)) %>% 
  dplyr::mutate(canonicalname = dplyr::case_when(TreeSpeciesName == "Beech" ~ "Fagus sylvatica"))

# query GBIFs taxonomic information for all species
tax <- taxize::get_gbifid_(sci = unique(species_names$canonicalname)) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == "EXACT") %>%
  tidyr::separate(canonicalname, c("Genus", "specificEpithet"), remove = FALSE) %>%
  dplyr::select("canonicalname", "scientificName" = "scientificname", "kingdom", 
                "phylum", "class", "order", "family", "genus", "specificEpithet") %>% 
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
  dplyr::left_join(samples %>% 
                     dplyr::rowwise() %>% 
                     dplyr::mutate(sumNuts = sum(NbrWhole, NbrEaten, NbrWithCaterpillars, NbrRotten, 
                                                 NbrEmpty, NbrRemainder, na.rm = TRUE)) %>% 
                     dplyr::select("BeechSampleID", "sumNuts", "Position"), 
                   by = c("BeechSampleID", "Position")) %>% 
  dplyr::mutate(occurrenceID = paste(eventID, paste0("o", 1:dplyr::n()), sep = "_"), 
                .by = eventID,
                organismQuantity = sumNuts,
                organismQuantityType = "nuts")

# create occurrence table for level 3 events
occurrence_L3 <- events_level3 %>% 
  dplyr::mutate(occurrenceID = paste(eventID, paste0("o", 1:dplyr::n()), sep = "_"), .by = eventID) %>%
  dplyr::mutate(organismQuantity = NbrNuts,
                organismQuantityType = "nuts") 

# bind occurrence files together and add general terms
occurrence <- dplyr::bind_rows(occurrence_L1, occurrence_L2, occurrence_L3) %>% 
  dplyr::left_join(tax %>% 
                     dplyr::select(!canonicalname), 
                   by = "TreeID") %>%
  dplyr::mutate(basisOfRecord = "HumanObservation",
                occurrenceStatus = "present") %>% 
  dplyr::select("eventID", "occurrenceID", "organismID", "recordedByID", 
                "organismQuantity", "organismQuantityType", "occurrenceStatus", 
                "scientificName", "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet")


# III. Measurement or fact -----------------------------------------------------

# measurements and counts on plot level (level 2 events)
measures_2 <- events_level2 %>% 
  dplyr::left_join(occurrence_L2 %>% 
                     dplyr::select("BeechSampleID","Position", "occurrenceID"),
                   by = c("BeechSampleID", "Position"), relationship = "many-to-many") %>% 
  tidyr::pivot_longer(cols = c(starts_with("Nbr"), 
                               all_of(c("TotalGrossWeightWhole", "Position", "SampleTypeID"))), 
                      names_to = "variable", 
                      values_to = "measurementValue") %>%
  dplyr::mutate(measurementDeterminedDate = lubridate::make_date(year, MonthWeight, DayWeight),
                measurementDeterminedBy = AnalysticObserverID,
                measurementID = paste(stringr::str_remove(string = occurrenceID, pattern = "o"), 
                                      paste0("m", 1:dplyr::n()), sep = "_"), 
                .by = occurrenceID,
                measurementRemarks = sample_Remarks)

# measurements on individual nut level (level 3 events)
measures_3 <- occurrence_L3 %>% 
  tidyr::pivot_longer(cols = all_of(c("GrossWeight", "NetWeight")), names_to = "variable", values_to = "measurementValue") %>%
  dplyr::mutate(measurementID = paste(stringr::str_remove(string = occurrenceID, pattern = "o"), 
                                      paste0("m", 1:dplyr::n()), sep = "_"), 
                .by = occurrenceID,
                measurementDeterminedDate = eventDate,
                measurementDeterminedBy = recordedByID,
                measurementRemarks = weight_Remarks)

# bind different measurements together and add remaining terms
measurement_or_fact <- dplyr::bind_rows(measures_2, measures_3) %>% 
  dplyr::mutate(
    verbatimMeasurementType = variable,
    measurementType = dplyr::case_when(
      variable == "NbrWhole" ~ "Number of whole (PATO:0001446) nut fruits (PO:0030102)", 
      variable == "TotalGrossWeightWhole" ~ "Nut fruit weight (TO:0001093) with pericarp (PO:0009084) of all whole (PATO:0001446) nut fruits (PO:0030102)",
      variable == "NbrEaten" ~ "Number of nut fruits (PO:0030102) that have been fed on",
      variable == "NbrWithCaterpillars" ~ "Number of nut fruits (PO:0030102) with signs of caterpillar usage",
      variable == "NbrRotten" ~ "Number of rotten nut fruits (PO:0030102)",
      variable == "NbrRemainder" ~ "Number of nut fruits (PO:0030102) belonging to no other category",
      variable == "NbrEmpty" ~ "Number of empty (SIO:001339) nut fruits (PO:0030102)",
      variable == "GrossWeight" ~ "Nut fruit weight (TO:0001093) with pericarp (PO:0009084) of individual whole (PATO:0001446) nut fruits (PO:0030102)",
      variable == "NetWeight" ~ "Nut fruit weight (TO:0001093) without pericarp (PO:0009084) of individual whole (PATO:0001446) nut fruits (PO:0030102)",
      variable == "Position" ~ "Numeric position of plot",
      variable == "SampleTypeID" ~ "Sample Type (NCIT:C210102) ID"
    ),
    measurementUnit = dplyr::if_else((stringr::str_detect(string = measurementType, pattern = "weight") & 
                                        !is.na(measurementValue)), "milligram", NA),
    measurementMethod = dplyr::case_when(
      variable == "NbrWhole" ~ "Hand count all shiny and firm whole nut fruits", 
      variable == "TotalGrossWeightWhole" ~ "Weigh all whole nut fruits with pericarp",
      variable == "NbrEaten" ~ "Hand count nut fruits with frayed wholes (usually) at thick side or corner of nut fruit",
      variable == "NbrWithCaterpillars" ~ "Hand count nut fruits with small round wholes and caterpillar droppings inside",
      variable == "NbrRotten" ~ "Hand count nut fruits that are light in weight and contain smaller balck nut fruits inside",
      variable == "NbrRemainder" ~ "Hand count nut fruits belonging to no other category",
      variable == "NbrEmpty" ~ "Hand count nut fruits that are completely empfty and can easily be squashed",
      variable == "GrossWeight" ~ "Weigh individual nut fruit with pericarp",
      variable == "NetWeight" ~ "Weigh individual nut fruit without pericarp",
      variable == "SampleTypeID" ~ sampletype_Remark)) %>% 
  dplyr::select("eventID", "occurrenceID", "measurementID", "measurementType", "verbatimMeasurementType", "measurementValue",
                "measurementUnit", "measurementDeterminedDate", "measurementDeterminedBy", "measurementMethod",
                "measurementRemarks")


# IV. Save DwC-A files -----------------------------------------------------

# choose directory to store files
dir_loc <- rstudioapi::selectDirectory()


write.csv(event, file = paste0(dir_loc, "/", "beechcrop_event.csv"), row.names = FALSE)
write.csv(occurrence, file = paste0(dir_loc, "/", "beechcrop_occurrence.csv"), row.names = FALSE)
write.csv(measurement_or_fact, file = paste0(dir_loc, "/", "beechcrop_extendedmeasurementorfact.csv"), row.names = FALSE)


# V. Create meta.xml for beech crop DwC-A -----------------------------------

# fetch functions to create meta.xml file from according script
source(here::here("R", "create-meta-xml-of-DwCA.R"))

# create meta.xml file for beech crop DwC-A
create_meta_xml(core = c("Event" = paste0(dir_loc, "/", "beechcrop_event.csv")),
                extensions = c("ExtendedMeasurementOrFact" = paste0(dir_loc, "/", "beechcrop_extendedmeasurementorfact.csv"),
                               "Occurrence" = paste0(dir_loc, "/", "beechcrop_occurrence.csv")),
                file = paste0(dir_loc, "/", "beechcrop_meta.xml"))

