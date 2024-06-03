# Map CLUE field vegetation data of experiment 1 to Darwin Core ####

# Author: Cherine Jantzen
# Created: 2024-03-25
# Last updated: 2024-06-03


# I. Preparation ----------------------------------------------------------

# load packages
library(dplyr)
library(tidyr)
library(taxize)
library(readxl)
library(stringr)

# read in data
plantCover <- readxl::read_xlsx(choose.files()) # "master database exp1.xlsx"

# retrieve function to retrieve taxonomic information from GBIF
source(here::here("R", "CLUE", "retrieve-taxonInformation-from-GBIF.R"))

# retrieve function to create meta.xml file
source(here::here("R", "create-meta-xml-of-DwCA.R"))

# II. Create event core ---------------------------------------------------

# create eventID
event <- plantCover %>% 
  dplyr::mutate(eventID = paste(year, treatment, block, pq, sep = "-"),
                language = "en",
                country = "Netherlands",
                countryCode = "NL",
                institutionID = "https://ror.org/01g25jp36",
                institutionCode = "NIOO-KNAW",
                type = "Event",
                samplingProtocol = "Fukami, T., Martijn Bezemer, T., Mortimer, S.R. and van der Putten, W.H. (2005), Species divergence and trait convergence in experimental plant community assembly. Ecology Letters, 8: 1283-1290. https://doi.org/10.1111/j.1461-0248.2005.00829.x",
                sampleSizeValue = 1,
                sampleSizeUnit = "square metre",
                decimalLongitude = "5.75",
                decimalLatitude = "52.07", 
                geodeticDatum = "EPSG:4326", 
                verbatimLocality = "Mossel", 
                eventDate = paste0(year, "-07")) %>%  
  dplyr::select("eventID", "samplingProtocol", "sampleSizeValue",
                "sampleSizeUnit", "eventDate", "year", "country",
                "countryCode", "verbatimLocality", "decimalLatitude", "decimalLongitude",
                "geodeticDatum", "type", "language", "institutionID", "institutionCode")


# III. Create occurrence file ---------------------------------------------

# recode scientific names
sciNames <- tibble::tibble(taxonName = names(plantCover)[5:length(plantCover)]) %>% 
  dplyr::mutate(newName = dplyr::case_when(taxonName == "Apiaceaa" ~ "Apiaceae",
                                           taxonName == "Trifolium spp. (repens/pratense)" ~ "Trifolium",
                                           TRUE ~ taxonName),
                newName = stringr::str_remove_all(string = newName, pattern = " sp$| sp\\.| spp\\."),
                newName = stringr::str_replace(string = newName, pattern = "Sision amomum", replace = "Sison amomum"),
                newName = stringr::str_replace(string = newName, pattern = "Polygonum  hydropiper", replace = "Polygonum hydropiper"))


# call function to retrieve taxonomic information
taxonInformation <- get_taxonInformation(scientificNames = sciNames$newName,
                                         taxa_kingdom = "Plantae")

# reorder retrieved tax. information and merge with original names
taxon_reorderd <- taxonInformation %>% 
  dplyr::arrange(canonicalname) %>% 
  dplyr::bind_cols(sciNames %>% 
                     dplyr::arrange(newName)) %>% 
  tidyr::separate_wider_delim(cols = species,  names = c("Genus", "specificEpithet"), delim = " ", cols_remove = FALSE)

# create occurrence file
occurrence <- plantCover %>% 
  dplyr::mutate(eventID = paste(year, treatment, block, pq, sep = "-")) %>% 
  tidyr::pivot_longer(cols = names(plantCover[5]):names(plantCover[length(plantCover)]), values_to = "organismQuantity", names_to = "taxon") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, 1:dplyr::n(), sep = "_"), .by = eventID,
                organismQuantityType = "% cover",
                basisOfRecord = "humanObservation",
                occurrenceStatus = "present") %>% 
  dplyr::left_join(taxon_reorderd, by = c("taxon" = "taxonName")) %>%
  dplyr::rename("scientificName" = "scientificname") %>% 
  dplyr::select("eventID", "occurrenceID", "organismQuantity", 
                "organismQuantityType", "basisOfRecord", "occurrenceStatus",
                "scientificName", "kingdom", "phylum", "class", "order",
                "family", "genus", "specificEpithet")


# IV. Create Measurement or fact file -------------------------------------

measurementorfact <- plantCover %>% 
  dplyr::mutate(eventID = paste(year, treatment, block, pq, sep = "-")) %>%  
  dplyr::mutate(measurementID = paste(eventID, 1:dplyr::n(), sep = "_"), .by = eventID, 
                measurementValue = dplyr::case_when(treatment == "CA" ~ "Continued agricultural rotation",
                                                    treatment == "NC" ~ "Natural control",
                                                    treatment == "HD" ~ "High diversity sowing", 
                                                    treatment == "LD" ~ "Low diversity sowing"),
                measurementUnit = NA, 
                measurementType = "treatment (AGRO:00000322)",
                measurementMethod = "Fukami, T., Martijn Bezemer, T., Mortimer, S.R. and van der Putten, W.H. (2005), Species divergence and trait convergence in experimental plant community assembly. Ecology Letters, 8: 1283-1290. https://doi.org/10.1111/j.1461-0248.2005.00829.x") %>% 
  dplyr::select("eventID", "measurementID", "measurementType", "measurementValue", "measurementUnit",  "measurementMethod")


# V. Save files for DwC-A -------------------------------------------------

write.csv(event, file = here::here("data", "CLUE-exp1_event.csv"), row.names = FALSE)
write.csv(occurrence, file = here::here("data", "CLUE-exp1_occurrence.csv"), row.names = FALSE)
write.csv(measurementorfact, file = here::here("data", "CLUE-exp1_measurementOrFact.csv"), row.names = FALSE)
save(taxonInformation, file = here::here("data", "CLUE_taxonomicInformation.Rda"))

# VI. Write meta.xml file -------------------------------------------------

create_meta_xml(core = c("Event" = here::here("data", "CLUE-exp1_event.csv")),
                extensions = c("Occurrence" = here::here("data", "CLUE-exp1_occurrence.csv"),
                               "ExtendedMeasurementOrFact" = here::here("data", "CLUE-exp1_measurementOrFact.csv")),
                file = here::here("data", "CLUE-exp1_meta.xml"))


# VI. Remove data files (should not be public yet) ------------------------

file.remove(here::here("data", "CLUE-exp1_event.csv"))
file.remove(here::here("data", "CLUE-exp1_occurrence.csv"))
file.remove(here::here("data", "CLUE-exp1_measurementOrFact.csv"))
