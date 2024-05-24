# Map CLUE field master database of plant experiment 1 to Darwin Core ####

# Author: Cherine Jantzen
# Created: 2024-03-25
# Last updated: 2024-05-24


# I. Preparation ----------------------------------------------------------

# load packages
library(dplyr)
library(tidyr)
library(taxize)
library(readxl)
library(stringr)

# read in data
plants <- readxl::read_xlsx(choose.files()) # "master database exp1.xlsx"

source("retrieve-taxonInformation-from-GBIF.R")

# II. Create event core ---------------------------------------------------

# create eventID
event <- plants %>% 
  dplyr::mutate(eventID = paste(year, treatment, block, pq, sep = "-"),
                language = "en",
                country = "Netherlands",
                countryCode = "NL",
                institutionID = "https://ror.org/01g25jp36",
                institutionCode = "NIOO",
                type = "Event",
                samplingProtocol = "?", #TODO
                sampleSizeValue = 1,
                sampleSizeUnit = "plot",
                decimalLongitude = "5.7519", 
                decimalLatitude = "52.0599", 
                geodeticDatum = "EPSG:4326", 
                verbatimLocality = "Mossel", 
                eventDate = year) %>%  #TODO which month? August/september?
  dplyr::select("eventID", "samplingProtocol", "sampleSizeValue",
                "sampleSizeUnit", "eventDate", "year", "country",
                "countryCode", "verbatimLocality", "decimalLatitude", "decimalLongitude",
                "geodeticDatum", "type", "language", "institutionID", "institutionCode")


# III. Create occurrence file ---------------------------------------------

# recode scientific names
sciNames <- tibble::tibble(taxonName = names(plants)[5:135]) %>% 
  dplyr::mutate(newName = dplyr::case_when(taxonName == "Apiaceaa" ~ "Apiaceae",
                                           taxonName == "Trifolium spp. (repens/pratense)" ~ "Trifolium",
                                           TRUE ~ taxonName),
                newName = stringr::str_remove_all(string = newName, pattern = " sp"),
                newName = stringr::str_replace(string = newName, pattern = "Aperaica-venti", replace = "Apera spica-venti"),
                newName = stringr::str_replace(string = newName, pattern = "Galeopsiseciosa", replace = "Galeopsis speciosa"), 
                newName = stringr::str_replace(string = newName, pattern = "Poaceaep.", replace = "Poaceae"),
                newName = stringr::str_replace(string = newName, pattern = "Graminae", replace = "Gramineae"),
                newName = stringr::str_replace(string = newName, pattern = "Sision amomum", replace = "Sison amomum"),
                newName = stringr::str_replace(string = newName, pattern = "Polygonum  hydropiper", replace = "Polygonum hydropiper"))


# call function to retrieve taxonomic information
taxonInformation <- get_taxonInformation(scientificNames = sciNames$newName,
                                         taxa_kingdom = "Plantae")
taxon_reorderd <- taxonInformation %>% 
  dplyr::arrange(canonicalname) %>% 
  dplyr::bind_cols(sciNames %>% 
                     dplyr::arrange(newName)) %>% 
  tidyr::separate_wider_delim(cols = species,  names = c("Genus", "specificEpithet"), delim = " ", cols_remove = FALSE)

occurrence <- plants %>% 
  dplyr::mutate(eventID = paste(year, treatment, block, pq, sep = "-")) %>% 
  tidyr::pivot_longer(cols = names(plants[5]):names(plants[length(plants)])) %>% 
  dplyr::mutate(occurrenceID = paste(eventID, 1:dplyr::n(), sep = "_"), .by = eventID,
                organismQuantityType = "individuals??",
                basisOfRecord = "humanObservation",
                occurrenceStatus = "present") %>% 
  dplyr::left_join(taxon_reorderd, by = c("name" = "taxonName")) %>%
  dplyr::rename("organismQuantity" = "value",
                "scientificName" = "scientificname") %>% 
  dplyr::select("eventID", "occurrenceID", "organismQuantity", 
                "organismQuantityType", "basisOfRecord", "occurrenceStatus",
                "scientificName", "kingdom", "phylum", "class", "order",
                "family", "genus", "specificEpithet")


# IV. Save files for DwC-A ------------------------------------------------

write.csv(event, file = "CLUE-plants-exp1_event.csv", row.names = FALSE)
write.csv(occurrence, file = "CLUE-plants-exp1_occurrence.csv", row.names = FALSE)
save(taxonInformation, file= "CLUE_taxonomicInformation.Rda")
# V. Write meta.xml file --------------------------------------------------

# source(here::here("R", "create-meta-xml-of-DwCA.R"))
# 
# create_meta_xml(core = c("Event" = here::here("data", "CLUE-plants-exp1_event.csv")),
#                 extensions = "Occurrence" = here::here("data", "CLUE-plants-exp1_occurrence.csv"),
#                 file = here::here("data", "CLUE-plants-exp1_meta.xml"))
