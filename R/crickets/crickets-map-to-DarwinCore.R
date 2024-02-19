# Map cricket data to Darwin Core ####

# Author: Cherine Jantzen
# Created: 2024-02-07
# Last updated: 2024-02-19


# I. Preparation ----------------------------------------------------------

# load packages
library(dplyr)
library(tidyr)
library(here)

source(here::here("R", "retrieveData-API-Dataverse.R"))

# Retrieve data from Data station

dataverse_list <- retrieve_dataverse_data(dataset = "doi:10.17026/dans-zsa-f3y9",
                                          server = "lifesciences.datastations.nl")


purrr::map2(.x = c("gryllus", "meta_info_gryllus", "meta_info_plantchem", "plantchem"),
            .y = dataverse_list[names(dataverse_list) %in% c("gryllus", "meta_info_gryllus", "meta_info_plantchem", "plantchem")],
            .f = ~{
              
              assign(.x, .y, envir = .GlobalEnv)
              
            })


# II. Mapping of plant data  ----------------------------------------------

## create eventID: event is measure per plot (within one block) 
plants <- plantchem %>% 
  dplyr::mutate(eventID = paste0(paste0(substring(Block, 1, 2), substring(Block, 4, 4)), Treat))


# create event table
event_plants <- plants %>%
  dplyr::select("eventID") %>% 
  dplyr::mutate(verbatimLocality = "Hoge Veluwe",
                samplingProtocol = "randomized complete block design (OBI:0500007)", 
                sampleSizeValue = 225, 
                sampleSizeUnit = "square metre",
                type = "Event",
                decimalLongitude = 5.829359788293456,
                decimalLatitude = 52.04231849823671,
                geodeticDatum = "EPSG:4326")

# create occurrence file 
# get taxonomic information for Gryllus campestris & Tracheophyta
taxon_info_plant <- taxize::get_gbifid_(sci = "Tracheophyta") %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == "EXACT") %>%
  dplyr::select("scientificName" = "scientificname", "kingdom", "phylum") 

occurrence_plants <- event_plants %>% 
  dplyr::select("eventID") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, 1, sep = "_"),
                phylum = "Tracheophyta",
                individualCount = NA, 
                basisOfRecord = "HumanObservation",
                occurrenceStatus = "present",
                taxonRank = "phylum") %>% 
  dplyr::left_join(taxon_info_plant, by = "phylum") 

# measurement or fact
MOF_plants <- plants %>%
  tidyr::pivot_longer(cols = P:NPmol, names_to = "variable", values_to = "measurementValue") %>%
  dplyr::mutate(measurementUnit = dplyr::case_when(variable %in% c("Pper","Cper","Nper") ~ "percent (UO:0000187)",
                                                   variable %in% c("CNper", "CPper", "NPper", "CNmol", "CPmol", "NPmol", "P", "Dol") ~ NA,
                                                   TRUE ~ "micromole/gram (SNOMED:258816005) dry weight"),
                measurementType = dplyr::case_when(variable == "P" ~ "Phosphorus treatment (AGRO:00000322)",
                                                   variable == "Dol" ~ "Liming process (AGRO:00000112) treatment (AGRO:00000322)",
                                                   variable == "Al" ~ "Aluminium (CHEBI:28984) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",
                                                   variable == "Ca" ~ "Calcium (CHEBI:22984) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",
                                                   variable == "Fe" ~ "Iron (CHEBI:18248) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",
                                                   variable == "K" ~ "Potassium (CHEBI:26216) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)", 
                                                   variable == "Mg" ~ "Magnesium (CHEBI:25107) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   variable == "Mn" ~ "Manganese (CHEBI:18291) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   variable == "S" ~ "Sulfur (CHEBI:26833) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   variable == "Si" ~ "Silicon (CHEBI:27573) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   variable == "Zn" ~ "Zinc (CHEBI:27363) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   variable == "Nmol" ~ "Nitrogen (CHEBI:25555) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                             
                                                   variable == "Cmol" ~ "Carbon (CHEBI:27594) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                             
                                                   variable == "Pmol" ~ "Phosphorus (CHEBI:28659) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                             
                                                   variable == "Nper" ~ "Percentage (SIO:001413) of nitrogen (CHEBI:25555) in plant tissue (NCIT:C18945)",                             
                                                   variable == "Cper" ~ "Percentage (SIO:001413) of carbon (CHEBI:27594) in plant tissue (NCIT:C18945)",                             
                                                   variable == "Pper" ~ "Percentage (SIO:001413) of phosphorus (CHEBI:28659) in plant tissue (NCIT:C18945)",                             
                                                   variable == "CNper" ~ "Ratio (SIO:001018) of percentage (SIO:001413) of carbon (CHEBI:27594) and nitrogen (CHEBI:25555)",               
                                                   variable == "CPper" ~ "Ratio (SIO:001018) of percentage (SIO:001413) of carbon (CHEBI:27594) and phosphorus (CHEBI:28659)",             
                                                   variable == "NPper" ~ "Ratio (SIO:001018) of percentage (SIO:001413) of nitrogen (CHEBI:25555) and phosphorus (CHEBI:28659)",             
                                                   variable == "NPmol" ~ "Ratio (SIO:001018) of concentration (NCIT:C41185) of nitrogen (CHEBI:25555) and phosphorus (CHEBI:28659)",        
                                                   variable == "CNmol" ~ "Ratio (SIO:001018) of concentration (NCIT:C41185) of carbon (CHEBI:27594) and nitrogen (CHEBI:25555)",               
                                                   variable == "CPmol" ~ "Ratio (SIO:001018) of concentration (NCIT:C41185) of carbon (CHEBI:27594) and phosphorus (CHEBI:28659)"), 
                measurementMethod = dplyr::case_when(variable %in% c("Al", "Ca", "Fe", "K", "Zn", "S", "Si", "Mg", "Mn", "Pmol") ~ "inductively coupled plasma-atomic emission spectrometry (MMO:0000231)",
                                                     variable %in% c("Cmol", "Nmol") ~ "elemental CNS analyzer",
                                                     TRUE ~ NA),
                measurementRemarks = dplyr::case_when(variable == "CNper" ~ "Calculated as percentage of carbon divided by percentage of nitrogen",               
                                                      variable == "CPper" ~ "Calculated as percentage of carbon divided by percentage of phosphorus",             
                                                      variable == "NPper" ~ "Calculated as percentage of nitrogen divided by percentage of phosphorus",             
                                                      variable == "NPmol" ~ "Calculated as molar concentration of nitrogen divided by molar concentration of phosphorus",        
                                                      variable == "CNmol" ~ "Calculated as molar concentration of carbon divided by molar concentration of nitrogen",               
                                                      variable == "CPmol" ~ "Calculated as molar concentration of carbon divided by molar concentration of phosphorus",
                                                      TRUE ~ NA)) %>%
  dplyr::select(!c("Block", "Treat", "variable")) %>%
  dplyr::left_join(occurrence_plants %>%
                     dplyr::select("eventID", "occurrenceID"),
                   by = "eventID") %>% 
  dplyr::mutate(measurementID = paste(occurrenceID, 1:n(), sep = "_"), .by = "eventID") 


# III. Mapping of cricket data --------------------------------------------

# prepare input for each variable
measurement_info_cricket <- meta_info_gryllus %>%
  # add missing variables & its description
  dplyr::bind_rows(tibble::tibble(Name = "W14d", Description = "Mass (PATO:0000125) after 14 days of abstinence")) %>%
  # group measurements into event groups
  dplyr::mutate(eventGroup = dplyr::case_when(Name == "InitW" ~ "01",
                                              Name %in% c("W14d", "Delta_W_init14") ~ "02",
                                              Name == "DeltaW_14d_repr_lifespan" ~ "03",
                                              Name == "Rep30" ~ "04",
                                              Name %in% c("DeltaW_adult_lifespan", "DeltaW_daily_adult_lifespan", "Adult_lifespan",
                                                          "Reprd_lifespan", "DeltaW_repr_period", "DeltaW_daily_repr_lifespan",
                                                          "No_reprod", "Cannibal") ~ "05",
                                              Name == "batch1" ~ "06",
                                              Name == "batch2" ~ "07",
                                              Name == "batch3" ~ "08",
                                              Name == "batch4" ~ "09",
                                              Name == "batch5" ~ "10",
                                              Name == "batch6" ~ "11",
                                              Name == "batch7" ~ "12",
                                              Name == "batch8" ~ "13",
                                              Name == "batch9" ~ "14",
                                              Name == "batch10" ~ "15",
                                              Name %in% c("Total_repr_suc", "batch11") ~ "16",
                                              Name == "Early_death" ~ "17",
                                              Name %in% c("P", "Lime") ~ "18"),
                measurementType = dplyr::case_when(Name == "InitW" ~ "Initial mass (PATO:0000125) at eclosion (GO:0007562)",
                                                   Name == "W14d" ~ "Mass (PATO:0000125) after 14 days of abstinence",
                                                   Name == "Delta_W_init14" ~ "Weight change (NCIT:C9232) after 14 days of abstinence",
                                                   Name == "DeltaW_14d_repr_lifespan" ~ "Weight change (NCIT:C9232) during first 14 days of reproductive life span (VT:0015036)",
                                                   Name == "DeltaW_repr_period" ~ "Weight change (NCIT:C9232) during complete reproductive life span (VT:0015036)",
                                                   Name == "DeltaW_daily_repr_lifespan" ~ "Daily (NCIT:C25473) weight change (NCIT:C9232) during reproductive life span (VT:0015036)",
                                                   Name == "Lime" ~ "Liming process (AGRO:00000112) treatment (AGRO:00000322)",
                                                   Name == "P" ~ "Phosphorus (CHEBI:28659) treatment (AGRO:00000322)",
                                                   Name == "Cannibal" ~ "Sexual cannibalism (NBO:0020101)",
                                                   Name == "Early_death" ~ "Death (GSSO:004103) within first 14 days of egg laying (NCIT:C82474) period",
                                                   Name == "No_reprod" ~ "Infertile (GSSO:007162) female",
                                                   Name == "DeltaW_adult_lifespan" ~ "Weight change (NCIT:C9232) during adult (EFO:0001272) lifespan (APO:0000030)",
                                                   Name == "DeltaW_daily_adult_lifespan" ~ "Daily (NCIT:C25473) weight change (NCIT:C9232) during adult (EFO:0001272) lifespan (APO:0000030)",
                                                   Name == "Adult_lifespan" ~ "Adult (EFO:0001272) lifespan (APO:0000030)",
                                                   Name == "Reprd_lifespan" ~ "Reproductive life span (VT:0015036)",
                                                   Name == "Total_repr_suc" ~ "Total reproductive success (GSSO:000912) (total number of hatched eggs)",
                                                   Name == "Rep30" ~ "Reproductive success (GSSO:000912) in first 30 days of experiment (SIO:000994)",
                                                   Name == "batch1" ~ "Number of hatched eggs in batch 1",
                                                   Name == "batch2" ~ "Number of hatched eggs in batch 2",
                                                   Name == "batch3" ~ "Number of hatched eggs in batch 3",
                                                   Name == "batch4" ~ "Number of hatched eggs in batch 4",
                                                   Name == "batch5" ~ "Number of hatched eggs in batch 5",
                                                   Name == "batch6" ~ "Number of hatched eggs in batch 6",
                                                   Name == "batch7" ~ "Number of hatched eggs in batch 7",
                                                   Name == "batch8" ~ "Number of hatched eggs in batch 8",
                                                   Name == "batch9" ~ "Number of hatched eggs in batch 9",
                                                   Name == "batch10" ~ "Number of hatched eggs in batch 10",
                                                   Name == "batch11" ~ "Number of hatched eggs in batch 11"),
                measurementUnit = dplyr::case_when(Name %in% c("W14d", "InitW", "Delta_W_init14", "DeltaW_14d_repr_lifespan",
                                                               "DeltaW_repr_period", "DeltaW_adult_lifespan") ~ "g",
                                                   Name %in% c("Adult_lifespan", "Reprd_lifespan") ~ "days", 
                                                   Name %in% c("DeltaW_daily_adult_lifespan", "DeltaW_daily_repr_lifespan") ~ "g/day",
                                                   Name %in% c("Total_repr_suc", "Rep30", "batch1", "batch2", "batch3", "batch4", "batch5",
                                                               "batch6", "batch7", "batch8", "batch9", "batch10", "batch11") ~ "eggs"),
                measurementMethod = "https://doi.org/10.3389/fevo.2021.659363")

# create measurement or fact file
measurements_crickets <- gryllus %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  tidyr::pivot_longer(cols = Lime:batch11, names_to = "variable", values_to = "measurementValue") %>% 
  dplyr::left_join(measurement_info_cricket %>%
                     dplyr::select(!"Description"), 
                   by = c("variable" = "Name")) %>% 
  dplyr::mutate(eventID = paste0("Cr", Femnum, "-", eventGroup))

# create event file
event_cricket <- measurements_crickets %>%
  dplyr::distinct(., eventID, .keep_all = TRUE) %>% 
  dplyr::mutate(verbatimLocality = "Radboud University",
                samplingProtocol = "https://doi.org/10.3389/fevo.2021.659363", 
                sampleSizeValue = 1, 
                sampleSizeUnit = "individual",
                type = "Event") %>% 
  dplyr::rename(organismID = Femnum)

# get taxonomic information for Gryllus campestris & Tracheophyta
taxon_info_cricket <- taxize::get_gbifid_(sci = "Gryllus campestris") %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == "EXACT") %>%
  tidyr::separate(canonicalname, c("Genus", "specificEpithet"), remove = FALSE) %>%
  dplyr::select("scientificName" = "scientificname", "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet") 

# create occurrence file
occurrence_crickets <- event_cricket %>% 
  dplyr::select("eventID", "organismID") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, 1, sep = "_"),
                individualCount = 1,
                basisOfRecord = "HumanObservation", 
                occurrenceStatus = "present",
                specificEpithet = "campestris",
                taxonRank = "species") %>% 
  dplyr::left_join(taxon_info_cricket, by = "specificEpithet")

# add measurementID and organism ID
MOF_crickets <- measurements_crickets %>% 
  left_join(occurrence_crickets %>% 
              dplyr::select("eventID", "occurrenceID"),
            by = "eventID") %>%
  dplyr::mutate(measurementID = paste(occurrenceID, 1:n(), sep = "_"), .by = "eventID") %>%
  dplyr::select("measurementID", "eventID", "measurementType", "measurementValue", "measurementUnit", "measurementMethod")

# IV. combine files for plants and crickets into final DwC-A files  ------------

event <- dplyr::bind_rows(event_plants, event_cricket) %>% 
  dplyr::mutate(eventDate = "2014-04-01/2014-07-01",
                year = 2014,
                language = "en",
                country = "Netherlands",
                countryCode = "NL",
                institutionCode = "RU Radboud University",
                institutionID = "https://ror.org/016xsfp80",
                bibliographicCitation = "J.J. Vogels; W.C.E.P. Verberk; J.T. Kuper; M.J. Weijters; R. Bobbink; H. Siepel, 2021, 'Data from: How to restore invertebrate diversity of degraded heathlands?', https://doi.org/10.17026/dans-zsa-f3y9, DANS Data Station Life Sciences, V2") %>% 
  dplyr::select("eventID", "samplingProtocol", "sampleSizeValue", "sampleSizeUnit", "eventDate", 
                "year", "country", "countryCode", "decimalLatitude","decimalLongitude","geodeticDatum", "verbatimLocality", "type", 
                "language", "bibliographicCitation", "institutionID", "institutionCode")

measurement_or_fact <- dplyr::bind_rows(MOF_plants, MOF_crickets) %>% 
  dplyr::mutate(measurementValue = stringr::str_replace(string = measurementValue, pattern = "NO", replacement = "no"),
                measurementValue = stringr::str_replace(string = measurementValue, pattern = "YES", replacement = "yes")) %>% 
  dplyr::select("measurementID", "eventID", "measurementType", "measurementValue", "measurementUnit", "measurementMethod", "measurementRemarks")

occurrence <- dplyr::bind_rows(occurrence_crickets, occurrence_plants) %>% 
  dplyr::select("eventID", "occurrenceID", "individualCount", "basisOfRecord", "occurrenceStatus", "organismID", "scientificName", "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet", "taxonRank")


# V. Save DwC-A files -----------------------------------------------------

write.csv(measurement_or_fact, file = here::here("data", "crickets_extendedmeasurementorfact.csv"), row.names = FALSE)
write.csv(event, file = here::here("data", "crickets_event.csv"), row.names = FALSE)
write.csv(occurrence, file = here::here("data", "crickets_occurrence.csv"), row.names = FALSE)


# VI. Create meta.xml for cricket DwC-A -------------------------------------

# fetch functions to create meta.xml file from according script and create meta.xml file for cricket DwC-A
source(here::here("R", "create-meta-xml-of-DwCA.R"))

create_meta_xml(core = c("Event" = here::here("data", "crickets_event.csv")),
                extensions = c("ExtendedMeasurementOrFact" = here::here("data", "crickets_extendedmeasurementorfact.csv"),
                               "Occurrence" = here::here("data", "crickets_occurrence.csv")),
                file = here::here("data", "crickets_meta.xml"))
