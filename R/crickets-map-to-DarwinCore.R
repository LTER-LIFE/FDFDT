# Map cricket data to Darwin Core ####

# Author: Cherine Jantzen
# Created: 2024-02-07
# Last updated: 2024-02-09


# I. Preparation ----------------------------------------------------------

# load packages
library(dplyr)
library(tidyr)
library(here)

# load data
## plant chemical data
plant_chem <- read.table(here::here("data", "plantchem.csv"), header = TRUE, sep = ";")
crickets <- read.table(here::here("data", "gryllus.txt"), header = TRUE)

## info files with columns descriptions
meta_info_gryllus <- read.table(here::here("data", "meta_info_gryllus.txt"), header = TRUE) 
meta_info_plantchem <- read.table(here::here("data", "meta_info_plantchem.txt"), header = TRUE) 


# II. Mapping of plant data  ----------------------------------------------

## create eventID: event is measure per plot (within one block) 
df <- plant_chem %>% 
    dplyr::mutate(eventID = paste("plant", paste0(substring(Block, 1, 2), substring(Block, 4, 4)),
                                  substring(Treat, 1, 2), substring(Treat, 3, 5), sep = "_"))

# create event table
event_plants <- df %>%
  dplyr::select("eventID") %>% 
  dplyr::mutate(verbatimLocality = "Hoge Veluwe",
                samplingProtocol = "randomized complete block design (OBI:0500007)", 
                sampleSizeValue = 225, 
                sampleSizeUnit = "square metre",
                type = "Event") # is that correct? 



# measurement or fact
MOF_plants <- df %>%
  dplyr::relocate(c("Nper", "Cper", "Pper"), .after = "Pmol") %>%
  tidyr::pivot_longer(cols = Al:NPmol, names_to = "elements", values_to = "measurementValue") %>%
  dplyr::mutate(measurementUnit = dplyr::case_when(elements %in% c("Pper","Cper","Nper") ~ "percent (UO:0000187)",
                                                   elements %in% c("CNper", "CPper", "NPper") ~ "?",
                                                   elements %in% c("CNmol", "CPmol", "NPmol") ~ "?",
                                                   TRUE ~ "micromole/gram (SNOMED:258816005) dry weight"),
                measurementType = dplyr::case_when(elements == "Al" ~ "Aluminium (CHEBI:28984) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",
                                                   elements == "Ca" ~ "Calcium (CHEBI:22984) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",
                                                   elements == "Fe" ~ "Iron (CHEBI:18248) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",
                                                   elements == "K" ~ "Potassium (CHEBI:26216) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   elements == "Mg" ~ "Magnesium (CHEBI:25107) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   elements == "Mn" ~ "Manganese (CHEBI:18291) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   elements == "S" ~ "Sulfur (CHEBI:26833) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   elements == "Si" ~ "Silicon (CHEBI:27573) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   elements == "Zn" ~ "Zinc (CHEBI:27363) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                                    
                                                   elements == "Nmol" ~ "Nitrogen (CHEBI:25555) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                             
                                                   elements == "Cmol" ~ "Carbon (CHEBI:27594) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                             
                                                   elements == "Pmol" ~ "Phosphorus (CHEBI:28659) concentration (NCIT:C41185) of plant tissue (NCIT:C18945)",                             
                                                   elements == "Nper" ~ "Percentage (SIO:001413) of nitrogen (CHEBI:25555) in plant tissue (NCIT:C18945)",                             
                                                   elements == "Cper" ~ "Percentage (SIO:001413) of carbon (CHEBI:27594) in plant tissue (NCIT:C18945)",                             
                                                   elements == "Pper" ~ "Percentage (SIO:001413) of phosphorus (CHEBI:28659) in plant tissue (NCIT:C18945)",                             
                                                   elements == "CNper" ~ "Ratio (SIO:001018) of percentage (SIO:001413) of carbon (CHEBI:27594) and nitrogen (CHEBI:25555)",               
                                                   elements == "CPper" ~ "Ratio (SIO:001018) of percentage (SIO:001413) of carbon (CHEBI:27594) and phosphorus (CHEBI:28659)",             
                                                   elements == "NPper" ~ "Ratio (SIO:001018) of percentage (SIO:001413) of nitrogen (CHEBI:25555) and phosphorus (CHEBI:28659)",             
                                                   elements == "NPmol" ~ "Ratio (SIO:001018) of concentration (NCIT:C41185) of nitrogen (CHEBI:25555) and phosphorus (CHEBI:28659)",        
                                                   elements == "CNmol" ~ "Ratio (SIO:001018) of concentration (NCIT:C41185) of carbon (CHEBI:27594) and nitrogen (CHEBI:25555)",               
                                                   elements == "CPmol" ~ "Ratio (SIO:001018) of concentration (NCIT:C41185) of carbon (CHEBI:27594) and phosphorus (CHEBI:28659)"), 
                measurementMethod = dplyr::case_when(elements %in% c("Al", "Ca", "Fe", "K", "Zn", "S", "Si", "Mg", "Mn", "Pmol") ~ "inductively coupled plasma-atomic emission spectrometry (MMO:0000231)",
                                                     elements %in% c("Cmol", "Nmol") ~ "elemental CNS analyzer")) %>%
  dplyr::select(!c("Block", "Treat", "elements")) %>%
  dplyr::group_by(eventID) %>% 
  dplyr::mutate(measurementID = paste(eventID, 1:n(), sep = "_"),
                measurementValue = as.character(measurementValue))

# create occurrence file 
# get taxonomic information for Gryllus campestris & Tracheophyta
taxon_info_plant <- taxize::get_gbifid_(sci = "Tracheophyta") %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == "EXACT") %>%
  dplyr::select("scientificName" = "scientificname", "kingdom", "phylum") %>% 
  dplyr::mutate("class" = NA,
                "order" = NA,
                "family" = NA, 
                "genus" = NA, 
                "specificEpithet" = NA)

occurrence_plants <- event_plants %>% 
  dplyr::select("eventID") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, 1, sep = "_"),
                phylum = "Tracheophyta",
                individualCount = 1, # not really true??
                basisOfRecord = "HumanObservation",
                occurrenceStatus = "present",
                occurrenceRemarks = NA) %>% 
  dplyr::left_join(taxon_info_plant, by = "phylum")

# III. Mapping of cricket data --------------------------------------------
# Add missing variable & description
h1 <- tibble::tibble(Name = "W14d", Description = "Mass (PATO:0000125) after 14 days of abstinence")

# prepare input for each variable
measurement_info_cricket <- meta_info_gryllus %>%
  dplyr::bind_rows(h1) %>%
  dplyr::mutate(eventGroup = dplyr::case_when(Name == "InitW" ~ "A",
                                              Name %in% c("W14d", "Delta_W_init14") ~ "B",
                                              Name == "DeltaW_14d_repr_lifespan" ~ "C",
                                              Name == "Rep30" ~ "D",
                                              Name %in% c("DeltaW_adult_lifespan", "DeltaW_daily_adult_lifespan", "Adult_lifespan",
                                                          "Reprd_lifespan", "DeltaW_repr_period", "DeltaW_daily_repr_lifespan",
                                                          "No_reprod", "Cannibal") ~ "E",
                                              Name == "batch1" ~ "F",
                                              Name == "batch2" ~ "G",
                                              Name == "batch3" ~ "H",
                                              Name == "batch4" ~ "I",
                                              Name == "batch5" ~ "J",
                                              Name == "batch6" ~ "K",
                                              Name == "batch7" ~ "L",
                                              Name == "batch8" ~ "M",
                                              Name == "batch9" ~ "N",
                                              Name == "batch10" ~ "O",
                                              Name %in% c("Total_repr_suc", "batch11") ~ "P",
                                              Name == "Early_death" ~ "Q"), # TODO add group for treatments
                measurementType = dplyr::case_when(Name == "InitW" ~ "Initial mass (PATO:0000125) at eclosion (GO:0007562)",
                                                   Name == "W14d" ~ "Mass (PATO:0000125) after 14 days of abstinence",
                                                   Name == "Delta_W_init14" ~ "Weight change (NCIT:C9232) after 14 days of abstinence",
                                                   Name == "DeltaW_14d_repr_lifespan" ~ "Weight change (NCIT:C9232) during first 14 days of reproductive period",
                                                   Name == "DeltaW_repr_period" ~ "Weight change (NCIT:C9232) during complete reproductive period",
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
                                                   Name == "Total_repr_suc" ~ "Total reproductive success (GSSO:000912)",
                                                   Name == "Rep30" ~ "Reproductive success (GSSO:000912) in first 30 days of experiment",
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
                measurementUnit = dplyr::case_when(Name %in% c("InitW", "Delta_W_init14", "DeltaW_14d_repr_lifespan",
                                                               "DeltaW_repr_period", "DeltaW_adult_lifespan") ~ "g",
                                                   Name %in% c("Adult_lifespan", "Reprd_lifespan") ~ "days", 
                                                   Name %in% c("DeltaW_daily_adult_lifespan", "DeltaW_daily_repr_lifespan") ~ "g/day"),
                measurementID = NA,
                measurementMethod = "https://doi.org/10.3389/fevo.2021.659363")

# create measurement or fact file
MOF_crickets <- crickets %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  tidyr::pivot_longer(cols = Lime:batch11, names_to = "variable", values_to = "measurementValue") %>% 
  dplyr::left_join(measurement_info_cricket %>%
                     dplyr::select(!"Description"), 
                   by = c("variable" = "Name")) %>% 
  dplyr::mutate(eventID = paste("cricket", paste0("F", Femnum), substring(Treat, 1, 2), substring(Treat, 3, 5), eventGroup, sep = "_")) %>%
  dplyr::group_by(eventID) %>%
  dplyr::mutate(measurementID = paste(eventID, 1:n(), sep = "_")) %>%
  dplyr::ungroup() %>% 
  dplyr::select("measurementID", "eventID", "measurementType", "measurementValue",
                "measurementUnit", "measurementMethod")

# create event file
event_cricket <- MOF_crickets %>%
  dplyr::distinct(., eventID, .keep_all = FALSE) %>% 
  dplyr::mutate(verbatimLocality = "Radboud University",
                samplingProtocol = "randomized complete block design (OBI:0500007)", 
                sampleSizeValue = 1, # what is the correct sample Size here? 
                sampleSizeUnit = "individual",
                type = "Event")

# get taxonomic information for Gryllus campestris & Tracheophyta
taxon_info_cricket <- taxize::get_gbifid_(sci = "Gryllus campestris") %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == "EXACT") %>%
  tidyr::separate(canonicalname, c("Genus", "specificEpithet"), remove = FALSE) %>%
  dplyr::select("scientificName" = "scientificname", "species" = "canonicalname", # TODO no species & canonical Name
                "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet") 

# create occurrence file
occurrence_crickets <- event_cricket %>% 
  dplyr::select("eventID") %>% 
  dplyr::mutate(occurrenceID = paste(eventID, 1, sep = "_"),
                specificEpithet = "campestris",
                individualCount = 1,
                basisOfRecord = "HumanObservation", 
                occurrenceStatus = "present",
                organismID = NA) %>% # TODO add organismID
  dplyr::left_join(taxon_info_cricket, by = "specificEpithet")

# IV. combine files for plants and crickets into final DwC-A files  ------------

event <- dplyr::bind_rows(event_plants, event_cricket) %>% 
  dplyr::mutate(eventDate = "2014-04-01/2014-07-01",
                year = 2014,
                language = "en",
                country = "Netherlands",
                countryCode = "NL",
                institutionCode = "RU Radboud University",
                institutionID = "https://ror.org/016xsfp80",
                bibliographicCitation = "J.J. Vogels; W.C.E.P. Verberk; J.T. Kuper; M.J. Weijters; R. Bobbink; H. Siepel, 2021, 'Data from: How to restore invertebrate diversity of degraded heathlands?', https://doi.org/10.17026/dans-zsa-f3y9, DANS Data Station Life Sciences, V2")

measurement_or_fact <- dplyr::bind_rows(MOF_plants, MOF_crickets)    

occurrence <- dplyr::bind_rows(occurrence_crickets, occurrence_plants)


# V. Save DwC-A files -----------------------------------------------------

#write.csv(measurement_or_fact, file = here::here("data", "crickets_extendedmeasurementorfact.csv"), row.names = FALSE)
#write.csv(event, file = here::here("data", "crickets_event.csv"), row.names = FALSE)
#write.csv(occurrence, file = here::here("data", "crickets_occurrence.csv), row.names = FALSE)