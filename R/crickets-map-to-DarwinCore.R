# Map cricket data to Darwin Core ####

# Author: Cherine Jantzen
# Created: 07/02/2024
# Last updated: 08/02/2024


# I. Preparation ----------------------------------------------------------

# load packages
library(dplyr)
library(tidyr)

# load data
## plant chemical data
plant_chem <- read.table("plantchem.csv", header = TRUE, sep = ";")
crickets <- read.table("gryllus.txt", header = TRUE)

# meta files
meta_info_gryllus <- read.table("meta_info_gryllus.txt", header = TRUE) # file contains explanation of headers used in gryllus.txt
meta_info_plantchem <- read.table("meta_info_plantchem.txt", header = TRUE) # file contains explanation of headers used in plantchem.txt



# II. Mapping of plant data  ----------------------------------------------

# create eventID: event is measure of each plot within block 

df <- plant_chem %>% 
  tidyr::unite("Block","Treat", col = "eventID", sep = "_", remove = FALSE)

# create event table
d_event <- df %>%
  dplyr::select("eventID") %>% 
  dplyr::mutate(eventDate = 2014,
                year = 2014,
                language = "en",
                country = "Netherlands",
                countryCode = "NL",
                verbatimLocality = "Hoge Veluwe",
                samplingProtocol = "randomized complete block design (OBI:0500007)", 
                sampleSizeValue = 225, # what is the correct sample Size here? area of plots? no of plots? no of blocks?
                sampleSizeUnit = "m²")



# measurement or fact

measurements_plants <- df %>%
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
                                                   elements == "CPmol" ~ "Ratio (SIO:001018) of concentration (NCIT:C41185) of carbon (CHEBI:27594) and phosphorus (CHEBI:28659)",             
                                                   TRUE ~ NA), # delete??
                measurementID = NA,
                measurementMethod = "https://doi.org/10.3389/fevo.2021.659363")



# III. Mapping of cricket data --------------------------------------------

h1 <- tibble::tibble(Name = "W14d", Description = "Mass (PATO:0000125) after 14 days of abstinence")

meta_info_gryllus <- meta_info_gryllus %>%
  dplyr::bind_rows(h1) %>%
  dplyr::mutate(measurementType = dplyr::case_when(variable == "InitW" ~ "Initial mass (PATO:0000125) at eclosion (GO:0007562)",
                                                   variable == "W14d" ~ "Mass (PATO:0000125) after 14 days of abstinence",
                                                   variable == "Delta_W_init14" ~ "Weight change (NCIT:C9232) after 14 days of abstinence",
                                                   variable == "DeltaW_14d_repr_lifespan" ~ "Weight change (NCIT:C9232) during first 14 days of reproductive period",
                                                   variable == "DeltaW_repr_period" ~ "Weight change (NCIT:C9232) during complete reproductive period",
                                                   variable == "DeltaW_daily_repr_lifespan" ~ "reproductive life span (VT:0015036)",
                                                   variable == "Lime",
                                                   variable == "P",
                                                   variable == "Cannibal",
                                                   variable == "Early_death",
                                                   variable == "No_reprod",
                                                   variable == "DeltaW_adult_lifespan",
                                                   variable == "DeltaW_daily_adult_lifespan",
                                                   variable == "Adult_lifespan",
                                                   variable == "Reprd_lifespan",
                                                   variable == "Total_repr_suc",
                                                   variable == "Rep30",
                                                   variable == "batch1",
                                                   variable == "batch2",
                                                   variable == "batch3",
                                                   variable == "batch4",
                                                   variable == "batch5",
                                                   variable == "batch6",
                                                   variable == "batch7",
                                                   variable == "batch8",
                                                   variable == "batch9",
                                                   variable == "batch10",
                                                   variable == "batch11"),
                measurementUnit = dplyr::case_when(Name %in% c("InitW", "Delta_W_init14", "DeltaW_14d_repr_lifespan",
                                                               "DeltaW_repr_period", "DeltaW_daily_repr_lifespan",
                                                               "DeltaW_adult_lifespan") ~ "g",
                                                   Name %in% c("Adult_lifespan", "Reprd_lifespan") ~ "days", 
                                                   Name == "DeltaW_daily_adult_lifespan" ~ "g/day"),
                measurementID = NA,
                measurementMethod = "https://doi.org/10.3389/fevo.2021.659363")

# pivot for Boolean variables

measurements_boolean <- crickets %>% 
  dplyr::select("Femnum", "Treat", "Lime", "P", "Cannibal", "Early_death", "No_reprod") %>% 
  tidyr::pivot_longer(cols = Lime:No_reprod, names_to = "variable", values_to = "measurementValue")%>%
  dplyr::left_join(meta_info_gryllus %>%
                     dplyr::select(!"Description"), 
                   by = c("variable" = "Name")) 

measurements_crickets <- crickets %>% 
  dplyr::select(!c("Lime", "P", "Cannibal", "Early_death", "No_reprod")) %>%
  tidyr::pivot_longer(cols = W14d:batch11, names_to = "variable", values_to = "measurementValue") %>%
  dplyr::left_join(meta_info_gryllus %>%
                     dplyr::select(!"Description"), 
                   by = c("variable" = "Name")) %>%
  dplyr::mutate(measurementValue = as.character(measurementValue))


measurement_or_fact <- dplyr::bind_rows(measurements_boolean, measurements_crickets)


# get taxonomic information for Gryllus campestris
taxon_cricket <- taxize::get_gbifid_(sci = "Gryllus campestris") %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == "EXACT") %>%
  tidyr::separate(canonicalname, c("Genus", "specificEpithet"), remove = FALSE) %>%
  dplyr::select("scientificName" = "scientificname", "species" = "canonicalname",
                "kingdom", "phylum", "class", "order", "family", "genus", "specificEpithet") 
    