# EML file for CLUE field data: Vegetation cover of experiment 1 ####

# Author: Cherine Jantzen
# Created: 2024-04-19
# Last updated: 2024-05-24

# I. Preparation ----------------------------------------------------------

# Load packages
library(emld)
library(EML)
library(ids)
library(xml2)
library(here)

# load taxonomic Information of the data from R object
load(here::here("data", "CLUE_taxonomicInformation.Rda"))

# load function to assign package uuid
source(here::here("R", "assign_uuid.R"))

# II. Fill in metadata ----------------------------------------------------

# Title of the data set
title <- "CLUE field data - Vegetation cover under 4 different treatments - Terrestrial Ecology/NIOO-KNAW" # FIXME other title?

# Information on the creator of the data set
creator <- list(organizationName = "Netherlands Institute of Ecology (NIOO-KNAW)",
                individualName = list(givenName = "Wim H.",
                                      surName = "van der Putten"),
                positionName = "head of department",
                address = list(country = "NL",
                               city = "Wageningen"),
                electronicMailAddress = "W.vanderPutten@nioo.knaw.nl",
                userID = "0000-0002-9341-4442")

# Information on the provider of the meta data
metadataProvider <- list(individualName = list(givenName = "Cherine C.",
                                               surName = "Jantzen"),
                         organizationName = "Netherlands Institute of Ecology (NIOO-KNAW)",
                         address = list(country = "NL",
                                        city = "Wageningen"),
                         electronicMailAddress = "C.Jantzen@nioo.knaw.nl",
                         userId = "0009-0006-0723-2682")

# Information on the contact person
contact_person <- list(organizationName = "Netherlands Institute of Ecology (NIOO-KNAW)",
                       individualName = list(givenName = "Ciska",
                                             surName = "Veen"),
                       address = list(country = "NL",
                                      city = "Wageningen"),
                       electronicMailAddress = "c.veen@nioo.knaw.nl",
                       onlineUrl = "https://nioo.knaw.nl/en/facilities/clue-field-veluwe-database",
                       userId = "0000-0001-7736-9998")

# Language of the data
language <- "en"

# Abstract describing the data set
abstract <- list(para = "The data was collected as part of an long-term grassland biodiversity experiment in which plant community composition is studied under different treatments. In a randomized-block design with 5 blocks, each consisting of 4 plots, 4 different treatments were randomly assigned and different plant species were sown.") # TODO 


# List of keywords and the thesaurus they are listed in
keywords <- list(list(keyword = list("primary succession", "grassland", "vegetation", "nematodes", "biodiversity", "ecosystem function", "secondary succession", "plant biomass", "productivity", "plant diversity"),
                      keywordThesaurus = "envThes"),
                 list(keyword = list("long-term experiment", "soil biodiversity"),
                      keywordThesaurus = "GEMET"),
                 list(keyword = list("soil community", "soil inoculation", "ragwort", "low diversity", "high diversity", "Jacobaea vulgaris")))

# License for the work
licensed <- list(licenseName = "Creative Commons Attribution 4.0 International",
                 url = "https://creativecommons.org/licenses/by/4.0/",
                 identifier = "CC-BY-4.0")

# Geographic coverage of the data
geographic_coverage <- list(geographicDescription = "Mossel, Netherlands",
                            boundingCoordinates = list(westBoundingCoordinate = "5.751",
                                                       eastBoundingCoordinate = "5.752",
                                                       northBoundingCoordinate = "52.060",
                                                       southBoundingCoordinate = "52.059"))

# Temporal coverage of the data
temporal_coverage <- list(rangeOfDates = list(beginDate = list(calendarDate = "1996"),
                                              endDate = list(calendarDate = "2016")))

# Taxonomic coverage of the data
# count species per family
speciesPerFamily <- taxonInformation %>% dplyr::count(family)

# get family names
family_names <- speciesPerFamily  %>%  dplyr::pull(family)

# retrieve GBIF ID for each family
family_IDs <- taxize::get_gbifid(sci = family_names) %>% 
  dplyr::bind_cols(family_names, .) %>% 
  dplyr::rename("family" = "...1",
                "GBIF_ID" = "...2")

# create list for all families and their IDs for EML term "taxonomicClassification"
taxonomicClassification <- purrr::map(.x = 1:25,
                                      .f = ~{
                                        
                                        taxonList <- list(taxonRankName = "Family",
                                                          taxonRankValue = family_IDs$family[.x],
                                                          taxonId = family_IDs$GBIF_ID[.x])
                                      })


# create description on how many species per family are included in the data fpr EML term "generalTaxonomicCoverage"
taxCoverage <- purrr::map_chr(.x = 1:24, 
                              .f = ~{
                                
                                first24_families <- paste(speciesPerFamily$n[.x], "species of the family", paste0(speciesPerFamily$family[.x], ","))
                                
                              }) %>% 
  paste(., collapse = " ") %>% 
  paste(., "and", speciesPerFamily$n[25], "species of the family", paste0(speciesPerFamily$family[25], "."))

# combine both terms into list for EML term "taxonomicCoverage"
taxonomic_coverage <- list(generalTaxonomicCoverage = paste0("Data covers 131 plant species coming from 25 families. It includes ", taxCoverage),
                           taxonomicClassification = taxonomicClassification)


# Combine all three types of coverage
coverage <- list(geographicCoverage = geographic_coverage,
                 temporalCoverage = temporal_coverage,
                 taxonomicCoverage = taxonomic_coverage)

# Maintenance: frequency of updates
maintenance <- list(maintenanceUpdateFrequency = "annually",
                    description = list(para = "Data is updated annually."))


# Methods for data collection
methods <- list(methodStep = list(list(description = list(para = "Starting in 1996, a former agricultural field was transformed into an experimental field site and fenced to exclude large mammalian herbivores.")),
                                  list(description = list(para = "A randomized block design with 5 blocks each containing 4 plots of 10 m x 10 m was established and 4 different treatments assigned: 1) high diversity sowing, where each replicate was treated with the same seed mixture of mid- to high-secondary succession species of the local species pool; 2) low diversity sowing, where each replicate was treated with a different subset of the high diversity sowing mixture; 3) a natural colonization control and 4) a continued agricultural control.")),
                                  list(description = list(para = "Until 2004, all plants were allowed natural colonisation without weeding and aboveground biomass was mowed and removed every year from all plots and border rows in September to October.")),
                                  list(description = list(para = "In each plot 12 permanent subplots of 1m x 1m were established in which vegetation cover was assessed every year in July at peak standing biomass. The per cent cover of each plant species was estimated by using approximate midpoints of six classes (i.e. < 1% estimated as 1%, < 5% as 2%, < 10% as5%, < 25% as 15%, < 50% as 25% and > 50% as 50%) until 2001. From 2002 onwards, percent cover was measured directly.")), 
                                  list(description = list(para = "See also Fukami et al., 2005 - DOI: 10.1111/j.1461-0248.2005.00829.xÓ2005"))))


# 2. Create the EML.xml file ----------------------------------------------

# Package uuid
packageId <- assign_uuid(dataset = "CLUE_exp1")


# Combine all components in one list
eml <- list(dataset =
              list(title = title,
                   creator = creator,
                   metadataProvider = metadataProvider,
                   #pubDate = publication_date,
                   language = language,
                   abstract = abstract,
                   keywordSet = keywords,
                   licensed = licensed,
                   coverage = coverage,
                   maintenance = maintenance,
                   contact = contact_person,
                   methods = methods),
            system = "uuid",
            packageId = packageId
)

# Write EMl file
EML::write_eml(eml, file = here::here("data", "CLUE-exp1_EML.xml"))


# 3. Add attributes for specific nodes ------------------------------------

# Read EML file as XML file
EML <- xml2::read_xml(here::here("data", "CLUE-exp1_EML.xml"))

# Identify all taxonId nodes for which attribute shall be set
taxonId_node <- xml2::xml_find_all(EML, xpath = "//taxonId")

# Set "provider" attribute for taxonId nodes
xml2::xml_set_attr(taxonId_node, attr = "provider", value = "https://www.gbif.org/")

# Identify title node
title_node <- xml2::xml_find_all(EML, xpath = "//title")

# Set title attribute
xml2::xml_set_attr(title_node, attr = "xml:lang", value = "en")

# Identify userId node
userId_node <- xml2::xml_find_all(EML, xpath = "//userId")

# Set directory attribute
xml2::xml_set_attr(userId_node, attr = "directory", value = "https://orcid.org/")

# 4. Validate EML file ----------------------------------------------------

if(!emld::eml_validate(EML)) {
  
  stop("The generated EML is not schema-valid.")
  
}

# Write final EML file
xml2::write_xml(EML, file = here::here("data", "CLUE-exp1_EML.xml"))
