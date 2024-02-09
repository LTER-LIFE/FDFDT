# Create EML.xml file for cricket data set ####

# Author. Cherine Jantzen
# Created: 2024-02-09

# Load packages
library(emld)
library(EML)
library(ids)
library(keyring)
library(tidyverse)
library(here)

# 1. Fill in metadata ----------------------------------------------------

# Title of the data set
title <- "Data from: How to restore invertebrate diversity of degraded heathlands?"

# Information on the creator of the data set
creator <- list(list(individualName = list(salutation = "drs.",
                                           givenName = "J.J.",
                                           surName = "Vogels"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userID = "327509856"), 
                list(individualName = list(salutation = "dr",
                                           givenName = "W.C.E.P.",
                                           surName = "Verberk"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userID = "265948576"),
                list(individualName = list(salutation = "drs.",
                                           givenName = "J.T.",
                                           surName = "Kuper"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userID = "274568349"), 
                list(individualName = list(salutation = "drs.",
                                           givenName = "M.J.",
                                           surName = "Weijters"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userID = "334082919"), 
                list(individualName = list(salutation = "dr",
                                           givenName = "R.",
                                           surName = "Bobbink"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userID = "075033682"),
                list(individualName = list(salutation = "prof. dr.",
                                           givenName = "H.",
                                           surName = "Siepel"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userID = "125440413"))

# Information on the contact person
contact_person <- list(organizationName = "Radboud University",
                       address = list(country = "NL",
                                      city = "Nijmegen"))

# Date of publication of the data set
publication_date <- "2021-05-04" # FIXME has to be our publishing date, right?

# Language of the data
language <- "en"

# Abstract describing the data set
abstract <- list(para = "We tested whether increased P-limitation and/or acidification reduced food quality of plants after sod-cutting management by performing feeding experiments with the field cricket (Gryllus campestris), using food obtained from a field experiment in which we applied P and/or lime to sod-cut heathland. We show that increased plant N:P ratio following sod-cutting constrains the reproductive potential in Gryllus campestris. However, liming greatly reduced reproductive performance. Detailed analysis on elemental stoichiometry revealed skewed Fe:Mg and Mn:Mg ratio’s induced by liming, which diverge strongly from invertebrate ratios. The datafiles are part of a study that investigated the effects of methods aimed to restore P availability (reduced due to sod-cutting) and of reducing soil acidity (increased due to acid deposition) on heathland vegetation nutritional quality for invertebrates, using the field cricket Gryllus campestris (L.) as model species. This study was performed in the National Park the Hoge Veluwe (experimental plots) and the Radboud University Nijmegen (controlled feeding experiment) from April to July 2014. This dataset contains field cricket life history and reproduction measurements and plant tissue elemental content data results from the experiment. The full methodology can be found in the linked paper.")


# List of keywords and the thesaurus they are listed in
keywords <- list(keyword = list("ecological stoichiometry","acidification", "management", "nitrogen deposition", "elemental ecology", "invertebrate diversity", "insect decline", "sod-cutting"),
                 keywordThesaurus = "???") # TODO Find thesaurus for keywords

# License for the work
licensed <- list(licenseName = "Creative Commons Attribution 4.0 International (CC BY 4.0)",
                 url = "https://creativecommons.org/licenses/by/4.0/")

# intellectual rights
# FIXME what about rightsHolder and accesRights?? what EML equivalent can be used here? 

# Geographic coverage of the data
geographic_coverage <- list(geographicDescription = "Field experiment in Hoge Veluwe National Park and breeding experiments at Radboud University",
                            boundingCoordinates = list(eastBoundingCoordinate = "5.829359788293456",
                                                       northBoundingCoordinate = "52.04231849823671"))

# Temporal coverage of the data
temporal_coverage <- list(rangeOfDates = list(beginDate = list(calendarDate = "2014-04-01"),
                                              endDate = list(calendarDate = "2014-07-01")))

# Taxonomic coverage of the data
taxonomic_coverage <- list(generalTaxonomicCoverage = "For the feeding experiment the two dominant plant species (common heather and purple moor-grass) were harvested and the breeding experiment was conducted on the European field cricket",
                           taxonomicClassification = list(list(taxonRankName = "Species",
                                                               taxonRankValue = "Molinia caerulea",
                                                               taxonId = "5289877",
                                                               commonName = "Purple moor-grass"),
                                                          list(taxonRankName = "Species",
                                                               taxonRankValue = "Calluna vulgaris",
                                                               taxonId = "2882482",
                                                               commonName = "Common heather"),
                                                          list(taxonRankName = "Species",
                                                               taxonRankValue = "Gryllus campestris",
                                                               taxonId = "1716462",
                                                               commonName = "European field cricket")))
# Combine all three types of coverage
coverage <- list(geographicCoverage = geographic_coverage,
                 temporalCoverage = temporal_coverage,
                 taxonomicCoverage = taxonomic_coverage)

# Maintenance??

# TODO Methods for data collection
# how detailed do wen want that to be?


# III. Create EML file ----------------------------------------------------

# Fetch existing UUID or create new UUID
packageId <- dplyr::if_else(condition = keyring::key_list() |>
                              dplyr::filter(username == "Cricket EML packageId") |> nrow() > 0,
                            rstudioapi::askForSecret("Cricket EML packageId"),
                            ids::uuid(n = 1, drop_hyphens = FALSE, use_time = FALSE)) # FIXME creating a new one doesn't work for whatever reason

# Combine all components in one list
eml <- list(dataset =
              list(title = title,
                   creator = creator,
                   pubDate = publication_date,
                   language = language,
                   abstract = abstract,
                   keywordSet = keywords,
                   licensed = licensed,
                   coverage = coverage,
                   contact = contact_person),
            system = "uuid")

# Write EMl file
EML::write_eml(eml, file = here::here("data", "cricket_EML.xml"))          

# 3. Add attributes for specific nodes ------------------------------------

# Read EML file as XML file
EML <- xml2::read_xml(here::here("data", "cricket_EML.xml"))

# Identify all taxonId nodes for which attribute shall be set
taxonId_node <- xml2::xml_find_all(EML, xpath = "//taxonId")

# Set "provider" attribute for taxonId nodes
xml2::xml_set_attr(taxonId_node, attr = "provider", value = "https://www.gbif.org/")

# Identify title node
title_node <- xml2::xml_find_all(EML, xpath = "//title")

# Set title attribute
xml2::xml_set_attr(title_node, attr = "xml:lang", value = "en")

# Identify userID node
userID_node <- xml2::xml_find_all(EML, xpath = "//userID")

# Set title attribute
xml2::xml_set_attr(userID_node, attr = "directory", value = "info:eu-repo/dai/nl/")

# 4. Validate EML file ----------------------------------------------------
if(!emld::eml_validate(EML)) {
  
  stop("The generated EML is not schema-valid.")
  
} # FIXME not yet schema valid -> additionally, it would be better to also give the errors if invalid

# Write final EML file
xml2::write_xml(EML, file = here::here("data", "cricket_EML.xml"))

