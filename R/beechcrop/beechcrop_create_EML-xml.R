# Pipeline for creation of EML metadata file of the DwC-A of beech crop data ####

# Authors:          Cherine Jantzen
# Created:          2024-02-28
# Last updated:     2026-02-18


# Load packages
library(emld)
library(EML)
library(xml2)
library(here)

# 1. Fill in metadata ----------------------------------------------------

# Title of the data set
title <- "Long-term seed production data of European beeches in the Hoge Veluwe, Netherlands"

# Information on the creator of the data set
creator <- list(organizationName = "Netherlands Institute of Ecology (NIOO-KNAW) - Department of Animal Ecology",
                positionName = "Databank manager",
                address = list(country = "NL",
                               city = "Wageningen"),
                electronicMailAddress = "AnE_Database@nioo.knaw.nl",
                onlineUrl = "https://nioo.knaw.nl/en/facilities/hole-breeding-passerines-monitoring-hoge-veluwe ")

# Information on the provider of the meta data
metadataProvider <- list(individualName = list(givenName = "Cherine",
                                               surName = "Jantzen"),
                         organizationName = "Netherlands Institute of Ecology (NIOO-KNAW)",
                         address = list(country = "NL",
                                        city = "Wageningen"),
                         electronicMailAddress = "C.Jantzen@nioo.knaw.nl")

# Information on the contact person
contact_person <- list(organizationName = "Netherlands Institute of Ecology (NIOO-KNAW) - Department of Animal Ecology",
                       address = list(country = "NL",
                                      city = "Wageningen"),
                       positionName = "Databank manager",
                       electronicMailAddress = "AnE_Database@nioo.knaw.nl")

# Language of the data
language <- "en"

# Abstract describing the data set
abstract <- list(para = "Seed production of European beech (Fagus sylvatica) is assessed yearly by the Netherlands Institute of Ecology (NIOO-KNAW) since 1976. The data contains individual-level information on beechnut production of a selected set of trees in the Nationalpark Hoge Veluwe, Netherlands, consisting of counts and weights.")

# List of keywords and the thesaurus they are listed in
keywords <- list(list(keyword = list("trees", "ecology", "seed production"),
                      keywordThesaurus = "envThes"),
                 list(keyword = list("beech", "beechnut", "masting", "Fagus sylvatica")))

# License for the work
licensed <- list(licenseName = "Creative Commons Attribution 4.0 International (CC BY 4.0)",
                 url = "https://creativecommons.org/licenses/by/4.0/")

# Geographic coverage of the data
geographic_coverage <- list(geographicDescription = "The Nationalpark Hoge Veluwe",
                            boundingCoordinates = list(westBoundingCoordinate = "5.824",
                                                       eastBoundingCoordinate = "5.870",
                                                       northBoundingCoordinate = "52.047",
                                                       southBoundingCoordinate = "52.032"))

# Temporal coverage of the data
temporal_coverage <- list(rangeOfDates = list(beginDate = list(calendarDate = "1976-09-10"),
                                              endDate = list(calendarDate = "2025-10-16")))

# Taxonomic coverage of the data
taxonomic_coverage <- list(generalTaxonomicCoverage = "Data is collected on European beech trees.",
                           taxonomicClassification = list(list(taxonRankName = "Species",
                                                               taxonRankValue = "Fagus sylvatica",
                                                               taxonId = "2882316",
                                                               commonName = "European beech")))

# Combine all three types of coverage
coverage <- list(geographicCoverage = geographic_coverage,
                 temporalCoverage = temporal_coverage,
                 taxonomicCoverage = taxonomic_coverage)

# Maintenance: frequency of updates
maintenance <- list(maintenanceUpdateFrequency = "unknown",
                    description = list(para = "Update frequency is unknown."))

# date of publication
publication_date <- "2026-02-18"


# Methods for data collection
methods <- list(methodStep = list(list(description = list(para = "A set of selected beech trees is visited each year in autumn.")),
                                  list(description = list(para = "Four grids of 25 by 25 cm are placed in the direction of either a cross carved into the bark of the tree or a coloured pin. The first grid is placed half a meter from the stem, the last grid prependicular to the end point of the farthest overhanging branch, and the other two grids in equal distances in between the first and last grid.")),
                                  list(description = list(para = "In each grid, all beechnuts are collected (from whole nuts to partial nuts) and packed, per grid, in labelled bags.")),
                                  list(description = list(para = "Within few days, nuts have to be analysed. From 2011 onwards, the nuts are dried (outside the bags) at room temperature for at least 24 hours before analysis. After drying, the nuts are sorted into six categories: whole nuts (shiny and firm; total number and gross weight in milligrams are determined), caterpillar nuts (have small round hole and, when opened, usually are full of caterpillar droppings; total number is counted), eaten nuts (hole in nut is not smooth but frayed and mainly located at the thick side of the nut or the corner; total number is counted), empty nuts (can easily be squashed with the fingers and are completely empty inside; total number is counted), rotten nuts (usually seem firm and whole, but are very light in weight and have smaller black nuts in the inside when opened; total number is counted) and other (nut that do not belong to any other category; total number is counted).")),
                                  list(description = list(para = "For whole nuts, the total gross weight of all whole nuts is taken. Additionally, whole nuts are individually weighted with peel (gross weight) and without peel (net weight) with a maximum of five nuts per sample.")),
                                  list(description = list(para = "For all details on the data collection and deviations from the general methods decribed here, please see Jantzen and Visser (submitted)."))))

# 2. Create the EML.xml file ----------------------------------------------

# Package uuid
source(here::here("R", "assign_uuid.R"))

packageId <- assign_uuid(dataset = "beechcrop")

# Combine all components in one list
eml <- list(dataset =
              list(title = title,
                   creator = creator,
                   metadataProvider = metadataProvider,
                   pubDate = publication_date,
                   language = language,
                   abstract = abstract,
                   keywordSet = keywords,
                   licensed = licensed,
                   coverage = coverage,
                   maintenance = maintenance,
                   contact = contact_person,
                   methods = methods),
            system = "uuid",
            packageId = packageId)

# Write EMl file
EML::write_eml(eml, file = here::here("data", "beechcrop_EML.xml"))


# 3. Add attributes for specific nodes ------------------------------------

# Read EML file as XML file
EML <- xml2::read_xml(here::here("data", "beechcrop_EML.xml"))

# Identify all taxonId nodes for which attribute shall be set
taxonId_node <- xml2::xml_find_all(EML, xpath = "//taxonId")

# Set "provider" attribute for taxonId nodes
xml2::xml_set_attr(taxonId_node, attr = "provider", value = "https://www.gbif.org/")

# Identify title node
title_node <- xml2::xml_find_all(EML, xpath = "//title")

# Set title attribute
xml2::xml_set_attr(title_node, attr = "xml:lang", value = "en")


# 4. Validate EML file ----------------------------------------------------
if(!emld::eml_validate(EML)) {
  
  stop("The generated EML is not schema-valid.")
  
}

# Write final EML file
xml2::write_xml(EML, file = here::here("data", "beechcrop_EML.xml"))


# Save file locally -------------------------------------------------------

# choose directory to store files
dir_loc <- rstudioapi::selectDirectory()

xml2::write_xml(EML, file = paste0(dir_loc, "/", "beechcrop_EML.xml"))
