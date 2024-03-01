# Create EML.xml file for cricket data set ####

# Author. Cherine Jantzen
# Created: 2024-02-09
# Last updated: 2024-03-01

# Load packages
library(emld)
library(EML)
library(ids)
library(keyring)
library(xml2)
library(here)
library(purrr)
library(dplyr)
library(httr)
library(jsonlite)

# 1. Fill in metadata ----------------------------------------------------
server <- "lifesciences.datastations.nl"
dataset_doi <- "doi:10.17026/dans-zsa-f3y9"
key <- rstudioapi::askForSecret("API key")

# retrieve metadata from Data Station
meta_fields <- httr::GET(url = paste0("https://", server, "/api/",
                                      "datasets/:persistentId?persistentId=", dataset_doi),
                         httr::add_headers("X-Dataverse-key" = key)) |>
  httr::content(as = "text", encoding = "UTF-8") |>
  jsonlite::fromJSON() |>
  purrr::pluck("data") |>
  purrr::pluck("latestVersion") |>
  purrr::pluck("metadataBlocks") |>
  purrr::pluck("citation") |>
  purrr::pluck("fields")

# Title of the data set
title <- list(list(value = meta_fields |> dplyr::filter(typeName == "title") |> purrr::pluck("value")),
              list(value = meta_fields |> dplyr::filter(typeName == "alternativeTitle") |> purrr::pluck("value")))

# Information on the creator of the data set
creator <- list(list(individualName = list(salutation = "drs.",
                                           givenName = "J.J.",
                                           surName = "Vogels"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userId = "327509856"), 
                list(individualName = list(salutation = "dr",
                                           givenName = "W.C.E.P.",
                                           surName = "Verberk"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userId = "265948576"),
                list(individualName = list(salutation = "drs.",
                                           givenName = "J.T.",
                                           surName = "Kuper"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userId = "274568349"), 
                list(individualName = list(salutation = "drs.",
                                           givenName = "M.J.",
                                           surName = "Weijters"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userId = "334082919"), 
                list(individualName = list(salutation = "dr",
                                           givenName = "R.",
                                           surName = "Bobbink"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userId = "075033682"),
                list(individualName = list(salutation = "prof. dr.",
                                           givenName = "H.",
                                           surName = "Siepel"),
                     organizationName = "Radboud University",
                     address = list(country = "NL",
                                    city = "Nijmegen"),
                     userId = "125440413"))

# Information on the contact person
contact_person <- list(organizationName = meta_fields |>
                         dplyr::filter(typeName == "datasetContact") |>
                         purrr::pluck("value") |>
                         dplyr::bind_rows() |>
                         purrr::pluck("datasetContactName") |>
                         purrr::pluck("value"),
                       address = list(country = "NL",
                                      city = "Nijmegen"))

# Metadata provider 
metadataProvider <- list(organizationName = "Radboud University",
                         address = list(country = "NL",
                                        city = "Nijmegen"))

# Language of the data
language <- "en"

# Abstract describing the data set
abstract <- list(para = "We tested whether increased P-limitation and/or acidification reduced food quality of plants after sod-cutting management by performing feeding experiments with the field cricket (Gryllus campestris), using food obtained from a field experiment in which we applied P and/or lime to sod-cut heathland. We show that increased plant N:P ratio following sod-cutting constrains the reproductive potential in Gryllus campestris. However, liming greatly reduced reproductive performance. Detailed analysis on elemental stoichiometry revealed skewed Fe:Mg and Mn:Mg ratio’s induced by liming, which diverge strongly from invertebrate ratios. The datafiles are part of a study that investigated the effects of methods aimed to restore P availability (reduced due to sod-cutting) and of reducing soil acidity (increased due to acid deposition) on heathland vegetation nutritional quality for invertebrates, using the field cricket Gryllus campestris (L.) as model species. This study was performed in the National Park the Hoge Veluwe (experimental plots) and the Radboud University Nijmegen (controlled feeding experiment) from April to July 2014. This dataset contains field cricket life history and reproduction measurements and plant tissue elemental content data results from the experiment. The full methodology can be found in the linked paper.")


# List of keywords and the thesaurus they are listed in
keywords <- list(keyword = list(meta_fields |>
                                  dplyr::filter(typeName == "keyword") |>
                                  purrr::pluck("value") |>
                                  dplyr::bind_rows() |>
                                  purrr::pluck("keywordValue") |>
                                  purrr::pluck("value")))

# License for the work
meta_license <- httr::GET(url = paste0("https://", server, "/api/",
                                  "datasets/:persistentId?persistentId=", dataset_doi),
                     httr::add_headers("X-Dataverse-key" = key)) |>
  httr::content(as = "text", encoding = "UTF-8") |>
  jsonlite::fromJSON() |>
  purrr::pluck("data") |>
  purrr::pluck("latestVersion") |>
  purrr::pluck("license")

licensed <- list(licenseName = meta_license$name,
                 url = meta_license$uri)

# intellectual rights
intellectualRights <- list(para = "Rights holder of this resource is Radboud University.") 

# Geographic coverage of the data
geographic_coverage <- list(geographicDescription = "Field experiment in Hoge Veluwe National Park and breeding experiments at Radboud University",
                            boundingCoordinates = list(westBoundingCoordinate = "5.8293598",
                                                       eastBoundingCoordinate = "5.8293598",
                                                       northBoundingCoordinate = "52.0423185",
                                                       southBoundingCoordinate = "52.0423185")) 

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

# Maintenance: frequency of updates
maintenance <- list(maintenanceUpdateFrequency = "unknown",
                    description = list(para = "As this dataset belongs to a publication, it is unknown whether the data is still further updated."))


# Methods for data collection
methods <- list(methodStep = list(description = list(para = "In a randomized block design treatment combinations of liming and phosphorous addition were applied to 20 plots which previously have undergone sod-cutting. The two dominant plant species have been cut and used as food for crickets in a breeding experiment. Individual female crickets were fed with plants of one treatment and their reproductive success measured. For details see the cited publication"),
                                  citation = list(bibtex = rcrossref::cr_cn("10.3389/fevo.2021.659363", format = "bibtex"))))


# III. Create EML file ----------------------------------------------------

# Package uuid
source(here::here("R", "assign_uuid.R"))

packageId <- assign_uuid(dataset = "crickets")


# Combine all components in one list
eml <- list(dataset =
              list(title = title,
                   creator = creator,
                   #pubDate = publication_date,
                   language = language,
                   abstract = abstract,
                   keywordSet = keywords,
                   licensed = licensed,
                   coverage = coverage,
                   contact = contact_person,
                   methods = methods,
                   maintenance = maintenance,
                   intellectualRights = intellectualRights),
            packageId = packageId, 
            system = "uuid")

# Write EML file
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

# Identify userId node
userId_node <- xml2::xml_find_all(EML, xpath = "//userId")

# Set directory attribute
xml2::xml_set_attr(userId_node, attr = "directory", value = "info:eu-repo/dai/nl/")


# 4. Validate EML file ----------------------------------------------------
if(!emld::eml_validate(EML)) {
  
  stop("The generated EML is not schema-valid.")
  
}

# Write final EML file
xml2::write_xml(EML, file = here::here("data", "cricket_EML.xml"))
