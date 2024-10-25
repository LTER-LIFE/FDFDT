# Function to retrieve a species' English common name from Wikidata

# Author: Stefan Vriend
# Created: 2024-10-24
# Last updated: 2024-10-25


# Load packages -----------------------------------------------------------

library(WikidataQueryServiceR)
library(WikidataR)


# Function ----------------------------------------------------------------

# Arguments
# sci_name: Character specifying one or more scientific names

get_common_name <- function(sci_name) {
  
  # Query common name from Wikidata
  query <- paste0('
      SELECT
        ?item ?common_name
      WHERE {
        ?item wdt:P225 ?scientific_name;
              wdt:P1843 ?common_name.

      FILTER(LANGMATCHES(LANG(?common_name), "en"))

      FILTER(lcase(str(?scientific_name)) IN (',
        '"', tolower(sci_name), '"',
      '))

      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }}
      ')
  
  common_name <- WikidataQueryServiceR::query_wikidata(sparql_query = query,
                                                       format = "smart") %>%
    dplyr::pull("common_name") |>
    stringr::str_to_sentence() |>
    unique()
  
  return(common_name)
  
}

# Alternative -------------------------------------------------------------

get_common_name2 <- function(sci_name) {
  
  # Query common name from Wikidata
  item <- WikidataR::find_item(sci_name) |> 
    purrr::flatten()
  
  wiki <- WikidataR::get_item(item$id)
  
  wiki[[1]]$claims$P1843 |> 
    purrr::pluck("mainsnak", "datavalue", "value") |> 
    dplyr::filter(language == "en") |> 
    dplyr::pull(text) |> 
    stringr::str_to_sentence() |>
    unique()
  
}
