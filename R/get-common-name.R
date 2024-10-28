# Function to retrieve a species' English common name from Wikidata

# Author: Stefan Vriend
# Created: 2024-10-24
# Last updated: 2024-10-28


# Load packages -----------------------------------------------------------

library(WikidataQueryServiceR)
library(dplyr)
library(stringr)
#library(WikidataR)

# Get common name through {WikidataQueryServiceR} -----------------

# Note: this code uses SPARQL (an RDF-query language)

# Arguments
# sci_name: Character specifying one or more scientific names
# lang: Character specifying one or more languages the common name

get_common_name <- function(sci_name,
                            lang = "en") {
  
  # SPARQL query to select common name from Wikidata
  query <- paste0('
      SELECT DISTINCT
        ?item ?common_name (LANG(?common_name) AS ?lang)
      WHERE {
        ?item wdt:P225', '"', sci_name, '"', ';
              wdt:P1843 ?common_name.

      FILTER(LANGMATCHES(LANG(?common_name), ', '"', lang, '"))

      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }}
      ')
  
  # Send query to Wikidata query service and extract common name
  common_name <- WikidataQueryServiceR::query_wikidata(sparql_query = query,
                                                       format = "smart") |> 
    dplyr::bind_rows() |> 
    dplyr::mutate(common_name = stringr::str_to_title(common_name)) |> 
    dplyr::distinct()
  
  return(common_name)
  
}


# (alternative) Get English common name through {WikidataR} ---------------

# Arguments
# sci_name: Character specifying one or more scientific names

# get_common_name2 <- function(sci_name) {
#   
#   purrr::map_chr(.x = sci_name,
#                  .f = ~{
#                    
#                    # Search Wikidata item corresponding to scientific name
#                    item <- WikidataR::find_item(.x) |> 
#                      purrr::flatten()
#                    
#                    # Retrieve properties from Wikidata item
#                    wiki <- WikidataR::get_item(item$id)
#                    
#                    # Extract English common name
#                    wiki[[1]]$claims$P1843 |> 
#                      purrr::pluck("mainsnak", "datavalue", "value") |> 
#                      dplyr::filter(language == "en") |> 
#                      dplyr::pull("text") |> 
#                      purrr::as_vector() |> 
#                      stringr::str_to_sentence() |>
#                      unique()
#                    
#                  })
#   
# }