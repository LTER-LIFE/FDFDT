# Function to retrieve a species' English common name from Wikidata

# Author: Stefan Vriend
# Created: 2024-10-24
# Last updated: 2024-10-25


# Load packages -----------------------------------------------------------

library(WikidataQueryServiceR)
#library(WikidataR)

# Get English common name through {WikidataQueryServiceR} -----------------

# Note: this code uses SPARQL (an RDF-query language)

# Arguments
# sci_name: Character specifying one or more scientific names

get_common_name <- function(sci_name) {
  
  # SPARQL query to select common name from Wikidata
  query <- paste0('
      SELECT
        ?item ?common_name
      WHERE {
        ?item wdt:P225', '"', sci_name, '"', ';
              wdt:P1843 ?common_name.

      FILTER(LANGMATCHES(LANG(?common_name), "en"))

      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }}
      ')
  
  # Send query to Wikidata query service and extract English common name
  common_name <- WikidataQueryServiceR::query_wikidata(sparql_query = query,
                                                       format = "smart") |> 
    purrr::map("common_name") |>
    purrr::as_vector() |> 
    stringr::str_to_sentence() |>
    unique()
  
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