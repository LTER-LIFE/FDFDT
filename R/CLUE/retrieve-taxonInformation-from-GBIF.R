# Function to retrieve taxonomic information for a large number of species from GBIF ####

# Author: Cherine Jantzen
# Created: 2024-04-02
# Last updated: 2024-04-24


# I. Load packages --------------------------------------------------------

library(taxize)
library(dplyr)


# II. Function ------------------------------------------------------------

## Arguments

# - scientificNames: character vector containing all scientific names for which taxonomic information should be queried
# - kingdom: optional character specifying the kingdom the taxa belong to; useful if all taxon belong to same kingdom to avoid confusion with similar species names from another kingdom


get_taxonInformation <- function(scientificNames,
                                 taxa_kingdom = NA) {
  
  # retrieve taxonomic information from GBIF for all species
  taxInf_all <- taxize::get_gbifid_(sci = scientificNames) %>%   
    dplyr::bind_rows() 
  
  # filter for accepted terms with exact word match
  taxonInformation <- taxInf_all %>%
    dplyr::filter(status == "ACCEPTED", matchtype == "EXACT", kingdom == taxa_kingdom)
  
    # check whether there are scientific names still missing 
  missing <- scientificNames[!scientificNames %in% taxonInformation$canonicalname]
  
  
  if(length(missing) != 0) {
    
    # if there are missing taxa, take the synonyms with exact word match
    synonym_exact <-  taxInf_all %>% 
      dplyr::filter(canonicalname %in% missing, kingdom == taxa_kingdom, status == "SYNONYM", matchtype == "EXACT")
    
    taxonInformation <- dplyr::bind_rows(taxonInformation, synonym_exact)
    
    # check whether there are still taxa missing 
    remaining <- missing[!(missing %in% synonym_exact$canonicalname)]
    
    if(length(remaining) != 0) {
      
      # query only missing taxa from GBIF again (otherwise fuzzy matches are difficult to find)
      fuzzy <- taxize::get_gbifid_(sci = remaining) %>% 
        dplyr::bind_rows() %>% 
        dplyr::filter(kingdom == taxa_kingdom, status == "ACCEPTED"| status == "SYNONYM", matchtype == "FUZZY") 
      
      # check for accepted terms in fuzzy matches
      fuzzy_accepted <- fuzzy %>% 
        dplyr::filter(status == "ACCEPTED")  
      
      # check for synonym terms in fuzzy matches
      fuzzy_snyonym <- fuzzy %>% 
        dplyr::filter(!(canonicalname %in% fuzzy_accepted$canonicalname)) 
      
      taxonInformation <- dplyr::bind_rows(taxonInformation, fuzzy_accepted, fuzzy_snyonym)
      
    }
    
  }
  
  # some taxa come with different authorship information in the scientific name 
  # to resolve this, we find the scientific name most commonly used across taxonomies and use that
  
  # check for taxa with several entries
  checkAuthor <- taxonInformation %>% 
    dplyr::count(canonicalname) %>%  
    dplyr::filter(n > 1) %>% 
    dplyr::pull(canonicalname)
  
  # find names in global names resolver
  resolved_names <- taxize::gnr_resolve(sci = checkAuthor)
  
  # get the scientific names that are used most often
  verified_names <- resolved_names %>% 
    dplyr::count(matched_name) %>% 
    dplyr::left_join(resolved_names, by = "matched_name") %>% 
    dplyr::group_by(submitted_name) %>% 
    dplyr::filter(n == max(n)) %>% 
    dplyr::distinct(., matched_name, .keep_all = TRUE) %>% 
    dplyr::pull(matched_name)
  
  # filter taxon information for the verified names
  taxonInformation_verified <- taxonInformation %>% 
    dplyr::filter(scientificname %in% verified_names)
  
  # bind verified with remaining names 
  taxon_GBIF <- taxonInformation %>% 
    dplyr::filter(!canonicalname %in% taxonInformation_verified$canonicalname) %>% 
    dplyr::bind_rows(taxonInformation_verified)
  
  return(taxon_GBIF)
  
}

