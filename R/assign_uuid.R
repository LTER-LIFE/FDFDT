## Function to assign uuids and save them ####

# Author: Cherine Jantzen
# Created: 2024-03-01

library(uuid)
library(dplyr)
library(here)


# Function ----------------------------------------------------------------

# Arguments:
# - dataset: Character, specifying the name of the data set for which the uuid should be created or looked for

assign_uuid <- function(dataset){
  
  uuid_lookup <- read.csv(here::here("data", "uuid_lookup.csv"))
  
  if (nrow(uuid_lookup %>% dplyr::filter(dataset_name == dataset)) != 0){
    
    uuid <- uuid_lookup %>% dplyr::filter(dataset_name == dataset) %>% dplyr::pull("uuid")
    
    return(uuid)
    
  } else if (nrow(uuid_lookup %>% dplyr::filter(dataset_name == dataset)) == 0){
    
    new_uuid <- uuid::UUIDgenerate(use.time = FALSE)
    
    df <- tibble::tibble(dataset_name = dataset, uuid = new_uuid)
    
    dplyr::bind_rows(uuid_lookup, df) %>% 
      write.csv(file = here::here("data", "uuid_lookup.csv"), row.names = FALSE)
    
    return(new_uuid)
    
  }
  
}
