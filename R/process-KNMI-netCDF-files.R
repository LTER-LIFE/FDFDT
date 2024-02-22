# Function to process individual netCDF files ####

# Author: Cherine Jantzen
# Created: 2024-02-21
# Last updated: 2024-02-22

# I. Load packages --------------------------------------------------------

library(tidync)
library(ncmeta)
library(RNetCDF)
library(dplyr)
library(purrr)
library(tidyr)
library(here)

# II. Function ------------------------------------------------------------

# Arguments: 
# - bbox: Spatial bounding box for which to retrieve data. Vector of four numeric values, indicating western-most, southern-most, eastern-most and northern-most point of the bounding box (in Amersfoort/RD New (ESPG:28992)).
# - netCDF_location: Character specifying the folder name in which the netCDF files are stored. Folder needs to be a sub-folder of the working directory. 

process_netCDF <- function(bbox,
                           netCDF_location){
  
  
  # set longitude and latitude range according to boundng box
  lonrange <- c(bbox[1], bbox[3])
  latrange <- c(bbox[2], bbox[4])
  
  # get all available netCDF files in the specified directory
  files <- list.files(here::here(netCDF_location), pattern = ".nc")
  
  # loop thorugh netCDF files to format them 
  climate_variable <- purrr::map(.x = files,
                            .f = ~{
                              
                              # connect to netCDF source
                              df <- tidync::tidync(here::here(netCDF_location, .x))
                              
                              # extract time information
                              time_ex <- df %>% 
                                tidync::activate("D2") %>% 
                                tidync::hyper_array()
                              
                              # extract time unit from metadata of netCDF
                              tunit <- ncmeta::nc_atts(here::here(netCDF_location, .x), "time") %>% 
                                tidyr::unnest(cols = c(value)) %>% 
                                dplyr::filter(name == "units")
                              
                              # convert extracted time information into R date
                              time_parts <- RNetCDF::utcal.nc(tunit$value, time_ex$time)
                              
                              date <- 
                                ISOdatetime(time_parts[,"year"], 
                                            time_parts[,"month"], 
                                            time_parts[,"day"], 
                                            time_parts[,"hour"], 
                                            time_parts[,"minute"], 
                                            time_parts[,"second"],
                                            tz = "Europe/Amsterdam")
                              
                              # extract time unit from metadata of netCDF
                              variable_name <- ncmeta::nc_atts(here::here(netCDF_location, .x), "prediction") %>% 
                                filter(name == "long_name") %>% 
                                unlist() %>% 
                                purrr::pluck("value.long_name")
                              
                              # slice data to the selected bounding box
                              fx_slice <- 
                                df %>% 
                                tidync::hyper_filter(x = dplyr::between(x, lonrange[1], lonrange[2]), 
                                                     y = dplyr::between(y, latrange[1], latrange[2])) %>% 
                                tidync::hyper_tibble()
                              
                              # add date information to tibble and select only desired output columns
                              sliced_data <-
                                fx_slice %>%
                                dplyr::mutate(date = date,
                                              variable = variable_name) %>%
                                dplyr::rename(value = 'prediction',
                                              longitude = 'x',
                                              latitude = 'y') %>%
                                dplyr::select(!c('stations', 'stationvalues', 'time'))
                              
                            }, 
                            .progress = TRUE
  ) %>% 
    purrr::list_c() %>% 
    dplyr::mutate(date = as.Date(date, tz = "Europe/Amsterdam"))
  
  return(climate_variable)
}


# III. Use function to process netCDF for Hoge Veluwe ---------------------

test_data <- process_netCDF(bbox = c(185003.9, 449430.1, 188144.4, 451036.0),
                            netCDF_location = "data/netCDF files")

