# KNMI Open API ####

# Author: Cherine Jantzen
# Created: 2024/02/20
# Last updated: 2024/02/22


# I. Load packages --------------------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)
library(httr)
library(purrr)
library(jsonlite)
library(here)


# Function to retrieve netCDF files from KNMI Open API --------------------

# Arguments:
# - variable: Character providing the weather variable of interest. Either "mean temperature", "max temperature", "min temperature" or "precipitation".
# - start_date: Character providing the start date of period for which to retrieve data in the format: "yyyy-mm-dd".
# - end_date: Character providing the end date of period for which to retrieve data in the format: "yyyy-mm-dd". If empty, current year is used.
# - season_start: Vector specifying the start month and day of the period of the year for which data should be retrieved in the format: c(mm, dd), e.g., c(2, 10) for February 10. Allows to limit data retrieval to a selected period of the year. If empty, data for the full year is retrieved. 
# - season_end: Vector specifying the end month and day of the period of the year for which data should be retrieved in the format: c(mm, dd), e.g., c(5, 15) for May 15. If provided, season_start also needs to be provided.
# - version: Character specifying the version of the data set. (Version "5" for mean temperature and precipitation, version "2" for min and max temperature)
# - netCDF_location: Character specifying the path of the folder the downloaded netCDF files should be stored in, e.g., "data/netCDF files/"
# - knmi_open_key: Character specifying user-specific KNMI Open API key (request here: https://developer.dataplatform.knmi.nl/apis).


retrieve_knmi_open_data <- function(variable = c("mean temperature", "max temperature",
                                            "min temperature", "precipitation"),
                               start_year,
                               end_year = NULL,
                               season_start = NULL,
                               season_end = NULL,
                               version,
                               netCDF_location,
                               knmi_open_key = rstudioapi::askForSecret("Open_API_KEY")) {
  
  
  # Create KNMI variable lookup table to match variable inputs to KNMI collection & its parameters
  knmi_var_lookup <- tibble::tibble(
    collection = c("Tg1", "Tx1", "Tn1", "Rd1"),
    parameter = c("temperature", "temperature", "temperature", "precipitation"),
    var_name = c("mean temperature", "max temperature", "min temperature", "precipitation"),
    file_ending_before2018 = c("0005.nc", "0002.nc", "0002.nc", "0005.nc"),
    filename_timestamp = c("T000000", "T000000", "T000000", "T080000"),
    min_date_dataset = c("1961-01-01", "1961-01-01", "1961-01-01", "1951-01-01")
  )
  
  
  # check whether the selected variable exists
  if(!(variable %in% knmi_var_lookup$var_name)) {
    
    stop("The weather variable you provided does not exist. Select one of: 'mean temperature', 'max temperature', 'min temperature', or 'precipitation'.")
    
  }
  
  
  # check whether selected start date is before start date of the data set
  earliest_year <- knmi_var_lookup %>% dplyr::filter(var_name == variable) %>% dplyr::pull(min_date_dataset)
  
  if(start_year < lubridate::year(earliest_year)) {
    
    message(paste0("The selected start year is before data collection startet. Data for this variable can only be queried from ", earliest_year, " onwards."))
    
    stop()
    
  }
  
  # if end year is not provided, use current year
  if(rlang::is_empty(end_year)) {
    
    end_year <- Sys.Date() %>% substring(first = 1, last = 4)
    
  }
  
  # check whether season_start is provided when season_end is provided 
  if(rlang::is_empty(season_start) & !rlang::is_empty(season_end)) {
    
    stop("Please always provide season_start if season_end is provided.")
  }
  
  
  # restrict time to season for which data should be retrieved
  if(!rlang::is_empty(season_start)) {
    
    # account for seasons going across years
    lst_periods <- purrr::map(.x = start_year:end_year,
                              .f = ~{
                                # create start and end date
                                
                                ## account for seasons going across years
                                if(season_start[1] > season_end[1]) {
                                  
                                  start_date <- lubridate::make_date(.x - 1, season_start[1], season_start[2])
                                  
                                } else {
                                  ## for seasons that start and end in the same year 
                                  start_date <- lubridate::make_date(.x, season_start[1], season_start[2])
                                  
                                }
                                
                                end_date <- lubridate::make_date(.x, season_end[1], season_end[2])
                                
                                # create sequence of dates between start and end date
                                start_seq <- seq(start_date, end_date - 1, by = 'days') %>% as.character()
                                end_seq <- seq(start_date + 1, end_date, by = 'days') %>% as.character()
                                
                                return(tibble::lst(start_seq, end_seq))
                                
                              }
    ) %>% 
      purrr::list_c()
    
    
    # bind list elements for each year together
    start_date_seq <- lst_periods %>% purrr::keep(names(.) == "start_seq") %>% unlist()
    end_date_seq <- lst_periods %>% purrr::keep(names(.) == "end_seq") %>% unlist()
    
  } else {
    
    # Create dates for the full year
    
    ## Create start and end date  
    start_date <- lubridate::make_date(year = start_year, month = 1, day = 1)
    end_date <- lubridate::make_date(year = end_year, month = 12, day = 31)
    
    ## Get sequence of dates between start and end date
    start_date_seq <- seq(as.Date(start_date), as.Date(end_date) - 1, by = 'days') %>% as.character() 
    end_date_seq <- seq(as.Date(start_date) + 1, as.Date(end_date), by = 'days') %>% as.character()
    
  }
  
  
  # Create .nc file names per day
  file_names <- purrr::map_chr(.x = 1:length(start_date_seq),
                           .f = ~{
    
    names <- paste0("INTER_OPER_R___", 
                             knmi_var_lookup %>% 
                               dplyr::filter(var_name == variable) %>% 
                               dplyr::pull("collection") %>% 
                               toupper(), 
                             "_____L3__", 
                             stringr::str_split_1(start_date_seq[.x], pattern = "-")[1], 
                             stringr::str_split_1(start_date_seq[.x], pattern = "-")[2],
                             stringr::str_split_1(start_date_seq[.x], pattern = "-")[3],
                             knmi_var_lookup %>% 
                               dplyr::filter(var_name == variable) %>% 
                               dplyr::pull("filename_timestamp"), "_",
                             stringr::str_split_1(end_date_seq[.x], pattern = "-")[1], 
                             stringr::str_split_1(end_date_seq[.x], pattern = "-")[2],
                             stringr::str_split_1(end_date_seq[.x], pattern = "-")[3],
                             knmi_var_lookup %>% 
                               dplyr::filter(var_name == variable) %>% 
                               dplyr::pull("filename_timestamp"), "_",
                             
                             if(lubridate::as_date(start_date_seq[.x]) < lubridate::as_date("2018-01-01")) {
                               
                               knmi_var_lookup %>% 
                                 dplyr::filter(var_name == variable) %>% 
                                 dplyr::pull("file_ending_before2018")
                               
                             } else {
                               
                               "0006.nc"
                               
                             }
    )
  },
  .progress = TRUE) 
  

  
  # GET request KNMI Open API
  purrr::map(.x = file_names,
             .f = ~{
               
               knmi_api <- httr::GET(url = paste0("https://api.dataplatform.knmi.nl/open-data/v1/datasets/",
                                                  knmi_var_lookup %>%
                                                    dplyr::filter(var_name == variable) %>%
                                                    dplyr::pull("collection"),
                                                  "/versions/", version, "/files/",
                                                  .x, "/url"),
                                     httr::add_headers(Authorization = knmi_open_key)) 
               
               # print warning if GET request failes
               if(!rlang::is_empty(jsonlite::fromJSON(txt = rawToChar(x = knmi_api$content))$message)) {
                 message(paste0("API failed to retrieve data for the file name: ", .x))
                 }
               
               # dowload netCDF file per day
               download.file(url = jsonlite::fromJSON(rawToChar(knmi_api$content))$temporaryDownloadUrl,
                             destfile = paste0(netCDF_location, knmi_var_lookup %>% 
                                                 dplyr::filter(var_name == variable) %>% dplyr::pull("collection"), "_",
                                               stringr::str_sub(string = .x, start = 28, end = 58), ".nc"), 
                             mode = "wb", quiet = TRUE)
               
               
             },
             .progress = TRUE)
  
}


# III. Call function ------------------------------------------------------

call_knmi_open_api(variable = "min temperature",
                   start_year = "1999",
                   season_start = c(1, 1),
                   season_end = c(2, 22),
                   end_year = "2023",
                   version = "2",
                   netCDF_location = "data/netCDF files/") 
