# Retrieve data from SQL database of AnE-NIOO ####

# Author: Cherine Jantzen
# Date: 08/11/2023
# Last updated: 29/02/2024

# load packages
library(dplyr)
library(dbplyr)
library(DBI)
library(odbc)
library(rstudioapi)


# connect to NIOO AnE SQL server to retrieve data
con <- DBI::dbConnect(drv = odbc::odbc(),
                      Driver = {'ODBC Driver 17 for SQL Server'},
                      UID = rstudioapi::askForPassword("Uid"),
                      Pwd = rstudioapi::askForPassword("Pwd"),
                      Server = rstudioapi::askForPassword("Server"))


## tbl_BeechSample
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_BeechCrop", schema = "dbo", table ="tbl_BeechSample")) %>% 
  dplyr::collect() -> d_sample

## tbl_NameSite
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_BeechCrop", schema = "dbo", table = "tbl_NameSite")) %>% 
  dplyr::collect() -> d_site

## tbl_WeightPerNut
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_BeechCrop", schema = "dbo", table = "tbl_WeightPerNut")) %>% 
  dplyr::collect() -> d_weight

# ## tbl_Position
# dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_BeechCrop", schema = "dbo", table = "tbl_Position")) %>% 
#   dplyr::collect() -> d_position

## tbl_Observer
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_BeechCrop", schema = "dbo", table = "tbl_Observer")) %>% 
  dplyr::collect() -> d_observer


## tbl_Tree
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Tree")) %>%
  dplyr::collect() -> d_tree

## tbl_TreeSpecies
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_TreeSpecies")) %>%
  dplyr::collect() -> d_species

##tbl_Area
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Area")) %>%
  dplyr::collect() -> d_area

