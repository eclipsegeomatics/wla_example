#=======================================================================
#Wildlife Area Intersection Example Script
#Script for intersecting and calculating length of railways within wildlife areas
#Created by Ekaterina Daviel (Eclipse Geomatics Ltd.), November 2022
#Uses scripts published by the Province of BC:
#BC Data: https://bcgov.github.io/bcdata/articles/bcdata.html
#=======================================================================

#Load required libraries
library(devtools)
library(tidyverse)
library(sf)
library(bcdata)
library(mapview)
library(dplyr)
library(units)
library(flextable)

#This package needs to be installed after loading tidyverse
#install_github("bcgov/rems")

# Load input data ---

#Connect to Wildlife Habitat Areas - Approved dataset on BC Data Catalogue
wildlife_areas_metadata <- bcdc_get_record("b19ff409-ef71-4476-924e-b3bcf26a0127") #this is the ID for the named FWA watersheds layer in the Data BC catalog
#print(wildlife-habitat-areas-approved_metadata)

wildlife_areas <- bcdc_query_geodata(wildlife_areas_metadata) %>%
  collect()

#Connect to Railway Track Line dataset on BC Data Catalogue
railways_metadata <- bcdc_get_record("4ff93cda-9f58-4055-a372-98c22d04a9f8") #this is the ID for the Public layer in the Data BC catalog

#Download layer from DataBC
railways <- bcdc_query_geodata(railways_metadata) %>%
  select() %>% 
  collect()

# Intersection of Wildlife Areas and Railways ---

railway_int <- st_intersection(railways, wildlife_areas)

# Calculate length of railway line within each wildlife area ---

rail_length_per_wildlife_area <- railway_int %>%
  mutate(length=drop_units(st_length(.)/1000)) %>% ##length in km
  as.data.frame() %>%
  group_by(HABITAT_AREA_ID) %>%
  summarize(rail_length=sum(length))

# Display results ---

rail_length_per_wildlife_area %>%
  arrange(-rail_length) %>%
  flextable(col_keys=c("HABITAT_AREA_ID", "rail_length")) %>%
  colformat_double(j=c("rail_length"), digits=2) %>%
  colformat_num(j = c("HABITAT_AREA_ID"), big.mark="") %>%
  set_header_labels(HABITAT_AREA_ID="Habitat Area ID", rail_length="Railway Length (km)") %>%
  width(j=1,width=2) %>%
  width(j=2,width=2) %>%
  theme_box()  %>%
  align(align = "center", part="header")

