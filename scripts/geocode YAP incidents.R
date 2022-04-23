library(tidygeocoder)
library(tidyverse)
library(sf)
library(rio)

shootingIncidents <- import("data/YAP_Incidents.xlsx", which = "shooting incident") %>%
  janitor::clean_names(.) %>%
  geocode(address = address,
          method = "arcgis") %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4269)

communityEvent <- import("data/YAP_Incidents.xlsx", which = "community event") %>%
  janitor::clean_names(.) %>%
  geocode(address = address,
          method = "arcgis") %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4269)

st_write(shootingIncidents, "data/YAP Shooting Incidents.geojson")
st_write(communityEvent, "data/YAP Community Events.geojson")

unique(shootingIncidents$neighborhood)
