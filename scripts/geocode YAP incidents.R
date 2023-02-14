library(tidygeocoder)
library(tidyverse)
library(sf)
library(rio)
library(lubridate)

datalib <- "C:/Users/micha/CPAL Dropbox/" #michael laptop

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

focusareas <- st_read("data/CRED_FocusAreas.geojson") %>%
  st_transform(crs = 4269) %>%
  select(-Shape_Leng, -Shape_Area)

shootingEnrich <- shootingIncidents %>%
  st_join(., focusareas) %>%
  mutate(InOut = ifelse(is.na(NAME), "OUT", "IN"),
         RespTime = time_length(response_date-incident_date, unit = "days"))

communityEnrich <- communityEvent %>%
  st_join(., focusareas) %>%
  mutate(InOut = ifelse(is.na(NAME), "OUT", "IN"))

##### Generate Tables of Metrics
# Total Incidents by In/Out
inAll <- shootingEnrich %>%
  st_drop_geometry(.) %>%
  group_by(InOut) %>%
  summarise(TotAll = n()) %>%
  mutate(PerAll = TotAll/sum(TotAll))

in2Wks <- shootingEnrich %>%
  st_drop_geometry(.) %>%
  filter(response_date >= max(response_date)-weeks(2)) %>%
  group_by(InOut) %>%
  summarise(Tot2Wks = n()) %>%
  mutate(Per2Wks = Tot2Wks/sum(Tot2Wks))

inOutTbl <- full_join(inAll, in2Wks)

# Total Incidents by In/Out and Neighborhood
ngdAll <- shootingEnrich %>%
  st_drop_geometry(.) %>%
  group_by(InOut, NAME) %>%
  summarise(TotAll = n()) %>%
  mutate(PerAll = TotAll/sum(TotAll))

ngd2Wks <- shootingEnrich %>%
  st_drop_geometry(.) %>%
  filter(response_date >= max(response_date)-weeks(2)) %>%
  group_by(InOut, NAME) %>%
  summarise(Tot2Wks = n()) %>%
  ungroup(.) %>%
  mutate(Per2Wks = Tot2Wks/sum(Tot2Wks))

ngTbl <- full_join(ngdAll, ngd2Wks)

# Community Events
cmyAll <- communityEnrich %>%
  st_drop_geometry(.) %>%
  group_by(InOut, NAME) %>%
  summarise(TotAll = n()) %>%
  ungroup(.) %>%
  mutate(PerAll = TotAll/sum(TotAll))

cmy2Wks <- communityEnrich %>%
  st_drop_geometry(.) %>%
  filter(date >= max(date)-weeks(2)) %>%
  group_by(InOut, NAME) %>%
  summarise(Tot2Wks = n()) %>%
  ungroup(.) %>%
  mutate(Per2Wks = Tot2Wks/sum(Tot2Wks))

cmyTbl <- full_join(cmyAll, cmy2Wks)

rio::export(cmyTbl, "data/YAP Incident Summaries.xlsx", which = "Community Event Summary")
rio::export(ngTbl, "data/YAP Incident Summaries.xlsx", which = "Incident Responses Summary")
rio::export(inOutTbl, "data/YAP Incident Summaries.xlsx", which = "Focus Area Summary")

rio::export(cmyTbl, paste0(datalib, "Safe Surroundings/02_Data/CRED/YAP/YAP Incident Summaries.xlsx"), which = "Community Event Summary")
rio::export(ngTbl, paste0(datalib, "Safe Surroundings/02_Data/CRED/YAP/YAP Incident Summaries.xlsx"), which = "Incident Responses Summary")
rio::export(inOutTbl, paste0(datalib, "Safe Surroundings/02_Data/CRED/YAP/YAP Incident Summaries.xlsx"), which = "Focus Area Summary")
