library(sf)
library(jsonlite)
library(dplyr)
library(readr)
library(stringi)

# metadata about states and their fips codes
state_lookup <- read_csv("state_lookup.csv")
state_lookup <- filter(state_lookup, !is.na(alpha))

# create lookup table of places
# data from: https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_place_500k.zip
geo_place <- st_read("cb_2019_us_place_500k/cb_2019_us_place_500k.shp")
ll <- st_coordinates(st_centroid(geo_place, of_largest_polygon = TRUE))
place_lookup <- tibble(
  fips = as.character(geo_place$STATEFP),
  place = as.character(geo_place$NAME),
  lon = ll[,1],
  lat = ll[,2],
)
place_lookup <- left_join(place_lookup, state_lookup, by = "fips")
place_lookup <- select(place_lookup, state_name, state_alpha = alpha, place, lon, lat)
place_lookup <- arrange(place_lookup, state_name, place)
stopifnot(nrow(geo_place) == nrow(place_lookup))
write_csv(place_lookup, "place_lookup.csv")

# create lookup table of historic counties
geo_county <- st_read("counties.json")
ll <- st_coordinates(st_centroid(geo_county, of_largest_polygon = TRUE))
county_lookup <- tibble(
  state_name = as.character(geo_county$state_terr),
  county = stri_trans_totitle(as.character(geo_county$name)),
  nhgis_join = as.character(geo_county$nhgis_join),
  lon = ll[,1],
  lat = ll[,2],
)
county_lookup <- left_join(county_lookup, state_lookup, by = "state_name")
county_lookup <- select(
  county_lookup, state_name, state_alpha = alpha, county, nhgis_join, lon, lat
)
county_lookup <- arrange(county_lookup, state_name, county)
stopifnot(nrow(geo_county) == nrow(county_lookup))
write_csv(county_lookup, "county_lookup.csv")
