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

# Clean up the places dataset
balance <- which(stri_detect(place_lookup$place, fixed = "(balance)"))
place_lookup$place[balance] <- stri_extract(place_lookup$place[balance], regex = "^[A-Za-z]+")
place_lookup$place <- stri_replace_all(place_lookup$place, "a", fixed = "á")
place_lookup$place <- stri_replace_all(place_lookup$place, "a", fixed = "å")
place_lookup$place <- stri_replace_all(place_lookup$place, "e", fixed = "é")
place_lookup$place <- stri_replace_all(place_lookup$place, "n", fixed = "ñ")
place_lookup$place <- stri_replace_all(place_lookup$place, "o", fixed = "ó")
place_lookup$place <- stri_replace_all(place_lookup$place, "u", fixed = "ú")
place_lookup$place <- stri_replace_all(place_lookup$place, "u", fixed = "ü")
place_lookup <- group_by(place_lookup, state_name) %>% filter(!duplicated(place)) %>% ungroup()
names(place_lookup)[c(4, 5)] <- c("lon_city", "lat_city")

# spatial join with historic counties
shp_county <- st_read("nhgis0006_shapefile_tl2008_us_county_1940/US_county_1940_conflated.shp")
shp_county <- select(shp_county, state_name = STATENAM, county = NHGISNAM)
shp_county$state_name <- as.character(shp_county$state_name)
shp_county$county <- as.character(shp_county$county)

shp_place <- st_as_sf(x = place_lookup, coords = c("lon_city", "lat_city"))
shp_place$lon_city <- place_lookup$lon_city
shp_place$lat_city <- place_lookup$lat_city
st_crs(shp_place) <- 4326
shp_place <- st_transform(shp_place, st_crs(shp_county))
shp_join <- st_join(shp_place, shp_county)

shp_join$state_name <- stri_replace_all(shp_join$state_name, "", fixed = " Territory")
shp_join <- filter(shp_join, geo_state == state_name | is.na(state_name))
shp_join <- as_tibble(shp_join)
shp_join <- select(shp_join, geo_country, geo_state, geo_place, name, type, county, lon, lat)


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

county_lookup$county <- stri_trim(county_lookup$county)
county_lookup <- bind_rows(
  county_lookup,
  tibble(
    state_name = "New Mexico",
    state_alpha = "NM",
    county = c("Cibola"),
    nhgis_join = "NULL",
    lon = c( -107.99971),
    lat = c(34.91257)
  ),
  tibble(
    state_name = "Virgin Islands of the U.S.",
    state_alpha = "VI",
    county = c("St. Thomas Island", "St. Croix Island", "St. John Island"),
    nhgis_join = "NULL",
    lon = c(-64.92755, -64.74795, -64.73771),
    lat = c(18.34537, 17.73563, 18.32758)
  )
)
write_csv(county_lookup, "county_lookup.csv")
