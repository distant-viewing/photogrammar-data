library(readr)
library(dplyr)
library(xml2)
library(stringi)
library(tidyr)
library(jsonlite)
library(stringdist)
library(sf)

source("clean_names.R")

###########################################################################################
# load data and define small helper functions
marc <- read_csv("../regeo/marc_records.csv.bz2")
state <- read_csv("../geo_table/state_lookup.csv")
place <- read_csv("../geo_table/place_lookup.csv")
county <- read_csv("../geo_table/county_lookup.csv")

# Clean up the places dataset
balance <- which(stri_detect(place$place, fixed = "(balance)"))
place$place[balance] <- stri_extract(place$place[balance], regex = "^[A-Za-z]+")
place$place <- stri_replace_all(place$place, "a", fixed = "á")
place$place <- stri_replace_all(place$place, "a", fixed = "å")
place$place <- stri_replace_all(place$place, "e", fixed = "é")
place$place <- stri_replace_all(place$place, "n", fixed = "ñ")
place$place <- stri_replace_all(place$place, "o", fixed = "ó")
place$place <- stri_replace_all(place$place, "u", fixed = "ú")
place$place <- stri_replace_all(place$place, "u", fixed = "ü")
place <- group_by(place, state_name) %>% filter(!duplicated(place)) %>% ungroup()
names(place)[c(4, 5)] <- c("lon_city", "lat_city")

filter_first_na <- function(df, remove_dot = FALSE)
{
  df <- select(df, id, text)
  df <- df[!duplicated(df$id),]
  if (remove_dot) df$text <- stri_replace(stri_trim(df$text), "", regex = "\\.$")
  df_new <- left_join(tibble(id = unique(marc$id)), df, by = "id")
  df_new$text
}

###########################################################################################
# create dataset with one row for each photograph
output_data <- tibble(id = unique(marc$id))

output_data$geo_country <- filter_first_na(filter(marc, tag == "752", code == "a"), TRUE)
output_data$geo_state <- filter_first_na(filter(marc, tag == "752", code == "b"), TRUE)
output_data$geo_county <- filter_first_na(filter(marc, tag == "752", code == "c"), TRUE)
output_data$geo_city <- filter_first_na(filter(marc, tag == "752", code == "d"), TRUE)

###########################################################################################
# clean up the geographic data
output_data <- clean_non_48(output_data)

cset <- output_data$geo_county
sset <- output_data$geo_state
output_data$geo_county <- clean_county_names(cset, sset)

###########################################################################################
# create datasets of missing data
z <- count(output_data, geo_country, geo_state, geo_county)
z <- filter(z, geo_country == "United States", !is.na(geo_state), !is.na(geo_county))
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " County"))
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " Parish"))
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " (Va.)"))
z <- mutate(z, geo_county = stri_trim(geo_county))
z <- mutate(z, geo_county = stri_trans_totitle(geo_county))
z <- anti_join(z, county, by = c("geo_state" = "state_name", "geo_county" = "county"))
county_data <- arrange(z, desc(n))

z <- count(output_data, geo_country, geo_state, geo_city)
z <- filter(z, geo_country == "United States", !is.na(geo_state), !is.na(geo_city))
z <- mutate(z, geo_city = stri_trim(geo_city))
z <- anti_join(z, place, by = c("geo_state" = "state_name", "geo_city" = "place"))
city_data <- arrange(z, desc(n))

###########################################################################################
# query geonames

country <- rep("US", nrow(county_data))
country[county_data$geo_state == "Puerto Rico"] <- "PR"
country[county_data$geo_state == "Virgin Islands of the U.S."] <- "VI"
search_term <- county_data$geo_county
urls <- sprintf(
  "http://api.geonames.org/searchJSON?q=%s&country=%s&username=tba3&orderby=relevance",
  stri_replace_all(search_term, "+", fixed = " "),
  country
)
county_data$path_out <- sprintf("%s.json", stri_replace_all(search_term, "_", regex = "[ ]+"))
county_data$path_out <- file.path("geonames", stri_trans_tolower(county_data$path_out))

for ( i in seq_along(urls) )
{
  if (!file.exists(county_data$path_out[i]))
  {
    x <- read_lines(urls[i])
    write_lines(x, county_data$path_out[i])
    print(sprintf("%d results for %s", parse_json(x)$totalResultsCount, search_term[i]))
    system("sleep 1")
  }
}

country <- rep("US", nrow(city_data))
country[city_data$geo_state == "Puerto Rico"] <- "PR"
country[city_data$geo_state == "Virgin Islands of the U.S."] <- "VI"
search_term <- city_data$geo_city
urls <- sprintf(
  "http://api.geonames.org/searchJSON?q=%s&country=%s&username=tba3&orderby=relevance",
  stri_replace_all(search_term, "+", fixed = " "),
  country
)
city_data$path_out <- sprintf("%s.json", stri_replace_all(search_term, "_", regex = "[ ]+"))
city_data$path_out <- file.path("geonames", stri_trans_tolower(city_data$path_out))

for ( i in seq_along(urls) )
{
  if (!file.exists(city_data$path_out[i]))
  {
    x <- read_lines(urls[i])
    write_lines(x, city_data$path_out[i])
    print(sprintf("%d results for %s", parse_json(x)$totalResultsCount, search_term[i]))
    system("sleep 1")
  }
}

###########################################################################################
# clean the geonames data
geonames_out <- NULL
for ( i in seq_len(nrow(county_data)) )
{
  geo <- read_json(county_data$path_out[i])
  if (length(geo$geonames))
  {
    geo_df <- tibble(
      state = sapply(geo$geonames, getElement, "adminName1"),
      lat = sapply(geo$geonames, getElement, "lat"),
      lon = sapply(geo$geonames, getElement, "lng"),
      name = sapply(geo$geonames, getElement, "name"),
      type = sapply(geo$geonames, getElement, "fcodeName"),
      geo_state = county_data$geo_state[i],
      geo_county = county_data$geo_county[i]
    )
    if (!(county_data$geo_state[i] %in% c("Puerto Rico", "Virgin Islands of the U.S."))) {
      geo_df <- geo_df[geo_df$state == county_data$geo_state[i], ]
    }
    if (nrow(geo_df))
    {
      geonames_out <- bind_rows(geonames_out, geo_df[1,])
    }
  }
}
county_data <- left_join(county_data, geonames_out, by = c("geo_state", "geo_county"))
county_data <- select(filter(county_data, !is.na(lat)), -n, -path_out, -state)
names(county_data)[3] <- "geo_place"

geonames_out <- NULL
for ( i in seq_len(nrow(city_data)) )
{
  geo <- read_json(city_data$path_out[i])
  if (length(geo$geonames))
  {
    geo_df <- tibble(
      state = sapply(geo$geonames, getElement, "adminName1"),
      lat = sapply(geo$geonames, getElement, "lat"),
      lon = sapply(geo$geonames, getElement, "lng"),
      name = sapply(geo$geonames, getElement, "name"),
      type = sapply(geo$geonames, getElement, "fcodeName"),
      geo_state = city_data$geo_state[i],
      geo_city = city_data$geo_city[i]
    )
    if (!(city_data$geo_state[i] %in% c("Puerto Rico", "Virgin Islands of the U.S."))) {
      geo_df <- geo_df[geo_df$state == city_data$geo_state[i], ]
    }
    if (nrow(geo_df))
    {
      geonames_out <- bind_rows(geonames_out, geo_df[1,])
    }
  }
}
city_data <- left_join(city_data, geonames_out, by = c("geo_state", "geo_city"))
city_data <- select(filter(city_data, !is.na(lat)), -n, -path_out, -state)
names(city_data)[3] <- "geo_place"

place_data <- bind_rows(city_data, county_data)
place_data$lon <- as.numeric(place_data$lon)
place_data$lat <- as.numeric(place_data$lat)

###########################################################################################
# spatial join back with county-level data
shp_county <- st_read("nhgis0006_shapefile_tl2008_us_county_1940/US_county_1940_conflated.shp")
shp_county <- select(shp_county, state_name = STATENAM, county = NHGISNAM)
shp_county$state_name <- as.character(shp_county$state_name)
shp_county$county <- as.character(shp_county$county)

shp_place <- st_as_sf(x = place_data, coords = c("lon", "lat"))
shp_place$lon <- place_data$lon
shp_place$lat <- place_data$lat
st_crs(shp_place) <- 4326
shp_place <- st_transform(shp_place, st_crs(shp_county))
shp_join <- st_join(shp_place, shp_county)

shp_join$state_name <- stri_replace_all(shp_join$state_name, "", fixed = " Territory")
shp_join <- filter(shp_join, geo_state == state_name | is.na(state_name))
shp_join <- as_tibble(shp_join)
shp_join <- select(shp_join, geo_country, geo_state, geo_place, name, type, county, lon, lat)

# clean a few county names
shp_join$county <- stri_replace_all(shp_join$county, "St. ", fixed = "St ")
shp_join$county[shp_join$county == "Second Judicicial Dist - No Data"] <- "Second Jud. Dist."
shp_join$county[shp_join$county == "Third Judicicial Dist - No Data"] <- "Third Jud. Dist."
shp_join$county[shp_join$county == "Fourth Judicicial Dist - No Data"] <- "Fourth Jud. Dist."
shp_join$county[shp_join$county == "Princess Anne"] <- "Princess Anne (Ext)"
shp_join$county[shp_join$county == "Elizabeth City"] <- "Elizabeth City (Ext)"
shp_join$county[shp_join$county == "Norfolk"] <- "Norfolk (Ext)"
shp_join$county[shp_join$county == "Laporte"] <- "La Porte"
shp_join$county <- stri_trans_totitle(shp_join$county)

write_csv(shp_join, "geonames_lookup.csv")
