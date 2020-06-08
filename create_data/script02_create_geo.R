library(sf)
library(jsonlite)
library(dplyr)
library(readr)
library(stringi)
library(geojsonsf)

source("funs.R")

##########################################################################################
# 1. load metadata about states and their fips codes
state_lookup <- read_csv(file.path("static", "state_lookup.csv"))
state_lookup <- filter(state_lookup, !is.na(alpha))

##########################################################################################
# 2. load historic county shape files from NHGIS
shp_county <- st_read(file.path(
  "static", "nhgis0006_shapefile_tl2008_us_county_1940", "US_county_1940_conflated.shp"
))
shp_county <- select(shp_county, state_name = STATENAM, county = NHGISNAM)
shp_county$state_name <- as.character(shp_county$state_name)
shp_county$state_name <- stri_replace_all(shp_county$state_name, "", fixed = " Territory")
shp_county$county <- as.character(shp_county$county)

##########################################################################################
# 3. load modern county shape files from Census Bureau, project, and join to the shp_county
#    set of historic counties
shp_county_cur <- st_read("static/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
shp_county_cur <- filter(shp_county_cur, STATEFP %in% c("72", "78"))
shp_county_cur$state_name <- "Puerto Rico"
shp_county_cur$state_name[shp_county_cur$STATEFP == "78"] <- "Virgin Islands of the U.S."
shp_county_cur$county <- shp_county_cur$NAME
shp_county_cur <- select(shp_county_cur, state_name, county)

shp_county_cur <- st_transform(shp_county_cur, st_crs(shp_county))
shp_county <- rbind(shp_county, shp_county_cur)
names(shp_county)[1] <- "state_name_cnt"

##########################################################################################
# 4. save county data
county_lookup <- as_tibble(shp_county)
county_lookup <- select(county_lookup, state_name = state_name_cnt, county = county)
county_lookup <- arrange(county_lookup, state_name, county)

write_csv(county_lookup, file.path("output", "county_lookup.csv"))

##########################################################################################
# 5. create lookup table of places for U.S. set of census designated places
# data from: https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_place_500k.zip
geo_place <- st_read("static/cb_2019_us_place_500k/cb_2019_us_place_500k.shp")
index <- match(geo_place$STATEFP, state_lookup$fips)
geo_place$state_name <- state_lookup$state_name[index]
geo_place <- select(geo_place, state_name, place = NAME)
ll <- st_coordinates(st_centroid(geo_place, of_largest_polygon = TRUE))
geo_place$lon <- ll[,1]
geo_place$lat <- ll[,2]

# match to historic county names
geo_place <- st_transform(geo_place, st_crs(shp_county))
geo_place_join <- st_join(geo_place, shp_county)
geo_place_join <- filter(geo_place_join, !is.na(state_name_cnt))
geo_place_join <- filter(geo_place_join, state_name == state_name_cnt)
geo_place_join <- geo_place_join[order(st_area(geo_place_join), decreasing = TRUE), ]
dup_rows <- duplicated(select(as_tibble(geo_place_join), state_name, place))
geo_place_join <- geo_place_join[!dup_rows,]

# create the lookup table
place_lookup <- as_tibble(geo_place_join)
place_lookup <- select(place_lookup, state_name, place, lon, lat, county)
place_lookup <- arrange(place_lookup, state_name, place)
place_lookup$place <- as.character(place_lookup$place)

# Clean up the places dataset
balance <- which(stri_detect(place_lookup$place, fixed = "(balance)"))
place_lookup$place[balance] <- stri_extract(
  place_lookup$place[balance], regex = "^[A-Za-z]+"
)
place_lookup$place <- stri_replace_all(place_lookup$place, "a", fixed = "á")
place_lookup$place <- stri_replace_all(place_lookup$place, "a", fixed = "å")
place_lookup$place <- stri_replace_all(place_lookup$place, "e", fixed = "é")
place_lookup$place <- stri_replace_all(place_lookup$place, "n", fixed = "ñ")
place_lookup$place <- stri_replace_all(place_lookup$place, "o", fixed = "ó")
place_lookup$place <- stri_replace_all(place_lookup$place, "u", fixed = "ú")
place_lookup$place <- stri_replace_all(place_lookup$place, "u", fixed = "ü")

write_csv(place_lookup, file.path("output", "place_lookup.csv"))

##########################################################################################
# 6. Determine missing names
marc <- read_csv("../regeo/marc_records.csv.bz2")

# create dataset with one row for each photograph
photo_data <- tibble(id = unique(marc$id))
photo_data$geo_country <- filter_first_na(filter(marc, tag == "752", code == "a"), TRUE)
photo_data$geo_state <- filter_first_na(filter(marc, tag == "752", code == "b"), TRUE)
photo_data$geo_county <- filter_first_na(filter(marc, tag == "752", code == "c"), TRUE)
photo_data$geo_city <- filter_first_na(filter(marc, tag == "752", code == "d"), TRUE)

# clean up the geographic data
photo_data <- clean_non_48(photo_data)
cset <- photo_data$geo_county
sset <- photo_data$geo_state
photo_data$geo_county <- clean_county_names(cset, sset)

# create dataset of missing county-level data
z <- count(photo_data, geo_country, geo_state, geo_county)
z <- filter(z, geo_country == "United States", !is.na(geo_state), !is.na(geo_county))
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " County"))
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " Parish"))
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " Municipio"))
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " (Va.)"))
z <- mutate(z, geo_county = stri_trim(geo_county))
z <- mutate(z, geo_county_lower = stri_trans_tolower(geo_county))
z <- mutate(z, geo_county_lower = stri_replace_all(geo_county_lower, "", fixed = "."))
z <- anti_join(z,
  mutate(county_lookup, county = stri_replace_all(stri_trans_tolower(county), "", fixed = ".")),
  by = c("geo_state" = "state_name", "geo_county_lower" = "county")
)
county_miss_data <- arrange(z, desc(n))

# create dataset of missing county-level data
z <- count(photo_data, geo_country, geo_state, geo_city)
z <- filter(z, geo_country == "United States", !is.na(geo_state), !is.na(geo_city))
z <- mutate(z, geo_city = stri_trim(geo_city))
z <- anti_join(z, place_lookup, by = c("geo_state" = "state_name", "geo_city" = "place"))
city_miss_data <- arrange(z, desc(n))

# combine the two, because missing counties are really just missing places
city_miss_data <- bind_rows(rename(county_miss_data, geo_city = geo_county), city_miss_data)
city_miss_data <- unique(city_miss_data)

##########################################################################################
# 7. Download and save data from GeoNames for missing names
country <- rep("US", nrow(city_miss_data))
country[city_miss_data$geo_state == "Puerto Rico"] <- "PR"
country[city_miss_data$geo_state == "Virgin Islands of the U.S."] <- "VI"
search_term <- city_miss_data$geo_city
urls <- sprintf(
  "http://api.geonames.org/searchJSON?q=%s&country=%s&username=tba3&orderby=relevance",
  stri_replace_all(search_term, "+", fixed = " "),
  country
)
city_miss_data$path_out <- sprintf(
  "%s.json", stri_replace_all(search_term, "_", regex = "[ ]+")
)
city_miss_data$path_out <- file.path(
  "web", "geonames", stri_trans_tolower(city_miss_data$path_out)
)

for ( i in seq_along(urls) )
{
  if (!file.exists(city_miss_data$path_out[i]))
  {
    x <- read_lines(urls[i])
    write_lines(x, city_miss_data$path_out[i])
    print(sprintf("%d results for %s", parse_json(x)$totalResultsCount, search_term[i]))
    system("sleep 1")
  }
}

##########################################################################################
# 8. Parse data from GeoNames
geonames_rows <- NULL
for ( i in seq_len(nrow(city_miss_data)) )
{
  geo <- read_json(city_miss_data$path_out[i])
  if (length(geo$geonames))
  {
    geo_df <- tibble(
      state = sapply(geo$geonames, getElement, "adminName1"),
      lat = sapply(geo$geonames, getElement, "lat"),
      lon = sapply(geo$geonames, getElement, "lng"),
      name = sapply(geo$geonames, getElement, "name"),
      type = sapply(geo$geonames, getElement, "fcodeName"),
      geo_state = city_miss_data$geo_state[i],
      geo_city = city_miss_data$geo_city[i]
    )
    if (!(city_miss_data$geo_state[i] %in% c("Puerto Rico", "Virgin Islands of the U.S."))) {
      geo_df <- geo_df[geo_df$state == city_miss_data$geo_state[i], ]
    }
    if (nrow(geo_df))
    {
      geonames_rows <- bind_rows(geonames_rows, geo_df[1,])
    }
  }
}
city_miss_data <- left_join(
  city_miss_data, geonames_rows, by = c("geo_state", "geo_city")
)
city_miss_data <- select(
  filter(city_miss_data, !is.na(lat)), state_name = geo_state, place = geo_city, lon, lat
)

##########################################################################################
# 9. Create GeoNames Lookup Table
shp_place <- st_as_sf(x = city_miss_data, coords = c("lon", "lat"))
shp_place$lon <- city_miss_data$lon
shp_place$lat <- city_miss_data$lat
st_crs(shp_place) <- 4326
shp_place <- st_transform(shp_place, st_crs(shp_county))
shp_join <- st_join(shp_place, shp_county)

# filter mismatched data
shp_join <- filter(shp_join, !is.na(state_name_cnt))
shp_join <- filter(shp_join, state_name == state_name_cnt)
dup_rows <- duplicated(select(as_tibble(shp_join), state_name, place))
shp_join <- shp_join[!dup_rows,]

# create the lookup table
geonames_lookup <- as_tibble(shp_join)
geonames_lookup <- select(geonames_lookup, state_name, place, lon, lat, county)
geonames_lookup <- arrange(geonames_lookup, state_name, place)
geonames_lookup$lon <- as.numeric(geonames_lookup$lon)
geonames_lookup$lat <- as.numeric(geonames_lookup$lat)

write_csv(geonames_lookup, file.path("output", "geonames_lookup.csv"))
