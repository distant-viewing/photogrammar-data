library(readr)
library(dplyr)
library(xml2)
library(stringi)
library(tidyr)

source("../geo_table/clean_names.R")

###########################################################################################
# load data and define small helper functions
marc <- read_csv("marc_records.csv.bz2")
place <- read_csv("../geo_table/place_lookup.csv")
county <- read_csv("../geo_table/county_lookup.csv")
geonames <- read_csv("../geo_table/geonames_lookup.csv")

###########################################################################################
# create output dataset
output_data <- tibble(id = unique(marc$id))
output_data$loc_code <- stri_sub(filter_first_na(filter(marc, tag == "035", code == "a")), 6, -1)
output_data$call_num <- filter_first_na(filter(marc, tag == "037", code == "a"))
output_data$photographer <- filter_first_na(filter(marc, tag == "100", i1 == 1, code == "a"))
output_data$caption <- filter_first_na(filter(marc, tag == "245", i1 == 1, code == "a"))
output_data$date <- filter_first_na(filter(marc, tag == "260", code == "c"), TRUE)
output_data$path <- filter_first_na(filter(marc, tag == "856", i1 == 4, i2 == 1, code == "f"))
output_data$path_start <- filter_first_na(filter(marc, tag == "985", code == "a"))
output_data$other_letter <- filter_first_na(filter(marc, tag == "084", code == "a"))
output_data$other_number <- filter_first_na(filter(marc, tag == "084", code == "b"))

output_data$geo_country <- filter_first_na(filter(marc, tag == "752", code == "a"), TRUE)
output_data$geo_state <- filter_first_na(filter(marc, tag == "752", code == "b"), TRUE)
output_data$geo_county <- filter_first_na(filter(marc, tag == "752", code == "c"), TRUE)
output_data$geo_city <- filter_first_na(filter(marc, tag == "752", code == "d"), TRUE)

###########################################################################################
# clean photographer names
first_names <- stri_extract(output_data$photographer, regex = ",[\\w\\W]+")
first_names <- stri_trim(stri_replace_all(first_names, "", fixed = ","))
first_names[is.na(first_names)] <- ""
last_names <- stri_extract(output_data$photographer, regex = "[^,]+,")
last_names <- stri_trim(stri_replace_all(last_names, "", fixed = ","))
last_names[is.na(last_names)] <- ""

full_names <- sprintf("%s %s", first_names, last_names)
full_names[full_names == " "] <- NA_character_
full_names <- stri_trim(full_names)
output_data$photographer <- full_names

###########################################################################################
# clean dates
num_years <- stri_count(output_data$date, regex = "[0-9][0-9][0-9][0-9]")
first_year <- stri_extract(output_data$date, regex = "[0-9][0-9][0-9][0-9]")
first_text <- stri_extract(output_data$date, regex = "[A-Z][a-z]+")
month <- rep(NA_character_, nrow(output_data))
month[first_text == "Jan"] <- "01"
month[first_text == "Feb"] <- "02"
month[first_text %in% c("Mar", "March")] <- "03"
month[first_text %in% c("Apr", "April")] <- "04"
month[first_text == "May"] <- "05"
month[first_text == "June"] <- "06"
month[first_text == "July"] <- "07"
month[first_text %in% c("Aug", "August")] <- "08"
month[first_text == "Sept"] <- "09"
month[first_text == "Oct"] <- "10"
month[first_text %in% c("Nov", "November")] <- "11"
month[first_text %in% c("Dec", "December")] <- "12"
month[first_text == "Spring"] <- "04"
month[first_text == "Summer"] <- "07"
month[first_text == "Fall"] <- "10"
month[first_text == "Winter" & first_year == 1942] <- "12"
month[first_text == "Winter" & first_year == 1943] <- "01"

year <- if_else(num_years == 1, first_year, NA_character_)
month <- if_else(num_years == 1, month, NA_character_)
month[year <= 1934] <- NA_character_
month[year > 1943] <- NA_character_
year[year <= 1934] <- NA_character_
year[year > 1943] <- NA_character_

output_data$year <- year
output_data$month <- month

###########################################################################################
# clean category codes
table(is.na(output_data$other_letter), is.na(output_data$other_number))

category_number <- sprintf("%s-%s", output_data$other_letter, output_data$other_number)
category_number[is.na(output_data$other_letter)] <- NA_character_
category_number[is.na(output_data$other_number)] <- NA_character_

###########################################################################################
# path to photographs
path_id <- output_data$path
path_id[stri_length(output_data$path) != 7L] <- NA_character_

url <- sprintf(
  "https://cdn.loc.gov/service/pnp/fsa/%s000/%s00/%sr.jpg",
  stri_sub(path_id, 1, 4),
  stri_sub(path_id, 1, 5),
  path_id
)

# output_data$path_start
#
# https://cdn.loc.gov/service/%s/8c07000/8c07400/8c07459r.jpg
#
#

###########################################################################################
# clean up the geographic data
output_data <- clean_non_48(output_data)

cset <- output_data$geo_county
sset <- output_data$geo_state
output_data$geo_county <- clean_county_names(cset, sset)

###########################################################################################
# join with geographic data

# match counties
output_data$county <- output_data$geo_county
output_data$county[output_data$geo_country != "United States"] <- NA
output_data$county[is.na(output_data$geo_state)] <- NA
output_data$county[is.na(output_data$geo_county)] <- NA
output_data$county <- stri_replace_all(output_data$county, "", fixed = " County")
output_data$county <- stri_replace_all(output_data$county, "", fixed = " Parish")
output_data$county <- stri_trim(output_data$county)
output_data$county <- stri_trans_totitle(output_data$county)
output_data <- left_join(
  output_data,
  select(county, state_name, county, nhgis_join, county_lon = lon, county_lat = lat),
  by = c("geo_state" = "state_name", "county" = "county")
)

table(
  has_county_field = !is.na(output_data$county),
  in_county_table = !is.na(output_data$county_lon)
)

# match cities to census designated places
output_data$city <- output_data$geo_city
output_data$city[output_data$geo_country != "United States"] <- NA
output_data$city[is.na(output_data$geo_state)] <- NA
output_data <- left_join(
  output_data,
  select(place, state_name, place, lon_city, lat_city),
  by = c("geo_state" = "state_name", "city" = "place")
)

table(
  has_city_field = !is.na(output_data$city),
  in_places_table = !is.na(output_data$lon_city)
)

# match geonames to census designated places
output_data$place <- NA_character_
index <- which(!is.na(output_data$county) &
               is.na(output_data$county_lon) &
               is.na(output_data$lon_city))
output_data$place[index] <- output_data$county[index]
index <- which(is.na(output_data$lon_city) &
               is.na(output_data$place) &
               !is.na(output_data$city))
output_data$place[index] <- output_data$city[index]
output_data <- left_join(
  output_data,
  select(
    geonames, geo_state, geo_place, geoplace_name = name,
    geo_lon = lon, geo_lat = lat, geoplace_county = county
  ),
  by = c("geo_state" = "geo_state", "place" = "geo_place")
)

table(
  has_place_field = !is.na(output_data$place),
  in_geonames_table = !is.na(output_data$geo_lat)
)

# pick point-based lat/lon
output_data$lon <- output_data$lon_city
output_data$lat <- output_data$lat_city
output_data$lon[is.na(output_data$lon)] <- output_data$geo_lon[is.na(output_data$lon)]
output_data$lat[is.na(output_data$lat)] <- output_data$geo_lat[is.na(output_data$lat)]

# replace county with county from the geoplace dataset
these <- which(is.na(output_data$county_lon))
output_data$county[these] <- output_data$geoplace_county[these]

# spatial join to find the locations
shp_county <- st_read("../geo_table/nhgis0006_shapefile_tl2008_us_county_1940/US_county_1940_conflated.shp")
shp_county <- select(shp_county, state_name = STATENAM, county = NHGISNAM)
shp_county$state_name <- as.character(shp_county$state_name)
shp_county$county <- as.character(shp_county$county)


# check the results
table(county = !is.na(output_data$county_lat), latlong = !is.na(output_data$lat))

###########################################################################################
# fill in strips
cn <- output_data$call_num
strip_call_number <- stri_replace_all(cn, "", regex = "-M[0-9]+")
position <- as.numeric(stri_sub(stri_extract(cn, regex = "M[0-9]+"), -1, -1))
position[position == 0] <- NA
strip_call_number[is.na(position)] <- NA

first_non_na <- function(v) { v <- v[!is.na(v)] ; ifelse(length(v), v[1], NA)  }

strips <- unique(strip_call_number[!is.na(strip_call_number)])
for ( i in seq_along(strips) )
{
  index <- which(strip_call_number == strips[i])

  if ( length(these <- which(is.na(output_data$photographer[index]))) )
  {
    output_data$photographer[index][these] <- first_non_na(output_data$photographer[index])
  }
  if ( length(these <- which(is.na(output_data$year[index]))) )
  {
    output_data$year[index][these] <- first_non_na(output_data$year[index])
  }
  if ( length(these <- which(is.na(output_data$month[index]))) )
  {
    output_data$month[index][these] <- first_non_na(output_data$month[index])
  }
}

###########################################################################################
# create output data
