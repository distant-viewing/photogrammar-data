library(sf)
library(jsonlite)
library(dplyr)
library(readr)
library(stringi)
library(jsonlite)
library(geojsonsf)

source("funs.R")

##########################################################################################
# 1. load photo metadata and simplified geojson file
photo_meta <- read_csv(file.path("output", "photo_metadata_20200707.csv"), guess = 170000)

##########################################################################################
# 2. load historic county shape files from NHGIS
shp_county <- st_read(file.path(
  "static", "nhgis0006_shapefile_tl2008_us_county_1940", "US_county_1940_conflated.shp"
))
shp_county <- select(shp_county, state_name = STATENAM, county = NHGISNAM, nhgis_join = GISJOIN)
shp_county$state_name <- as.character(shp_county$state_name)
shp_county$nhgis_join <- as.character(shp_county$nhgis_join)
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
shp_county_cur$nhgis_join <- sprintf(
  "G%s00%s", as.character(shp_county_cur$STATEFP), as.character(shp_county_cur$COUNTYFP)
)
shp_county_cur <- select(shp_county_cur, state_name, county, nhgis_join)

shp_county_cur <- st_transform(shp_county_cur, st_crs(shp_county))
shp_county <- rbind(shp_county, shp_county_cur)
names(shp_county)[1] <- "state_name_cnt"

##########################################################################################
# 4. create simplified shape file for the web
shp_simple <- shp_county
num_polygon <- sapply(st_geometry(shp_simple), length)
for ( i in which(num_polygon > 1) )
{
  z <- st_cast(shp_simple$geometry[i], "POLYGON")
  areas <- as.numeric(st_area(z)) / 10000
  index <- which((areas >= 0.05 * sum(areas)) | (areas == max(areas)))
  z <- z[index]
  shp_simple$geometry[i] <- st_union(z)
}

shp_simple <- st_simplify(shp_simple, preserveTopology = TRUE, dTolerance = 500)
shp_simple <- st_make_valid(shp_simple)
names(shp_simple)[1:2] <- c("state_terr", "name")

shp_simple_ll <- st_transform(shp_simple, 4326)
geo_json <- as.character(sf_geojson(shp_simple_ll))
write_lines(geo_json, file.path("output", "county_full.json"))

##########################################################################################
# 5. filter based on counties present in the data and save GeoJSON
shp_simple <- semi_join(
  shp_simple, photo_meta, by = c("state_terr" = "state", "name" = "county")
)
shp_simple

shp_simple_ll <- st_transform(shp_simple, 4326)
geo_json <- as.character(sf_geojson(shp_simple_ll))
write_lines(geo_json, file.path("output", "county_filter.json"))

###########################################################################################
# 6. add nhgis_join
photo_meta <- left_join(
  photo_meta,
  select(as_tibble(shp_county), state_name_cnt, county, nhgis_join),
   by = c("state" = "state_name_cnt", "county" = "county")
)

write_csv(photo_meta, file.path("output", "photo_metadata_20200707.csv"))



##########################################################################################
# 6. try to do projection

# shp_lower <- filter(
#   shp_simple,
#   !(state_terr %in% c("Virgin Islands of the U.S.", "Puerto Rico", "Alaska", "Hawaii"))
# )
#
# shp_lower <- filter(
#   shp_simple,
#   !(state_terr %in% c("Alaska", "Hawaii"))
# )
#
# shp_alaska <- st_transform(filter(shp_simple, state_terr == "Alaska"), 3467)
# shp_hawaii <- st_transform(filter(shp_simple, state_terr == "Hawaii"), 4135)
# shp_island <- filter(
#   st_transform(shp_simple, 2866),
#   state_terr %in% c("Virgin Islands of the U.S.", "Puerto Rico")
# )
#
# shp_lower <- st_coordinates(shp_alaska)
#
# st_crs(shp_lower) <- 4326
# st_crs(shp_alaska) <- 4326
# st_crs(shp_hawaii) <- 4326
# st_crs(shp_island) <- 4326
#
# rbind(shp_hawaii, shp_island)
#
#
# z <- parse_json(as.character(sf_geojson(shp_alaska)))
#
# z$features


# anti_join(
#   filter(photo_meta, !is.na(county)),
#   as_tibble(shp_simple), by = c("state" = "state_terr", "county" = "name")
# )
