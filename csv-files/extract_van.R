library(readr)
library(dplyr)
library(xml2)
library(stringi)
library(tidyr)

output <- read_csv("~/Desktop/temp_table.csv", guess_max = 12000)
photo <- read_csv("../csv-files/photo_metadata_20190330.csv", guess_max = 12000)

output <- filter(output, !is.na(other_letter))
output <- filter(output, !is.na(other_number))
photo <- select(photo, loc_code = loc_item_link, v1 = vanderbilt_level1, v2 = vanderbilt_level2, v3 = vanderbilt_level3)
output <- left_join(output, photo, by = "loc_code")
output <- filter(output, !is.na(v1))
output <- distinct(output, other_number, v1, v2, v3)

output <- arrange(output, v1, v2, v3)
write_csv(output, "../create_data/static/vanderbilt_lookup.csv")

# sanity checks
y <- read_csv("csv-files/photo_metadata_20190330.csv", guess_max = 12000)
x <- read_csv("create_data/output/photo_metadata_20200707.csv", guess_max = 12000)

all( y$loc_item_link == x$loc_item_link )
table(y$month, x$month)
table(y$year, x$year)
table(y$photographer_name == x$photographer)
table(y$state == x$state)
y_county <- y$county
y_county <- stri_replace_all(y_county, "", fixed = " County")
y_county <- stri_replace_all(y_county, "", fixed = " Parish")
y_county <- stri_replace_all(y_county, "", fixed = " Borough")
table(y_county == x$county)
table(y$vanderbilt_level1 == x$v1)
table(y$vanderbilt_level2 == x$v2)
table(y$vanderbilt_level3 == x$v3)

index <- which(y$vanderbilt_level3 != x$v3)
z <- tibble(v3 = x[index,]$v3, van3 = y[index,]$vanderbilt_level3, caption = y[index,]$caption)
z <- distinct(z, v3, van3, .keep_all=TRUE)
print(z, n = Inf)
