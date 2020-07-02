
###########################################################################################
# test code for join with geographic data
sort(table(output_data$geo_country))

# which counties do not have a match?
z <- count(output_data, geo_country, geo_state, geo_county)
z <- filter(z, geo_country == "United States", !is.na(geo_state), !is.na(geo_county))
z <- filter(z, stri_detect(geo_county, fixed = " County") |
               stri_detect(geo_county, fixed = " Parish") |
               stri_detect(geo_county, fixed = " Municipio") )
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " County"))
z <- mutate(z, geo_county = stri_replace_all(geo_county, "", fixed = " Parish"))
z <- mutate(z, geo_county = stri_trim(geo_county))
z <- mutate(z, geo_county = stri_trans_totitle(geo_county))

z <- anti_join(z, county, by = c("geo_state" = "state_name", "geo_county" = "county"))
print(z, n = Inf)
filter(county, state_name == "Virginia") %>% print(n = Inf)

# which "counties" are not counties?
z <- filter(output_data, is.na(geo_city))
z <- count(z, geo_country, geo_state, geo_county)
z <- filter(z, geo_country == "United States", !is.na(geo_state), !is.na(geo_county))
z <- filter(z, !stri_detect(geo_county, fixed = " County") &
               !stri_detect(geo_county, fixed = " Parish") &
               !stri_detect(geo_county, fixed = " Municipio") )
z <- anti_join(z, county, by = c("geo_state" = "state_name", "geo_county" = "county"))
z <- arrange(z, desc(n))
print(z, n = Inf)

# which cities do not have a match?
z <- count(output_data, geo_country, geo_state, geo_city)
z <- filter(z, geo_country == "United States", !is.na(geo_state), !is.na(geo_city))
z <- mutate(z, geo_city = stri_trim(geo_city))
z <- filter(z, !stri_detect(geo_city, fixed = "Farm"))
z <- filter(z, !stri_detect(geo_city, fixed = "Creek"))
z <- filter(z, !stri_detect(geo_city, fixed = "Field"))

z <- anti_join(z, place, by = c("geo_state" = "state_name", "geo_city" = "place"))
z <- arrange(z, desc(n))
print(z, n = 50)

write_csv(z, "city_field_nomatch.csv")
