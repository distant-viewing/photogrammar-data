library(readr)
library(dplyr)
library(ggplot2)
library(ggmaptile)

photo_metadata <- read_csv(file.path("output", "photo_metadata_20200707.csv"), guess = 10000)

photo_metadata %>%
  filter(!(state %in% c("Hawaii", "Alaska"))) %>%
  filter(country == "United States") %>%
  filter(!is.na(lat)) %>%
  group_by(lon, lat) %>%
  summarize(n = sqrt(n()) / 3) %>%
  arrange(desc(n)) %>%
  ggplot(aes(lon, lat)) +
    stat_maptiles(alpha = 0.2) +
    geom_point(aes(size = n), color = "#355e3b", alpha = 0.2) +
    theme_void() +
    scale_size_identity()

photo_metadata %>%
  filter((state %in% c("North Dakota", "Montana", "Wyoming", "South"))) %>%
  filter(country == "United States") %>%
  filter(!is.na(lat)) %>%
  group_by(lon, lat) %>%
  summarize(n = sqrt(n()) / 1) %>%
  arrange(desc(n)) %>%
  ggplot(aes(lon, lat)) +
    stat_maptiles(alpha = 0.2) +
    geom_point(aes(size = n), color = "#355e3b", alpha = 0.2) +
    theme_void() +
    scale_size_identity()

photo_metadata %>%
  filter(state %in% c("Puerto Rico", "Virgin Islands of the U.S.")) %>%
  filter(country == "United States") %>%
  filter(!is.na(lat)) %>%
  group_by(lon, lat) %>%
  summarize(n = sqrt(n()) / 1) %>%
  arrange(desc(n)) %>%
  ggplot(aes(lon, lat)) +
    stat_maptiles(alpha = 0.2) +
    geom_point(aes(size = n), color = "#355e3b", alpha = 0.4) +
    theme_void() +
    scale_size_identity()

photo_metadata %>%
  filter(state %in% c("Alaska")) %>%
  filter(lon < 0) %>%
  filter(country == "United States") %>%
  filter(!is.na(lat)) %>%
  group_by(lon, lat) %>%
  summarize(n = sqrt(n()) * 4) %>%
  arrange(desc(n)) %>%
  ggplot(aes(lon, lat)) +
    stat_maptiles(alpha = 0.2) +
    geom_point(aes(size = n), color = "#355e3b", alpha = 0.2) +
    theme_void() +
    scale_size_identity()

photo_metadata %>%
  filter(state %in% c("Alaska")) %>%
  filter(lon > 0) %>%
  filter(country == "United States") %>%
  filter(!is.na(lat)) %>%
  group_by(lon, lat) %>%
  summarize(n = sqrt(n()) * 4) %>%
  arrange(desc(n)) %>%
  ggplot(aes(lon, lat)) +
    stat_maptiles(alpha = 0.2) +
    geom_point(aes(size = n), color = "#355e3b", alpha = 0.2) +
    theme_void() +
    scale_size_identity()
