library(tidyverse)

old <- read_csv(file.path("output", "photo_metadata_20200707.csv"), guess_max = 170000L)
new <- read_csv(file.path("output", "photo_metadata_20210419.csv"), guess_max = 170000L)

stopifnot(nrow(old) == nrow(new))
stopifnot(ncol(old) == ncol(new))

diff_old <- setdiff(old, new)
diff_new <- setdiff(new, old)

diff_old$photographer %>% table()
diff_new$photographer %>% table()

diff_old$state %>% table()
diff_new$state %>% table()

# Here is "Migrant Mother"
filter(diff_new, place == "Nipomo")$caption
