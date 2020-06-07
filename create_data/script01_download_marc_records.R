library(readr)
library(dplyr)
library(xml2)
library(stringi)
library(tidyr)

# Download all of the index files (there are 20 links on each page)
for (i in seq_len(8768))
{
  url <- sprintf("https://www.loc.gov/pictures/search/?sp=%d&co=fsa", i)
  fout <- file.path("web",  "index", sprintf("%04d.html", i))
  if (!file.exists(fout))
  {
    download.file(url, fout, quiet = TRUE)
    system("sleep 0.2")
  }
}

# create a file "loc_photo_ids.txt" that contains a link to every record in the
# FSA-OWI collection
if (!file.exists(id_path <- file.path("output", "loc_photo_ids.txt")))
{
  href_list <- c()
  i_list <- c()
  for (i in seq_len(8768))
  {
    fin <- file.path("web", "index", sprintf("%04d.html", i))
    x <- read_html(fin)
    href <- xml_attr(xml_find_all(x, ".//div[@class='result_item']/p/a"), "href")
    href_list <- c(href_list, href)
    i_list <- c(i_list, rep(i, length(href)))
  }

  table(table(i_list)) # should all be 20, except the last one
  all(!duplicated(href_list))
  ids <- stri_extract(href_list, regex = "[0-9]+")
  write_lines(ids, id_path)
}

# read in the links and grab each page (there are around 176k), saving a copy locally
ids <- read_lines(id_path)
ids <- ids[ids != "NA"]
for (i in seq_along(ids))
try({
  fout <- file.path("web",  "loc", sprintf("%s.html", ids[i]))
  if (!file.exists(fout))
  {
    url <- sprintf("https://www.loc.gov/pictures/item/%s/marc/", ids[i])
    download.file(url, fout, quiet = TRUE)
    system("sleep 0.2")
    print(sprintf("Done with %d of %d", i, length(ids)))
  }
})

# some pages had not completely downloaded
bad_id <- c()
for (i in seq_along(ids))
{
  x <- read_lines(file.path("web",  "loc", sprintf("%s.html", ids[i])))
  if (x[length(x)] != "</html>") {
    # record bad id and redownload
    bad_id <- c(bad_id, i)
    url <- sprintf("https://www.loc.gov/pictures/item/%s/marc/", ids[i])
    fout <- file.path("web",  "loc", sprintf("%s.html", ids[i]))
    download.file(url, fout)
  }
}

# grab the marc records and create one large data set
df_list <- vector("list", length(ids))
for (i in seq_along(ids))
{
  x <- read_html(file.path("web", "loc", sprintf("%s.html", ids[i])))
  tr <- xml_text(xml_find_all(x, ".//table/tr/td"))
  if((length(tr) %% 5) != 0) { print(i) }
  mat <- matrix(tr, byrow=TRUE, ncol = 5)
  df <- tibble(
    id = ids[i], tag = mat[,1], i1 = mat[,2], i2 = mat[,3], code = mat[,4], text = mat[,5]
  )
  df$tag <- stri_trim(df$tag)
  df$i1 <- stri_trim(df$i1)
  df$i2 <- stri_trim(df$i2)
  df$code <- stri_trim(df$code)
  df$text <- stri_trim(df$text)

  df$tag[df$tag == ""] <- NA
  df$i1[df$i1 == ""] <- NA
  df$i2[df$i2 == ""] <- NA
  df_list[[i]] <- fill(df, tag)

  if ((i %% 100) == 0) print(sprintf("Done with %i", i))
}

df_list <- bind_rows(df_list)
write_csv(df_list, file.path("output", "marc_records.csv.bz2"))
