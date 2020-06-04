library(readr)
library(dplyr)
library(xml2)
library(stringi)
library(tidyr)

for (i in seq_len(8768))
{
  url <- sprintf("https://www.loc.gov/pictures/search/?sp=%d&co=fsa", i)
  fout <- file.path("index", sprintf("%04d.html", i))
  if (!file.exists(fout))
  {
    download.file(url, fout, quiet = TRUE)
    system("sleep 0.2")
  }
}

if (!file.exists(id_path <- "ids.txt"))
{
  href_list <- c()
  i_list <- c()
  for (i in seq_len(8768))
  {
    fin <- file.path("index", sprintf("%04d.html", i))
    x <- read_html(fin)
    href <- xml_attr(xml_find_all(x, ".//div[@class='result_item']/p/a"), "href")
    href_list <- c(href_list, href)
    i_list <- c(i_list, rep(i, length(href)))
  }

  table(table(i_list)) # all 20, except the last one
  all(!duplicated(href_list))
  ids <- stri_extract(href_list, regex = "[0-9]+")
  write_lines(ids, id_path)
}

ids <- read_lines(id_path)

for (i in seq_along(ids))
{
  fout <- file.path("web", sprintf("%s.html", ids[i]))
  if (!file.exists(fout))
  {
    url <- sprintf("https://www.loc.gov/pictures/item/%s/marc/", ids[i])
    download.file(url, fout, quiet = TRUE)
    system("sleep 0.2")
    print(sprintf("Done with %d of %d", i, length(ids)))
  }
}

ret_val <- function(v) if_else(length(v) >= 1, v[1], NA_character_)

data <- tibble(id = ids, loc = "", call_num = "", a = "", b = "", c = "", d = "")
for (i in seq_along(ids))
{
  fout <- file.path("web", sprintf("%s.html", ids[i]))
  x <- read_html(fout)

  tr <- xml_text(xml_find_all(x, ".//table/tr/td"))
  mat <- matrix(tr, byrow=TRUE, ncol = 5)
  df <- tibble(tag = mat[,1], i1 = mat[,2], i2 = mat[,3], code = mat[,4], text = mat[,5])
  df$tag[df$tag == ""] <- NA
  df <- fill(df, tag)

  data$loc[i] <- ret_val(filter(df, tag == "035", code == "a")$text)
  data$call_num[i] <- ret_val(filter(df, tag == "037", code == "a")$text)
  data$a[i] <- ret_val(filter(df, tag == "752", code == "a")$text)
  data$b[i] <- ret_val(filter(df, tag == "752", code == "b")$text)
  data$c[i] <- ret_val(filter(df, tag == "752", code == "c")$text)
  data$d[i] <- ret_val(filter(df, tag == "752", code == "d")$text)
}

data$loc <- stri_trim(stri_replace_all(data$loc, "", regex = "\\.$"))
data$call_num <- stri_trim(stri_replace_all(data$call_num, "", regex = "\\.$"))
data$a <- stri_replace_all(stri_trim(data$a), "", regex = "\\.$")
data$b <- stri_replace_all(stri_trim(data$b), "", regex = "\\.$")
data$c <- stri_replace_all(stri_trim(data$c), "", regex = "\\.$")
data$d <- stri_replace_all(stri_trim(data$d), "", regex = "\\.$")

data <- data[!is.na(data$a),]
data <- data[data$a == "Puerto Rico",]

data$loc <- stri_sub(data$loc, 6, -1)
index <- match(data$loc, x$loc_item_link)
data <- data[!is.na(index),]

write_csv(data, "puerto_rico_photos.csv")
