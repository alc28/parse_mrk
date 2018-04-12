# load libraries
library(tidyverse)
library(stringr)
#library(plyr)

# function: convert MARC mrk output from MarcEdit to df

mrk_to_df <- function(f) {
  x <- readLines(f)
  nx <- !nchar(x)
#  df <- rbind.fill( lapply(split(x[!nx], cumsum(nx)[!nx]),
#                           function(x) data.frame(t(x))  ) )
  df <- rbind_all( lapply(split(x[!nx], cumsum(nx)[!nx]),
                         function(x) data.frame(t(x))  ) )
  return(df)
}

# files 

import_filename <- "data/OCLC_Masters_matching_Voyager_with_MARCEdit_URIs.mrk"
export_filename <- "data/tidy_oclc_masters.txt"


df <- mrk_to_df(import_filename)
df <- df %>%
  mutate_all(as.character) 

# convert wide df to long 
df$bib_id <- df$X2
df <- df[c(ncol(df),1:ncol(df)-1)] # 
# df <- df[c(ncol(df),1:43)] # works
tidy <- df %>%
  gather(key = "sequence", value = "rawfield", 2:ncol(df))

tidy <- tidy %>%
  filter(!is.na(rawfield)) %>%
  mutate(bib_id = str_trunc(bib_id, 9, side = "left", ellipsis = "")) %>%
  mutate(bib_id = str_trim(bib_id)) %>%
  mutate(mrcfield = str_sub(rawfield, 2,4)) %>%
  arrange(bib_id)

tidy <- tidy[c(1,2,4,3)]

write_delim(tidy, export_filename, delim = "\t")


