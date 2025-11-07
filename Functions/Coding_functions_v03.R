###
###Functions to prepare text data of intervention scenarios database (modify the coding, recode categories, summarize categories, calculate frequency of categories across scenarios, etc.)
###

library(tidyverse)
library(tm) #needed for text mining analysis

###
#1) Function to calculate frequency of unique values in a character column ####
###
#x has to be the target column of specific dataset (character values)
Frequency <- function(x) {
  Overview <- strsplit(x,",")
  Overview <- unlist(Overview)
  Overview <- Overview[Overview!=""]
  Overview <- data.frame(table(Overview))
  
  print(htmlTable::htmlTable(unique(Overview)))
  return(Interventions_Overview = Overview)
}

###
#2) Function to keep only unique values within each string of a character column ####
###
#x has to be the target column of specific dataset (character values), or a specific set of columns (Using sapply to pass in function)
Unique_values <- function(x) {
  xs <- sapply(strsplit(x, ","), trimws)
  xs <- sapply(xs, unique)
  xs <- sapply(xs, paste, collapse = ",")
}

###
#3) Function to eliminate NAs appearing after recoding of categories, after text recoding often strings need to be cleaned up afterwards ####
###
Eliminate_nas <- function(x) {
  x <- str_replace_all(x, c("NA|,NA|NA, NA|NA,"), "")
  x <- ifelse(x == "", NA, x)
  return(x)
}

###
#4) Function to eliminate , appearing at the start of some strings after recoding of categories ####
###
clean_entries <- function(x) {
  
  ifelse(grepl("^,", x), sub("^,", "", x), x)
  
  }
