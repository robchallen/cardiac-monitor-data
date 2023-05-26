#!/usr/bin/env Rscript
# This script loads data from csv files
# If called from the command line it expects an input directory and an output directory
# e.g. ./data-wrangling.R ~/tmp/input ~/tmp/output

# requires the following packages, tidyverse, here, digest, fs
if (!"tidyverse" %in% rownames(installed.packages()))
  install.packages("tidyverse", repos="https://www.stats.bris.ac.uk/R/", lib=Sys.getenv("R_LIBS_USER"))
if (!"here" %in% rownames(installed.packages()))
  install.packages("here", repos="https://www.stats.bris.ac.uk/R/", lib=Sys.getenv("R_LIBS_USER"))
if (!"digest" %in% rownames(installed.packages()))
  install.packages("digest", repos="https://www.stats.bris.ac.uk/R/", lib=Sys.getenv("R_LIBS_USER"))
if (!"fs" %in% rownames(installed.packages()))
  install.packages("fs", repos="https://www.stats.bris.ac.uk/R/", lib=Sys.getenv("R_LIBS_USER"))

library(tidyverse)

here::i_am("data-wrangling.R")
a = commandArgs(trailingOnly=TRUE)

# Pick up input directories from command line:
if(exists("a") && length(a)>0) {
  in_dir = normalizePath(a[1])
} else {
  in_dir = here::here("input")
}

# OR: define manually
# in_dir = "C:\\something\\somewhere\\input_directory"

# Pick up output directory from command line:
if(exists("a") && length(a)>1) {
  out_dir = normalizePath(a[2])
} else {
  out_dir = here::here("output")
}

if (in_dir == out_dir) stop("input and output directory cannot be the same")
fs::dir_create(in_dir)
fs::dir_create(out_dir)

# OR: define manually
# out_dir = "C:\\something\\somewhere\\output_directory"

# get al lthe csv files in the directory:
files = fs::dir_ls(in_dir, glob="*.csv")

for (file in files) {
  csv = suppressMessages(readr::read_delim(file,delim = ";",na = c("---","")))
  
  # scan through the columns looking for things that can be converted to dates
  # using the slightly weird date format(s) in the input csv
  for (name in colnames(csv)) {
    # name = colnames(csv)[1]
    col = csv[[name]]
    if (is.character(col)) {
      tmp = as.POSIXct(col,tryFormats=c("%d-%b-%Y  %T","%d-%b-%Y %T"),optional = TRUE)
      # any successful dates?
      if (!all(is.na(tmp))) {
        csv[[name]] = tmp
      }
    }
  }
  
  # wherever there is a date column create an equivalence number of days since date of implantation 
  # as an additional column
  csv2 = csv %>%
    mutate(across(c(where(is.POSIXct)), .fns = ~ as.double(difftime(`Date of implantation`,.x,units = "days")), .names = "{.col} (days)")) %>%
    # get rid of original date colums
    select(-where(is.POSIXct)) %>%
    # reorder dates to front of CSV
    select(c(ends_with("(days)"),everything())) %>%
    # hash the serial number.
    mutate(
      `Serial number` = digest::digest(`Serial number`,algo = "md5"),
      `Transmitter SN` = digest::digest(`Transmitter SN`,algo = "md5")
    )
  
  # write the files out.
  out_file = fs::path(out_dir,fs::path_file(file))
  readr::write_csv(csv2, out_file, append = FALSE)
  message("written to: ", out_file)
  
}


