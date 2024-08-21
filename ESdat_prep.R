library(tidyverse)
library(XML)
library(zip)

source("KCEL_to_ESdat.R")
source("ESdat_header.R")

# copy relevant files to lab report folders and zip files
for(i in 1:length(lab_reports)){
  report_num <- lab_reports[i]
  ESdat_files <- grep(paste0(proj_num, ".", report_num), list.files("./data_secondary"), value = TRUE)
  for(k in 1:length(ESdat_files)){
    if (dir.exists(paste0("./lab_reports/", report_num))){
    } else {dir.create(paste0("./lab_reports/", report_num))}
    file.copy(paste0("./data_secondary/", ESdat_files[k]), paste0("./lab_reports/", report_num, "/", ESdat_files[k]), overwrite = TRUE )
  }
  # change working directory to avoid internal folders in zipfile
  my_wd<-getwd()
  setwd(paste0(my_wd, "/lab_reports/", report_num))
  zip(paste0(my_wd, "/zips/", report_num, ".zip"), files = list.files(), include_directories = FALSE)
  setwd(my_wd)
}

    