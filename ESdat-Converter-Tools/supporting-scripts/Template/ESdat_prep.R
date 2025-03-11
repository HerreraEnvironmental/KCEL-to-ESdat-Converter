## ---------------------------
## Script name:         ESdat_prep.R  
## Purpose of script:   Exports zipped folders with ESdat header and files from original lab EDD
## Author:              N. VandePutte
## Date Created:        2025-03-11
## Date Updated:        2025-03-11
## Project #:           N/A
## Task # (optional):   N/A
## ---------------------------
## Notes: This script is ran in conjunction with the ESdat_header.R and EXAMPLE_to_ESdat.R scripts
##
##    *** Have you updated the config.yaml file? Please do so before running any scripts ***
## 
## Find and replace EXAMPLE with your folder name
## ---------------------------

## Required Packages (install as necessary)
  library(tidyverse)
  library(XML)
  library(zip)

## Run other scripts
  source("ESdat-Converter-Tools/supporting-scripts/EXAMPLE/EXAMPLE_to_ESdat.R", local = T)
  source("ESdat-Converter-Tools/supporting-scripts/EXAMPLE/ESdat_header.R", local = T)

## Copy relevant files to lab report folders and zip files
  for(i in 1:length(lab_reports)){
    report_num <- lab_reports[i]
    ESdat_files <- grep(paste0(proj_num, ".", report_num), list.files("data/EXAMPLE/data_secondary"), value = TRUE)
    for(k in 1:length(ESdat_files)){
      if (dir.exists(paste0("lab_reports/", report_num))){
      } else {dir.create(paste0("lab_reports/", report_num))}
      file.copy(paste0("data/EXAMPLE/data_secondary/", ESdat_files[k]), paste0("lab_reports/", report_num, "/", ESdat_files[k]), overwrite = TRUE )
    }
## Change working directory to avoid internal folders in zipfile
    my_wd<-getwd()
    setwd(paste0(my_wd, "/lab_reports/", report_num))
    zip(paste0(my_wd, "/zips/", report_num, ".zip"), files = list.files(), include_directories = FALSE)
    setwd(my_wd)
  }


    