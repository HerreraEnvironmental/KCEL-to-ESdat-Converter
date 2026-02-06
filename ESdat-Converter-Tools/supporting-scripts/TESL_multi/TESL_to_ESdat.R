## ---------------------------
## Script name:         TESL_to_ESdat.R  
## Purpose of script:   Convert TESL EDDs to ESdat format
## Author:              N. VandePutte
## Date Created:        2025-03-07
## Date Updated:        2025-12-11
## Project #:           N/A
## Task # (optional):   N/A
## ---------------------------
## Notes: This script is ran in conjunction with the ESdat_prep.R and ESdat_header.R scripts
##
##    *** Have you updated the config.yaml file? Please do so before running any scripts ***
##        
## ---------------------------

## Required Packages (install as necessary)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(lubridate)
  library(tidyverse)
  library(XML)
  library(yaml)

## Import files
  # Raw files
  chemistry_files <- list.files("data/TESL/data_raw", full.names = TRUE, pattern = "*ChemistryFile*")
  sample_files <- list.files("data/TESL/data_raw", full.names = TRUE, pattern = "*SampleFile*")
  
  chem_codes <- read.csv("ESdat-Converter-Tools/supporting-scripts/TESL/chem_code_lookup.csv")

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/TESL/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  proj_site     <- config$project_info$project_site
  locations     <- config$project_info$locations
  
  # locations <- read.csv("ESdat_locations.csv") %>%
  #   filter(Site_ID == proj_site)
  
## Retrieve lab report names
  lab_reports <- str_match(chemistry_files, pattern = "data/TESL/data_raw/(.*)_ChemistryFile[0-9]+.csv")[,2]
  lab_reports <- lapply(lab_reports, function(x){
    ifelse(nchar(x)>7, substring(x, 1, 7), x)
  })
# lab_reports <- lapply(lab_reports, 
# function(x){
#   if (grepl(",", x)){
#    str_split(x, pattern = ", ") 
#   } else
#   if (grepl(".", x)){
#     str_split(x, pattern = "\\.")
#   } else {
#     x
#   }
#   }
# )

# test <- lapply(lab_reports, function(x){
#   num <- x[[1]][[1]][1]
#  if (length(x[[1]][[1]])>1) {
#   for (i in 2:length(x[[1]][[1]])){
#     num <- paste0(num, "-", str_extract(x[[1]][[1]][i], "[0-9]{2}$"))
#   }
# }
#   num
# })
# test <- lapply(lab_reports, function(x){
#   num <- x[[1]][[1]][1]
#  if (length(x[[1]][[1]])>1) {
#   for (i in 2:length(x[[1]][[1]])){
#     num <- paste0(num, "-", str_extract(x[[1]][[1]][i], "[0-9]{2}$"))
#   }
# }
#  num
# })
# for (i in 1:length(lab_reports)){
#   num <- lab_reports[[i]][[1]][1]
#   if (length(lab_reports[[i]][[1]])>1) {
#   num <- lab_reports[[i]][[1]][1]
#   for (i in 2:length(lab_reports[[1]][[1]])){
#     num <- paste0(num, "-", str_extract(lab_reports[[1]][[1]][i], "[0-9]{2}$"))
#   }
#   }}

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(lab_reports)){
    # Assign lab report
    lab_report <- unlist(lab_reports[i])
    # first_rep <- substring(lab_report, 1, 7)

    sample_file <- grep(lab_report, sample_files, value = TRUE)
    if (grepl(".xls", sample_file)) {
      sample <- read_xls(sample_file, sheet = "SAMP")
      qc <- read_xls(sample_file, sheet = "QA")
      sample <- rbind(sample, qc)
    } else {
      sample <- read.csv(sample_file)
    }
    
    chem_file <- grep(lab_report, chemistry_files, value = TRUE)
    if (grepl(".xls", chem_file)) {
      chemistry <- read_xls(chem_file, sheet = "SAMP")
      qc <- read_xls(chem_file, sheet = "QA")
      chemistry <- rbind(chemistry, qc)
    } else {
      chemistry <- read.csv(chem_file)
    }
  
  # Sample CSV dataframe building
    sample <- sample %>%
      mutate(Sampled_Date_Time = mdy_hms(Sampled_Date_Time),
             Site_ID = proj_site,
             Depth = "",
             Location_Code = ifelse(Field_ID == "", "", 
                                    str_extract(Field_ID, paste0(locations, collapse = "|"))),
             Sample_Type = case_when(grepl("DUP", Field_ID) ~ "Field_D", 
                                     grepl("QA", Field_ID) ~ "Field_B",
                                     grepl("BLK", Sample_Type) ~ "MB",
                                     grepl("DUP", Sample_Type) ~ "LAB_D",
                                     grepl("MS", Sample_Type) ~ "MS",
                                     grepl("SRM|MRL|CCV|CAL|LCV|SCV|IBL", Sample_Type) ~ "SRM",
                                     grepl("BS", Sample_Type) ~ "LCS",
                                     .default = "Normal"), # Required
             Lab_Report_Number = lab_report
             ) %>%
      distinct(SampleCode, Sampled_Date_Time, Sample_Type, .keep_all = TRUE)
    
  # Export Sample file
    write.csv(sample, paste0("data/TESL/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = "")
  
  # Chemistry CSV dataframe building
    chemistry <- chemistry %>%
      mutate(Prefix = ifelse(Result == "ND", "<", ""),
             Result = ifelse(Result == "ND", EQL, Result), # Required
             Total_or_Filtered = as.character(if_else(grepl("Dissolved", OriginalChemName)|OriginalChemName=="Phosphate, Ortho", "F", "T")),
             Extraction_Date = mdy_hms(Extraction_Date),
             Analysed_Date = mdy_hms(Analysed_Date),
             Lab_Analysis_ID = SampleCode,
             Lab_Qualifier = ifelse(Result == "ND", paste0("U", Lab_Qualifier), Lab_Qualifier),
             Lab_Report_Number = lab_report
             ) %>%
      select(-grep("Result_Type", names(chemistry), value = TRUE))
    
    chemistry <- left_join(chemistry, chem_codes) %>%
      mutate(Result_Type = case_when(Result_Type == "SUR"~"SUR",
                                     grepl("-BS", SampleCode)~"SC",
                                     .default = "REG"))
    
  # Export Chemistry file
    write.csv(chemistry, paste0("data/TESL/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), na = "")
  }
  
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("data/TESL/data_raw", full.names = TRUE, pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("data/TESL/data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
