## ---------------------------
## Script name:         TESL_to_ESdat.R  
## Purpose of script:   Convert TESL EDDs to ESdat format
## Author:              N. VandePutte
## Date Created:        2025-03-07
## Date Updated:        2025-03-11
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
  lab_reports <- substring(list.files("data/TESL/data_raw", pattern = "*ChemistryFile*"), 1, 7) # TODO adjust to TESL report names

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(lab_reports)){
    # Assign lab report
    lab_report <- lab_reports[i]
  
    sample <- read.csv(grep(lab_report, sample_files, value = TRUE))
    
    chem_file <- grep(lab_report, chemistry_files, value = TRUE)
    if (grep(".xls", chem_file)) {
      chemistry <- read_xls(chem_file, sheet = "SAMP")
      qc <- read_xls(chem_file, sheet = "QA")
      chemistry <- rbind(chemistry, qc)
    } else {
      read.csv(chem_file)
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
                                     grepl("SRM", Sample_Type) ~ "SRM",
                                     grepl("BS", Sample_Type) ~ "LCS",
                                     .default = "Normal") # Required
             ) %>%
      distinct(SampleCode, Sampled_Date_Time, Sample_Type, .keep_all = TRUE)
    
  # Export Sample file
    write.csv(sample, paste0("data/TESL/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = "")
  
  # Chemistry CSV dataframe building
    chemistry <- chemistry %>%
      mutate(Prefix = ifelse(Result == "ND", "<", ""),
             Result = ifelse(Result == "ND", EQL, Result), # Required
             Total_or_Filtered = as.character(if_else(Total_or_Filtered == "D", "F", "T")),
             Extraction_Date = mdy_hms(Extraction_Date),
             Analysed_Date = mdy_hms(Analysed_Date),
             Lab_Analysis_ID = SampleCode,
             Lab_Qualifier = ifelse(Result == "ND", paste0("U", Lab_Qualifier), Lab_Qualifier)
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
