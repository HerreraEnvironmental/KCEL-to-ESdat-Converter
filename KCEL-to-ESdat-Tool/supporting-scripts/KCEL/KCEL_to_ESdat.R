## ---------------------------
## Script name:         KCEL_to_ESdat.R  
## Purpose of script:   Convert KCEL EDDs to 
## Author:              N. Harris
## Date Created:        2024-08-21
## Date Updated:        2024-08-21
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
  files <- list.files("./supporting-scripts/KCEL/data_raw", full.names = TRUE, pattern = "*.csv")
  dfs <- lapply(files, read.csv)
  # Chem codes
  chem_codes <- read.csv("supporting-scripts/KCEL/chem_code_lookup.csv")

## Import config.yaml file
  config        <- read_yaml("supporting-scripts/KCEL/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  
## Retrieve lab report names
  lab_reports <- substring(list.files("./supporting-scripts/KCEL/data_raw", pattern = "*.csv"), 1, 6)

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(files)){
  # Set dataframe for iteration
    df <-dfs[[i]]
  # Assign lab report
    lab_report <- lab_reports[i]
  # Sample CSV dataframe building
    sample <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", Lab.ID),
             Sampled_Date_Time = Collect.Date,
             Field_ID = "",
             Blank1 = "",
             Depth = Depth.m.,
             Blank2 = "",
             Matrix_Type = "Water",
             Sample_Type = "Normal",
             Parent_Sample = "",
             Blank3 = "",
             SDG = lab_report,
             Lab_Name = "KCEL",
             Lab_SampleID = Lab.ID,
             Lab_Comments = "",
             Lab_Report_Number = lab_report,
             .keep = "none")
  # Export Sample file
    write.csv(sample, paste0("./supporting-scripts/KCEL/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"))
  
  # Chemistry CSV dataframe building
    chemistry <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", Lab.ID),
             ChemCode = "",
             OriginalChemName = Parameter.Name,
             Prefix = "",
             Result = Result,
             Result_Unit = Units,
             Total_or_Filtered = "",
             Result_Type = "REG",
             Method_Type = "",
             Method_Name = Method,
             Extraction_Date = Preparation.Date,
             Anaysed_Date = Analysis.Date,
             Lab_Analysis_ID = Lab.ID,
             Lab_Preperation_Batch_ID = "",
             Lab_Analysis_Batch_ID = "",
             EQL = RDL,
             RDL = RDL,
             MDL = MDL,
             ODL = "",
             Detection_Limit_Units = Units,
             Lab_Comments = "",
             Lab_Qualifier = Qualifier,
             UCL = "",
             LCL = "",
             Dilution_Factor = DF,
             Spike_Concentration = "",
             Spike_Measurement = "",
             Spike_Units = "",
             .keep = "none")
    
    chemistry <- merge(chemistry, chem_codes, by = "OriginalChemName", all.x = TRUE) %>%
      mutate(ChemCode = ChemCode.y, .after = SampleCode) %>%
      select(-c(ChemCode.x, ChemCode.y))
  # Export Chemistry file
    write.csv(chemistry, paste0("./supporting-scripts/KCEL/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"))
  }
  
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("./supporting-scripts/KCEL/data_raw", full.names = TRUE, pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("./supporting-scripts/KCEL/data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
