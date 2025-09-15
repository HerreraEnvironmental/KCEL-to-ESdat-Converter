## ---------------------------
## Script name:         EXAMPLE_to_ESdat.R  
## Purpose of script:   Template for converting lab EDDs to ESdat format
## Author:              N. VandePUtte
## Date Created:        2025-03-11
## Date Updated:        2025-03-11
## Project #:           N/A
## Task # (optional):   N/A
## ---------------------------
## Notes: This script is ran in conjunction with the ESdat_prep.R and ESdat_header.R scripts
##
##    *** Have you updated the config.yaml file? Please do so before running any scripts ***
##  
##  Make a copy of the Template folder and rename for the lab you are converting from
##  Find and replace "EXAMPLE" with the folder name
##  Create folders in the data folder: data/EXAMPLE/data_raw and data/EXAMPLE/data_secondary
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
  # edit file extension / function depending on file type (csv or xls/xlsx, read.csv or read_excel)
  files <- list.files("data/EXAMPLE/data_raw", full.names = TRUE, pattern = "*.csv")
  dfs <- lapply(files, read.csv)
  # Chem codes 
  # Update OriginalChemName column with parameter names from your lab's EDD
  chem_codes <- read.csv("ESdat-Converter-Tools/supporting-scripts/EXAMPLE/chem_code_lookup.csv")

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/EXAMPLE/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  proj_site     <- config$project_info$project_site
  
## Retrieve lab report names
  # Assumes lab report number is in file name. Update character numbers based on file names
  lab_reports <- substring(list.files("data/EXAMPLE/data_raw", pattern = "*.csv"), 1, 6)

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(files)){
  # Set dataframe for iteration
    df <-dfs[[i]]
  # Assign lab report
    lab_report <- lab_reports[i]
  # Sample CSV dataframe building
    sample <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", Lab.ID), # Required  # replace Lab.ID with appropriate column
             Sampled_Date_Time = Collect.Date,                         # replace Collect.Date with appropriate column
             Field_ID = Locator,                                       # replace Locator with appropriate column
             Site_ID = proj_site,
             Location_Code = "",                                       # must match location codes in ESdat. write a function or fill in manually in ESdat
             Depth = Depth.m.,                                         # replace Depth.m with appropriate column
             Matrix_Type = c("Soil", "Water", "Gas",       # Required  # pick one, or insert appropriate column
                             "SoilGas", "other"),          
             Sample_Type = "Normal",                       # Required  # usually Normal, unless QA sample-Update accordingly
             Parent_Sample = "",                                       # only for duplicates or matrix spikes-update accordingly
             SDG = lab_report,                             # Required
             Lab_Name = "EXAMPLE",                         # Required  # replace with lab name
             Lab_SampleID = Lab.ID,                        # Required  # replace Lab.ID with appropriate column
             Lab_Comments = "",                                        # add appropriate column or remove
             Lab_Report_Number = lab_report,               # Required
             .keep = "none")
    
  # Export Sample file
    write.csv(sample, paste0("data/EXAMPLE/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"))
  
  # Chemistry CSV dataframe building
    chemistry <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", Lab.ID), # Required  # replace Lab.ID with appropriate column
             ChemCode = "",                                # Required  # add appropriate column, or remove and join with chem_code_lookup.csv 
             OriginalChemName = Parameter.Name,            # Required  # replace Parameter.Name with appropriate column
             Prefix = "",                                  # Required if below the detection limit (< or >)
             Result = Result,                              # Required  # replace Result with appropriate column
             Result_Unit = Units,                          # Required  # replace Units with appropriate column
             Total_or_Filtered = "",                       # Required  # Write a function - "T" for Total, "F" for filtered. "T" is default
             Result_Type = "REG",                          # Required  # usually REG, unless surrogate or spiked sample
             Method_Type = "",                             # Required  # PAH, pesticides, inorganic, metals, etc.
             Method_Name = Method,                         # Required  # Method code - replace Method with appropriate column
             Extraction_Method = Preparation.Method,                   # Replace Preparation.Method
             Extraction_Date = Preparation.Date,                       # Replace Preparation.Date
             Anaysed_Date = Analysis.Date,                             # Replace Analysis.Date
             Lab_Analysis_ID = Lab.ID,                     # Required  # Replace Lab.ID
             Lab_Preperation_Batch_ID = "",                # Required  # add appropriate column, or just indicate the lab report number
             Lab_Analysis_Batch_ID = "",                   # Required  # add appropriate column, or just indicate the lab report number
             EQL = RDL,                                    # Required  # reporting or detection limit, whichever is available
             RDL = RDL,                                                # reporting limit
             MDL = MDL,                                                # detection limit
             ODL = "",                                                 # other detection limit
             Detection_Limit_Units = Units,                # Required  # Replace Units
             Lab_Comments = "",                                        # add appropriate column or remove
             Lab_Qualifier = Qualifier,                                # replace Qualifier appropriate column
             UCL = "",                                                 # Upper confidence limit for QA recoveries
             LCL = "",                                                 # lower confidence limit for QA recoveries
             Dilution_Factor = DF,                                     # replace DF with appropriate column or remove
             Spike_Concentration = "",                                 # for QA samples
             Spike_Measurement = "",                                   # measured concentration of spike or surrogate in QA sample
             Spike_Units = "",                                         # units for spike concentration and measurement
             .keep = "none")
    
    # if getting ChemCode from lookup file
    chemistry <- merge(chemistry, chem_codes, by = "OriginalChemName")
    
  # Export Chemistry file
    write.csv(chemistry, paste0("data/EXAMPLE/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"))
  }
  
## if you want to upload lab report PDFs
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("data/EXAMPLE/data_raw", full.names = TRUE, pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("data/EXAMPLE/data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
