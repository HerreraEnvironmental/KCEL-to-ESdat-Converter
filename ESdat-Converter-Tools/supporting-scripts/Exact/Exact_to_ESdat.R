## ---------------------------
## Script name:         Exact_to_ESdat.R  
## Purpose of script:   Converting Exact EDDs to ESdat format
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
  files <- list.files("data/Exact/data_raw", full.names = TRUE, pattern = "*.xls")
  dfs <- lapply(files, read_xls)
  # Chem codes 
  # Update OriginalChemName column with parameter names from your lab's EDD
  chem_codes <- read.csv("ESdat-Converter-Tools/supporting-scripts/Exact/chem_code_lookup.csv")

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/Exact/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  proj_site     <- config$project_info$project_site
  samp_matrix   <- config$project_info$sample_matrix
  
## Retrieve lab report names
  # TODO not sure if this is a typical file name, might need adjustment
  lab_reports <- gsub("LandscapeExcelExport_", "",
                 gsub(".xls", "", list.files("data/Exact/data_raw", pattern = "*.xls")))

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(files)){
  # Set dataframe for iteration
    df <-dfs[[i]]
  # filter out 
    df <- df %>% 
      filter(analyte_name %in% chem_codes$OriginalChemName)
  # Assign lab report
    lab_report <- lab_reports[i]
  # Sample CSV dataframe building
    sample <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", `Lab Sample Number`), # Required
             Sampled_Date_Time = `Date Sampled`,
             Depth = "",
             Field_ID = `Sample Description`,
             Site_ID = proj_site,
             Location_Code = sub("^(([^-]*-){1}[^-]*).*", "\\1", `Sample Description`),
             Matrix_Type =  samp_matrix,                   # Required
             Sample_Type = ifelse(grepl("QA", `Sample Description`), "Field_D", "Normal"), # Required
             Parent_Sample = "",                                       # only for duplicates or matrix spikes-update accordingly
             SDG = lab_report,                             # Required
             Lab_Name = "Exact",                           # Required  
             Lab_SampleID = `Lab Sample Number`,           # Required 
             Lab_Comments = comments,
             Lab_Report_Number = lab_report,               # Required
             .keep = "none") %>%
      distinct()
    
  # Export Sample file
    write.csv(sample, paste0("data/Exact/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), 
              na = "", row.names = FALSE)
  
  # Chemistry CSV dataframe building
    chemistry <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", `Lab Sample Number`), # Required
             #ChemCode = "",                               # Required 
             OriginalChemName = analyte_name,              # Required 
             Prefix = ifelse(grepl("<", result), "<", ""), # Required
             Result = as.numeric(gsub("<", "", result)),   # Required  
             Result_Unit = units,                          # Required  
             Total_or_Filtered = as.character(ifelse(grepl("Dissolved", analyte_name)|analyte_name == "Orthophosphate-P", "F", "T")), # Required  
             Result_Type = "REG",                          # Required
             Method_Type = test_group_name,                # Required 
             Method_Name = analytical_method_name,         # Required
             Extraction_Method = "",                   
             Extraction_Date = "",                       
             Anaysed_Date = date_analyzed,                             
             Lab_Analysis_ID = `Lab Sample Number`,        # Required  
             Lab_Preperation_Batch_ID = lab_report,        # Required  
             Lab_Analysis_Batch_ID = lab_report,           # Required 
             EQL = mdl,                                    # Required
             RDL = "",                                               
             MDL = mdl,                                                
             ODL = "",                                               
             Detection_Limit_Units = units,                # Required  
             Lab_Comments = comments,                                        
             Lab_Qualifier = qualifier,                                
             UCL = "",                                                 # Upper confidence limit for QA recoveries
             LCL = "",                                                 # lower confidence limit for QA recoveries
             Dilution_Factor = "",                                     # replace DF with appropriate column or remove
             Spike_Concentration = "",                                 # for QA samples
             Spike_Measurement = "",                                   # measured concentration of spike or surrogate in QA sample
             Spike_Units = "",                                         # units for spike concentration and measurement
             .keep = "none")
    
    # merge with chem_code lookup
    chemistry <- merge(chemistry, chem_codes, by = "OriginalChemName")
    
  # Export Chemistry file
    write.csv(chemistry, paste0("data/Exact/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), 
              na = "", row.names = FALSE)
  }
  
## if you want to upload lab report PDFs
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("data/Exact/data_raw", full.names = TRUE, pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("data/Exact/data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
