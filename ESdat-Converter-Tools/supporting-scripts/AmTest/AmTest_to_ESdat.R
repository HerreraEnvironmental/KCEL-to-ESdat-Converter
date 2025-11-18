## ---------------------------
## Script name:         AmTest_to_ESdat.R  
## Purpose of script:   Template for converting lab EDDs to ESdat format
## Author:              N. VandePUtte
## Date Created:        2025-11-14
## Date Updated:        2025-11-14
## Project #:           N/A
## Task # (optional):   N/A
## ---------------------------
## Notes: This script is ran in conjunction with the ESdat_prep.R and ESdat_header.R scripts
##
##    *** Have you updated the config.yaml file? Please do so before running any scripts ***
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
  files <- list.files("data/AmTest/data_raw", full.names = TRUE, pattern = "*.csv")
  dfs <- lapply(files, read.csv)
  # Chem codes 
  chem_codes <- read.csv("ESdat-Converter-Tools/supporting-scripts/AmTest/chem_code_lookup.csv")

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/AmTest/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  proj_site     <- config$project_info$project_site
  
## Retrieve lab report names
  # Assumes lab report number is in file name. Update character numbers based on file names
  lab_reports <- substring(list.files("data/AmTest/data_raw", pattern = "*.csv"), 1, 8)

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(files)){
  # Set dataframe for iteration
    df <-dfs[[i]]
  # Assign lab report
    lab_report <- lab_reports[i]
  # Sample CSV dataframe building
    sample <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", Sample_ID), 
             Sampled_Date_Time = paste0(Field_Collection_Start_Date, "", Field_Collection_Start_Time),
             Field_ID = Study_Specific_Location_ID,
             # TODO: Site_ID = proj_site,
             Location_Code = "",                                
             Depth = "",                                  
             Matrix_Type = Sample_Source,          
             Sample_Type = "Normal",                        
             Parent_Sample = "",                          
             SDG = lab_report,                             
             Lab_Name = Result_Lab_Name,                           
             Lab_SampleID = Sample_ID,                        
             Lab_Comments = "",                                        
             Lab_Report_Number = lab_report,               
             .keep = "none")
    
  # Export Sample file
    write.csv(sample, paste0("data/AmTest/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"))
  
  # Chemistry CSV dataframe building
    chemistry <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", Sample_ID),
             ChemCode = "",                                # Required  # add appropriate column, or remove and join with chem_code_lookup.csv 
             OriginalChemName = Result_Parameter_Name,
             Prefix = "",                                  # Required if below the detection limit (< or >)
             Result = Result_Value,                       
             Result_Unit = Result_Value_Units,                      
             Total_or_Filtered = as.character(ifelse(Fraction_Analyzed=="Total", "T", "F")),
             Result_Type = "REG",
             Method_Type = "",                             # Required  # PAH, pesticides, inorganic, metals, etc.
             Method_Name = Result_Method,
             Extraction_Method = Digestion_Method,
             Extraction_Date = "",                       
             Analysed_Date = paste0(Lab_Analysis_Date, " ", Lab_Analysis_Time),
             Lab_Analysis_ID = Sample_ID,                     
             Lab_Preperation_Batch_ID = lab_report,                
             Lab_Analysis_Batch_ID = lab_report,                   
             EQL = Result_Reporting_Limit,                                    
             RDL = Result_Reporting_Limit,                                                
             MDL = Result_Detection_Limit,                                                
             ODL = "",                                                 
             Detection_Limit_Units = Result_Value_Units,                
             Lab_Comments = "",                                        
             Lab_Qualifier = Result_Data_Qualifier,                   
             UCL = "",                                                 
             LCL = "",                                                 
             Dilution_Factor = "",                                     
             Spike_Concentration = "",                                 
             Spike_Measurement = "",                                   
             Spike_Units = "",                                         
             .keep = "none")
    
    # if getting ChemCode from lookup file
    chemistry <- merge(chemistry, chem_codes, by = "OriginalChemName")
    
  # Export Chemistry file
    write.csv(chemistry, paste0("data/AmTest/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"))
  }
  
## if you want to upload lab report PDFs
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("data/AmTest/data_raw", full.names = TRUE, pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("data/AmTest/data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
