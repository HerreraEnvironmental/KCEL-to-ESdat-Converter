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
  exact_files <- list.files("data/Exact/data_raw", full.names = TRUE, pattern = "*.xls")
  dfs <- lapply(exact_files, read_xls)
  
  ari_files <- list.files("./data/Exact/data_raw", full.names = TRUE, pattern = "*4.csv")
  
  # Chem codes 
  # Update OriginalChemName column with parameter names from your lab's EDD
  chem_codes <- read.csv("ESdat-Converter-Tools/supporting-scripts/Exact/chem_code_lookup.csv")

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/Exact/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  proj_site     <- config$project_info$project_site
  samp_matrix   <- config$project_info$sample_matrix
  
  # locations <- read.csv("ESdat_locations.csv") %>%
  #   filter(Project_ID == proj_num)
  
## Retrieve lab report names
  lab_reports <- unique(substring(list.files("./data/Exact/data_raw", pattern = "*.xls"), 1, 8))

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(exact_files)){
  # Set dataframe for iteration
    exact_df <-dfs[[i]]
  # filter out extras
    exact_df <- exact_df %>% 
      filter(analyte_name %in% chem_codes$OriginalChemName) %>%
      filter(!grepl("Analytical Resources Report", result))
    
  # Assign lab report
    lab_report <- lab_reports[i]
  # Sample CSV dataframe building
    exact_sample <- exact_df %>%
      mutate(SampleCode = paste0(lab_report, "_", `Lab Sample Number`), # Required
             Sampled_Date_Time = `Date Sampled`,
             Depth = "",
             Field_ID = `Sample Description`,
             Site_ID = proj_site,
             Location_Code = gsub("[0-9]+\\s[0-9]+", "", `Sample Description`), #sub("^(([^-]*-){1}[^-]*).*", "\\1", `Sample Description`),
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
  
  # Chemistry CSV dataframe building
    exact_chem <- exact_df %>%
      mutate(SampleCode = paste0(lab_report, "_", `Lab Sample Number`), # Required
             #ChemCode = "",                               # Required
             OriginalChemName = analyte_name,              # Required
             Prefix = ifelse(grepl("<", result), "<", ""), # Required
             Result = as.numeric(gsub("<", "", result)),   # Required
             Result_Unit = units,                          # Required
             Total_or_Filtered = ifelse(grepl("Dissolved", analyte_name)
                                              |analyte_name == "Orthophosphate-P", "F", "T"), # Required
             Result_Type = "REG",                          # Required
             Method_Type = test_group_name,                # Required
             Method_Name = analytical_method_name,         # Required
             Extraction_Method = "",
             Extraction_Date = "",
             Anaysed_Date = mdy_hms(date_analyzed),
             Lab_Analysis_ID = `Lab Sample Number`,        # Required
             Lab_Preperation_Batch_ID = lab_report,        # Required
             Lab_Analysis_Batch_ID = lab_report,           # Required
             EQL = as.numeric(mdl),                        # Required
             RDL = as.numeric(NA),
             MDL = as.numeric(mdl),
             ODL = "",
             Detection_Limit_Units = units,                # Required
             Lab_Comments = case_when(qualifier == "*Estimated Value below Quantitation limit" ~ qualifier,
                                      .default = comments),
             Lab_Qualifier = case_when(qualifier == "*Estimated Value below Quantitation limit" ~ "J",
                                       .default = qualifier),
             UCL = as.numeric(NA),                                                 # Upper confidence limit for QA recoveries
             LCL = as.numeric(NA),                                                 # lower confidence limit for QA recoveries
             Dilution_Factor = as.numeric(NA),                                     # replace DF with appropriate column or remove
             Spike_Concentration = as.numeric(NA),                                 # for QA samples
             Spike_Measurement = as.numeric(NA),                                   # measured concentration of spike or surrogate in QA sample
             Spike_Units = "",                                         # units for spike concentration and measurement
             .keep = "none")
  
    ## if there's an ARI file, combine with Exact file
    if (any(grep(lab_report, ari_files))) {
      ari_report <- grep(lab_report, ari_files, value = TRUE)
      ari_chem <- read.csv(ari_report[1]) %>% 
        mutate(Total_or_Filtered = ifelse(OriginalChemName == "Ortho Phosphorus", "F",
                                                                                case_when(Total_or_Filtered == TRUE ~ "T",
                                                                                          Total_or_Filtered == FALSE ~ "F"))) %>%
        select(-ChemCode)
        
      ari_sample <- read.csv(ari_report[2]) %>%
        mutate(Lab_Report_Number = lab_report) # assigns Exact lab report number to ARI samples
      
      full_chem <- full_join(exact_chem, ari_chem)
      
      # merge with chem_code lookup
      full_chem <- merge(full_chem, chem_codes, by = "OriginalChemName")
      
      write_csv(full_chem, paste0("./data/EXact/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), na = "")
      
      full_sample <- full_join(exact_sample, ari_sample)
      full_sample <- distinct(full_sample) %>%
        mutate(Sample_Type = ifelse(grepl("QA", Field_ID), "Field_D", Sample_Type),              # assign field duplicates
               Parent_Sample = ifelse(grepl("QA", Field_ID),                                     # only works with the "*-QA" naming convention
                                      substring(Field_ID, 1, nchar(Field_ID)-3), Parent_Sample))
      
      write.csv(full_sample, paste0("./data/EXact/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = "")
      
    }else {
      
      # merge with chem_code lookup
      exact_chem <- merge(exact_chem, chem_codes, by = "OriginalChemName")
      write.csv(exact_chem, paste0("./data/EXact/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), na = "")
      write.csv(exact_sample, paste0("./data/EXact/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = "")
      
    }
  }
  
## if you want to upload lab report PDFs
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("data/Exact/data_raw", full.names = TRUE, pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("data/Exact/data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
