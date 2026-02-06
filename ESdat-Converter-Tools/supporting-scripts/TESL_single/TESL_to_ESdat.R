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
  files <- list.files("data/TESL/data_raw", full.names = TRUE, pattern = "*.xls")
  # sample data
  sampdata <- lapply(files, function(files)read_xls(files, sheet = "SAMPDATA"))
  # qc sample data
  qcdata <- lapply(files, function(files)read_xls(files, sheet = "QCDATA"))
  # Chem codes
  chem_codes <- read.csv("ESdat-Converter-Tools/supporting-scripts/TESL/chem_code_lookup.csv") # TODO update original chem names with TESL values

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/TESL/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  proj_site     <- config$project_info$project_site
  
  # locations <- read.csv("ESdat_locations.csv") %>%
  #   filter(Site_ID == proj_site)
  
## Retrieve lab report names
  lab_reports <- substring(list.files("data/TESL/data_raw", pattern = "*.xls"), 1, 7) # TODO adjust to TESL report names

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(files)){
  # Set dataframes for iteration
    df <-sampdata[[i]]
    qc <-qcdata[[i]]
  # Assign lab report
    lab_report <- lab_reports[i]
  # Sample CSV dataframe building
    sample <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", LABSAMPID), # Required
             Sampled_Date_Time = SAMPDATE,
             Field_ID = SAMPLENAME,
             Site_ID = proj_site,
             Location_Code = sub("^(([^_]*_){1}[^_]*).*", "\\1", SAMPLENAME), # everything before 2nd underscore in Sample name, specific to CEC proj
             Matrix_Type = MATRIX, # Required
             Sample_Type = ifelse(grepl("DUP", SAMPLENAME), "Field_D", ifelse(grepl("QA", SAMPLENAME), "Field_B", "Normal")), # Required
             Parent_Sample = "", # TODO see if parent sample is required for field dups when uploading
             SDG = lab_report, # Required
             Lab_Name = "TESL", # Required
             Lab_SampleID = LABSAMPID, # Required
             #Lab_Comments = "",
             Lab_Report_Number = lab_report, # Required
             .keep = "none")
    
    #sample <- left_join(sample, locations)
  # QC samples
    qcsample <- qc %>%
      mutate(SampleCode = paste0(lab_report, "_", LABSAMPID), # Required
             Matrix_Type = MATRIX, # Required
             Sample_Type = QCTYPE, # TODO match TESL codes to ESdat codes
             Parent_Sample = ifelse(SOURCEID != "" | !is.na(SOURCEID), paste0(lab_report, "_", SOURCEID), ""),
             SDG = lab_report, # Required
             Lab_Name = "TESL", # Required
             Lab_SampleID = LABSAMPID, # Required
             #Lab_Comments = "",
             Lab_Report_Number = lab_report, # Required
             .keep = "none")
    
    sample <- full_join(sample, qcsample)
    
  # Export Sample file
    write.csv(sample, paste0("data/TESL/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"))
  
  # Chemistry CSV dataframe building
    chemistry <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", LABSAMPID), # Required
             ChemCode = CASNUMBER, # Required
             OriginalChemName = ANALYTE, # Required
             Prefix = "",
             Result = Result, # Required
             Result_Unit = UNITS, # Required
             #Total_or_Filtered = if_else(grepl("Dissolved", Parameter.Name), "F", "T"),
             Result_Type = "REG", # Required
             Method_Type = METHODCODE, # Required
             Method_Name = METHODNAME, # Required
             Extraction_Method = PREPNAME,
             Extraction_Date = PREPDATE,
             Anaysed_Date = mdy_hm(ANADATE),
             Lab_Analysis_ID = LABSAMPID, # Required
             Lab_Preperation_Batch_ID = "", # Required
             Lab_Analysis_Batch_ID = "", # Required
             EQL = RL, # Required
             RDL = RL,
             MDL = DL,
             #ODL = "",
             Detection_Limit_Units = UNITS, # Required
             Lab_Comments = "",
             Lab_Qualifier = LNOTE,
             # UCL = "",
             # LCL = "",
             Dilution_Factor = DILUTION,
             # Spike_Concentration = "",
             # Spike_Measurement = "",
             # Spike_Units = "",
             .keep = "none")
    
    qcchem <- qc %>%
      mutate(SampleCode = paste0(lab_report, "_", LABSAMPID), # Required
             ChemCode = CASNUMBER, # Required
             OriginalChemName = ANALYTE, # Required
             Prefix = "",
             Result = Result, # Required
             Result_Unit = UNITS, # Required
             #Total_or_Filtered = if_else(grepl("Dissolved", Parameter.Name), "F", "T"),
             Result_Type = ifelse(SURROGATE == TRUE, "SUR", "REG"), # Required
             Method_Type = METHODCODE, # Required
             Method_Name = METHODNAME, # Required
             Extraction_Method = PREPNAME,
             Extraction_Date = PREPDATE,
             Anaysed_Date = mdy_hm(ANADATE),
             Lab_Analysis_ID = LABSAMPID, # Required
             Lab_Preperation_Batch_ID = "", # Required
             Lab_Analysis_Batch_ID = "", # Required
             EQL = RL, # Required
             RDL = RL,
             MDL = DL,
             #ODL = "",
             Detection_Limit_Units = UNITS, # Required
             Lab_Comments = "",
             Lab_Qualifier = LNOTE,
             UCL = UPPERCL,
             LCL = LOWERCL,
             Dilution_Factor = DILUTION,
             Spike_Concentration = SPIKELEVEL,
             Spike_Measurement = RECOVERY,
             Spike_Units = "%",
             .keep = "none")
    
    # TODO update chem code lookup with TESL values
    chemistry <- merge(chemistry, chem_codes, by = "OriginalChemName", all.x = TRUE) %>%
      mutate(ChemCode = ChemCode.y, .after = SampleCode) %>%
      select(-c(ChemCode.x, ChemCode.y))
    
  # Export Chemistry file
    write.csv(chemistry, paste0("data/TESL/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"))
  }
  
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("data/TESL/data_raw", full.names = TRUE, pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("data/TESL/data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
