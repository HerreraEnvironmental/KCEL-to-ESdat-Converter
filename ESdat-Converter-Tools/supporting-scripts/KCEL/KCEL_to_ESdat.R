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

# run the QC file converter
if(any(grep("QC.xlsx", list.files("data/KCEL/data_raw")))){
  source("ESdat-Converter-Tools/supporting-scripts/KCEL/KCEL_QC.R", local = T)
}
## Import files
  # Raw files
  files <- list.files("data/KCEL/data_raw", full.names = TRUE, pattern = "*.csv")
  dfs <- lapply(files, read.csv)
  
  # Chem codes
  library(readr)
  chem_codes <- read_csv("ESdat-Converter-Tools/supporting-scripts/KCEL/chem_code_lookup.csv")

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/KCEL/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  
## Retrieve lab report names
  lab_reports <- substring(list.files("data/KCEL/data_raw", pattern = "*.csv"), 1, 6)

## Iterate through lab reports and create Sample and Chemistry CSV files
  for(i in 1:length(files)){
  # Set dataframe for iteration
    df <-dfs[[i]]
    
    field_id <- ""
    if ("Textvalue" %in% colnames(df)){
      field_id <- df %>%
        filter(Parameter.Name=="Client Locator") %>%
        select(Lab.ID, Textvalue) %>%
        rename(Field_ID = Textvalue,
          Lab_SampleID = Lab.ID)

      df <- df %>%
        filter(Parameter.Name != "Client Locator")
    }
  # Assign lab report
    lab_report <- lab_reports[i]
    
  # read in QC data
    qc_flag <- FALSE
    if(file.exists(paste0("data/KCEL/data_secondary/", lab_report, "_QC.ESdatSample.csv"))){
      qc_flag <- TRUE
      qc_sample <- read.csv(paste0("data/KCEL/data_secondary/", lab_report, "_QC.ESdatSample.csv"))
      qc_chem <- read.csv(paste0("data/KCEL/data_secondary/", lab_report, "_QC.ESdatChemistry.csv"))
    }
  
  # Sample CSV dataframe building
    sample <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", Lab.ID),
             Sampled_Date_Time = Collect.Date,
             Field_ID = Lab.ID,
             Depth = Depth.m.,
             Matrix_Type = "Water",
             Sample_Type = "Normal",
             Parent_Sample = "",
             SDG = lab_report,
             Lab_Name = "KCEL",
             Lab_SampleID = Lab.ID,
             Lab_Comments = "",
             Lab_Report_Number = lab_report,
             .keep = "none") %>%
      distinct()

   if (is.data.frame(field_id)){
     sample <- sample %>%
       select(-Field_ID) %>%
       full_join(field_id)
   }
    
  # join QC
    if(qc_flag == TRUE){
    qc_sample <- qc_sample %>%
      filter(!SampleCode %in% sample$SampleCode)
    sample <- full_join(sample, qc_sample)
    }
  # Export Sample file
    write.csv(sample, paste0("data/KCEL/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), 
              row.names = FALSE, na = "")
  
  # Chemistry CSV dataframe building
    chemistry <- df %>%
      mutate(SampleCode = paste0(lab_report, "_", Lab.ID),
             ChemCode = "",
             OriginalChemName = Parameter.Name,
             Prefix = ifelse(Qualifier=="<MDL", "<", ""),
             Result = ifelse(Qualifier=="<MDL", MDL, Result),
             Result_Unit = ifelse(Units=="mg CaCO3/L", "mg/L", 
                ifelse(Units=="% Volume", "%", Units)),
             Total_or_Filtered = if_else(grepl("Dissolved", Parameter.Name), "F", "T"),
             Result_Type = "REG",
             Method_Type = "",
             Method_Name = trimws(Method),
             Extraction_Date = parse_date_time(Preparation.Date, orders = c("%m-%d-%Y %H:%M", "%d-%b-%y %H:%M:s", "%d-%b-%y %H:%M")),
             Analysed_Date = parse_date_time(Analysis.Date, orders = c("%m-%d-%Y %H:%M", "%d-%b-%y %H:%M:s", "%d-%b-%y %H:%M")),
             Lab_Analysis_ID = Lab.ID,
             Lab_Preperation_Batch_ID = "",
             Lab_Analysis_Batch_ID = "",
             EQL = RDL,
             RDL = RDL,
             MDL = MDL,
             ODL = "",
             Detection_Limit_Units = ifelse(Units=="mg CaCO3/L", "mg/L", 
                ifelse(Units=="% Volume", "%", Units)),
             Lab_Comments = "",
             Lab_Qualifier = Qualifier,
             UCL = as.numeric(NA),
             LCL = as.numeric(NA),
             Dilution_Factor = DF,
             Spike_Concentration = as.numeric(NA),
             Spike_Measurement = as.numeric(NA),
             Spike_Units = NA,
             .keep = "none")
    # remove unnecessary fields
    chemistry <- chemistry %>%
      filter(!OriginalChemName %in% c('Field Personnel', 'Sampling Method', 'Sample Function', 'Sample Depth'))
    
    chemistry <- merge(chemistry, chem_codes, by = "OriginalChemName", all.x = TRUE) %>%
      mutate(ChemCode = ChemCode.y, .after = SampleCode) %>%
      select(-c(ChemCode.x, ChemCode.y))
  # join QC
    if(qc_flag == TRUE){
    qc_chem <- qc_chem %>%
      mutate(Total_or_Filtered = ifelse(Total_or_Filtered == "Filtered", "F", "T"))
    chemistry <- full_join(chemistry, qc_chem) %>%
      distinct(ChemCode, SampleCode, Total_or_Filtered, Result_Type, Method_Name, Lab_Analysis_ID, 
               .keep_all = TRUE)
    }
  # Export Chemistry file
    write.csv(chemistry, paste0("data/KCEL/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), 
              row.names = FALSE, na = "")
  }
  
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("data/KCEL/data_raw", full.names = TRUE, pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("data/KCEL/data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
