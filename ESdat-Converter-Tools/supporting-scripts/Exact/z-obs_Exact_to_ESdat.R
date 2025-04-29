## ---------------------------
## Script name:         Exact_to_ESdat.R  
## Purpose of script:   Convert KCEL EDDs to 
## Author:              N. VandePutte
## Date Created:        2024-09-16
## Date Updated:        2024-09-16
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

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/Exact/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name

  #TODO: incorporate into config.yaml
  sites <- c("AS-IN", "AS-OUT")
  
## Import files
  # Raw files by lab
  exact_files <- list.files("./data/Exact/data_raw", full.names = TRUE, pattern = "Exact")
  ari_files <- list.files("./data/Exact/data_raw", full.names = TRUE, pattern = "*4.csv")
  
  ## Retrieve lab report names
  lab_reports <- unique(substring(list.files("./data/Exact/data_raw", pattern = "*.csv"), 1, 8))
  
  ## Combine Exact and ARI EDDs if applicable, move and rename for consistency
  for(i in 1:length(lab_reports)){
    lab_report <- lab_reports[i]
    
    exact_report <- grep(lab_report, exact_files, value = TRUE) 
    exact_chem <- read.csv(exact_report[1]) %>%
      mutate(Result_Unit = ifelse(Result_Unit == "/100mL", "CFU/100mL", Result_Unit)) # convert bacteria units
    exact_sample <- read.csv(exact_report[2])
    exact_sample <- exact_sample %>%
      filter(SampleCode != "") %>%
      distinct()
    
    ## if there's an ARI file, combine with Exact file
    if (any(grep(lab_report, ari_files))) {
      ari_report <- grep(lab_report, ari_files, value = TRUE)
      ari_chem <- read.csv(ari_report[1])
      ari_sample <- read.csv(ari_report[2]) %>%
        mutate(Lab_Report_Number = lab_report, # assigns Exact lab report number to ARI samples
               Field_ID = ifelse(grepl(paste0(sites, collapse = "|"), Field_ID), # cuts out extra numbers in ARI ID
                                 substring(Field_ID, 16),                        # not dynamic, assumes same # characters
                                 Field_ID)) 
      
      full_chem <- full_join(exact_chem, ari_chem) %>%
        mutate(Total_or_Filtered = ifelse(OriginalChemName == "Ortho Phosphorus", "F",
                                          case_when(Total_or_Filtered == TRUE ~ "T",
                                             Total_or_Filtered == FALSE ~ "F")))
      
      write_csv(full_chem, paste0("./data/EXact/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), na = "")
      
      full_sample <- full_join(exact_sample, ari_sample)
      full_sample <- distinct(full_sample) %>%
        mutate(Sample_Type = ifelse(grepl("QA", Field_ID), "Field_D", Sample_Type),              # assign field duplicates
               Parent_Sample = ifelse(grepl("QA", Field_ID),                                     # only works with the "*-QA" naming convention
                                      substring(Field_ID, 1, nchar(Field_ID)-3), Parent_Sample))
      
      write_csv(full_sample, paste0("./data/EXact/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = "")
      
    } else {
      exact_chem <- exact_chem %>%
        mutate(Total_or_Filtered = case_when(Total_or_Filtered == TRUE ~ "T",
                                             Total_or_Filtered == FALSE ~ "F"))
      
      write_csv(exact_chem, paste0("./data/EXact/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), na = "")
      
      exact_sample <- distinct(exact_sample) %>%
        mutate(Sample_Type = ifelse(grepl("QA", Field_ID), "Field_D", Sample_Type),              # assign field duplicates
               Parent_Sample = ifelse(grepl("QA", Field_ID),                                     # only works with the "*-QA" naming convention
                                      substring(Field_ID, 1, nchar(Field_ID)-3), Parent_Sample))
      
      write_csv(exact_sample, paste0("./data/EXact/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = "")
    }
  }
  
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("./data/Exact/data_raw", full.names = TRUE, pattern = "*.pdf")
  names <- list.files("./data/Exact/data_raw", pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("./data/Exact/data_secondary/", proj_num, ".", names[i], ".ESdat.pdf"))
    }
