## ---------------------------
## Script name:         Onsite_to_ESdat.R  
## Purpose of script:   Convert Onsite EDDs to 
## Author:              N. VandePutte
## Date Created:        2024-11-19
## Date Updated:        2024-11-19
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
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/Onsite/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name
  proj_site     <- config$project_info$project_site

  #TODO: incorporate into config.yaml
  #sites <- c("AS-IN", "AS-OUT")
  
## Import files
  # Raw files by lab
  Onsite_files <- list.files("./data/Onsite/data_raw", full.names = TRUE, pattern = "HERR")
  Amtest_files <- list.files("./data/Amtest/data_raw", full.names = TRUE, pattern = "AmTest")
  
  chem_lookup <- read.csv("ESdat-Converter-Tools/supporting-scripts/Onsite/chem_code_lookup.csv")
  locations <- read.csv("ESdat_locations.csv") %>%
    filter(Site_ID == proj_site)
  
  ## Retrieve lab report names
  lab_reports <- unique(substring(list.files("./data/Onsite/data_raw", pattern = "*.csv"), 1, 8))
  
  ## Combine Onsite and AmTest EDDs if applicable, move and rename for consistency
  for(i in 1:length(lab_reports)){
    lab_report <- lab_reports[i]
    
    onsite_report <- read.csv(grep(lab_report, Onsite_files, value = TRUE))

    onsite_report <- onsite_report %>%
      # filter out empty rows
      filter(!(SDG == "") & !is.na(SDG)) %>%
      # make total or filtered a character column
      mutate(Total.or.Filtered = as.character(ifelse(Total.or.Filtered == TRUE, "T", "F")))
    
    # select and rename relevant columns for chemistry file
    onsite_chem <- onsite_report %>%
      mutate(SampleCode = Sample.Code,
             OriginalChemName = Original.Chem.Name,
             Prefix = Prefix,
             Result = Result,
             Result_Unit = Result.Unit,
             Total_or_Filtered = Total.or.Filtered,
             Result_Type = Result.Type,
             Method_Type = Method.Type,
             Method_Name = Method.Name,
             Extraction_Date = Extraction.Date,
             Analysed_Date = Analysed.Date,
             Lab_Analysis_ID = substr(Lab.Analysis.ID, start=1, stop=20),
             Lab_Preperation_Batch_ID = substr(Lab.Preperation.Batch.ID, start=1, stop=20),
             Lab_Analysis_Batch_ID = substr(Lab.Analysis.Batch.ID, start=1, stop=20),
             EQL = EQL,
             RDL = RDL,
             MDL = MDL,
             ODL = NA,
             Detection_Limit_Units = EQL.Units,
             Lab_Comments = Lab.Comments,
             Lab_Qualifier = Lab.Qualifier,
             UCL = UCL,
             LCL = LCL,
             Dilution_Factor = Dilution.Factor,
             Spike_Concentration = Spike.Concentration,
             Spike_Units = ifelse(Spike.Units=="mg eqt. CaCO3/L (ppm)", "mg/L", Spike.Units),
             Spike_Measurement = Spike.Measurement,
             .keep = "none")
    # join with lookup for chemcode
    onsite_chem <- left_join(onsite_chem, chem_lookup)
    
    # select and rename relevant columns for sample file
    onsite_sample <- onsite_report %>%
      mutate(SampleCode = Sample.Code,
             Sampled_Date_Time = Sampled.Date.Time,
             Site_ID = proj_site,
             Field_ID = Field.ID,
             Blank1 = NA,
             Depth = NA,
             Blank2 = NA,
             Matrix_Type = Matrix.Type,
             Sample_Type = Sample.Type,
             Parent_Sample = Parent.Sample,
             Blank3 = NA,
             SDG = SDG,
             Lab_Name = Lab.Name,
             Lab_SampleID = Lab.Sample.ID,
             Lab_Comments = Lab.Comments,
             Lab_Report_Number = Lab.Report.Number,
             .keep = 'none')
    onsite_sample <- distinct(onsite_sample)
    
    ## if there's an associated Amtest file, combine with Onsite file
    if (any(grep(lab_report, Amtest_files))) {
      
      amtest_report <- read.csv(grep(lab_report, Amtest_files, value = TRUE))
      
      amtest_chem <- amtest_report %>%
        mutate(SampleCode = Sample_ID,
               OriginalChemName = Result_Parameter_Name,
               Prefix = as.character(case_when(Result_Data_Qualifier == "U" ~ "<", 
                                               .default = "")),
               Result = Result_Value,
               Result_Unit = Result_Value_Units,
               Total_or_Filtered = "T",
               Result_Type = "REG",
               Method_Type = Result_Method,
               Method_Name = Result_Method,
               Extraction_Date = NA,
               Analysed_Date = Lab_Analysis_Date,
               Lab_Analysis_ID = Sample_ID,
               Lab_Preperation_Batch_ID = NA,
               Lab_Analysis_Batch_ID = NA,
               EQL = Result_Detection_Limit,
               RDL = Result_Reporting_Limit,
               MDL = Result_Detection_Limit,
               ODL = NA,
               Detection_Limit_Units = Result_Value_Units,
               Lab_Comments = Result_Comment,
               Lab_Qualifier = Result_Data_Qualifier,
               UCL = NA,
               LCL = NA,
               Dilution_Factor = NA,
               Spike_Concentration = NA,
               Spike_Units = NA,
               Spike_Measurement = NA,
               .keep = "none")
      # join with lookup for chemcode
      amtest_chem <- left_join(amtest_chem, chem_lookup)
      
      amtest_sample <- amtest_report %>%
        mutate(SampleCode = Sample_ID,
               Sampled_Date_Time = paste0(Field_Collection_Start_Date, " ", Field_Collection_Start_Time),
               Field_ID = Study_Specific_Location_ID,
               Site_ID = proj_site,
               Blank1 = NA,
               Depth = NA,
               Blank2 = NA,
               Matrix_Type = ifelse(Sample_Matrix == 'W', 'Water', Sample_Matrix),
               Sample_Type = "Normal",
               Parent_Sample = NA,
               Blank3 = NA,
               SDG = lab_report,
               Lab_Name = Result_Lab_Name,
               Lab_SampleID = Sample_ID,
               Lab_Comments = NA,
               Lab_Report_Number = lab_report,
               .keep = 'none')
      amtest_sample <- distinct(amtest_sample)
      
      full_chem <- full_join(onsite_chem, amtest_chem)
      
      write.csv(full_chem, paste0("./data/Onsite/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"))
      
      full_sample <- full_join(onsite_sample, amtest_sample) %>%
        mutate(Sample_Type = ifelse(grepl("QA", Field_ID), "Field_D", Sample_Type))
      
      ## assign field ID
      samp_date<-gsub("-", "", substring(mdy_hm(onsite_sample$Sampled_Date_Time)[1], 1, 10))
      locations <- locations %>%
        mutate(Field_ID = paste0(Field_ID, "-", samp_date))
      
      ## join for location codes
      full_sample<-left_join(full_sample, locations) %>%
        mutate(Location_Code = ifelse(is.na(Location_Code), '', Location_Code))
      
      write.csv(full_sample, paste0("./data/Onsite/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = '')
      
    } else {
      
      ## assign field ID
      samp_date<-gsub("-", "", substring(mdy_hm(onsite_sample$Sampled_Date_Time)[1], 1, 10))
      locations <- locations %>%
        mutate(Field_ID = paste0(Field_ID, "-", samp_date))
     
       ## join for location codes
      onsite_sample <- left_join(onsite_sample, locations) %>%
        mutate(Location_Code = ifelse(is.na(Location_Code), '', Location_Code))
      
      write.csv(onsite_chem, paste0("./data/Onsite/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"))
      
      write.csv(onsite_sample, paste0("./data/Onsite/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = '')
    }
  }
  
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("./data/Onsite/data_raw", full.names = TRUE, pattern = "*.pdf")
  names <- list.files("./data/Onsite/data_raw", pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("./data/Onsite/data_secondary/", proj_num, ".", names[i], ".ESdat.pdf"))
    }
