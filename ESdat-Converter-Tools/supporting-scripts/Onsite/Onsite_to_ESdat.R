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
  
## Import files
  # Raw files by lab
  Onsite_files <- list.files("./data/Onsite/data_raw", full.names = TRUE, pattern = "HERR")
  Amtest_files <- list.files("./data/Onsite/data_raw", full.names = TRUE, pattern = "AmTest")
  #Enthalpy_files <- list.files("./data/Onsite/data_raw", full.names = TRUE, pattern = "Enthalpy")
  
  chem_lookup <- read.csv("ESdat-Converter-Tools/supporting-scripts/Onsite/chem_code_lookup.csv")
  locations <- read.csv("ESdat_locations.csv") %>%
    filter(Project_ID == proj_num)
  
  ## Retrieve lab report names
  lab_reports <- unique(substring(list.files("./data/Onsite/data_raw", pattern = "*.xls"), 1, 8))
  
  ## Combine Onsite and AmTest EDDs if applicable, move and rename for consistency
  for(i in 1:length(lab_reports)){
    lab_report <- lab_reports[i]
    report_files <- grep(lab_report, Onsite_files, value = TRUE)
    onsite_report <- read_xls(report_files[1])
    
    if (length(report_files) > 1) {
      for (k in 2:length(grep(lab_report, Onsite_files))){
        onsite_report <- rbind(onsite_report, read_xls(report_files[k]))
        }
    }
    
    # onsite_report <- onsite_report %>%
    #   # filter out empty rows
    #   filter(!(SDG == "") & !is.na(SDG)) %>%
    #   # make total or filtered a character column
    #   mutate(Total.or.Filtered = as.character(ifelse(Total.or.Filtered == TRUE, "T", "F")))
    
    # select and rename relevant columns for chemistry file
    onsite_chem <- onsite_report %>%
      mutate(SampleCode = ifelse(`Matrix Type` == "TCLP Extract", paste0(`Sample Code`, "_TCLP"), `Sample Code`),
             OriginalChemName = `Original Chem Name`,
             ChemCode = `Chem Code`,
             Prefix = Prefix,
             Result = Result,
             Result_Unit = ifelse(grepl("mg eqt. CaCO3/L", `Result Unit`), "mg/L", `Result Unit`),
             Total_or_Filtered = `Total or Filtered`,
             Result_Type = `Result Type`,
             Method_Type = `Method Type`,
             Method_Name = `Method Name`,
             Extraction_Date = `Extraction Date`,
             Analysed_Date = `Analysed Date`,
             Lab_Analysis_ID = substr(`Lab Analysis ID`, start=1, stop=20),
             Lab_Preperation_Batch_ID = substr(`Lab Preperation Batch ID`, start=1, stop=20),
             Lab_Analysis_Batch_ID = substr(`Lab Analysis Batch ID`, start=1, stop=20),
             EQL = EQL,
             RDL = RDL,
             MDL = MDL,
             ODL = NA,
             Detection_Limit_Units = ifelse(grepl("mg eqt. CaCO3/L", `EQL Units`), "mg/L", `EQL Units`),
             Lab_Comments = `Lab Comments...51`,
             Lab_Qualifier = `Lab Qualifier`,
             UCL = UCL,
             LCL = LCL,
             Dilution_Factor = `Dilution Factor`,
             Spike_Concentration = `Spike Concentration`,
             Spike_Units = ifelse(grepl("mg eqt. CaCO3/L", `Spike Units`), "mg/L", `Spike Units`),
             Spike_Measurement = `Spike Measurement`,
             .keep = "none")
    # join with lookup for chemcode
    #onsite_chem <- left_join(onsite_chem, chem_lookup)
    
    # select and rename relevant columns for sample file
    onsite_sample <- onsite_report %>%
      mutate(SampleCode = ifelse(`Matrix Type` == "TCLP Extract", paste0(`Sample Code`, "_TCLP"), `Sample Code`),
             Sampled_Date_Time = `Sampled Date Time`,
             #Site_ID = proj_site,
             Field_ID = `Field ID`,
             Depth = str_match(`Field ID`, "S-\\d+-(\\d+)")[,2],
             Matrix_Type = ifelse(`Matrix Type` == "TCLP Extract", "Other", `Matrix Type`),
             Sample_Type = `Sample Type`,
             Parent_Sample = ifelse(`Sample Type`== 'LAB_D', gsub(' DUP', '', `Sample Code`), `Parent Sample`),
             SDG = SDG,
             Lab_Name = `Lab Name`,
             Lab_SampleID = `Lab Sample ID`,
             Lab_Comments = `Lab Comments...37`,
             Lab_Report_Number = `Lab Report Number`,
             .keep = 'none') %>%
      distinct(SampleCode, .keep_all = TRUE)
    
    ## if there's an associated Amtest file, combine with Onsite file
    if (any(grep(lab_report, Amtest_files))) {
      
      amtest_report <- read.csv(grep(lab_report, Amtest_files, value = TRUE))
      
      if (names(amtest_report)[1]=="Study_ID"){
        # if EIM format
        amtest_chem <- amtest_report %>%
          mutate(SampleCode = Sample_ID,
                 OriginalChemName = Result_Parameter_Name,
                 Prefix = as.character(case_when(Result_Value == "ND" ~ "<",
                                                 Result_Data_Qualifier == "U" ~ "<",
                                                 .default = "")),
                 Result = as.numeric(ifelse(Result_Value=="ND", Result_Reporting_Limit, gsub(",","",Result_Value))),
                 Result_Unit = Result_Value_Units,
                 Total_or_Filtered = "T",
                 Result_Type = "REG",
                 Method_Type = Result_Method,
                 Method_Name = Result_Method,
                 Extraction_Date = NA,
                 Analysed_Date = parse_date_time(paste0(Lab_Analysis_Date, " ", Lab_Analysis_Time), 
                                                 orders = c("%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S %p")),
                 Lab_Analysis_ID = Sample_ID,
                 Lab_Preperation_Batch_ID = NA,
                 Lab_Analysis_Batch_ID = NA,
                 EQL = Result_Reporting_Limit,
                 RDL = Result_Reporting_Limit,
                 MDL = Result_Detection_Limit,
                 ODL = NA,
                 Detection_Limit_Units = Result_Value_Units,
                 Lab_Comments = NA,
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
        
        # if other format
        amtest_sample <- amtest_report %>%
          mutate(SampleCode = Sample_ID,
                 Sampled_Date_Time = parse_date_time(paste0(Field_Collection_Start_Date, " ", 
                                                            trimws(Field_Collection_Start_Time)), 
                                                     orders = c("%m/%d/%Y %H:%M")),
                 Field_ID = Study_Specific_Location_ID,
                 #Site_ID = proj_site,
                 Depth = "",#str_match(Client.ID, "S-\\d+-(\\d+)")[,2],
                 Matrix_Type = 'Water',
                 Sample_Type = "Normal",
                 Parent_Sample = NA,
                 SDG = lab_report,
                 Lab_Name = 'AmTest',
                 Lab_SampleID = Sample_ID,
                 Lab_Comments = NA,
                 Lab_Report_Number = lab_report,
                 .keep = 'none') %>%
          distinct()
      } else {
      # # if other format
      amtest_chem <- amtest_report %>%
        mutate(SampleCode = AmTest.ID,
               OriginalChemName = Parameter,
               Prefix = as.character(case_when(Result == "ND" ~ "<",
                                               Qualifier == "U" ~ "<",
                                               .default = "")),
               Result = as.numeric(ifelse(Result=="ND", Det..Lim., Result)),
               Result_Unit = units,
               Total_or_Filtered = "T",
               Result_Type = "REG",
               Method_Type = Method,
               Method_Name = Method,
               Extraction_Date = NA,
               Analysed_Date = parse_date_time(Date.Analyzed, orders = c("%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S %p")),
               Lab_Analysis_ID = AmTest.ID,
               Lab_Preperation_Batch_ID = NA,
               Lab_Analysis_Batch_ID = NA,
               EQL = Det..Lim.,
               RDL = NA,
               MDL = Det..Lim.,
               ODL = NA,
               Detection_Limit_Units = units,
               Lab_Comments = NA,
               Lab_Qualifier = Qualifier,
               UCL = NA,
               LCL = NA,
               Dilution_Factor = NA,
               Spike_Concentration = NA,
               Spike_Units = NA,
               Spike_Measurement = NA,
               .keep = "none")
      # join with lookup for chemcode
      amtest_chem <- left_join(amtest_chem, chem_lookup)

      # if other format
      amtest_sample <- amtest_report %>%
        mutate(SampleCode = AmTest.ID,
               Sampled_Date_Time = parse_date_time(paste0(Date.Sampled, " ", trimws(Time.Sampled)), orders = c("%m/%d/%Y %H:%M:%S %p")),
               Field_ID = Client.ID,
               #Site_ID = proj_site,
               Depth = str_match(Client.ID, "S-\\d+-(\\d+)")[,2],
               Matrix_Type = 'Water',
               Sample_Type = "Normal",
               Parent_Sample = NA,
               SDG = lab_report,
               Lab_Name = 'AmTest',
               Lab_SampleID = AmTest.ID,
               Lab_Comments = NA,
               Lab_Report_Number = lab_report,
               .keep = 'none') %>%
        distinct()
      }
      # enthalpy_chem <- NULL
      # enthalpy_sample <- NULL
      # # if Enthalpy report 
      # if (any(grep(lab_report, Enthalpy_files))){
      #   enthalpy_report <- read_xls(grep(lab_report, Enthalpy_files, value = TRUE))
      #   
      #   enthalpy_chem <- enthalpy_report %>%
      #     mutate(SampleCode = paste0(lab_report, "_", `Sample ID`),
      #            OriginalChemName = `Result Parameter Name`,
      #            ChemCode = `Result Parameter CAS Number`,
      #            Prefix = as.character(case_when(grepl("U", `Result Data Qualifier`) ~ "<",
      #                                            .default = "")),
      #            Result = as.numeric(gsub(",", "", `Result Value`)),
      #            Result_Unit = `Result Value Units`,
      #            Total_or_Filtered = as.character("T"),
      #            Result_Type = "REG",
      #            Method_Type = `Result Method`,
      #            Method_Name = `Result Method`,
      #            #Extraction_Date = "",
      #            Analysed_Date = mdy_hms(paste(`Lab Analysis Date`, " ", `Lab Analysis Time`)),
      #            Lab_Analysis_ID = paste0(lab_report, "_", ifelse(`Sample ID`=="Method Blank", "MB", `Sample ID`)),
      #            Lab_Preperation_Batch_ID = NA,
      #            Lab_Analysis_Batch_ID = NA,
      #            EQL = as.numeric(`Result Reporting Limit`),
      #            RDL = as.numeric(`Result Reporting Limit`),
      #            MDL = as.numeric(`Result Detection Limit`),
      #            ODL = NA,
      #            Detection_Limit_Units = `Result Value Units`,
      #            Lab_Comments = NA,
      #            Lab_Qualifier = `Result Data Qualifier`,
      #            UCL = NA,
      #            LCL = NA,
      #            Dilution_Factor = NA,
      #            Spike_Concentration = NA,
      #            Spike_Units = NA,
      #            Spike_Measurement = NA,
      #            .keep = "none")
      #   # join with lookup for chemcode
      #   #amtest_chem <- left_join(amtest_chem, chem_lookup)
      #   
      #   enthalpy_sample <- enthalpy_report %>%
      #     mutate(SampleCode = paste0(lab_report, "_", `Sample ID`),
      #            Sampled_Date_Time = mdy_hms(paste0(`Field Collection Start Date`, " ", `Field Collection Start Time`)),
      #            Field_ID = `Sample ID`,
      #            #Site_ID = proj_site,
      #            Depth = NA,
      #            Matrix_Type = ifelse(`Sample Matrix` == 'Solid/Sediment', 'Solid', `Sample Matrix`),
      #            Sample_Type = ifelse(`Sample ID`=="Method Blank", "MB","Normal"),
      #            Parent_Sample = NA,
      #            SDG = lab_report,
      #            Lab_Name = `Result Lab Name`,
      #            Lab_SampleID = `Sample ID`,
      #            Lab_Comments = NA,
      #            Lab_Report_Number = lab_report,
      #            .keep = 'none') %>%
      #     distinct()
      # }
      # 
      # if (!is.null(enthalpy_chem)){
      #   full_chem <- full_join(onsite_chem, amtest_chem) %>%
      #     full_join(enthalpy_chem)
      #   } else {
          full_chem <- full_join(onsite_chem, amtest_chem)
      # }
      
      write.csv(full_chem, paste0("./data/Onsite/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), na = '')
      
      # if (!is.null(enthalpy_sample)){
      #   full_sample <- full_join(onsite_sample, amtest_sample) %>%
      #     full_join(enthalpy_sample)
      # } else {
        full_sample <- full_join(onsite_sample, amtest_sample) %>%
          mutate(Sample_Type = ifelse(grepl("QA", Field_ID), "Field_D", Sample_Type))
      # }
      
      ### assign field ID
      if (proj_num == "19-07202-000"){
        samp_date<-gsub("-", "", substring(mdy_hm(onsite_sample$Sampled_Date_Time)[1], 1, 10))
        locations <- locations %>%
          mutate(Field_ID = paste0(Field_ID, "-", samp_date))
      } else {
        locations <- locations %>%
          mutate(Field_ID = Location_Code)
      }
      
      ## join for location codes
      full_sample<-left_join(full_sample, locations) %>%
        mutate(Location_Code = ifelse(is.na(Location_Code), '', Location_Code))
      
      write.csv(full_sample, paste0("./data/Onsite/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = '')
      
    } else {
      
      ## assign field ID
      if (proj_num == "19-07202-000"){
        samp_date<-gsub("-", "", substring(mdy_hm(onsite_sample$Sampled_Date_Time)[1], 1, 10))
        locations <- locations %>%
          mutate(Field_ID = paste0(Field_ID, "-", samp_date))
      } else {
        locations <- locations %>%
          mutate(Field_ID = Location_Code)
      }
     
       ## join for location codes
      onsite_sample <- left_join(onsite_sample, locations) %>%
        mutate(Location_Code = ifelse(is.na(Location_Code), '', Location_Code))
      
      write.csv(onsite_chem, paste0("./data/Onsite/data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"), na = '')
      
      write.csv(onsite_sample, paste0("./data/Onsite/data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"), na = '')
    }
  }
  
## Import PDF lab reports and copy to secondary folder
  pdfs <- list.files("./data/Onsite/data_raw", full.names = TRUE, pattern = "*.pdf")
  names <- list.files("./data/Onsite/data_raw", pattern = "*.pdf")
  for (i in 1:length(pdfs)){
    file.copy(pdfs[i], paste0("./data/Onsite/data_secondary/", proj_num, ".", names[i], ".ESdat.pdf"))
    }
