## ---------------------------
## Script name:         ESdat_header.R  
## Purpose of script:   Template to create ESdat headers from lab EDDs
## Author:              N. VandePutte
## Date Created:        2025-03-11
## Date Updated:        2025-03-11
## Project #:           N/A
## Task # (optional):   N/A
## ---------------------------
## Notes: This script is ran in conjunction with the ESdat_prep.R and KCEL_to_ESdat.R scripts
##
##    *** Have you updated the config.yaml file? Please do so before running any scripts ***
## 
## Find and replace EXAMPLE with your folder/lab name
## ---------------------------

## Required Packages (install as necessary)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(lubridate)
  library(tidyverse)
  library(XML)
  library(yaml)

  temp_dir <- tempdir()

## Import config.yaml file
  config        <- read_yaml("ESdat-Converter-Tools/supporting-scripts/EXAMPLE/config.yaml")
  proj_num      <- config$project_info$project_number
  proj_ID       <- config$project_info$project_name

## Import files
  # Get lab report names, change file extension as necessary
  files         <- list.files("data/EXAMPLE/data_raw", pattern = "*.csv")
  lab_reports   <- substring(files, 1, 6) # assumes report number is in file name, adjust character numbers as needed
  
  # Sets report date to today's date. Can write a function if you want the true report date, but not super important
  dates         <- as.character(Sys.Date())
  
  # Read in secondary files
  files         <- list.files("data/EXAMPLE/data_secondary")
  
  # Build XML based on project
  for (i in 1:length(lab_reports)){
    report_date <- dates[i]
    report_num <- lab_reports[i]
    sample_file <- grep(paste0(report_num, ".ESdatSample"), files, value = TRUE)
    chemistry_file <- grep(paste0(report_num, ".ESdatChemistry"), files, value = TRUE)
    
    # BUILD XML TREE
    doc = newXMLDoc()     
    root = newXMLNode("ESdat", doc = doc)
    
    xmlAttrs(root) = c(schemaVersion="1.0.1", 
                       fileType="eLabReport", 
                       generated="2013-05-09T10:20:17+10:00",
                       xmlns="http://www.escis.com.au/2013/XML")              
    report_node = newXMLNode("LabReport", parent=root)
    xmlAttrs(report_node) = c(Lab_Signatory = "Lab Signatory",     # Insert usual lab signatory           
                              Lab_Name = "EXAMPLE",
                              Project_Number = proj_num,
                              Project_ID = proj_num,
                              Date_Reported = report_date,
                              Lab_Report_Number = report_num)
    files_node = newXMLNode("Associated_Files", parent = report_node)
    xmlAttrs(files_node) = c(xmlns="http://www.escis.com.au/2013/XML/LabReport")
    file_1 = newXMLNode("File", parent = files_node)
    xmlAttrs(file_1) = c(File_Type = "ESdat_Sample_CSV", File_Name = sample_file)
    file_2 = newXMLNode("File", parent = files_node)
    xmlAttrs(file_2) = c(File_Type = "ESdat_Chemistry_CSV", File_Name = chemistry_file)
    
    copies_node = newXMLNode("Copies_Sent_To", parent = report_node)
    xmlAttrs(copies_node) = c(xmlns = "http://www.escis.com.au/2013/XML/LabReport")
    
    cocs_node = newXMLNode("eCoCs", parent = report_node)
    xmlAttrs(cocs_node) = c(xmlns = "http://www.escis.com.au/2013/XML/LabReport")
    ecoc = newXMLNode("eCoC", parent = cocs_node)
    xmlAttrs(ecoc) = c(CoC_Number = report_num)
    requests_node = newXMLNode("Lab_Requests", parent = ecoc)
    request = newXMLNode("Lab_Request", parent = requests_node)
    xmlAttrs(request) = c(Version = "1", Number = "1")
    
    saveXML(doc, file = paste0("data/EXAMPLE/data_secondary/", proj_num, ".", report_num, ".ESdatHeader.xml"))
    
  }
