# script to create ESdat XML headers and copy files to zippable folders

library(tidyverse)
library(XML)

files <- list.files("./data_raw", pattern = "*.csv")
# get lab report names
lab_reports <- substring(files, 1, 6)

dates <- substring(files, 24, 30)
dates <- as.character(dmy(dates))
# dfs <- lapply(files, read.csv)
files <- list.files("./data_secondary")

proj_num <- "24-08319-000"
proj_ID <- "White Lake Monitoring"

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
xmlAttrs(report_node) = c(Lab_Signatory = "Meghan Elkey",
                          Lab_Name = "KCEL",
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


# qualifier_node = newXMLNode("Lab_Qualifiers", parent = report_node)
# xmlAttrs(qualifier_node) = c(xmlns = "http://www.escis.com.au/2013/XML/LabReport")
# qual_1 = newXMLNode("Lab_Qualifier", parent = qualifier_node)
# xmlAttrs(qual_1) = c(Description = "The concentration is below the detection limit", Code = "U")
# qual_2 = newXMLNode("Lab_Qualifier", parent = qualifier_node)
# xmlAttrs(qual_2) = c(Description = "The concentration is below the reporting limit", Code = "J")

saveXML(doc, file = paste0("./data_secondary/", proj_num, ".", report_num, ".ESdatHeader.xml"))

}
