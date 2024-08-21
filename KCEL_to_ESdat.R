## script to format KCEL EDDs for ESdat upload

library(tidyverse)

# load in files
files <- list.files("./data_raw", full.names = TRUE, pattern = "*.csv")
dfs <- lapply(files, read.csv)

chem_codes <- read.csv("chem_code_lookup.csv")

proj_num <- "24-08319-000"
proj_ID <- "White Lake Monitoring"

# get lab report names
lab_reports <- substring(list.files("./data_raw", pattern = "*.csv"), 1, 6)

# loop through lab reports and create Sample and Chemistry csvs

for(i in 1:length(files)){
  
df <-dfs[[i]]
lab_report <- lab_reports[i]

sample <- df %>%
  mutate(SampleCode = paste0(lab_report, "_", Lab.ID),
         Sampled_Date_Time = Collect.Date,
         Field_ID = "",
         Blank1 = "",
         Depth = Depth.m.,
         Blank2 = "",
         Matrix_Type = "Water",
         Sample_Type = "Normal",
         Parent_Sample = "",
         Blank3 = "",
         SDG = lab_report,
         Lab_Name = "KCEL",
         Lab_SampleID = Lab.ID,
         Lab_Comments = "",
         Lab_Report_Number = lab_report,
         .keep = "none")

write.csv(sample, paste0("./data_secondary/", proj_num, ".", lab_report, ".ESdatSample.csv"))

chemistry <- df %>%
  mutate(SampleCode = paste0(lab_report, "_", Lab.ID),
         ChemCode = "",
         OriginalChemName = Parameter.Name,
         Prefix = "",
         Result = Result,
         Result_Unit = Units,
         Total_or_Filtered = "",
         Result_Type = "REG",
         Method_Type = "",
         Method_Name = Method,
         Extraction_Date = Preparation.Date,
         Anaysed_Date = Analysis.Date,
         Lab_Analysis_ID = Lab.ID,
         Lab_Preperation_Batch_ID = "",
         Lab_Analysis_Batch_ID = "",
         EQL = RDL,
         RDL = RDL,
         MDL = MDL,
         ODL = "",
         Detection_Limit_Units = Units,
         Lab_Comments = "",
         Lab_Qualifier = Qualifier,
         UCL = "",
         LCL = "",
         Dilution_Factor = DF,
         Spike_Concentration = "",
         Spike_Measurement = "",
         Spike_Units = "",
         .keep = "none")

chemistry <- merge(chemistry, chem_codes, by = "OriginalChemName", all.x = TRUE) %>%
  mutate(ChemCode = ChemCode.y, .after = SampleCode) %>%
  select(-c(ChemCode.x, ChemCode.y))

write.csv(chemistry, paste0("./data_secondary/", proj_num, ".", lab_report, ".ESdatChemistry.csv"))
}

pdfs <- list.files("./data_raw", full.names = TRUE, pattern = "*.pdf")
for (i in 1:length(pdfs)){
  file.copy(pdfs[i], paste0("./data_secondary/", proj_num, ".", lab_reports[i], ".ESdat.pdf"))
  }
