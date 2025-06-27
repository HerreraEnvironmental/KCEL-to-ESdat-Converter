library(tidyverse)
library(readxl)
library(yaml)
library(janitor)

## ---- Data ----
files <- list.files("data/KCEL/data_raw", full.names = TRUE, pattern = "*.xlsx")

# Chem codes
chem_codes <- read.csv("ESdat-Converter-Tools/supporting-scripts/KCEL/chem_code_lookup.csv")

## Import config.yaml file
config <- read_yaml("ESdat-Converter-Tools/supporting-scripts/KCEL/config.yaml")
proj_num <- config$project_info$project_number
proj_ID <- config$project_info$project_name

## Retrieve lab report names
lab_reports <- substring(list.files("data/KCEL/data_raw", pattern = "*.xlsx"), 1, 6)

## ---- Start Loop ----
## Iterate through lab reports and create Sample and Chemistry CSV files
for (i in 1:length(files)) {
  # Set dataframe for iteration
  df <- read_xlsx(files[i], col_names = FALSE)
  # Assign lab report
  lab_report <- lab_reports[i]

  df <- df %>%
    # filter out empty rows & columns
    remove_empty(which = c('cols', 'rows')) %>%
    # assign sample types
    mutate(sample_type = case_when(substring(...1, 1, 3) == "LCS" ~ "LCS",
      substring(...1, 1, 2) == "LD" ~ "LAB_D",
      substring(...1, 1, 2) == "MB" ~ "MB",
      substring(...1, 1, 2) == "SB" ~ "SB", # TODO: SB is storage blank in esdat
      substring(...1, 1, 3) == "MS:" ~ "MS",
      substring(...1, 1, 3) == "MSD" ~ "MS_D",
      substring(...1, 1, 2) == "CS" ~ "CS",
      substring(...1, 1, 4) == "FREP" ~ "Field_D",
      substring(...1, 1, 2) == "PC" ~ "PC",
      substring(...1, 1, 2) == "NC" ~ "NC",
      substring(...1, 1, 2) == "BF" ~ "BF",
      substring(...1, 1, 2) == "AF" ~ "AF",
      .default = NA
    )) %>%
    fill(sample_type, .direction = "down")
  
  if (grepl("Bellevue", files[i])){
    # last 7 rows are just notes, skip them
    df %>%
      head(-7)
  }

  ### ---- LCS ----

  # extract lab control sample data
  lcs <- df %>%
    filter(sample_type == "LCS" & ...1 != "(Lab Control Sample)") %>%
    remove_empty(which = "cols")

  # rename columns
  colnames(lcs) <- c(
    "parameter", "MDL", "RDL", "units", "true_value", "lcs_value",
    "recovery", "qualifier", "limits", "Sample_Type"
  )

  # extract sample info
  lcs <- lcs %>%
    mutate(
      LCL = sub("--.*", "", limits),
      UCL = sub(".*--", "", limits),
      sample = str_match(parameter, "LCS:\\s*(.*?)\\s* ")[, 2],
      matrix = str_match(parameter, "Matrix:\\s*(.*)\\s* Listtype:")[, 2],
      listtype = str_match(parameter, "Listtype:\\s*(.*)\\s* Method:")[, 2],
      method = str_match(parameter, "Method:\\s*(.*)\\s* Project:")[, 2],
      Result_Unit = "%"
    ) %>%
    fill(c(sample, matrix, listtype, method), .direction = "down") %>%
    filter(!grepl("Parameter", parameter) &
      !grepl("LCS", parameter) &
      !grepl("Workgroup", parameter))

  # rename for ESdat
  lcs <- lcs %>%
    rename(
      OriginalChemName = parameter,
      Spike_Units = units,
      Spike_Concentration = true_value,
      Spike_Measurement = lcs_value,
      Result_Value = recovery,
      Lab_Qualifier = qualifier,
      SampleCode = sample,
      Matrix_Type = matrix,
      Method_Name = method
    )

  ### ---- Duplicates ----

  # extract lab and field dups
  # ecoli dups have different columns, so separate it
  dups <- df %>%
    filter(sample_type =="LAB_D" & ...1 != "(Lab Duplicate)" 
           #& ...1 != "Escherichia coli"
      ) %>%
    mutate(
      #LCL = sub("--.*", "", limits),
      #UCL = sub(".*--", "", limits),
      sample = str_match(...1, ":\\s*(.*?)\\s* ")[, 2],
      Parent_Sample = str_match(...1, " \\s*(.*?)\\s* Matrix:")[, 2],
      matrix = str_match(...1, "Matrix:\\s*(.*)\\s* Listtype:")[, 2],
      listtype = str_match(...1, "Listtype:\\s*(.*)\\s* Method:")[, 2],
      method = str_match(...1, "Method:\\s*(.*)\\s* Project:")[, 2],
      project = str_match(...1, "Project:\\s*(.*)\\s* Pkey:")[, 2]
    ) %>%
    fill(c(sample, Parent_Sample, matrix, listtype, method, project), 
         .direction = "down") %>%
    remove_empty(which = "cols")
  
  # extract ecoli dups
  ecoli_dup <- dups %>%
    filter(...1 == "Escherichia coli") %>%
    rename("parameter" = ...1, 
           "MDL" = ...2, 
           "RDL" = ...3, 
           "units" = ...4, 
           "samp_value" = ...5, 
           "ld_value" = ...6,
           "rlog" = ...7, 
           "precision" = ...8, 
           "qualifier" = ...9, 
           "Sample_Type" = sample_type)

# filter out e. coli from regular dups
  dups <- dups %>%
    filter(...1 != "Escherichia coli") %>%
    rename("parameter" = ...1, 
           "MDL" = ...2, 
           "RDL" = ...3, 
           "units" = ...4, 
           "samp_value" = ...5, 
           "ld_value" = ...6,
           "rpd" = ...7, 
           "qualifier" = ...8, 
           "limits" = ...9, 
           "Sample_Type" = sample_type)
  

  # join dups together
  dups <- full_join(dups, ecoli_dup)

  # extract sample info
  dups <- dups %>%
    mutate(
      LCL = sub("--.*", "", limits),
      UCL = sub(".*--", "", limits)
    ) %>%
    filter(!grepl("Parameter", parameter) &
      !grepl("LD", parameter) &
      !grepl("Workgroup", parameter))

  # rename for ESdat
  dups <- dups %>%
    rename(
      OriginalChemName = parameter,
      Result_Unit = units,
      Result_Value = ld_value,
      Lab_Qualifier = qualifier,
      SampleCode = sample,
      Matrix_Type = matrix,
      Method_Name = method
    )

  ### ---- Method Blanks ----

  # extract method blanks
  method_blank <- df %>%
    filter(sample_type == "MB" & ...1 != "(Method Blank)") %>%
    remove_empty(which = "cols")

  # rename columns
  colnames(method_blank) <- c(
    "parameter", "MDL", "RDL", "units", "mb_value",
    "qualifier", "Sample_Type"
  )

  # extract sample info
  method_blank <- method_blank %>%
    mutate(
      sample = str_match(parameter, "MB:\\s*(.*)\\s* Matrix:")[, 2],
      matrix = str_match(parameter, "Matrix:\\s*(.*)\\s* Listtype:")[, 2],
      listtype = str_match(parameter, "Listtype:\\s*(.*)\\s* Method:")[, 2],
      method = str_match(parameter, "Method:\\s*(.*)\\s* Project:")[, 2]
    ) %>%
    fill(c(sample, matrix, listtype, method), .direction = "down") %>%
    filter(!grepl("Parameter", parameter) &
      !grepl("MB", parameter) &
      !grepl("Workgroup", parameter))

  # rename for ESdat
  method_blank <- method_blank %>%
    rename(
      OriginalChemName = parameter,
      Result_Unit = units,
      Result_Value = mb_value,
      Lab_Qualifier = qualifier,
      SampleCode = sample,
      Matrix_Type = matrix,
      Method_Name = method
    )

  ### ---- Spike Blanks ----

  # extract spike blanks
  spike_blank <- df %>%
    filter(sample_type == "SB" & ...1 != "(Spike Blank, Method Blank)") %>%
    remove_empty(which = "cols")

  # rename columns
  colnames(spike_blank) <- c(
    "parameter", "MDL", "RDL", "units", "mb_value", "true_value",
    "sb_value", "recovery", "qualifier", "limits", "Sample_Type"
  )

  # extract sample info
  spike_blank <- spike_blank %>%
    mutate(
      LCL = sub("--.*", "", limits),
      UCL = sub(".*--", "", limits),
      sample = str_match(parameter, "SB:\\s*(.*)\\s* MB:")[, 2],
      method_blank = str_match(parameter, "MB:\\s*(.*)\\s* Matrix:")[, 2],
      matrix = str_match(parameter, "Matrix:\\s*(.*)\\s* Listtype:")[, 2],
      listtype = str_match(parameter, "Listtype:\\s*(.*)\\s* Method:")[, 2],
      method = str_match(parameter, "Method:\\s*(.*)\\s* Project:")[, 2],
      Result_Unit = "%"
    ) %>%
    fill(c(sample, method_blank, matrix, listtype, method), .direction = "down") %>%
    filter(!grepl("Parameter", parameter) &
      !grepl("SB", parameter) &
      !grepl("Workgroup", parameter))

  # rename for ESdat
  spike_blank <- spike_blank %>%
    rename(
      OriginalChemName = parameter,
      Spike_Units = units,
      Spike_Concentration = true_value,
      Spike_Measurement = sb_value,
      Result_Value = recovery,
      Lab_Qualifier = qualifier,
      SampleCode = sample,
      Matrix_Type = matrix,
      Method_Name = method,
      Parent_Sample = method_blank
    )

  ### ---- Matrix Spikes ----

  # extract matrix spikes
  matrix_spike <- df %>%
    filter(sample_type == "MS" & ...1 != "(Matrix Spike)") %>%
    remove_empty(which = "cols")

  # rename columns
  colnames(matrix_spike) <- c(
    "parameter", "MDL", "RDL", "units", "samp_value", "true_value",
    "ms_value", "recovery", "qualifier", "limits", "Sample_Type"
  )

  # extract sample info
  matrix_spike <- matrix_spike %>%
    mutate(
      LCL = sub("--.*", "", limits),
      UCL = sub(".*--", "", limits),
      sample = str_match(parameter, "MS:\\s*(.*?)\\s* ")[, 2],
      Parent_Sample = str_match(parameter, " \\s*(.*)\\s* Matrix:")[, 2],
      matrix = str_match(parameter, "Matrix:\\s*(.*)\\s* Listtype:")[, 2],
      listtype = str_match(parameter, "Listtype:\\s*(.*)\\s* Method:")[, 2],
      method = str_match(parameter, "Method:\\s*(.*)\\s* Project:")[, 2],
      project = str_match(parameter, "Project:\\s*(.*)\\s* Pkey:")[, 2],
      Result_Unit = "%"
    ) %>%
    fill(c(sample, Parent_Sample, matrix, listtype, method, project), .direction = "down") %>%
    filter(!grepl("Parameter", parameter) &
      !grepl("MS", parameter) &
      !grepl("Workgroup", parameter))

  # rename for ESdat
  matrix_spike <- matrix_spike %>%
    rename(
      OriginalChemName = parameter,
      Spike_Units = units,
      Spike_Concentration = true_value,
      Spike_Measurement = ms_value,
      Result_Value = recovery,
      Lab_Qualifier = qualifier,
      SampleCode = sample,
      Matrix_Type = matrix,
      Method_Name = method
    )

  ### ---- Matrix Spike Dups ----

  # extract MS duplicates
  ms_dup <- df %>%
    filter(sample_type == "MS_D" & ...1 != "(Matrix Spike Duplicate, Matrix Spike)")
  
  # rename columns
  colnames(ms_dup) <- c(
    "parameter", "MDL", "RDL", "units", "samp_value", "true_value1",
    "ms_value", "recovery1", "qualifier1", "limits1", "true_value2",
    "msd_value", "recovery2", "qualifier2", "rpd", "qualifier3",
    "limits2", "Sample_Type"
  )
  
  if (sum(ms_dup) != 0){
  # extract sample info
  ms_dup <- ms_dup %>%
    mutate(
      lcl1 = sub("--.*", "", limits1),
      ucl1 = sub(".*--", "", limits1),
      lcl2 = sub("--.*", "", limits2),
      ucl2 = sub(".*--", "", limits2),
      sample = str_match(parameter, "MSD:\\s*(.*?)\\s* MS:")[, 2],
      matrix_spike = str_match(parameter, "MS:\\s*(.*?)\\s* ")[, 2],
      Parent_Sample = str_match(parameter, "MS:WG[0-9\\-]+\\s*(.*?)\\s* Matrix:")[, 2],
      matrix = str_match(parameter, "Matrix:\\s*(.*)\\s* Listtype:")[, 2],
      listtype = str_match(parameter, "Listtype:\\s*(.*)\\s* Method:")[, 2],
      method = str_match(parameter, "Method:\\s*(.*)\\s* Project:")[, 2],
      project = str_match(parameter, "Project:\\s*(.*)\\s* Pkey:")[, 2],
      Result_Unit = "%"
    ) %>%
    fill(c(sample, matrix_spike, Parent_Sample, matrix, listtype, method, project), .direction = "down") %>%
    filter(!grepl("Parameter", parameter) &
      !grepl("MSD", parameter) &
      !grepl("Workgroup", parameter))

  # rename for ESdat
  ms_dup <- ms_dup %>%
    rename(
      OriginalChemName = parameter,
      Spike_Units = units,
      Spike_Concentration = true_value2,
      Spike_Measurement = msd_value,
      Result_Value = recovery2,
      Lab_Qualifier = qualifier3,
      SampleCode = sample,
      Matrix_Type = matrix,
      Method_Name = method
    ) %>%
    mutate(
      Parent_Sample = matrix_spike,
      .keep = "unused"
    ) %>%
    filter(!is.na(Result_Value))
  }

  # E. coli and Check Standard QC samples don't translate well to ESdat format. Ok to leave out
  # ### ---- E. coli
  #
  # ecoli <- testing %>%
  #   filter(sample_type %in% c("PC", "NC", "BF", "AF")) %>%
  #   select(-c(7:17))
  #
  # colnames(ecoli) <- c("parameter", "MDL", "RDL", "units", "Result_Value", "qualifier", "sample_type")
  #
  # ecoli <-  ecoli %>%
  #   mutate(sample = str_match(parameter, ":\\s*(.*?)\\s* Matrix:")[,2],
  #          matrix = str_match(parameter, "Matrix:\\s*(.*)\\s* Listtype:")[,2],
  #          listtype = str_match(parameter, "Listtype:\\s*(.*)\\s* Method:")[,2],
  #          method = str_match(parameter, "Method:\\s*(.*)\\s* Project:")[,2],
  #          project = str_match(parameter, "Project:\\s*(.*)\\s* Pkey:")[,2]) %>%
  #   fill(c(sample, matrix, listtype, method, project), .direction = "down") %>%
  #   filter(!grepl("Parameter", parameter)
  #          & !grepl("WG", parameter)
  #          & !grepl("Workgroup", parameter)
  #          & !grepl("Control", parameter)
  #          & !grepl("Membrane", parameter))
  #
  # ecoli <- ecoli %>%
  #   rename(OriginalChemName = parameter,
  #          Result_Units = units,
  #          Lab_Qualifier = qualifier,
  #          SampleCode = sample,
  #          Matrix_Type = matrix,
  #          Method_Name = method)
  #
  # ### ---- check standard
  #
  # check_standard <- testing %>%
  #   filter(sample_type == "CS" & ...1 != "(Check Standard)") %>%
  #   select(-c(10:17))
  #
  # colnames(check_standard) <- c("parameter", "MDL", "RDL", "units", "true_value", "cs_value",
  #                               "recovery", "qualifier", "limits", "Sample_Type")
  #
  # check_standard <-  check_standard %>%
  #   mutate(LCL = sub("--.*", "", limits),
  #          UCL = sub(".*--", "", limits),
  #          sample = str_match(parameter, "CS:\\s*(.*?)\\s* Matrix:")[,2],
  #          matrix = str_match(parameter, "Matrix:\\s*(.*)\\s* Listtype:")[,2],
  #          listtype = str_match(parameter, "Listtype:\\s*(.*)\\s* Method:")[,2],
  #          method = str_match(parameter, "Method:\\s*(.*)\\s* Project:")[,2],
  #          Result_Unit = '%') %>%
  #   fill(c(sample, matrix, listtype, method), .direction = "down") %>%
  #   filter(!grepl("Parameter", parameter)
  #          & !grepl("CS", parameter)
  #          & !grepl("Workgroup", parameter))
  #
  # check_standard <- check_standard %>%
  #   rename(OriginalChemName = parameter,
  #          Spike_Units = units,
  #          Spike_Concentration = true_value,
  #          Spike_Measurement = cs_value,
  #          Result_Value = recovery,
  #          Lab_Qualifier = qualifier,
  #          SampleCode = sample,
  #          Matrix_Type = matrix,
  #          Method_Name = method)

  # ---- Join ----

  # join all of the data frames
  qc_results <- lcs %>%
    full_join(dups) %>%
    full_join(method_blank) %>%
    full_join(matrix_spike) %>%
    full_join(ms_dup) %>%
    full_join(spike_blank)


  # extract parent samples for dups and matrix spikes
  # parent samples for MS_D and spike blanks are already in data
  parent_samples <- qc_results %>%
    filter(!is.na(Parent_Sample) & !Sample_Type %in% c("MS_D", "SB")) %>%
    select(c(
      OriginalChemName, MDL, RDL, Matrix_Type, Spike_Units, Result_Unit,
      Method_Name, samp_value, Parent_Sample, project, Sample_Type
    )) %>%
    mutate(
      Result_Value = samp_value,
      Result_Unit = ifelse(Result_Unit == "%", Spike_Units, Result_Unit),
      Sample_Type = "NCP"
    ) %>%
    rename(SampleCode = Parent_Sample) %>%
    filter(!grepl(lab_report, SampleCode))

  # join parent samples to df
  qc_results <- full_join(qc_results, parent_samples)

  # fill in MDL and flags for non detects
  qc_results <- qc_results %>%
    mutate(
      Prefix = ifelse(Result_Value == "<MDL" | Lab_Qualifier == "<MDL", "<", ""),
      Lab_Qualifier = ifelse(is.na(Lab_Qualifier) & Result_Value == "<MDL", "<MDL", Lab_Qualifier),
      Result_Value = case_when(Result_Value == "<MDL" ~ MDL,
        Lab_Qualifier == "<MDL" ~ MDL,
        .default = Result_Value
      )
    )

  # make sure number columns are numbers
  qc_results <- qc_results %>%
    mutate(
      MDL = as.numeric(MDL),
      RDL = as.numeric(RDL),
      Spike_Concentration = as.numeric(Spike_Concentration),
      Spike_Measurement = as.numeric(Spike_Measurement),
      Result_Value = as.numeric(Result_Value),
      LCL = as.numeric(LCL),
      UCL = as.numeric(UCL)
    )

  # ---- ESdat format ----

  # Sample CSV dataframe building
  sample <- qc_results %>%
    mutate(
      SampleCode = trimws(paste0(lab_report, "_", SampleCode)),
      Sampled_Date_Time = NA,
      Field_ID = NA,
      Depth = NA,
      Matrix_Type = "Water",
      # no spike blank option in ESdat, consider a LCS
      Sample_Type = ifelse(Sample_Type == "SB", "LCS", Sample_Type),
      Parent_Sample = ifelse(is.na(Parent_Sample), NA, paste0(lab_report, "_", Parent_Sample)),
      SDG = lab_report,
      Lab_Name = "KCEL",
      Lab_SampleID = SampleCode,
      Lab_Comments = NA,
      Lab_Report_Number = lab_report,
      .keep = "none"
    ) %>%
    distinct()

  # Export Sample file
  write.csv(sample, paste0("data/KCEL/data_secondary/", lab_report, "_QC.ESdatSample.csv"), na = "", row.names = FALSE)

  # Chemistry CSV dataframe building
  chemistry <- qc_results %>%
    mutate(
      SampleCode = trimws(paste0(lab_report, "_", SampleCode)),
      ChemCode = NA,
      OriginalChemName = OriginalChemName,
      Prefix = Prefix,
      Result = Result_Value,
      Result_Unit = Result_Unit,
      Total_or_Filtered = if_else(grepl("Dissolved", OriginalChemName), "Filtered", "Total"),
      Result_Type = ifelse(Sample_Type %in% c("Field_D", "LAB_D", "MB", "NCP"), "REG", "SC"),
      Method_Type = NA,
      Method_Name = Method_Name,
      Extraction_Date = NA,
      Anaysed_Date = NA,
      Lab_Analysis_ID = trimws(SampleCode),
      Lab_Preperation_Batch_ID = NA,
      Lab_Analysis_Batch_ID = NA,
      EQL = RDL,
      RDL = RDL,
      MDL = MDL,
      ODL = NA,
      Detection_Limit_Units = ifelse(Result_Unit == "%", Spike_Units, Result_Unit),
      Lab_Comments = NA,
      Lab_Qualifier = Lab_Qualifier,
      UCL = UCL,
      LCL = LCL,
      Dilution_Factor = NA,
      Spike_Concentration = Spike_Concentration,
      Spike_Measurement = Spike_Measurement,
      Spike_Units = ifelse(is.na(Spike_Concentration), NA, Spike_Units),
      .keep = "none"
    ) %>%
    filter(!is.na(Result)) %>%
    distinct()

  # add chem codes, filter out parameters not in our report
  chemistry <- merge(chemistry, chem_codes, by = "OriginalChemName", all.x = TRUE) %>%
    mutate(ChemCode = ChemCode.y, .after = SampleCode) %>%
    select(-c(ChemCode.x, ChemCode.y)) %>%
    filter(!is.na(ChemCode)) %>%
    distinct()

  # Export Chemistry file
  write.csv(chemistry, paste0("data/KCEL/data_secondary/", lab_report, "_QC.ESdatChemistry.csv"), na = "", row.names = FALSE)
}
