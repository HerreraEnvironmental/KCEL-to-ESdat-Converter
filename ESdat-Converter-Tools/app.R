#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinymaterial)
library(shinydashboard)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(DT)
library(plotly)
library(httr)
library(jsonlite)
library(stringr)
library(data.table)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
              title = "ESdat Convertor Tool Library",
              header = dashboardHeader(
                titleWidth = "100%",
                title = img(src = "HEC.png", height = 70)
                #itle = "Thurston County Water Quality",
                #tags$li(class = "dropdown",
                #        tags$style(".main-header .logo {padding-top: 10px}"),
                #        tags$style(".main-header {max-height: 90px}"),
                #        tags$style(".main-header .logo {height: 90px}")
                #)
              ),
              dashboardSidebar(
                collapsed = FALSE,
                #tags$style(".left-side, .main-sidebar {padding-top: 90px}"),
                #tags$style(HTML(".sidebar-menu li a { font-size: 16px; }")),
                width = 325,
                sidebarMenu(
                  #menuItem("Station Data & Status", tabName = "status", icon = icon("table")),
                  menuItem("KCEL to ESdat Tool", tabName = "KCEL", icon = icon("table"))
                  #menuItem("Hydrologic Data Summary", tabName = "hydro", icon = icon("water")),
                  #menuItem("Regressions", tabName = "reg", icon = icon("chart-line")),
                  #menuItem("PSD Charts", tabName = "PSD", icon = icon("chart-area")),
                  #menuItem("Advanced Statistics", tabName = "stats", icon = icon("calculator"))
                )
              ),
              dashboardBody(
                tags$head(
                  tags$link(
                    rel = "stylesheet",
                    type = "text/css",
                    href = "CSS.css")
                ),
                tags$head(tags$style(HTML(
                  '.myClass {
            font-size: 50px;
            line-height: 90px;
            text-align: right;
            font-family: "Franklin Gothic",sans-serif;
            padding: 0 20px;
            overflow: hidden;
            color: #1b435b;
      }
        '))),
                tags$script(HTML(paste('
          $(document).ready(function() {
            $("header").find("nav").append(\'<span class="myClass">', paste("ESdat Convertor Tool Library", sep=""), '</span>\');
          })
         '))),
                tags$head(
                  tags$style(HTML("
                        .column_w_bar {
                        border-right-color: #eb4034;
                        border-right-width: 1px;
                        border-right-style: solid;
                        }
                        ") # end HTML
                  ) # end tags$style
                ), # end tags$head
                tabItems(
                  
                  #tabItem(tabName = "status",
                  # fluidRow(column(12,
                  #                 tags$h2("Station Data & Status")
                  #
                  # )),
                  # hr(),
                  # fluidRow(column(12,
                  #                 tags$h3("Station Summary"))),
                  # fluidRow(column(3, align = "center", h2("BAY 1"),
                  #                 h3("ADS EcoStream"),
                  #                 style='margin-bottom:30px;border:1px solid; padding: 10px;'),
                  #          column(3, align = "center", h2("BAY 2"),
                  #                 h3("Hydro-Int UFF-EMC")),
                  #          column(3, align = "center", h2("BAY 2.5"),
                  #                 h3("AquaShield Aqua-Ponic")),
                  #          column(3, align = "center", h2("BAY 4"),
                  #                 h3("Modular Wetlands 360")))
                  #
                  # ),
                  tabItem(tabName = "KCEL",
                          fluidRow(column(12, 
                                          h2("King County Environmental Lab (KCEL) to ESdat Convertor Tool"),
                                          HTML("To begin, please fill out the inputs below and upload your desired file(s). Once inputs are filled out and file(s) are uploaded, click <b> Convert Files </b>. To ensure the files are converted properly, be sure to input <b> Project Name </b> as shown in <i> KCEL filename </i> and the <b> Project Number </b> that is used in ESdat for this project.")
                          )),
                          fluidRow(column(6, align = "left",
                                          h3("Project Inputs"),
                                          textInput("proj_name", "Enter Project Name"),
                                          textInput("proj_num", "Enter Project Number (as saved in ESdat)"),
                                          fileInput("my_files", "Upload KCEL EDD(s)", multiple = T),
                                          actionButton("convert_files", "Convert Files"),
                                          textOutput("my_csv_name"),
                                          br(),
                                          downloadButton("downloadData", label = "Download zip files"),
                                          br(),
                                          br(),
                                          actionButton("remove_files", "Clear Cached Files")
                          ),
                          column(6, align = "left",
                                 h3("File Conversion Status"),
                                 h4("Sites Loaded"),
                                 verbatimTextOutput("sites"),
                                 br(),
                                 h4("Parameters Loaded"),
                                 verbatimTextOutput("params"),
                                 br(),
                                 h4("Files Ready for Export"),
                                 verbatimTextOutput("convert_status"))
                          )
                  )
                )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(tidyverse)
  library(XML)
  library(zip)
  library(readr)
  observeEvent(input$remove_files, {
    myfiles <- list.files(tempdir(), full.names = T)
    suppressWarnings(file.remove(myfiles))
  })
  
  observeEvent(input$convert_files, {
    temp_dir = paste(tempdir(), "/", sep="")
    file.copy(input$my_files$datapath, paste(temp_dir, input$my_files$name, sep=""), recursive = F)
    cache_CSV <- list.files(temp_dir, pattern = "*.csv", full.names = T)
    #config <- reactive({
    #  yaml <- list(project_info = list(project_number = input$proj_num,
    #                                   project_name = input$proj_name))
    #  write_yaml(yaml, "supporting-scripts/KCEL/config.yaml")
    #})
    
    ## Import KCEL files
    # Raw files
    # Chem codes
    chem_codes <- read.csv("supporting-scripts/KCEL/chem_code_lookup.csv")
    
    ## Import config.yaml file
    #config        <- read_yaml("supporting-scripts/KCEL/config.yaml")
    proj_num      <- input$proj_num
    proj_ID       <- input$proj_name
    
    ## Retrieve lab report names
    lab_reports <- substring(list.files(temp_dir, pattern = "*.csv"), 1, 6)
    
    files <- list.files(tempdir(), full.names = TRUE, pattern = "*.csv")
    print(files)
    dfs <- lapply(files, read.csv)
    
    dates         <- str_sub(files, -15)
    dates         <- substring(dates, 0, 7)
    print(dates)
    dates         <- as.character(dmy(dates))
    


    # Iterate through lab reports and create Sample and Chemistry CSV files
    for(i in 1:length(files)){
      # Set dataframe for iteration
      df <-dfs[[i]]
      # Assign lab report
      lab_report <- lab_reports[i]
      # Sample CSV dataframe building
      sample <- df %>%
        mutate(SampleCode = paste0(lab_report, "_", Lab.ID),
               Sampled_Date_Time = Collect.Date,
               Field_ID = Locator,
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
      sample <- sample[!duplicated(sample),]
      # Export Sample file
      write.csv(sample, paste0(temp_dir, proj_num, ".", lab_report, ".ESdatSample.csv"))

      # Chemistry CSV dataframe building
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
      
      chemistry <- chemistry[!is.na(chemistry$Result),]
      
      locations <- unique(sample$Field_ID)
      parameters <- unique(chemistry$OriginalChemName)
      
      # Export Chemistry file
      write.csv(chemistry, paste0(temp_dir, proj_num, ".", lab_report, ".ESdatChemistry.csv"))
    }

    ## Import PDF lab reports and copy to secondary folder
    pdfs <- list.files(temp_dir, full.names = TRUE, pattern = "*.pdf")
    cache_PDF <- pdfs
    for (i in 1:length(pdfs)){
      file.copy(pdfs[i], paste0(temp_dir, proj_num, ".", lab_reports[i], ".ESdat.pdf"))
    }
    
    #file.copy(input$my_files$datapath, paste(temp_dir, input$my_files$name, sep=""), recursive = F)
    #source("supporting-scripts/KCEL/KCEL_to_ESdat.R", local = T)
    #source("supporting-scripts/KCEL/ESdat_header.R", local = T)
    files         <- list.files(temp_dir, pattern = "*.csv")
    print(dates)
    # Read in secondary files
   
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
      
      saveXML(doc, file = paste0(temp_dir, proj_num, ".", report_num, ".ESdatHeader.xml"))
      
    }
    
    
    ## Copy relevant files to lab report folders and zip files
    for(i in 1:length(lab_reports)){
      report_num <- lab_reports[i]
      ESdat_files <- grep(paste0(proj_num, ".", report_num), list.files(), value = TRUE)
      for(k in 1:length(ESdat_files)){
        if (dir.exists(paste0(temp_dir, report_num))){
        } else {dir.create(paste0(temp_dir, report_num))}
        file.copy(paste0(temp_dir, ESdat_files[k]), paste0(temp_dir, report_num, "/", ESdat_files[k]), overwrite = TRUE )
      }
      ## Change working directory to avoid internal folders in zipfile
      #my_wd<-getwd()
      #setwd(paste0(my_wd, "/supporting-scripts/KCEL/lab_reports/", report_num))
      #zip(paste0(temp_dir, report_num, ".zip"), files = list.files(), include_directories = FALSE)
      #setwd(my_wd)
      
    }
    
    file.remove(cache_CSV, temp_dir)
    file.remove(cache_PDF, temp_dir)
    
    loaded_files <- list.files(tempdir(), pattern = "*.csv|*.pdf|*.xml")
    
    output$convert_status <- renderText(paste(loaded_files, " ready for export", "\n", sep = ""))
    output$sites <- renderText(paste(gsub("'", "", locations), "\n", sep=""))
    output$params <- renderText(paste(parameters, "\n", sep=""))
  })

   output$downloadData <- downloadHandler(
    #my_file = zip(list.files(temp_dir, pattern = "*.csv"))
    #print(my_file)
      filename <- function() {
       paste(input$proj_num, ".zip", sep="")
      },
      content <- function(file) {
        zip(file, files = list.files(tempdir(), pattern = "*.csv|*.pdf|*.xml"), root = tempdir(), mode = "cherry-pick")
      },
      contentType = "application/zip"
    
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
