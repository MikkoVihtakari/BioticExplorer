## Libraries required to run the app ####

require(shiny)

### Define operating system

if (Sys.info()["sysname"] == "Windows") {
  os <- "Win"
} else if (Sys.info()["sysname"] == "Darwin") {
  os <- "Mac"
} else if (Sys.info()["sysname"] == "Linux") {
  os <- "Linux"
}

### OS specific exceptions

if(os == "Linux") {
  if(!capabilities()["X11"] && capabilities()["cairo"]) {
    options(bitmapType = 'cairo')
  }
}

### Install missing packages

required.packages <- c("shiny" = TRUE, "shinyFiles" = TRUE, "shinydashboard" = TRUE, "DT" = TRUE, 
                       "data.table" = TRUE,  "tidyverse" = TRUE, "dtplyr" = TRUE, "devtools" = FALSE,
                       "leaflet" = TRUE, "leaflet.minicharts" = TRUE, "plotly" = TRUE, 
                       "openxlsx" = FALSE, "scales" = FALSE, "fishmethods" = FALSE, "viridis" = FALSE,
                       "mapview" = FALSE, "DBI" = FALSE) ## TRUE means that the package should be loaded. FALSE that the functions are used without loading the package

new.packages <- names(required.packages)[!(names(required.packages) %in% installed.packages()[,"Package"])]
if (length(new.packages) > 0) install.packages(new.packages)

sapply(names(required.packages)[required.packages], require, character.only = TRUE)

#### RstoxData
# (not loaded, use pointer when using the functions)

if (!"RstoxData" %in% installed.packages()[,"Package"]) {
  install.packages("RstoxData", repo="https://stoxproject.github.io/repo")
}

#### MonetDBLite
# (not loaded, use pointer when using the functions)

if (!"MonetDBLite" %in% installed.packages()[,"Package"]) {
  devtools::install_github("hannesmuehleisen/MonetDBLite-R")
}

## Source functions used by the app

source("R/other_functions.R", encoding = "utf-8")
source("R/processBiotic_functions.R", encoding = "utf-8")
source("R/figure_functions.R", encoding = "utf-8")
# source("R/sql_functions.R", encoding = "utf-8")


##____________________
## User interface ####

##..............
## Settings  and definitions ####

tagList(
  tags$head(
    tags$title("BioticExplorer"),
    tags$style(
      HTML(
        ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
      )
    ) 
  )
)

stationOverviewFigureList <- list("Species composition" = "speciesCompositionPlot", "Total catch weight" = "catchweightSumPlot",
                                  "Mean catch weight" = "catchweightMeanPlot", "Catch weight range" = "catchweightRangePlot",
                                  "Mean weight of specimen" = "catchIndMeanWeightPlot", "Mean number in catch" = "catchcountMeanPlot", 
                                  "Range of number in catch" = "catchcountRangePlot", "Total catch by gear type" = "gearCatchPlot", 
                                  "Station depth" = "stationDepthPlot", "Fishing depth of the six most dominant species" = "catchSpeciesWeightPlot"
)


stationMapList <- list("Total catch map" = "catchMap", "Catch composition map" = "catchCompMap")

dbPath <- "~/Desktop/IMR_db.monetdb" 
dbIndexPath <- "~/Desktop/dbIndex.rda"

if(file.exists(dbPath)) {
  message("dbPath found. Enabling server version.")
  if(file.exists(dbIndexPath)) {
    load(dbIndexPath, envir = .GlobalEnv)
    message("dbIndexPath found. Loading the database index.")
  }
} else {
  message("dbPath not found. Enabling desktop version.")
}

##............
## Header ####

header <- dashboardHeader(title = div(
  fluidRow(
    column(width = 1, 
           loadingLogo("https://www.hi.no", "logo.png", "logo_bw.png", 
                       height = "40px")), 
    column(width = 10, p("Biotic Explorer", align = "center"))
  )
),
dropdownMenu(type = "notifications", headerText = scan("VERSION", what = "character", quiet = TRUE),
             icon = icon("cog"), badgeStatus = NULL,
             notificationItem("Usage of Biotic Explorer", icon = icon("book"), status = "info", href = "https://github.com/MikkoVihtakari/BioticExplorer"),
             notificationItem("Data fields", icon = icon("question-circle"), status = "info", href = "http://www.imr.no/formats/nmdbiotic/v3/nmdbioticv3_en.html"),
             notificationItem("Data types and codes", icon = icon("question-circle"), status = "info", href = "https://hinnsiden.no/tema/forskning/PublishingImages/Sider/SPD-gruppen/H%C3%A5ndbok%205.0%20juli%202019.pdf#search=h%C3%A5ndbok%20pr%C3%B8vetaking"),
             notificationItem("Download NMD data", icon = icon("download"), status = "info", href = "https://datasetexplorer.hi.no/"),
             notificationItem("Data policy", icon = icon("creative-commons"), status = "info", href = "http://www.imr.no/filarkiv/2013/03/datapolitikk_nmd.pdf/nb-no")
)
)

##_____________
## Sidebar ####

sidebar <- dashboardSidebar(sidebarMenu(
  # Setting id makes input$tabs give the tabName of currently-selected tab
  id = "tabs",
  
  menuItem("Information", tabName = "info", icon = icon("info-circle")),
  
  menuItem("Load data & filter", tabName = "info", icon = icon("arrow-circle-up"),
           menuSubItem("From the database", tabName = "uploadDb", icon = icon("database")),
           menuSubItem("From files", tabName = "upload", icon = icon("file-code"))
           
  ),
  
  menuItem("Cruise overview", icon = icon("bar-chart-o"), tabName = "missionExamine"
  ),
  
  menuItem("Stations & catches", icon = icon("ship"),
           menuSubItem("Overview", tabName = "stnallOverview"),
           menuSubItem("Map of catches", tabName = "stnallMap"),
           menuSubItem("Examine data", tabName = "stnallExamine")
  ),
  
  menuItem("Individuals & ages", icon = icon("fish"),
           menuSubItem("Overview", tabName = "indallOverview"),
           menuSubItem("Species plots", tabName = "indallSpecies"),
           menuSubItem("Examine data", tabName = "indallExamine")
  ),
  
  menuItem("Hierarchical data tables", icon = icon("sort-by-attributes", lib = "glyphicon"),
           menuSubItem("Station data", icon = icon("bar-chart-o"), tabName = "fishstationExamine"),
           menuSubItem("Catch data", icon = icon("bar-chart-o"), tabName = "catchsampleExamine"),
           menuSubItem("Individual data", icon = icon("bar-chart-o"), tabName = "individualExamine"),
           menuSubItem("Age data", icon = icon("bar-chart-o"), tabName = "agedeterminationExamine")    
  ),
  
  menuItem("Download", icon = icon("arrow-circle-down"),
           menuSubItem("Data", tabName = "downloadDatasets", icon = icon("table")),
           menuSubItem("Figures", tabName = "exportFigures", icon = icon("file-image"))
  )
  
)
)

##..........
## Body ####

body <- 
  dashboardBody(
    tabItems(
      
      ## Info tab ####   
      
      tabItem("info", 
              
              fluidRow(
                column(width = 12,
                       h1("Welcome to the Biotic Explorer", align = "center"),
                       br(),
                       p("This is a", a("Shiny app", href = "http://shiny.rstudio.com"), "allowing data exploration of the Institute of Marine Research's database (IMR). The app can also be used to examine Norwegian Maritime Data-center (NMD) Biotic v3 .xml files. Click the", strong("'Load data & filter'"), "tab on the side panel to get started. Note that the IMR logo turns to a 'BUSY' symbol when the app is processing information. Avoid clicking anything while the app is busy."),
                       h3("Usage"),
                       h4("Read"),
                       p(strong("Database:"), "Click 'Load data & filter -> From the database'. Select the desired data and click 'Send inquiry'. The 'BUSY' symbol on the left top corner will disappear when the operation is done. This may take time depending on the size of the selected dataset. You will get an overview of selected data and positions on the right. You can now further limit the dataset using the 'Subset' button and reset the data selection using the 'Reset' button."),
                       p(strong("File:"), "Click 'Load data & filter -> From files -> Browse..' and select one or multiple .xml files from your computer. An overview of data and sampling station locations will be shown below. Use the 'Filter data by' options to select data you want to keep. Click the 'Subset' button once you are ready and see how the overview will change based on the information you selected. The 'Reset' button will reset the selection."),
                       p(strong("Resume a previous session:"), "Click 'Load data & filter -> From files -> Browse..' and open an .rds file saved using the app (see 'Download'). You can now continue working on data from an earlier Biotic Explorer session."),
                       h4("Examine"),
                       p(strong("Cruise overview:"), "Click the 'Cruise overview' tab to see all cruises in the dataset. These data comprise of the '$mission' element in NMD Biotic files."),
                       p(strong("Stations & catches:"), "The 'Overview' tab lists selected plots to establish an overview of the '$fishstation' and '$catchsample' elements in NMD Biotic files. 'Map of catches' tab shows a location overview of catches and the 'Examine data' the data in a tabular form."),
                       p(strong("Individuals & ages:"), "The 'Overview' tab shows a general overview of the '$individual' and '$agedetermination' elements in NMD Biotic files. The 'Species plots' tab can be used to generate plots describing basic life-history parameters of a species with sufficient data and the 'Examine data' tab shows the data in a tabular form."),
                       p(strong("Hierarchical data tables"), "tab shows the data in NMD Biotic hierarchical format."),
                       h4("Download"),
                       p(strong("Data:"), "Data from a Biotic Explorer session can be downloaded using the 'Download -> Data' tab. If you want to reopen the data in Biotic Explorer or open the data in R, use the 'R' option without changing 'Data to download' options. This will save the data as an .rds file, which can be opened using the", a("'readRDS'", href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html"), "function in R and reopened using Biotic Explorer. Data can also be downloaded as .zip compressed .csv files or as an Excel file. The data are automatically placed to tabs in Excel files."),
                       p(strong("Figures:"), "You can select which Biotic Explorer figures to download and in which format using the 'Download -> Figures' tab. If you want to modify the figures beyond the options given in the app, you may", a("download Biotic Explorer", href = "https://github.com/MikkoVihtakari/BioticExplorer"), "and modify the figure functions listed under 'R/figure_functions.R'."),
                       br(),
                       br(),
                       h5("Authors: The StoX project team (Mikko Vihtakari, Ibrahim Umar)", align = "left"),
                       h5("Contact person: Mikko Vihtakari (mikko.vihtakari@hi.no)", align = "left"),
                       h5("(c) Institute of Marine Research, Norway, acknowledging the", a("RStudio team, Shiny developers", href = "https://www.rstudio.com/about/"), "and the", a("community", hfer = "https://stackoverflow.com/questions/tagged/shiny"), align = "left")
                )
              )
      ),
      
      ## Upload file tab ####
      
      tabItem("upload",
              
              fluidRow(
                
                ## Column 1 ###
                column(width = 6,
                       
                       box(
                         title = "1. Upload NMD Biotic files", status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE, 
                         
                         p("Here you can open a local file from your computer. BioticExplorer accepts the standard NMD Biotic V3 xml files and R native rds files. The R files can be used to return to a previous data manipulation instance using this app. Use the 'R' download file type in the Download data tab to enable this feature."),
                         
                         strong("Performance mode:"),
                         checkboxInput("performanceMode", "For large (>200 Mb) files. Disables features that burden memory.", FALSE),
                         
                         fileInput("file1",
                                   label = "Choose xml or rds input file",
                                   multiple = TRUE,
                                   accept = c(".xml", ".rds")
                         ),
                         "Note that processing takes some time for large files even after 'Upload complete' shows up. Be patient.",
                         hr(),
                         
                         strong("Drop excess data:"),
                         checkboxInput("removeEmpty", "Remove empty columns", TRUE),
                         checkboxInput("coreDataOnly", "Keep only important columns", FALSE),
                         
                         radioButtons("lengthUnit", "Fish length unit:",
                                      c("Millimeter" = "mm",
                                        "Centimeter" = "cm",
                                        "Meter" = "m"),
                                      selected = "m",
                                      inline = TRUE),
                         
                         radioButtons("weightUnit", "Fish weight unit:",
                                      c("Grams" = "g",
                                        "Kilograms" = "kg"),
                                      selected = "kg",
                                      inline = TRUE)
                         
                       ), 
                       
                       box(
                         title = "Selected data", width = NULL, status = "primary",
                         valueBoxOutput("nCruisesBox"),
                         valueBoxOutput("nStationsBox"),
                         valueBoxOutput("nYearsBox"),
                         valueBoxOutput("nGearsBox"),
                         valueBoxOutput("nSpeciesBox"),
                         valueBoxOutput("nMeasuredBox"),
                         valueBoxOutput("DateStartBox", width = 6),
                         valueBoxOutput("DateEndBox", width = 6)
                       )
                ),
                
                ## Column 2 ###
                
                column(width = 6,
                       
                       box(
                         title = "2. Filter data by", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, width = NULL, 
                         
                         fluidRow(
                           column(6, 
                                  selectizeInput(inputId = "subYear", label = "Year:",
                                                 choices = NULL, multiple = TRUE),
                                  selectizeInput(inputId = "subCruise", label = "Cruise number:",
                                                 choices = NULL, multiple = TRUE),
                                  selectizeInput(inputId = "subPlatform", label = "Platform name:",
                                                 choices = NULL, multiple = TRUE),
                                  dateRangeInput(inputId = "subDate", label = "Date:",
                                                 start = "1900-01-01", startview = "decade", weekstart = 1)
                           ),
                           
                           column(6,
                                  selectizeInput(inputId = "subSpecies", label = "Species:", 
                                                 choices = NULL, multiple = TRUE),
                                  selectizeInput(inputId = "subSerialnumber", 
                                                 label = "Serial number:",
                                                 choices = NULL, multiple = TRUE),
                                  selectizeInput(inputId = "subGear", label = "Gear code:",
                                                 choices = NULL, multiple = TRUE),
                                  selectizeInput(inputId = "subMissionType", label = "Mission type:",
                                                 choices = NULL, multiple = TRUE)
                           )),
                         
                         
                         sliderInput(inputId = "subLon", label = "Longitude:", min = -180, 
                                     max = 180, value = c(-180, 180)),
                         sliderInput(inputId = "subLat", label = "Latitude:", min = -90, 
                                     max = 90, value = c(-90, 90)),
                         
                         actionButton(inputId = "Subset", label = "Subset"),
                         actionButton(inputId = "Reset", label = "Reset")
                         
                         , verbatimTextOutput("test") # For debugging
                         
                       ),
                       
                       box(title = "Station locations", status = "primary", width = NULL,
                           conditionalPanel(
                             condition = "input.performanceMode == false",
                             leafletOutput(outputId = "stationMap")
                           ),
                           
                           conditionalPanel(
                             condition = "input.performanceMode == true",
                             p("Performance mode. No map. Subset data and use 'Stations & catches' -> 'Map of catches' to examine station locations.", align = "center")
                           )       
                       )
                )
                
                ## End columns ###
                
              )
      ),
      
      ### Database tab ####
      
      tabItem("uploadDb",
              
              fluidRow(
                
                ## Column 1 ###
                column(width = 6,
                       
                       box(
                         title = "1. Select data from the IMR database", 
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE, 
                         
                         conditionalPanel(
                           condition = "output.serverVersion == true",
                           
                           conditionalPanel(condition = "output.fetchedDb == false",
                                            p("The IMR database contains tens of gigabytes of data. It is therefore important to select only the data you need before sending an inquiry. The estimated size of the database can be seen on the right. Once you have selected the needed data using this box, press the 'Send inquiry' button. Note that the data processing will take some time. Please, be patient. The server or your browser have probably not crashed even though you see nothing happening.")
                                            
                           ),
                           
                           conditionalPanel(condition = "output.fetchedDb == true",
                                            p("You have now selected data from the database and can see the overview on the right. You can use the 'Subset' button to further limit the data selection and the 'Reset' button to reload the entire database. Once you are happy with your dataset, you may proceed to other tabs in this application.")
                                            
                           ),
                           
                           strong("Drop excess data:"),
                           fluidRow(
                             column(4, checkboxInput("removeEmpty", "Remove empty columns", TRUE)),
                             column(4, checkboxInput("coreDataOnly", "Keep only important columns", FALSE))
                           ),
                           
                           fluidRow(
                             column(6, 
                                    selectizeInput(inputId = "selMissionTypeDb", 
                                                   label = "Mission type:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selCruiseSeriesDb", 
                                                   label = "Cruise series:",
                                                   choices = "Not implemented yet", multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selCruiseDb", 
                                                   label = "Cruise number:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selGeogAreaDb", 
                                                   label = "Geographic area:",
                                                   choices = "Not implemented yet", multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selICESAreaDb", 
                                                   label = "ICES area:",
                                                   choices = "Not implemented yet", multiple = TRUE),
                                    
                                    dateRangeInput(inputId = "selDateDb", label = "Date:",
                                                   start = "1900-01-01", startview = "decade", weekstart = 1)
                             ),
                             
                             column(6,
                                    selectizeInput(inputId = "selYearDb", 
                                                   label = "Year:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selSpeciesDb", 
                                                   label = "Species:", 
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selPlatformDb", 
                                                   label = "Platform name:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selSerialnumberDb", 
                                                   label = "Serial number:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selGearDb", 
                                                   label = "Gear code:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selGearCategoryDb", 
                                                   label = "Gear category:",
                                                   choices = "Not implemented yet", multiple = TRUE)
                                    
                                    
                                    
                             )),
                           
                           
                           sliderInput(inputId = "selLonDb", label = "Longitude:", min = -180, 
                                       max = 180, value = c(-180, 180)),
                           sliderInput(inputId = "selLatDb", label = "Latitude:", min = -90, 
                                       max = 90, value = c(-90, 90)),
                           
                           conditionalPanel(condition = "output.fetchedDb == false",
                                            actionButton(inputId = "doFetchDB", label = "Send inquiry") 
                           ),
                           
                           conditionalPanel(condition = "output.fetchedDb == true",
                                            actionButton(inputId = "SubsetDB", label = "Subset"),
                                            actionButton(inputId = "ResetDB", label = "Reset")
                           )
                           
                         ),
                         
                         conditionalPanel(
                           condition = "output.serverVersion == false",
                           h4("Desktop version. The database is not available", align = "center"),
                           p("If you are trying to run the app as a server version, check make sure that the dbPath argument is defined correctly.", align = "center")
                         )
                         
                       )
                ),
                ## Column 2 ###
                column(width = 6,
                       conditionalPanel(
                         condition = "output.fetchedDb == false",
                         box(
                           title = "All data in the database", width = NULL, status = "primary",
                           valueBoxOutput("EstStationsBox", width = 6),
                           valueBoxOutput("EstYearsBox", width = 6),
                           valueBoxOutput("EstGearsBox", width = 6),
                           valueBoxOutput("EstSpeciesBox", width = 6),
                           valueBoxOutput("EstDateStartBox", width = 6),
                           valueBoxOutput("EstDateEndBox", width = 6)
                           # ,verbatimTextOutput("test") # For debugging
                         )
                       ),
                       conditionalPanel(
                         condition = "output.fetchedDb == true",
                         box(
                           title = "Selected data", width = NULL, status = "primary",
                           valueBoxOutput("nCruisesBoxDb"),
                           valueBoxOutput("nStationsBoxDb"),
                           valueBoxOutput("nYearsBoxDb"),
                           valueBoxOutput("nGearsBoxDb"),
                           valueBoxOutput("nSpeciesBoxDb"),
                           valueBoxOutput("nMeasuredBoxDb"),
                           valueBoxOutput("DateStartBoxDb", width = 6),
                           valueBoxOutput("DateEndBoxDb", width = 6)
                           # ,verbatimTextOutput("test") # For debugging
                         ),
                         box(title = "Station locations", status = "primary", width = NULL,
                             leafletOutput(outputId = "stationMapDb")
                         )
                       )
                )
              )
              
      ),
      
      ### Stnall tab ####      
      tabItem("stnallOverview",
              fluidRow(
                box(title = "Species composition", width = 12, status = "info", 
                    solidHeader = TRUE, plotOutput("speciesCompositionPlot")
                ),
                
                box(title = "Total catch weight", width = 12, status = "info", 
                    solidHeader = TRUE, plotlyOutput("catchweightSumPlot")
                ),
                
                box(title = "Catch weight mean and standard error (stations containing the species are replicates)", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchweightMeanPlot")),
                
                box(title = "Catch weight range", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchweightRangePlot")),
                
                box(title = "Mean weight of specimen", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchIndMeanWeightPlot")),
                
                box(title = "Mean number in catch and standard error", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchcountMeanPlot")),
                
                box(title = "Range of number in catch", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchcountRangePlot")),
                
                box(title = "Total (summed) catch by gear type", width = 12, status = "info", solidHeader = TRUE, plotOutput("gearcatchPlot", height = "600px")),
                
                box(title = "Station depth", width = 12, status = "info", solidHeader = TRUE, plotOutput("stationDepthPlot")),
                box(title = "Fishing depth of six most dominant species", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchSpeciesWeightPlot"))
              )
      ),
      
      tabItem("stnallMap", 
              fluidRow(
                box(title = "Map of catches (in kg)", width = 12, status = "info", 
                    solidHeader = TRUE, height = 840,
                    selectInput("catchMapSpecies", "Select species:", 
                                choices = NULL),
                    leafletOutput(outputId = "catchMap", height = 700)
                ),
                
                box(title = "Catch composition", width = 12, status = "info", 
                    solidHeader = TRUE, height = 760,
                    leafletOutput(outputId = "catchCompMap", height = 700)
                )
              )
      ),
      
      tabItem("stnallExamine", DT::dataTableOutput("stnall")),
      
      ### Indall tab ####
      
      tabItem("indallOverview", 
              fluidRow(
                box(title = "Individual sample overview", width = 12, status = "info", 
                    solidHeader = TRUE,
                    DT::dataTableOutput("individualSummaryTable"),
                    br(),
                    box(title = "Median length and distribution of species with > 10 length measurements", width = 12, status = "info", solidHeader = TRUE, plotOutput("indLengthPlot"))
                )
                # box(title = "Ricker thing", width = 12, status = "info",
                #     solidHeader = TRUE,
                #     # plotOutput("rickerPlot")
                #     verbatimTextOutput("test")
                # ),
                
              )
      ),
      
      tabItem("indallSpecies", 
              fluidRow(
                box(title = "Select species", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    selectInput("indSpecies", "Select species:", choices = NULL)
                ),
                
                ### weightData
                
                box(title = "Length - weight relationship", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    
                    conditionalPanel(
                      condition = "output.weightData == true",
                      
                      plotlyOutput("lwPlot"),
                      
                      br(),
                      column(4,
                             checkboxInput("lwPlotLogSwitch", "Logarithmic axes", FALSE)),
                      column(8,
                             actionButton("lwPlotExcludeSwitch", "Exclude points"),
                             actionButton("lwPlotResetSwitch", "Reset")),
                      column(12,
                             verbatimTextOutput("lwPlotText"))
                    ),
                    
                    conditionalPanel(
                      condition = "output.weightData == false",
                      h4("Weight data not available for the species.", align = "center")
                    )
                ),
                
                ### ageData
                
                box(title = "Age - length relationship", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    
                    conditionalPanel(
                      condition = "output.ageData == true",
                      plotlyOutput("laPlot"),
                      
                      br(),
                      column(4,
                             checkboxInput("laPlotSexSwitch", "Separate by sex", FALSE)),
                      column(8,
                             actionButton("laPlotExcludeSwitch", "Exclude points"),
                             actionButton("laPlotResetSwitch", "Reset")),
                      column(12,
                             verbatimTextOutput("laPlotText"))
                    ),
                    
                    conditionalPanel(
                      condition = "output.ageData == false",
                      h4("Age data not available for the species.", align = "center")
                    )
                    
                ),
                
                ### maturityData
                
                box(title = "50% maturity based on length (L50)", width = 12, 
                    status = "info", solidHeader = TRUE,
                    
                    conditionalPanel(
                      condition = "output.maturityData == true",
                      plotOutput("l50Plot"),
                      verbatimTextOutput("l50PlotText")
                    ),
                    
                    conditionalPanel(
                      condition = "output.maturityData == false",
                      h4("Not enough maturity data available for the species.", align = "center")
                    )
                    
                ),
                
                ### sexData
                
                box(title = "Sex ratio", width = 12, status = "info", 
                    solidHeader = TRUE, height = 760,
                    
                    conditionalPanel(
                      condition = "output.sexData == true",
                      leafletOutput(outputId = "sexRatioMap", height = 700)
                    ),
                    
                    conditionalPanel(
                      condition = "output.sexData == false",
                      h4("Sex data not available for the species.", align = "center")
                    )
                    
                ),
                
                ### lengthData
                
                box(title = "Size distribution", width = 12, status = "info", 
                    solidHeader = TRUE, height = 760,
                    
                    conditionalPanel(
                      condition = "output.lengthData == true",
                      leafletOutput(outputId = "sizeDistributionMap", height = 700)
                    ),
                    
                    conditionalPanel(
                      condition = "output.lengthData == false",
                      h4("Length data not available for the species.", align = "center")
                    )
                )
              )
      ),
      
      tabItem("indallExamine", DT::dataTableOutput("indall")),
      
      ##..................
      ## NMD data tab ####
      
      tabItem("missionExamine", DT::dataTableOutput("missionTable")),
      # tabItem("fishstationExamine", DT::dataTableOutput("fishstation")),
      # tabItem("catchsampleExamine", DT::dataTableOutput("catchsample")),
      # tabItem("individualExamine", DT::dataTableOutput("individualTable")),
      # tabItem("agedeterminationExamine", DT::dataTableOutput("agedeterminationTable")),
      
      ##........................
      ## Export figures tab ####
      
      tabItem("exportFigures",
              box(title = "1. Select the figures to export", width = 12, status = "info", 
                  solidHeader = TRUE,
                  
                  checkboxGroupInput("cruiseMapExport", label = h4("Station map and cruise track (use only when the xml file consist of entire cruise)"), 
                                     choices = list("Cruise map" = 1),
                                     selected = NULL, inline = TRUE),
                  
                  radioButtons("plotCruiseTrack", "Cruise track:", 
                               choices = list("Do not plot" = "No", "From station sequence" = "Stations", "From file" = "File"),
                               selected = "No",
                               inline = TRUE
                  ),
                  
                  conditionalPanel(condition = "input.plotCruiseTrack == 'File'",
                                   fileInput("cruiseTrackFile",
                                             label = "Upload cruise track",
                                             multiple = TRUE,
                                             accept = NA)
                  ),
                  
                  checkboxGroupInput("stationOverviewExport", label = h4("Station overview figures"), 
                                     choices = stationOverviewFigureList,
                                     selected = NA, inline = TRUE),
                  
                  actionLink("selectAllStationOverviewExport", "Select/deselect all"),
                  
                  h4("Station based maps"),
                  
                  splitLayout(cellWidths = c("25%", "40%"),
                              checkboxGroupInput("stationCatchMapExport", label = NULL, choices = stationMapList[1]),
                              selectInput("catchMapExportSpecies", NULL, choices = NULL)
                  ),
                  
                  checkboxGroupInput("stationMapExport", label = NULL, 
                                     choices = stationMapList[-1],
                                     selected = NA, inline = TRUE),
                  
                  
                  actionLink("selectAllStationMapExport", "Select/deselect all")
                  
              ),
              
              box(title = "2. Download selected figures", width = 12, status = "info", 
                  solidHeader = TRUE,
                  
                  numericInput("figureWidth", "Figure width in cm (the height will be scaled automatically)", value = 18), 
                  
                  radioButtons("downloadFiguresAs", "Download as:", 
                               choices = list("Figure files" = "File", "Cruise report template" = "Report"),
                               selected = "File",
                               inline = TRUE
                  ),
                  
                  
                  conditionalPanel(condition = "input.downloadFiguresAs == 'File'",
                                   radioButtons("downloadFigureFormat", "File format:",
                                                choices = list("Png" = ".png", "Jpeg" = ".jpeg", "Pdf" = ".pdf"),
                                                selected = ".png",
                                                inline = TRUE)
                  ),
                  
                  conditionalPanel(condition = "input.downloadFiguresAs == 'Report'",
                                   radioButtons("downloadReportFormat", "File format:",
                                                choices = list("Rmarkdown" = "Rmd", "Word" = "Doc", "Pdf" = "Pdf"),
                                                selected = "Rmd",
                                                inline = TRUE)
                  ),
                  
                  downloadButton(outputId = "downloadFigures")
              )
      ),
      
      ##...................
      ## Download tab ####
      
      tabItem("downloadDatasets", 
              fluidRow(
                box(
                  title = "Download data", width = 12, status = "primary", solidHeader = TRUE,
                  radioButtons("downloadFileType", "Download file type:",
                               c("R" = ".rda",
                                 "csv" = ".csv",
                                 "Excel" = ".xlsx"),
                               selected = ".rda"
                  ),
                  
                  checkboxGroupInput("downloadDataType", "Data to download:",
                                     c("Cruise overview" = "mission",
                                       "Stations & catches" = "stnall",
                                       "Individuals & ages" = "indall"
                                       #"Original format" = "original"
                                     ),
                                     selected = c("mission", "stnall", "indall")
                  ),
                  
                  downloadButton(outputId = "downloadData")
                  # verbatimTextOutput("test")
                ) 
              )
      )
    )
  )

##.................
## Dashboard page ####

ui <- dashboardPage(header, sidebar, body)

##............
## Server ####

server <- shinyServer(function(input, output, session) {
  
  ## Options ####
  
  options(shiny.maxRequestSize = 1000*1024^2) ## This sets the maximum file size for upload. 1000 = 1 Gb. 
  
  output$serverVersion <- reactive(file.exists(dbPath)) 
  outputOptions(output, "serverVersion", suspendWhenHidden = FALSE)
  
  source("R/filtering_functions.R", local = TRUE) ## Source functions for the session. See https://shiny.rstudio.com/articles/scoping.html
  
  rv <- reactiveValues(stnall = NULL, indall = NULL, mission = NULL, uploadDbclicked = FALSE) ## Create reactive values
  
  ## Read data file ####
  
  observeEvent(req(input$file1), {
    
    tryCatch({
      
      if (length(input$file1[[1]]) > 1) {
        rv$inputData <- processBioticFiles(files = input$file1$datapath, lengthUnit = input$lengthUnit, weightUnit = input$weightUnit, removeEmpty = input$removeEmpty, coreDataOnly = input$coreDataOnly, convertColumns = TRUE)
      } else {
        if(tolower(gsub("^.+\\.", "", input$file1$name)) == "rds") {
          rv$inputData <- readRDS(file = input$file1$datapath)
        } else {
          rv$inputData <- processBioticFile(file = input$file1$datapath, lengthUnit = input$lengthUnit, weightUnit = input$weightUnit, removeEmpty = input$removeEmpty, coreDataOnly = input$coreDataOnly, convertColumns = TRUE)
        }
      }
    },
    error = function(e) {
      stop(safeError(e))
    }
    )
    
    rv$stnall <- rv$inputData$stnall
    rv$indall <- rv$inputData$indall
    rv$mission <- rv$inputData$mission
    
    obsPopulatePanel()
    
  })
  
  ## Read database ####
  
  observeEvent(input$tabs, {
    if(input$tabs == "uploadDb") {
      if(file.exists(dbPath)) {
        
        if(!rv$uploadDbclicked) {
          output$fetchedDb <- reactive(FALSE)
          outputOptions(output, "fetchedDb", suspendWhenHidden = FALSE)
        }
        
        rv$uploadDbclicked <- TRUE
        
        con_db <- DBI::dbConnect(MonetDBLite::MonetDBLite(), dbPath)
        
        rv$inputData$stnall <- dplyr::tbl(con_db, "stnall")
        rv$inputData$indall <- dplyr::tbl(con_db, "indall")
        rv$inputData$mission <- dplyr::tbl(con_db, "mission")
        
        # Running the indexing below, saving to a file and specifying dbIndexPath in Settings section is a time-saver, not a necessity. 
        if(!exists("index")) {
          index <<- list()
          index$missiontypename <- rv$inputData$mission %>% lazy_dt() %>% select(missiontypename) %>% distinct() %>% pull() %>% sort()
          index$cruise <- rv$inputData$mission %>% lazy_dt() %>% select(cruise) %>% distinct() %>% pull() %>% sort()
          index$year <- rv$inputData$mission %>% lazy_dt() %>% select(startyear) %>% distinct() %>% pull() %>% sort()
          index$nstations <- rv$inputData$stnall %>% lazy_dt() %>% select(missionid, startyear, serialnumber) %>% distinct() %>% count() %>% pull()
          index$commonname <- rv$inputData$stnall %>% lazy_dt() %>% select(commonname) %>% distinct() %>% pull() %>% sort()
          index$platformname <- rv$inputData$stnall %>% lazy_dt() %>% select(platformname) %>% distinct() %>% pull() %>% sort()
          index$serialnumber <- rv$inputData$stnall %>% lazy_dt() %>% select(serialnumber) %>% distinct() %>% pull() %>% sort()
          index$gear <- rv$inputData$stnall %>% lazy_dt() %>% select(gear) %>% distinct() %>% pull() %>% sort()
        }
        
        updateFilterform(loadDb = TRUE)
        
        output$EstStationsBox <- renderValueBox({
          
          valueBox(
            value = tags$p(index$nstations, style = "font-size: 80%;"),
            subtitle = "Stations"
          )
        })
        
        output$EstYearsBox <- renderValueBox({
          valueBox(
            length(index$year),
            "Unique years"
          )
        })
        
        output$EstGearsBox <- renderValueBox({
          valueBox(
            length(index$gear),
            "Gear types"
          )
        })
        
        output$EstSpeciesBox <- renderValueBox({
          valueBox(
            length(index$commonname),
            "Unique species"
          )
        })
        
        output$EstDateStartBox <- renderValueBox({
          valueBox(
            value = tags$p(index$year %>% min(),
                           style = "font-size: 80%;"),
            subtitle = "First year"
          )
        })
        
        output$EstDateEndBox <- renderValueBox({
          valueBox(
            value = tags$p(index$year %>% max(),
                           style = "font-size: 80%;"),
            subtitle = "Last year"
          )
        })
        
      }
    }
    
  })
  
  ### Filter the database ####
  
  observeEvent(input$doFetchDB, {
    
    output$fetchedDb <- reactive(TRUE)
    
    tmp <- makeFilterChain(db = TRUE)
    rv$filterChain <- paste(tmp$filterChain, collapse = "; ")
    rv$sub <- tmp$sub
    
    rv$stnall <- rv$inputData$stnall %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
    rv$indall <- rv$inputData$indall %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
    rv$mission <- rv$inputData$mission %>% filter(missionid %in% !!unique(rv$stnall$missionid)) %>% collect() %>% as.data.table()
    
    obsPopulatePanel(db = TRUE)
    
  })
  
  #.................
  ## Test output ####
  
  output$test <- renderText({
    #   # #   #
    #   # #   #   # length(input$file1[[1]])
    # paste(input$file1, collapse = "; ")
    paste(rv$fileext, collapse = "; ")
    #   # #   #   paste(rv$filterChain, collapse = "; ")
    #  paste(input$tabs, collapse = "; ")
  })
  # 
  
  ##................... 
  ## Export figures tab
  
  observeEvent(c(input$selectAllStationOverviewExport), {
    
    if(input$selectAllStationOverviewExport > 0) {
      if(input$selectAllStationOverviewExport %% 2 == 1) {
        updateSelectInput(session, "stationOverviewExport", selected = stationOverviewFigureList)
      } else {
        updateSelectInput(session, "stationOverviewExport", selected = NA)
      }
    } 
    
  })
  
  observeEvent(c(input$selectAllStationMapExport), {
    
    if(input$selectAllStationMapExport > 0) {
      if(input$selectAllStationMapExport %% 2 == 1) {
        updateSelectInput(session, "stationMapExport", selected = stationMapList)
      } else {
        updateSelectInput(session, "stationMapExport", selected = NA)
      }
    } 
    
  })
  
  
  ##................
  ## Subsetting ####
  
  observeEvent(input$Subset, {
    
    tmp <- makeFilterChain()
    rv$filterChain <- paste(tmp$filterChain, collapse = "; ")
    rv$sub <- tmp$sub
    
    rv$stnall <- rv$stnall %>% lazy_dt() %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
    rv$indall <- rv$indall %>% lazy_dt() %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
    rv$mission <- rv$mission %>% lazy_dt() %>% filter(missionid %in% !!unique(rv$stnall$missionid)) %>% collect() %>% as.data.table()
    
    obsPopulatePanel()
    
  })
  
  observeEvent(input$SubsetDB, {
    
    tmp <- makeFilterChain(db = TRUE)
    rv$filterChain <- paste(tmp$filterChain, collapse = "; ")
    rv$sub <- tmp$sub
    
    rv$stnall <- rv$stnall %>% lazy_dt() %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
    rv$indall <- rv$indall %>% lazy_dt() %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
    rv$mission <- rv$mission %>% lazy_dt() %>% filter(missionid %in% !!unique(rv$stnall$missionid)) %>% collect() %>% as.data.table()
    
    obsPopulatePanel(db = TRUE)
    
  })
  
  ##...............
  ## Resetting ####
  
  observeEvent(input$Reset, {
    
    rv$stnall <- rv$inputData$stnall
    rv$indall <- rv$inputData$indall
    rv$mission <- rv$inputData$mission
    
    obsPopulatePanel()
    
  })
  
  observeEvent(input$ResetDB, {
    
    output$fetchedDb <- reactive(FALSE)
    
    rv$stnall <- NULL
    rv$indall <- NULL
    rv$mission <- NULL
    
    updateFilterform(loadDb = TRUE)
    
  })
  
  ##.................
  ## Data tables ####
  
  output$stnall <- DT::renderDataTable({
    DT::datatable(rv$stnall, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) %>% formatRound(columns = c(FALSE, grepl("latitude|longitude|distance|weight|speed|soaktime", names(rv$stnall))), mark = " ")
  })
  
  output$indall <- DT::renderDataTable({
    DT::datatable(rv$indall, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) %>% formatRound(c("longitudestart", "latitudestart", "length", "individualweight"))
  })
  
  
  output$missionTable <- DT::renderDataTable({
    DT::datatable(rv$mission,
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    )
  })
  
  # output$fishstation <- DT::renderDataTable({
  #   DT::datatable(rv$fishstation, 
  #                 options = list(scrollX = TRUE, 
  #                                pageLength = 20
  #                 ) 
  #   ) %>% formatRound(c("longitudestart", "latitudestart", "distance"))
  # })
  # 
  # output$catchsample <- DT::renderDataTable({
  #   DT::datatable(rv$catchsample, 
  #                 options = list(scrollX = TRUE, 
  #                                pageLength = 20
  #                 ) 
  #   ) %>% formatRound(c("catchweight", "lengthsampleweight"))
  # })
  # 
  # output$individualTable <- DT::renderDataTable({
  #   DT::datatable(rv$individual, 
  #                 options = list(scrollX = TRUE, 
  #                                pageLength = 20
  #                 ) 
  #   ) 
  # })
  # 
  # output$agedeterminationTable <- DT::renderDataTable({
  #   DT::datatable(rv$agedetermination,
  #                 options = list(scrollX = TRUE,
  #                                pageLength = 20
  #                 )
  #   )
  # })
  
  
  #..................
  ## Station map ####
  
  # observeEvent(req(input$file1), {
  #   
  #   if (!input$performanceMode) {
  #     output$stationMap <- renderLeaflet({
  #       
  #       tmp <- rv$stnall %>% 
  #         select(missiontype, startyear, platform, platformname, missionnumber, missionid, 
  #                serialnumber, latitudestart, longitudestart) %>% 
  #         filter(!is.na(longitudestart) & !is.na(latitudestart)) %>% distinct() 
  #       
  #       leaflet::leaflet(tmp) %>% 
  #         addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
  #                  attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
  #         addRectangles(
  #           lng1 = input$subLon[1], lat1 = input$subLat[1], lng2 = input$subLon[2], lat2 = input$subLat[2],
  #           fillColor = "transparent") %>% 
  #         addCircleMarkers(lat = ~ latitudestart, 
  #                          lng = ~ longitudestart, 
  #                          weight = 1, radius = 2, 
  #                          popup = ~as.character(platformname), 
  #                          label = ~as.character(serialnumber), 
  #                          fillOpacity = 0.5,
  #                          clusterOptions = markerClusterOptions()
  #         )
  #     })
  #   } else {
  #     output$stationMap <- NULL
  #   }
  #   
  # })
  # 
  ##......................
  ## Stn data figures ####
  
  observeEvent(input$tabs, {
    if(input$tabs == "stnallOverview") {
      
      spOverviewDat <- speciesOverviewData(rv$stnall)
      
      ## Number of stations containing the species plot
      
      output$speciesCompositionPlot <- renderPlot(speciesCompositionPlot(spOverviewDat))
      
      ## Summed catch weight plot
      
      output$catchweightSumPlot <- renderPlotly({
        
        p <- catchweightSumPlot(spOverviewDat)
        
        ggplotly(p) %>% plotly::layout(annotations = list(x = 1, y= 1, xref = "paper", yref = "paper", text = paste("Total catch\n all species\n", round(sum(spOverviewDat$catchS$sum), 0), "kg"), showarrow = FALSE, font = list(size = 12)))
        
      })
      
      ## Mean catch weight plot
      
      output$catchweightMeanPlot <- renderPlot(catchweightMeanPlot(spOverviewDat))
      
      ## Catch weight range plot
      
      output$catchweightRangePlot <- renderPlot(catchweightRangePlot(spOverviewDat))
      
      ## Mean weight of fish in catch
      
      output$catchIndMeanWeightPlot <- renderPlot(catchIndMeanWeightPlot(spOverviewDat))
      
      ## Mean n in catch plot
      
      output$catchcountMeanPlot <- renderPlot(catchcountMeanPlot(spOverviewDat))
      
      ## N range in catch plot
      
      output$catchcountRangePlot <- renderPlot(catchcountRangePlot(spOverviewDat))
      
      ## Catch in gear plot
      
      output$gearcatchPlot <- renderPlot(gearCatchPlot(spOverviewDat))
      
      ## Station depth plot
      
      output$stationDepthPlot <- renderPlot(stationDepthPlot(spOverviewDat))
      
      ## Fishing depth plot 
      
      output$catchSpeciesWeightPlot <- renderPlot(catchSpeciesWeightPlot(spOverviewDat))
      
    } 
  })
  
  
  observeEvent(c(input$tabs,  input$catchMapSpecies), {
    if(input$tabs == "stnallMap") {
      
      ## Catch map ###
      
      output$catchMap <- renderLeaflet(catchMap(rv$stnall, species = input$catchMapSpecies))
      
    }
  }) 
  
  observeEvent(c(input$tabs,  input$catchMapSpecies), {
    if(input$tabs == "stnallMap") {
      
      spOverviewDat <- speciesOverviewData(rv$stnall)
      
      ## Catch composition map
      
      #if(nrow(spOverviewDat$compDatW) != 0) {
      output$catchCompMap <- renderLeaflet(catchCompMap(spOverviewDat))
      #}
      
    }
  }) 
  
  ##..........................................
  ## Ind data overview figures and tables ####
  
  observeEvent(input$tabs, {
    if(input$tabs == "indallOverview") {
      
      indSumTab <- rv$indall %>% lazy_dt() %>% 
        dplyr::group_by(commonname) %>% 
        dplyr::summarise(Total = n(), 
                         Length = {if("length" %in% names(.)) sum(!is.na(length)) else 0},
                         Weight = {if("individualweight" %in% names(.)) sum(!is.na(individualweight)) else 0}, 
                         Sex = {if("sex" %in% names(.)) sum(!is.na(sex)) else 0}, 
                         Maturationstage = {if("maturationstage" %in% names(.)) sum(!is.na(maturationstage)) else 0}, 
                         Specialstage = {if("specialstage" %in% names(.)) sum(!is.na(specialstage)) else 0}, 
                         Age = {if("age" %in% names(.)) sum(!is.na(age)) else 0}
        ) %>% arrange(-Total) %>% collect() 
      
      output$individualSummaryTable <- DT::renderDataTable({
        DT::datatable(indSumTab, options = list(searching = FALSE))
      })
      
      output$indLengthPlot <- renderPlot(indLengthPlot(indall = rv$indall))
      
      
      # 
      # 
      # meanIndL <- meanIndLStn %>% group_by(commonname) %>% summarise(mean = mean(meanLength), sd = sd(meanLength), se = se(meanLength), n = length(meanLength))
      # 
      # ggplot(tmp, aes(x = commonname, y = meanLength)) + geom_point()
      # 
      # 
      # nLimit = 10
      # indall <- rv$indall
      # 
      # 
      # sps <- indall %>% lazy_dt() %>% filter(!is.na(individualweight)) %>% group_by(commonname) %>% count() %>% filter(n > nLimit) %>% pull(commonname) 
      # 
      # indWei <- indall %>% lazy_dt() %>%
      #   filter(!is.na(individualweight), commonname %in% sps) %>% collect() %>% as.data.table()
      # 
      # tmp <- rv$indall %>% lazy_dt() %>% 
      #   filter(!is.na(individualweight)) %>% 
      #   group_by(commonname) %>% 
      #   filter(n() > 10) %>%
      #   summarise(ricker = sd(log(individualweight))/sd(log(length)), n = n(), medWeigth = median(individualweight)) %>%
      #   arrange(-medWeigth) %>% collect()
      #
      # output$rickerPlot <- renderPlot({
      #   
        # ggplot() +
        #   geom_hline(yintercept = 3) +
        #   geom_text(data = tmp, aes(x = commonname, y = Inf, label = n), vjust = 1) +
        #   geom_point(data = tmp, aes(x = commonname, y = ricker)) +
        #   ylab("Sd(log(sum(weight)))/Sd(log(sum(length)))") +
        #   xlab("Species database name") +
        #   theme_classic(base_size = 14) +
        #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        # 
        # ggplot() +
        #   geom_violin()
        
        
      # })
      
      # Mean individual length & sex
      # Mean length - depth
    } 
  })
  
  ## Ind plots, selected species ####
  
  observe({
    
    ##..................
    ## Set switches ####
    
    output$ageData <- reactive(FALSE)
    output$weightData <- reactive(FALSE)
    output$maturityData <- reactive(FALSE)
    output$sexData <- reactive(FALSE)
    output$lengthData <- reactive(FALSE)
    
    outputOptions(output, "ageData", suspendWhenHidden = FALSE)
    outputOptions(output, "weightData", suspendWhenHidden = FALSE)
    outputOptions(output, "maturityData", suspendWhenHidden = FALSE)
    outputOptions(output, "sexData", suspendWhenHidden = FALSE)
    outputOptions(output, "lengthData", suspendWhenHidden = FALSE)
    
    if (input$tabs == "indallSpecies" & input$indSpecies != "" & !input$indSpecies %in% c("Select a species to generate the plots", "No species with sufficient data")) {
      
      ### Base individual data ####
      
      tmpBase <- rv$indall[commonname == input$indSpecies, ] 
      
      if (input$indSpecies == "blåkveite") {
        
        tmpTab <- data.table::dcast(tmpBase, cruise + startyear + serialnumber + longitudestart + latitudestart ~ catchpartnumber, fun.aggregate = length, value.var = "length")
        
        if(all(c(1, 2) %in% names(tmpTab))) {
          
          tmpTab$EggaSystem <- tmpTab$`1` > 0 & tmpTab$`2` > 0
          
          tmpBase <- dplyr::left_join(tmpBase, tmpTab[, !names(tmpTab) %in% 1:10, with = FALSE], by = c("startyear", "serialnumber", "cruise", "longitudestart", "latitudestart"))  
          
          tmpBase$sex <- ifelse(!is.na(tmpBase$sex), tmpBase$sex, ifelse(is.na(tmpBase$sex) & tmpBase$EggaSystem & tmpBase$catchpartnumber == 1, 1, ifelse(is.na(tmpBase$sex) & tmpBase$EggaSystem & tmpBase$catchpartnumber == 2, 2, NA)))
          
          tmpBase <- as.data.table(tmpBase[, names(tmpBase) != "EggaSystem"])
        }
      }
      
      ### Length-weight plot ####
      
      if (all(c("length", "individualweight") %in% names(tmpBase))) {
        
        lwDat <- tmpBase[!is.na(length) & !is.na(individualweight),]
        
        if(nrow(lwDat) > 0) {
          
          output$weightData <- reactive(TRUE)
          
          tmp <- lwDat
          if (input$lengthUnit == "mm") tmp$length <- tmp$length/10
          if (input$lengthUnit == "m") tmp$length <- tmp$length*100
          if (input$weightUnit == "kg") tmp$individualweight <- tmp$individualweight*1000
          
          lwMod <- lm(log(individualweight) ~ log(length), data = lwDat)
          tmpMod <- lm(log(individualweight) ~ log(length), data = tmp)
          
          output$lwPlot <- renderPlotly({
            
            p <- ggplot() +
              geom_point(data = lwDat, aes(x = length, y = individualweight, text = paste0(  "cruise: ", cruise, "\nserialnumber: ", serialnumber, "\ncatchpartnumber: ", catchpartnumber, "\nspecimenid: ", specimenid))) + 
              theme_classic(base_size = 12) 
            
            if (input$lwPlotLogSwitch) {
              p <- p + 
                scale_x_log10(paste0("Length [log10(", input$lengthUnit, ")]")) +
                scale_y_log10(paste0("Weight [log10(", input$weightUnit, ")]")) + 
                geom_smooth(data = lwDat, aes(x = length, y = individualweight), method = "lm", se = TRUE) 
              
            } else {
              p <- p + 
                scale_x_continuous(paste0("Length (", input$lengthUnit, ")")) +
                scale_y_continuous(paste0("Weight (", input$weightUnit, ")")) + 
                stat_function(data = 
                                data.frame(x = range(lwDat$length)), aes(x),
                              fun = function(a, b, x) {a*x^b},
                              args = list(a = exp(coef(lwMod)[1]), b = coef(lwMod)[2]),
                              color = "blue", size = 1)
            }
            
            ggplotly(p) 
            
          })
          
          output$lwPlotText <- renderText(paste0("Coefficients (calculated using cm and g): \n a = ", round(exp(coef(tmpMod)[1]), 3), "; b = ", round(coef(tmpMod)[2], 3), "\n Number of included specimens = ", nrow(lwDat), "\n Total number of measured = ", nrow(tmpBase), "\n Excluded (length or weight missing): \n Length = ", sum(is.na(tmpBase$length)), "; weight = ", sum(is.na(tmpBase$individualweight))))
          
        } 
      } 
      
      ### Age - length plot ####
      
      if (all(c("length", "age") %in% names(tmpBase))) {
        
        laDat <- tmpBase[!is.na(tmpBase$age) & !is.na(tmpBase$length), ]
        
        if(nrow(laDat) > 0) {
          
          output$ageData <- reactive(TRUE)
          
          if (input$laPlotSexSwitch) {
            
            laDat <- laDat %>% filter(!is.na(sex))
            
            laModF <- fishmethods::growth(age = laDat[laDat$sex == 1,]$age, size = laDat[laDat$sex == 1,]$length, Sinf = max(laDat[laDat$sex == 1,]$length), K = 0.1, t0 = 0, graph = FALSE)
            
            laModM <- fishmethods::growth(age = laDat[laDat$sex == 2,]$age, size = laDat[laDat$sex == 2,]$length, Sinf = max(laDat[laDat$sex == 2,]$length), K = 0.1, t0 = 0, graph = FALSE)
            
            laDat$sex <- as.factor(laDat$sex)
            laDat$sex <- dplyr::recode_factor(laDat$sex, "1" = "Female", "2" = "Male")
            
            output$laPlot <- renderPlotly({
              
              p <- ggplot() +
                geom_point(data = laDat, aes(x = age, y = length, color = as.factor(sex), text = paste0("cruise: ", cruise, "\nserialnumber: ", serialnumber, "\ncatchpartnumber: ", catchpartnumber, "\nspecimenid: ", specimenid))) +
                expand_limits(x = 0) +
                scale_color_manual("Sex", values = c(ColorPalette[4], ColorPalette[1])) + 
                geom_hline(yintercept = coef(laModF$vout)[1], linetype = 2, color = ColorPalette[4], alpha = 0.5) +
                geom_hline(yintercept = coef(laModM$vout)[1], linetype = 2, color = ColorPalette[1], alpha = 0.5) +
                ylab(paste0("Total length (", input$lengthUnit, ")")) +
                xlab("Age (years)") +
                theme_classic(base_size = 14) + 
                stat_function(data = data.frame(x = range(laDat$age)), aes(x),
                              fun = function(Sinf, K, t0, x) {Sinf*(1 - exp(-K*(x - t0)))},
                              args = list(Sinf = coef(laModM$vout)[1], 
                                          K = coef(laModM$vout)[2], 
                                          t0 = coef(laModM$vout)[3]),
                              color = ColorPalette[1], size = 1) +
                stat_function(data = data.frame(x = range(laDat$age)), aes(x),
                              fun = function(Sinf, K, t0, x) {Sinf*(1 - exp(-K*(x - t0)))},
                              args = list(Sinf = coef(laModF$vout)[1], 
                                          K = coef(laModF$vout)[2], 
                                          t0 = coef(laModF$vout)[3]),
                              color = ColorPalette[4], size = 1)
              
              ggplotly(p) 
            })
            
            output$laPlotText <- renderText(
              paste0("von Bertalanffy growth function coefficients\n for females and males, respectively: \n Linf (asymptotic average length) = ", round(coef(laModF$vout)[1], 3), " and ", round(coef(laModM$vout)[1], 3), " ", input$lengthUnit, 
                     "\n K (growth rate coefficient) = ", round(coef(laModF$vout)[2], 3), " and ", round(coef(laModM$vout)[2], 3), 
                     "\n t0 (length at age 0) = ", round(coef(laModF$vout)[3], 3), " and ", round(coef(laModM$vout)[3], 3), " ", input$lengthUnit, 
                     "\n tmax (life span; t0 + 3/K) = ", round(coef(laModF$vout)[3] + 3 / coef(laModF$vout)[2], 1), " and ", round(coef(laModM$vout)[3] + 3 / coef(laModM$vout)[2], 1), " years",
                     "\n Number of included specimens = ", nrow(laDat), 
                     "\n Total number of measured = ", nrow(tmpBase), 
                     "\n Excluded (length, age or sex missing): \n Length = ", sum(is.na(tmpBase$length)), "; age = ", sum(is.na(tmpBase$age)), "; sex = ", sum(is.na(tmpBase$sex))))
            
          } else {
            
            laMod <- fishmethods::growth(age = laDat$age, size = laDat$length, Sinf = max(laDat$length), K = 0.1, t0 = 0, graph = FALSE)
            
            
            output$laPlot <- renderPlotly({
              
              p <- ggplot() +
                geom_point(data = laDat, aes(x = age, y = length)) +
                expand_limits(x = 0) +
                geom_hline(yintercept = coef(laMod$vout)[1], linetype = 2, color = "grey") +
                ylab(paste0("Total length (", input$lengthUnit, ")")) +
                xlab("Age (years)") +
                theme_classic(base_size = 14) + 
                stat_function(data = 
                                data.frame(x = range(laDat$age)), aes(x),
                              fun = function(Sinf, K, t0, x) {Sinf*(1 - exp(-K*(x - t0)))},
                              args = list(Sinf = coef(laMod$vout)[1], 
                                          K = coef(laMod$vout)[2], 
                                          t0 = coef(laMod$vout)[3]),
                              color = "blue", size = 1)
              
              ggplotly(p) 
            })
            
            output$laPlotText <- renderText(paste0(
              "von Bertalanffy growth function coefficients: \n Linf (asymptotic average length) = ", round(coef(laMod$vout)[1], 3), " ", input$lengthUnit, 
              "\n K (growth rate coefficient) = ", round(coef(laMod$vout)[2], 3), 
              "\n t0 (length at age 0) = ", round(coef(laMod$vout)[3], 3), " ", input$lengthUnit, 
              "\n tmax (life span; t0 + 3/K) = ", round(coef(laMod$vout)[3] + 3 / coef(laMod$vout)[2], 1), " years", 
              "\n Number of included specimens = ", nrow(laDat), 
              "\n Total number of measured = ", nrow(tmpBase), 
              "\n Excluded (length or age missing): \n Length = ", sum(is.na(tmpBase$length)), "; age = ", sum(is.na(tmpBase$age)))
            )
          }  
        } 
      }
      
      ### L50 maturity plot  ####
      
      if (all(c("sex", "maturationstage") %in% names(tmpBase))) {
        
        l50Dat <- tmpBase[!is.na(tmpBase$sex) & !is.na(tmpBase$maturationstage), ]
        
        # l50Dat <- tmpBase %>% dplyr::filter(!is.na(sex) & !is.na(maturationstage))
        
        if(nrow(l50Dat) > 10) {
          
          output$maturityData <- reactive(TRUE)
          
          l50Dat$sex <- factor(l50Dat$sex)
          l50Dat$sex <- dplyr::recode_factor(l50Dat$sex, "1" = "Female", "2" = "Male", "3" = "Unidentified")
          
          l50Dat$maturity <- ifelse(l50Dat$maturationstage < 2, 0, ifelse(l50Dat$maturationstage >= 2, 1, NA))
          
          modF <- glm(maturity ~ length, data = l50Dat[l50Dat$sex == "Female",], family = binomial(link = "logit"))
          modM <- glm(maturity ~ length, data = l50Dat[l50Dat$sex == "Male",], family = binomial(link = "logit"))
          
          Fdat <- unlogit(0.5, modF)
          Fdat$sex <- "Female"
          Mdat <- unlogit(0.5, modM)
          Mdat$sex <- "Male"
          modDat <- rbind(Fdat, Mdat)
          
          output$l50Plot <- renderPlot({
            
            ggplot(l50Dat, aes(x = length, y = maturity, shape = sex)) + 
              geom_point() + 
              geom_segment(data = modDat, 
                           aes(x = mean, xend = mean, y = 0, yend = 0.5, color = sex),
                           linetype = 2) +
              geom_segment(data = modDat, 
                           aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5, color = sex),
                           linetype = 2) +
              geom_text(data = modDat, 
                        aes(x = mean, y = -0.03, label = paste(round(mean, 2), input$lengthUnit),
                            color = sex), size = 3) +
              stat_smooth(aes(color = sex), method="glm", 
                          method.args=list(family="binomial")) +
              ylab(paste0("Total length (", input$lengthUnit, ")")) +
              ylab("Maturity") + 
              scale_color_manual("Sex", values = c(ColorPalette[4], ColorPalette[1])) +
              scale_shape("Sex", solid = FALSE) + 
              theme_bw(base_size = 14) + 
              guides(color=guide_legend(override.aes=list(fill=NA))) + 
              theme(legend.position = c(0.9, 0.25), 
                    legend.background = element_blank(), legend.key = element_blank())
            
          })
          
          output$l50PlotText <- renderText({
            paste0("50% maturity at length (L50) based on logit regressions and assuming maturitystage >= 2 as mature:",
                   "\n\n Females: ", round(modDat[modDat$sex == "Female", "mean"], 3), " ", input$lengthUnit, ". 95% confidence intervals: ", round(modDat[modDat$sex == "Female", "ci.min"], 3), " - ", round(modDat[modDat$sex == "Female", "ci.max"], 3),
                   "\n  Number of specimens: ", nrow(l50Dat[l50Dat$sex == "Female",]),
                   "\n\n Males: ", round(modDat[modDat$sex == "Male", "mean"], 3), " ", input$lengthUnit, ". 95% confidence intervals: ", round(modDat[modDat$sex == "Male", "ci.min"], 3), " - ", round(modDat[modDat$sex == "Male", "ci.max"], 3),
                   "\n  Number of specimens: ", nrow(l50Dat[l50Dat$sex == "Male",]))
          })
        } 
        
      } 
      
      ## Sex ratio map ####
      
      if (c("sex") %in% names(tmpBase)) {
        
        srDat <- tmpBase %>% 
          dplyr::filter(!is.na(sex)) %>% 
          dplyr::group_by(cruise, startyear, serialnumber, longitudestart, latitudestart) %>% 
          dplyr::summarise(Female = sum(sex == 1), Male = sum(sex == 2), total = length(sex))
        
        if (nrow(srDat) > 0) {
          
          output$sexData <- reactive(TRUE)
          
          output$sexRatioMap <- renderLeaflet({
            
            leaflet::leaflet() %>% 
              addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
                       attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
              addMinicharts(
                srDat$longitudestart, srDat$latitudestart,
                type = "pie", chartdata = srDat[,c("Female", "Male")],
                colorPalette = c(ColorPalette[4], ColorPalette[1]),
                width = 40 * log10(srDat$total) / log10(max(srDat$total)), 
                transitionTime = 0
              )
            
          })
          
        } 
      } 
      
      ## Size distribution map ####
      
      if (all(c("cruise", "startyear", "serialnumber", "longitudestart", "latitudestart", "length") %in% names(tmpBase))) {
        
        sdDat <- tmpBase %>% 
          dplyr::filter(!is.na(length)) %>% 
          dplyr::select(cruise, startyear, serialnumber, longitudestart, latitudestart, length) %>% 
          dplyr::mutate(interval = ggplot2::cut_interval(length, n = 5)) %>% 
          dplyr::group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, interval, .drop = FALSE) %>% 
          dplyr::summarise(count = n())
        
        if(nrow(sdDat) > 0) {
          
          output$lengthData <- reactive(TRUE)
          
          sdDatW <- spread(sdDat, interval, count)
          
          sdDatW$total <- rowSums(sdDatW[,levels(sdDat$interval)])
          
          output$sizeDistributionMap <- renderLeaflet({
            
            leaflet::leaflet() %>% 
              addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
                       attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
              addMinicharts(
                sdDatW$longitudestart, sdDatW$latitudestart,
                type = "pie", chartdata = sdDatW[,levels(sdDat$interval)],
                colorPalette = viridis::viridis(5),
                width = 40 * log10(sdDatW$total) / log10(max(sdDatW$total)), 
                transitionTime = 0
              )
            
          })
          
        }
        
      }
      
      # 
      # tmp3 <- tmpBase %>% filter(!is.na(length)) %>% replace_na(list(sex = 3)) %>% mutate(sex = factor(sex)) 
      # tmp3$sex <- recode_factor(tmp3$sex, "1" = "Female", "2" = "Male", "3" = "Unidentified")
      # 
      # ggplot(tmp3, aes(x = length, color = sex)) +
      #   geom_density(adjust = 0.5) +
      #   xlab(paste0("Total length (", input$lengthUnit, ")")) +
      #   ylab("Count density") +
      #   #facet_wrap(~sex, ncol = 3, scales = "free_y") +
      #   scale_color_discrete("Sex") +
      #   theme_classic(base_size = 14)
      # 
      # tmp4 <- tmp3 %>% filter(sex != "Unidentified" & !is.na(maturationstage))
      # 
      # ggplot(tmp4, aes(x = length, stat(count),color = as.factor(maturationstage))) +
      #   geom_density(adjust = 0.5) +
      #   xlab(paste0("Total length (", input$lengthUnit, ")")) +
      #   ylab("Count density") +
      #   facet_wrap(~sex, ncol = 2, scales = "free_y") +
      #   scale_color_discrete("Maturation stage") +
      #   theme_classic(base_size = 14)
      # 
      # ggplot(tmp4, aes(x = length, stat(count), color = as.factor(specialstage))) +
      #   geom_density(adjust = 0.5) +
      #   xlab(paste0("Total length (", input$lengthUnit, ")")) +
      #   ylab("Count density") +
      #   facet_wrap(~sex, ncol = 2, scales = "free_y") +
      #   scale_color_discrete("Special stage") +
      #   theme_classic(base_size = 14)
      # 
      # Add stage & special stage
      
      # Add depth preference
      
      
    } 
    
  }) 
  
  ##..............
  ## Download ####
  
  ### Download figures ####
  
  output$downloadFigures <- downloadHandler(
    
    filename = function() "BioticExplorer_figures.zip",
    
    content = function(file) {
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL
      
      spOverviewDat <- speciesOverviewData(rv$stnall)
      
      ## Station overview figures
      
      if(!is.null(input$stationOverviewExport)) {
        
        for (i in 1:length(input$stationOverviewExport)) {
          
          fileName <- paste0(input$stationOverviewExport[i], input$downloadFigureFormat)
          
          ggplot2::ggsave(fileName, plot = get(input$stationOverviewExport[i])(spOverviewDat, base_size = 8), width = input$figureWidth, height = 0.8*input$figureWidth, units = "cm")
          
          files <- c(fileName,files)
        }
      }
      
      ## Station maps
      
      if(!is.null(input$stationMapExport)) {
        
        for(i in 1:length(input$stationMapExport)) {
          
          fileName <- paste0(input$stationMapExport[i], input$downloadFigureFormat)
          
          mapview::mapshot(get(input$stationMapExport[i])(spOverviewDat), file = fileName)
          
          files <- c(fileName,files)
          
        }
        
      }
      
      if(is.null(files)) {
        stop("Select figures to download")
      } else {
        zip(file,files)
      }
    }
  )
  
  ### Download data ####
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      if (length(input$downloadDataType) == 1 & !"original" %in% input$downloadDataType) {
        paste0(input$downloadDataType, input$downloadFileType)
      } else if (input$downloadFileType == ".rda") {
        "BioticExplorer_data.rds"
      } else if (input$downloadFileType == ".xlsx" & !"original" %in% input$downloadDataType) {
        "BioticExplorer_data.xlsx"
      } else {
        "BioticExplorer_data.zip"
      }
    },
    
    content = function(file) {
      
      if (sapply(strsplit(file, "\\."), "[", 2) == "zip") {
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL
        
        #loop through the sheets
        for (i in 1:length(input$downloadDataType)) {
          
          fileName <- paste0(input$downloadDataType[i], ".csv")
          write.csv(
            eval(parse(text = paste("rv", input$downloadDataType[i], sep = "$"))),
            fileName, row.names = FALSE) 
          files <- c(fileName,files)
        }
        #create the zip file
        zip(file,files)
        
      } else if (sapply(strsplit(file, "\\."), "[", 2) == "rds") {
        
        biotic <- lapply(input$downloadDataType, function(k) {
          eval(parse(text = paste("rv", k, sep = "$")))
        })
        
        names(biotic) <- input$downloadDataType
        
        saveRDS(biotic, file = file)
        
      } else if (sapply(strsplit(file, "\\."), "[", 2) == "xlsx") {
        wb <- openxlsx::createWorkbook()
        
        for (i in 1:length(input$downloadDataType)) {
          openxlsx::addWorksheet(wb, paste(input$downloadDataType[i]))
          openxlsx::writeData(wb, paste(input$downloadDataType[i]), 
                              eval(parse(text = paste("rv", input$downloadDataType[i], sep = "$"))))
        }
        
        openxlsx::saveWorkbook(wb, file)
        
      } else {
        tmp <- switch(input$downloadDataType,
                      "mission" = rv$mission,
                      "stnall" = rv$stnall,
                      "indall" = rv$indall)
        
        write.csv(tmp, file, row.names = FALSE) 
      }
    }
  )
  
  
})

##........................
## Compile to the app ####

shinyApp(ui, server)
