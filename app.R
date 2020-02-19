## Libraries required to run the app ####

require(shiny)

### Define operating system

if (Sys.info()["sysname"] == "Windows") {
  os <- "Win"
} else if (Sys.info()["sysname"] == "Darwin") {
  os <- "Mac"
} else if (Sys.info()["sysname"] == "Linux") {
  os <- "Linux"
  if(!capabilities()["X11"] && capabilities()["cairo"])
    options(bitmapType = 'cairo')
}

### OS specific exceptions

### Install missing packages

required.packages <- c("shiny", "shinyFiles", "shinydashboard", "DT", "tidyverse", "devtools", "leaflet", "leaflet.minicharts", "plotly", "openxlsx", "data.table", "scales", "fishmethods", "viridis", "DBI", "dplyr")

new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if (length(new.packages) > 0) install.packages(new.packages)

sapply(required.packages, require, character.only = TRUE)

if (!"RstoxData" %in% installed.packages()[,"Package"]) {
  devtools::install_github("StoXProject/RstoxData")
  require(RstoxData)
} else {
  require(RstoxData)
}

if (!"MonetDBLite" %in% installed.packages()[,"Package"]) {
  devtools::install_github("hannesmuehleisen/MonetDBLite-R")
}

## Source functions used by the app

source("other_functions.R", encoding = "utf-8")
source("processBiotic_functions.R", encoding = "utf-8")
source("figure_functions.R", encoding = "utf-8")
source("sql_functions.R", encoding = "utf-8")

##____________________
## User interface ####

##..............
## Settings  and definitions####

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
dropdownMenu(type = "notifications", headerText = "Version 0.3.5 (alpha), 2020-01-27",
             icon = icon("cog"), badgeStatus = NULL,
             notificationItem("Download NMD data", icon = icon("download"), status = "info", href = "https://datasetexplorer.hi.no/"),
             notificationItem("Explanation of data types and codes", icon = icon("question-circle"), status = "info", href = "https://hinnsiden.no/tema/forskning/PublishingImages/Sider/SPD-gruppen/H%C3%A5ndbok%205.0%20juli%202019.pdf#search=h%C3%A5ndbok%20pr%C3%B8vetaking"),
             notificationItem("Data policy", icon = icon("creative-commons"), status = "info", href = "http://www.imr.no/filarkiv/2013/03/datapolitikk_nmd.pdf/nb-no")
)
)

##_____________
## Sidebar ####

sidebar <- dashboardSidebar(sidebarMenu(
  # Setting id makes input$tabs give the tabName of currently-selected tab
  id = "tabs",
  
  menuItem("Information", tabName = "info", icon = icon("info-circle")),
  
  menuItem("Load data & filter", tabName = "upload", icon = icon("arrow-circle-up")
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
           menuSubItem("Export figures", tabName = "exportFigures", icon = icon("file-image")),
           menuSubItem("Download data", tabName = "downloadDatasets", icon = icon("table"))
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
                       p("This is a", a("Shiny app", href = "http://shiny.rstudio.com"), "allowing examination and manipulation of the Norwegian Maritime Data-center (NMD) standard xml files, which are used by the Institute of Marine Research. To start with the app, click the", strong("'Upload & filter'"), "tab on the side panel."),
                       h4("Work-flow"),
                       p("1)", strong("Upload data:"), "Click 'Browse..' and select an xml file from your computer. An overview of data and sampling station locations will be shown under. You can use the available options to remove data that are not relevant."),
                       p("2)", strong("Filter data:"), "Use the 'Filter data by' options to select data you want to keep. Click the 'Subset' button once you are ready and see how the overview will change based on the information you selected."),
                       p("3) You can examine the station and catch data by clicking the 'Stations & catches' tab. Use the 'Overview' sub-tab for a graphical overview or the 'Examine' tab for a tabular overview, which you can filter and search as you wish, but note that filtering here does not influence the returned data."),
                       p("4) Similarly, an overview of individual measured fish is given under 'Individuals & ages' tab."),
                       p("5) 'Mission data' through 'Age data' tabs give a tabular overview of each data type in the NMD xml Biotic file."),
                       p("6)", strong("Download"), "filtered data using the 'Download' tab. Select the format you want to download in (R, csv or Excel). If you select multiple data types, note that the csv format will be returned as a zip file. Downloading zip files might not work if you run the app in RStudio window. Try again using the 'Run External' option (i.e. run the app in web-browser."),
                       br(),
                       br(),
                       h5("Authors: The StoX project team (Mikko Vihtakari, Ibrahim Umar)", align = "left"),
                       h5("Contact person: Mikko Vihtakari (mikko.vihtakari@hi.no)", align = "left"),
                       h5("(c) Institute of Marine Research, Norway, acknowledging the", a("RStudio team, Shiny developers", href = "https://www.rstudio.com/about/"), "and the", a("community", hfer = "https://stackoverflow.com/questions/tagged/shiny"), align = "left")
                )
              )
      ),
      
      ## Upload tab ####
      
      tabItem("upload",
              
              fluidRow(
                
                ## Column 1 ###
                column(width = 6,
                       
                       box(
                         title = "1. Upload NMD Biotic xml files", status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE, 
                         strong("a) UPLOAD BIOTIC DATA"),
                         br(),
                         strong("Performance mode:"),
                         checkboxInput("performanceMode", "For large (>200 Mb) files. Disables features that burden memory.", FALSE),
                         
                         fileInput("file1",
                                   label = "Choose xml input file",
                                   multiple = TRUE,
                                   accept = c(".xml", ".rds")
                         ),
                         em("Note that processing takes some time for large files even after 'Upload complete' shows up. Be patient."),
                         hr(),
                         strong("b) GET BIOTIC DATA FROM DATABASE"),
                         br(),
                         actionButton("doFetchDB", "Fetch all NMD biotic data from database"),
                         br(),
                         br(),
                         em("Please wait as loading all year data takes time. Be patient."),
                         br(),
                         strong(textOutput("db_info")),
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
                         title = "Quick overview", width = NULL, status = "primary",
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
                                  selectInput(inputId = "subYear", label = "Year:",
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subCruise", label = "Cruise number:",
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subPlatform", label = "Platform name:",
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subDateFrom", label = "Date from:",
                                              choices = "Not implemented yet", multiple = TRUE)
                           ),
                           
                           column(6,
                                  selectInput(inputId = "subSpecies", label = "Species:", 
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subSerialnumber", 
                                              label = "Serial number:",
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subGear", label = "Gear code:",
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subDateTo", label = "Date to:",
                                              choices = "Not implemented yet", multiple = TRUE)
                           )),
                         
                         
                         sliderInput(inputId = "subLon", label = "Longitude:", min = -180, 
                                     max = 180, value = c(-180, 180)),
                         sliderInput(inputId = "subLat", label = "Latitude:", min = -90, 
                                     max = 90, value = c(-90, 90)),
                         
                         actionButton(inputId = "Subset", label = "Subset"),
                         actionButton(inputId = "Reset", label = "Reset")
                         
                         # , verbatimTextOutput("test")
                         
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
      
      ### Stnall tab ####      
      tabItem("stnallOverview",
              fluidRow(
                box(title = "Species composition", width = 12, status = "info", solidHeader = TRUE, plotOutput("speciesCompositionPlot")),
                
                box(title = "Total catch weight", width = 12, status = "info", solidHeader = TRUE, plotlyOutput("catchweightSumPlot")),
                
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
                    DT::dataTableOutput("individualSummaryTable")
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
      tabItem("fishstationExamine", DT::dataTableOutput("fishstation")),
      tabItem("catchsampleExamine", DT::dataTableOutput("catchsample")),
      tabItem("individualExamine", DT::dataTableOutput("individualTable")),
      tabItem("agedeterminationExamine", DT::dataTableOutput("agedeterminationTable")),
      
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
      ## Debugging tab ####
      
      # tabItem("debugging",
      #         )
      #       
      
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
                                     c("Stations & catches" = "stnall",
                                       "Individuals & ages" = "indall"
                                       #"Original format" = "original"
                                     ),
                                     selected = c("stnall", "indall")
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
  
  ## Read data ####
  
  rv <- reactiveValues(stnall = NULL, indall = NULL)
  
  observeEvent(input$file1, {
    
    tryCatch({
      
      if (length(input$file1[[1]]) > 1) {
        rv$inputData <- processBioticFiles(files = input$file1$datapath, lengthUnit = input$lengthUnit, weightUnit = input$weightUnit, removeEmpty = input$removeEmpty, coreDataOnly = input$coreDataOnly, dataTable = TRUE, convertColumns = TRUE)
      } else {
        rv$inputData <- processBioticFile(file = input$file1$datapath, lengthUnit = input$lengthUnit, weightUnit = input$weightUnit, removeEmpty = input$removeEmpty, coreDataOnly = input$coreDataOnly, dataTable = TRUE, convertColumns = TRUE)
      }
    },
    error = function(e) {
      stop(safeError(e))
    }
    )

    rv$stnall <- rv$inputData$stnall
    rv$indall <- rv$inputData$indall

    rv$mission <- rv$inputData$mission
    rv$fishstation <- rv$inputData$fishstation
    rv$catchsample <- rv$inputData$catchsample
    rv$individual <- rv$inputData$individual
    rv$agedetermination <- rv$inputData$agedetermination

    rv$stnall <- rv$inputData$stnall
    rv$indall <- rv$inputData$indall
    
    rv$mission <- rv$inputData$mission
    rv$fishstation <- rv$inputData$fishstation
    rv$catchsample <- rv$inputData$catchsample
    rv$individual <- rv$inputData$individual
    rv$agedetermination <- rv$inputData$agedetermination

    obsPopulatePanel()

  })
  
  # Get data from DB
  observeEvent(input$doFetchDB, {
    
    print("Download from DB")

    rv$inputData <- processBioticDB(con_db, lengthUnit = input$lengthUnit, weightUnit = input$weightUnit, removeEmpty = input$removeEmpty, coreDataOnly = input$coreDataOnly, dataTable = TRUE, convertColumns = TRUE)

    rv$stnall <- rv$inputData$stnall
    rv$indall <- rv$inputData$indall

    rv$mission <- rv$inputData$mission
    rv$fishstation <- rv$inputData$fishstation
    rv$catchsample <- rv$inputData$catchsample
    rv$individual <- rv$inputData$individual
    rv$agedetermination <- rv$inputData$agedetermination

    rv$stnall <- rv$inputData$stnall
    rv$indall <- rv$inputData$indall
    
    rv$mission <- rv$inputData$mission
    rv$fishstation <- rv$inputData$fishstation
    rv$catchsample <- rv$inputData$catchsample
    rv$individual <- rv$inputData$individual
    rv$agedetermination <- rv$inputData$agedetermination

    # Populate info
    output$db_info <-  renderText({rv$inputData$info})

    obsPopulatePanel()

    
  })

updateSelectors <- function() {

    # Update selectors
    rv$all <- list()
    rv$all$startyear <- rv$stnall %>% select(startyear) %>% distinct() %>% arrange(startyear) %>% pull()
    rv$all$commonname <- rv$stnall %>% select(commonname) %>% distinct() %>% arrange(commonname) %>% pull()
    rv$all$cruise <- rv$stnall %>% select(cruise) %>% distinct() %>% arrange(cruise) %>% pull()
    rv$all$platformname <- rv$stnall %>% select(platformname) %>% distinct() %>% arrange(platformname) %>% pull()
    rv$all$serialnumber <- rv$stnall %>% select(serialnumber) %>% distinct() %>% arrange(serialnumber) %>% pull()
    rv$all$gear <- rv$stnall %>% select(gear) %>% distinct() %>% arrange(gear) %>% pull()

    rv$all$indSpecies <- rv$indall %>%
		filter(!is.na(length) & !is.na(individualweight)) %>%
		group_by(commonname) %>% tally() %>% filter(n > 1) %>%
		select(commonname) %>% distinct() %>% arrange(commonname) %>% pull()

    lon <- rv$stnall %>% summarise(min = min(longitudestart), max = max(longitudestart)) %>% collect()
    lat <- rv$stnall %>% summarise(min = min(latitudestart), max = max(latitudestart)) %>% collect()

    rv$all$min.lon <- floor(lon$min)
    rv$all$max.lon <- ceiling(lon$max)
    rv$all$min.lat <- floor(lat$min)
    rv$all$max.lat <- ceiling(lat$max)

    rv$all$date <- rv$stnall %>% summarise(min = min(stationstartdate), max = max(stationstartdate)) %>% collect()
}

updateFilterform <- function() {
    updateSelectInput(session, "subYear", choices = rv$all$startyear, selected = rv$sub$year)
    updateSelectInput(session, "subSpecies", choices = rv$all$commonname, selected = rv$sub$species)
    updateSelectInput(session, "subCruise", choices = rv$all$cruise, selected = rv$sub$cruise)
    updateSelectInput(session, "subPlatform", choices = rv$all$platformname, selected = rv$sub$platform)
    updateSelectInput(session, "subSerialnumber", choices = rv$all$serialnumber, selected = rv$sub$serialnumber)
    updateSelectInput(session, "subGear", choices = rv$all$gear, selected = rv$sub$gear)

    updateSelectInput(session, "catchMapSpecies", choices = c("All", rv$stnall$commonname))
    updateSelectInput(session, "indSpecies", choices = c("Select a species to generate the plots", rv$all$indSpecies))

    updateSliderInput(session, "subLon", min = rv$all$min.lon, max = rv$all$max.lon, value = rv$sub$lon, step = 0.1)
    updateSliderInput(session, "subLat", min = rv$all$min.lat, max = rv$all$max.lat, value = rv$sub$lat, step = 0.1)
}

updateMap <- function() {
    ## Station map ##
    if (!input$performanceMode) {

      output$stationMap <- renderLeaflet({
        
        leaflet::leaflet(rv$stnall %>% select(missiontype, startyear, platform, platformname, missionnumber, missionid, serialnumber, latitudestart, longitudestart) %>% 
                          filter(!is.na(longitudestart) & !is.na(latitudestart)) %>% distinct() %>% collect()) %>% 
          addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
                   attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
          addRectangles(
            lng1 = rv$sub$lon[1], lat1 = rv$sub$lat[1], lng2 = rv$sub$lon[2], lat2 = rv$sub$lat[2],
            fillColor = "transparent") %>% 
          addCircleMarkers(lat = ~ latitudestart, lng = ~ longitudestart, 
                     weight = 1, radius = 2, 
                     popup = ~as.character(platformname), 
                     label = ~as.character(serialnumber), 
                     color = "red", fillOpacity = 0.5,
                     clusterOptions = markerClusterOptions()
          )
      })
    } else {
      output$stationMap <- NULL
    }
}

obsPopulatePanel <- function() {

    # Reset all subset
    rv$sub <- list()

    # Inform that a subset has never been performed
    rv$substart <- FALSE

    # Update selectors
    updateSelectors()

    # Reset some specific subsets as this is the first run
    rv$sub$lon <- c(rv$all$min.lon, rv$all$max.lon)
    rv$sub$lat <- c(rv$all$min.lat, rv$all$max.lat)

    # Reset form too
    updateFilterform()

    # Update stats
    output$nStationsBox <- renderValueBox({
      valueBox(
        value = tags$p(rv$fishstation %>% count() %>% pull(), style = "font-size: 80%;"),
        subtitle = "Stations"
      )
    })
    
    output$nYearsBox <- renderValueBox({
      valueBox(
        length(unique(rv$all$startyear)),
        "Unique years"
      )
    })
    
    output$nSpeciesBox <- renderValueBox({
      valueBox(
        length(unique(rv$all$commonname)),
        "Unique species"
      )
    })

    output$DateStartBox <- renderValueBox({
      valueBox(
        value = tags$p(rv$all$date$min,
                       style = "font-size: 80%;"),
        subtitle = "First date"
      )
    })
    
    output$DateEndBox <- renderValueBox({
      valueBox(
        value = tags$p(rv$all$date$max,
                       style = "font-size: 80%;"),
        subtitle = "Last date"
      )
    })
    
    output$nMeasuredBox <- renderValueBox({
      valueBox(
        value = tags$p(rv$individual %>% count() %>% pull(), style = "font-size: 60%;"),
        subtitle = "Measured specimen"
      )
    })
    
    output$nCruisesBox <- renderValueBox({
      valueBox(
        rv$mission %>% count() %>% pull(),
        "Cruises"
      )
    })
    
    output$nGearsBox <- renderValueBox({
      valueBox(
        length(unique(rv$all$gear)),
        "Gear types"
      )
    })

    updateMap()
}

  
  ##.................
  ## Test output ####
  
  # output$test <- renderText({
  # 
  #   # length(input$file1[[1]])
  #   # paste(input$file1[[1]], collapse = "; ")
  #   paste(input$indSpecies, collapse = "; ")
  #   # paste(dim(rv$indall), collapse = "; ")
  # })
  
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
    # Make a chain of filters
    filterChain <- list()

    rv$sub$year <- input$subYear
    if (!is.null(input$subYear)) { 
      filterChain <- append(filterChain, paste0("startyear %in% '", input$subYear, "'"))
    }

    rv$sub$species <- input$subSpecies    
    if (!is.null(input$subSpecies)) {
      filterChain <- append(filterChain, paste0("commonname %in% '", input$subSpecies, "'"))
    }

    rv$sub$cruise <- input$subCruise
    if (!is.null(input$subCruise)) {
      filterChain <- append(filterChain, paste0("cruise %in% '", input$subCruise, "'"))
    }

    rv$sub$platform <- input$subPlatform    
    if (!is.null(input$subPlatform)) {
      filterChain <- append(filterChain, paste0("platformname %in% '", input$subPlatform, "'"))
    }

    rv$sub$serialnumber <- input$subSerialnumber   
    if (!is.null(input$subSerialnumber)) {
      filterChain <- append(filterChain, paste0("serialnumber %in% '", input$subSerialnumber, "'")) 
    }

    rv$sub$gear <- input$subGear
    if (!is.null(input$subGear)) {
      filterChain <- append(filterChain, paste0("gear %in% '", input$subGear, "'"))
    }

    if (!identical(as.numeric(input$subLon), c(rv$all$min.lon, rv$all$max.lon))) {
      rv$sub$lon <- as.numeric(input$subLon)
      filterChain <- append(filterChain, paste0("longitudestart >= '", input$subLon[1], "' & longitudestart <= '", input$subLon[2], "'"))
    } else {
      rv$sub$lon <- NULL
    }
    
    if (!identical(as.numeric(input$subLat), c(rv$all$min.lat, rv$all$max.lat))) {
      rv$sub$lat <- as.numeric(input$subLat)
      filterChain <- append(filterChain, paste0("latitudestart >= '", input$subLat[1], "' & latitudestart <= '", input$subLat[2], "'"))
    } else {
      rv$sub$lat <- NULL
    }
    
    
    # Check whether there is a subset?
    if(length(filterChain) > 0) {
	    filterChain <- paste(filterChain, collapse = " & ")
	    rv$substart <- TRUE

	    ### Stnall subsetting
	    
	    # rv$stnall <- rv$inputData$stnall %>% dplyr::filter(
	    #   startyear %in% rv$sub$year,
	    #   commonname %in% rv$sub$species,
	    #   cruise %in% rv$sub$cruise,
	    #   platformname %in% rv$sub$platform,
	    #   serialnumber %in% rv$sub$serialnumber,
	    #   gear %in% rv$sub$gear,
	    #   longitudestart >= rv$sub$lon[1],
	    #   longitudestart <= rv$sub$lon[2],
	    #   latitudestart >= rv$sub$lat[1],
	    #   latitudestart <= rv$sub$lat[2]
	    # )
	    
            # TODO: Investigate the below behaviour!!!
	    if(any(class(rv$inputData$stnall) %in% c("data.table"))) {
                rv$stnall <- rv$inputData$stnall %>% filter(rlang::eval_tidy(rlang::parse_expr(filterChain)))
                rv$indall <- rv$inputData$indall %>% filter(rlang::eval_tidy(rlang::parse_expr(filterChain)))
            } else {
	        rv$stnall <- rv$inputData$stnall %>% filter(rlang::parse_expr(filterChain))
                rv$indall <- rv$inputData$indall %>% filter(rlang::parse_expr(filterChain))
	    }

	    ### Indall subsetting
	    
	    # rv$indall <- rv$inputData$indall[rv$inputData$indall$commonname %in% rv$sub$species,]
	    
	    # rv$indall <- rv$inputData$indall %>% dplyr::filter(
	    #   startyear %in% rv$sub$year,
	    #   commonname %in% rv$sub$species,
	    #   cruise %in% rv$sub$cruise,
	    #   platformname %in% rv$sub$platform,
	    #   serialnumber %in% rv$sub$serialnumber,
	    #   gear %in% rv$sub$gear,
	    #   longitudestart >= rv$sub$lon[1],
	    #   longitudestart <= rv$sub$lon[2],
	    #   latitudestart >= rv$sub$lat[1],
	    #   latitudestart <= rv$sub$lat[2]
	    # )
	    
	    # rv$indall <- rv$inputData$indall[
	    #     commonname %in% rv$sub$species,
	    #   ]
	    #     
	    
	    ### Mission subsetting
	    uniqueLvOne <- rv$stnall %>% select(missiontype, startyear, platform, missionnumber, missionid) %>% distinct()
	    rv$mission <- uniqueLvOne %>% left_join(rv$inputData$mission)
	    
	    ### Other subsetting (to be removed?)
	    uniqueLvTwo <- rv$stnall %>% select(missiontype, startyear, platform, missionnumber, missionid, serialnumber) %>% distinct()
	    rv$fishstation <- uniqueLvTwo %>% left_join(rv$inputData$fishstation)

            uniqueLvThree <- rv$stnall %>% select(missiontype, startyear, platform, missionnumber, missionid, serialnumber, catchsampleid) %>% distinct()
	    rv$catchsample <-  uniqueLvThree %>% left_join(rv$inputData$catchsample)

	    uniqueLvFour <- rv$indall %>% select(missiontype, startyear, platform, missionnumber, missionid, serialnumber, catchsampleid, specimenid) %>% distinct()
	    rv$individual <- uniqueLvFour %>% left_join(rv$inputData$individual)
	    rv$agedetermination <- uniqueLvFour %>% left_join(rv$inputData$agedetermination)

           # Update selectors
           updateSelectors()
    }
    # Lon Lat needs update
    if(is.null(rv$sub$lon))
          rv$sub$lon <- c(rv$all$min.lon, rv$all$max.lon)
    if(is.null(rv$sub$lat))
          rv$sub$lat <- c(rv$all$min.lat, rv$all$max.lat)
    
    updateFilterform()
  })
  
  ##...............
  ## Resetting ####
  
  observeEvent(input$Reset, {
    
    # Only run if there is a previous subset
    if(req(rv$substart) && rv$substart) {
	    rv$stnall <- rv$inputData$stnall
	    rv$indall <- rv$inputData$indall
	    
	    rv$mission <- rv$inputData$mission
	    rv$fishstation <- rv$inputData$fishstation
	    rv$catchsample <- rv$inputData$catchsample
	    rv$individual <- rv$inputData$individual
	    rv$agedetermination <- rv$inputData$agedetermination

      # Update selectors
	    updateSelectors()

	    # Reset some specific subsets as this is reset
      rv$substart <- FALSE
	    rv$sub <- list()
	    rv$sub$lon <- c(rv$all$min.lon, rv$all$max.lon)
	    rv$sub$lat <- c(rv$all$min.lat, rv$all$max.lat)

      # Update filterform
      updateFilterform()
    }

  })
   
  ##.................
  ## Data tables ####

  # Helper for switching between data.table and SQL source
  prepareDT <- function(src_dt, tblname, id) { 
    if(!any(class(src_dt) %in% c("data.table"))) {
	return(DTfromSQL(session, con_db, tblname, id))
    } else {
        return(DT::datatable(src_dt,
                  options = list(scrollX = TRUE,
                                 pageLength = 20
                  )))
    }
  }

  output$stnall <- DT::renderDataTable({
    prepareDT(rv$stnall, "stndat", "stnall") %>% formatRound(columns = c(FALSE, grepl("latitude|longitude|distance|weight|speed|soaktime", colnames(rv$stnall))), mark = " ")
  })
  
  output$indall <- DT::renderDataTable({
     prepareDT(rv$indall, "inddat", "indall") %>% formatRound(c("longitudestart", "latitudestart", "length", "individualweight"))
  })
  
  output$missionTable <- DT::renderDataTable({
     prepareDT(rv$mission, "mission", "mission")
  })
  
  output$fishstation <- DT::renderDataTable({
     prepareDT(rv$fishstation, "fishstation", "fishstation") %>% formatRound(c("longitudestart", "latitudestart", "distance"))
  })
  
  output$catchsample <- DT::renderDataTable({
     prepareDT(rv$catchsample, "catchsample", "catchsample") %>% formatRound(c("catchweight", "lengthsampleweight"))
  })
  
  output$individualTable <- DT::renderDataTable({
     prepareDT(rv$individual, "individual", "individualTable")
  })
  
  output$agedeterminationTable <- DT::renderDataTable({
     prepareDT(rv$agedetermination, "agedetermination", "agedeterminationTable")
  })
  
  ##......................
  ## Stn data figures ####
  
  observeEvent(c(req(input$file1)), {
    
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
    
    ## Stn data maps ####
    
    ## Catch composition map
    
    output$catchCompMap <- renderLeaflet(catchCompMap(spOverviewDat))
    
  })
  
  
  observeEvent(c(req(input$catchMapSpecies)), {
    
    ## Catch map ###
    
    output$catchMap <- renderLeaflet(catchMap(rv$stnall, species = input$catchMapSpecies))
    
  }) 
  
  ##..........................................
  ## Ind data overview figures and tables ####
  
  observeEvent(c(req(input$file1)), {
    
    indSumTab <- rv$indall %>% 
      dplyr::group_by(commonname) %>% 
      dplyr::summarise(Total = length(commonname), 
                       Length = {if("length" %in% names(.)) sum(!is.na(length)) else 0},
                       Weight = {if("individualweight" %in% names(.)) sum(!is.na(individualweight)) else 0}, 
                       Sex = {if("sex" %in% names(.)) sum(!is.na(sex)) else 0}, 
                       Maturationstage = {if("maturationstage" %in% names(.)) sum(!is.na(maturationstage)) else 0}, 
                       Specialstage = {if("specialstage" %in% names(.)) sum(!is.na(specialstage)) else 0}, 
                       Age = {if("age" %in% names(.)) sum(!is.na(age)) else 0}
      ) 
    
    output$individualSummaryTable <- DT::renderDataTable({
      DT::datatable(indSumTab, options = list(searching = FALSE))
    })
    
    
    # meanIndLStn <- rv$indall %>% filter(!is.na(length)) %>% group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, commonname) %>% summarise(meanLength = mean(length))
    # 
    # meanIndL <- meanIndLStn %>% group_by(commonname) %>% summarise(mean = mean(meanLength), sd = sd(meanLength), se = se(meanLength), n = length(meanLength))
    # 
    # ggplot(tmp, aes(x = commonname, y = meanLength)) + geom_point()
    # 
    # tmp <- rv$indall %>% filter(!is.na(length) & !is.na(individualweight)) %>% group_by(commonname) %>% filter(n() > 10) %>% summarise(ricker = sd(log(individualweight))/sd(log(length)), n = n())
    #
    # output$rickerPlot <- renderPlot({
    #   
    #   ggplot() + 
    #     geom_hline(yintercept = 3) + 
    #     geom_text(data = tmp, aes(x = commonname, y = Inf, label = n), vjust = 1) +
    #     geom_point(data = tmp, aes(x = commonname, y = ricker)) + 
    #     ylab("Sd(log(sum(weight)))/Sd(log(sum(length)))") +
    #     xlab("Species database name") +
    #     theme_classic(base_size = 14) +
    #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    #   
    # })
    
    # Mean individual length & sex
    # Mean length - depth
    
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
        # lwDat <- tmpBase %>% dplyr::filter(!is.na(length) & !is.na(individualweight))
        
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
                scale_x_log10(paste0("Total length [log10(", input$lengthUnit, ")]")) +
                scale_y_log10(paste0("Weight [log10(", input$weightUnit, ")]")) + 
                geom_smooth(data = lwDat, aes(x = length, y = individualweight), method = "lm", se = TRUE) 
              
            } else {
              p <- p + 
                scale_x_continuous(paste0("Total length (", input$lengthUnit, ")")) +
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
        wb <- createWorkbook()
        
        for (i in 1:length(input$downloadDataType)) {
          addWorksheet(wb, paste(input$downloadDataType[i]))
          writeData(wb, paste(input$downloadDataType[i]), 
                    eval(parse(text = paste("rv", input$downloadDataType[i], sep = "$"))))
        }
        
        saveWorkbook(wb, file)
        
      } else {
        tmp <- switch(input$downloadDataType,
                      "stnall" = rv$stnall,
                      "indall" = rv$indall)
        
        write.csv(tmp, file, row.names = FALSE) 
      }
    }
  )
  
  
})

## LOAD DB first
con_db <- dbConnect(MonetDBLite::MonetDBLite(), "/data/duckdb/allyear.monetdb")

## To resolve conflicting select
select <- dplyr::select

##........................
## Compile to the app ####

shinyApp(ui, server)
