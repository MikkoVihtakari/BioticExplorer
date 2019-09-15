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

if (os == "Linux") {
  
  if (!capabilities()["X11"]) {
    options(bitmapType = "cairo")
    
    required.packages <- c("shiny", "shinyFiles", "shinydashboard", "DT", "tidyverse", "devtools", "leaflet", "leaflet.minicharts", "plotly", "openxlsx", "dplyr", "data.table", "scales", "fishmethods", "viridis", "Cairo") 
  }
  
} else {
  required.packages <- c("shiny", "shinyFiles", "shinydashboard", "DT", "tidyverse", "devtools", "leaflet", "leaflet.minicharts", "plotly", "openxlsx", "dplyr", "data.table", "scales", "fishmethods", "viridis")
}

### Install missing packages

new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if (length(new.packages) > 0) install.packages(new.packages)

sapply(required.packages, require, character.only = TRUE)

if (!"RstoxData" %in% installed.packages()[,"Package"]) {
  devtools::install_github("StoXProject/RstoxData")
  require(RstoxData)
} else {
  require(RstoxData)
}

## Source functions used by the app

source("functions.R", encoding = "utf-8")

##____________________
## User interface ####

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
dropdownMenu(type = "notifications", headerText = "Version 0.2.5 (alpha), 2019-09-15",
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
  menuItem("Upload & filter", tabName = "upload", icon = icon("arrow-circle-up")),
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
  menuItem("Mission data", icon = icon("bar-chart-o"), tabName = "missionExamine"
  ),
  menuItem("Station data", icon = icon("bar-chart-o"), tabName = "fishstationExamine"
  ),
  menuItem("Catch data", icon = icon("bar-chart-o"), tabName = "catchsampleExamine"
  ),
  menuItem("Individual data", icon = icon("bar-chart-o"), tabName = "individualExamine"
  ),
  menuItem("Age data", icon = icon("bar-chart-o"), tabName = "agedeterminationExamine"
  ),
  menuItem("Download", tabName = "downloadDatasets", icon = icon("arrow-circle-down"))
),
textOutput("res")
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
                       h5("Author and contact person: Mikko Vihtakari (mikko.vihtakari@hi.no)", align = "left"),
                       h5("(c) Institute of Marine Research, Norway, acknowledging the", a("RStudio team and Shiny developers", href = "https://www.rstudio.com/about/"), align = "left")
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
                         
                         strong("Performance mode:"),
                         checkboxInput("performanceMode", "For large (>200 Mb) files. Disables features that burden memory.", FALSE),
                         
                         fileInput("file1",
                                   label = "Choose xml input file",
                                   multiple = TRUE,
                                   accept = c(
                                     ".xml"
                                   )
                         ),
                         "Note that processing takes some time for large files even after 'Upload complete' shows up. Be patient.",
                         hr(),
                         
                         strong("Drop excess data:"),
                         checkboxInput("removeEmpty", "Remove empty columns", FALSE),
                         checkboxInput("coreDataOnly", "Keep only important columns", TRUE),
                         
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
                                      inline = TRUE),
                         
                         strong("Remove outliers based on:"),
                         checkboxInput("weightOutliers", "Individual weight", FALSE),
                         checkboxInput("lengthOutliers", "Individual length", FALSE)
                         
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
                                  selectInput(inputId = "subSurveySeries", 
                                              label = "Survey series:",
                                              choices = "Not implemented yet", multiple = TRUE),
                                  selectInput(inputId = "subCruise", label = "Cruise number:",
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subPlatform", label = "Platform name:",
                                              choices = NULL, multiple = TRUE)
                           ),
                           
                           column(6,
                                  selectInput(inputId = "subSpecies", label = "Species:", 
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subSerialnumber", 
                                              label = "Serial number:",
                                              choices = NULL, multiple = TRUE),
                                  selectInput(inputId = "subGear", label = "Gear code:",
                                              choices = NULL, multiple = TRUE)
                           )),
                         
                         
                         sliderInput(inputId = "subLon", label = "Longitude:", min = -180, 
                                     max = 180, value = c(-180, 180)),
                         sliderInput(inputId = "subLat", label = "Latitude:", min = -90, 
                                     max = 90, value = c(-90, 90)),
                         actionButton(inputId = "Subset", label = "Subset")
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
                box(title = "Length - weight relationship", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    selectInput("indSpecies", "Select species:", 
                                choices = NULL),
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
                
                box(title = "Age - length relationship", width = 12, status = "info", 
                    solidHeader = TRUE, plotlyOutput("laPlot"),
                    br(),
                    column(4,
                           checkboxInput("laPlotSexSwitch", "Separate by sex", FALSE)),
                    column(8,
                           actionButton("laPlotExcludeSwitch", "Exclude points"),
                           actionButton("laPlotResetSwitch", "Reset")),
                    column(12,
                           verbatimTextOutput("laPlotText"))
                ),
                
                box(title = "50% maturity based on length (L50)", width = 12, 
                    status = "info", solidHeader = TRUE,
                    plotOutput("l50Plot"),
                    verbatimTextOutput("l50PlotText")
                ),
                
                box(title = "Sex ratio", width = 12, status = "info", 
                    solidHeader = TRUE, height = 760,
                    leafletOutput(outputId = "sexRatioMap", height = 700)
                ),
                
                box(title = "Size distribution", width = 12, status = "info", 
                    solidHeader = TRUE, height = 760,
                    leafletOutput(outputId = "sizeDistributionMap", height = 700)
                )
              )
      ),
      
      tabItem("indallExamine", DT::dataTableOutput("indall")),
      
      tabItem("missionExamine", DT::dataTableOutput("missionTable")),
      tabItem("fishstationExamine", DT::dataTableOutput("fishstation")),
      tabItem("catchsampleExamine", DT::dataTableOutput("catchsample")),
      tabItem("individualExamine", DT::dataTableOutput("individualTable")),
      tabItem("agedeterminationExamine", DT::dataTableOutput("agedeterminationTable")),
      
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
  
  rv <- reactiveValues()
  
  observeEvent(req(input$file1), {
    
    tryCatch({
      
      if (length(input$file1[[1]]) > 1) {
        dat <- processBioticFiles(files = input$file1$datapath, lengthUnit = input$lengthUnit, weightUnit = input$weightUnit, removeEmpty = input$removeEmpty, coreDataOnly = input$coreDataOnly, dataTable = FALSE, convertColumns = TRUE)
      } else {
        dat <- processBioticFile(file = input$file1$datapath, lengthUnit = input$lengthUnit, weightUnit = input$weightUnit, removeEmpty = input$removeEmpty, coreDataOnly = input$coreDataOnly, dataTable = FALSE, convertColumns = TRUE)
      }
    },
    error = function(e) {
      stop(safeError(e))
    }
    )
    
    rv$stnall <- dat$stnall
    rv$indall <- dat$indall
    
    rv$mission <- dat$mission
    rv$fishstation <- dat$fishstation
    rv$catchsample <- dat$catchsample
    rv$individual <- dat$individual
    rv$agedetermination <- dat$agedetermination
    
    rv$inputData <- dat
    
  })
  
  ##..............
  ## Test output ####
  
  # output$test <- renderText({
  #   # length(input$file1[[1]])
  #   # paste(input$file1[[1]], collapse = "; ")
  #   # paste(input$indSpecies, collapse = "; ")
  #   paste(input$weigthUnit)
  # })
  # 
  ##...................
  ## Update inputs ####
  
  observeEvent(c(req(input$file1), input$Subset), {
    updateSelectInput(session, "subYear", choices = sort(unique(rv$stnall$startyear)))
    updateSelectInput(session, "subSpecies", choices = sort(unique(rv$stnall$commonname)))
    updateSelectInput(session, "subCruise", choices = sort(unique(rv$stnall$cruise)))
    updateSelectInput(session, "subPlatform", choices = sort(unique(rv$stnall$platformname)))
    updateSelectInput(session, "subSerialnumber", choices = sort(unique(rv$stnall$serialnumber)))
    updateSelectInput(session, "subGear", choices = sort(unique(rv$stnall$gear)))
    updateSelectInput(session, "catchMapSpecies", choices = c("All", sort(unique(rv$stnall$commonname))))
    updateSelectInput(session, "indSpecies", choices = 
                        c("Select a species to generate the plots", 
                          rv$indall %>% 
                            filter(!is.na(length) & !is.na(individualweight)) %>% 
                            group_by(commonname) %>% dplyr::select(commonname) %>% 
                            filter(n() > 1) %>% unique() %>% pull() %>% sort()))
    
    min.lon <- floor(min(rv$stnall$longitudestart, na.rm = TRUE))
    max.lon <- ceiling(max(rv$stnall$longitudestart, na.rm = TRUE))
    updateSliderInput(session, "subLon", min = min.lon, max = max.lon, value = c(min.lon, max.lon), step = 0.1)
    
    min.lat <- floor(min(rv$stnall$latitudestart, na.rm = TRUE))
    max.lat <- ceiling(max(rv$stnall$latitudestart, na.rm = TRUE))
    updateSliderInput(session, "subLat", min = min.lat, max = max.lat, value = c(min.lat, max.lat), step = 0.1)
    
  })
  
  ##................
  ## Subsetting ####
  
  observeEvent(input$Subset, {
    
    rv$sub$year <- if (is.null(input$subYear)) {
      unique(rv$inputData$stnall$startyear)
    } else {
      input$subYear
    }
    
    rv$sub$species <- if (is.null(input$subSpecies)) {
      unique(rv$inputData$stnall$commonname)
    } else {
      input$subSpecies
    }
    
    rv$sub$cruise <- if (is.null(input$subCruise)) {
      unique(rv$inputData$stnall$cruise)
    } else {
      input$subCruise
    }
    
    rv$sub$platform <- if (is.null(input$subPlatform)) {
      unique(rv$inputData$stnall$platformname)
    } else {
      input$subPlatform
    }
    
    rv$sub$serialnumber <- if (is.null(input$subSerialnumber)) {
      unique(rv$inputData$stnall$serialnumber)
    } else {
      input$subSerialnumber
    }
    
    rv$sub$gear <- if (is.null(input$subGear)) {
      unique(rv$inputData$stnall$gear)
    } else {
      input$subGear
    }
    
    rv$sub$lon <- if (is.null(input$subLon)) {
      NULL
    } else {
      input$subLon
    }
    
    rv$sub$lat <- if (is.null(input$subLat)) {
      NULL
    } else {
      input$subLat
    }
    
    tmp <- rv$inputData$stnall
    tmp <- tmp %>% dplyr::filter(
      startyear %in% rv$sub$year, 
      commonname %in% rv$sub$species,
      cruise %in% rv$sub$cruise,
      platformname %in% rv$sub$platform,
      serialnumber %in% rv$sub$serialnumber,
      gear %in% rv$sub$gear,
      longitudestart >= rv$sub$lon[1],
      longitudestart <= rv$sub$lon[2], 
      latitudestart >= rv$sub$lat[1],
      latitudestart <= rv$sub$lat[2]
    )
    
    rv$stnall <- tmp
    
    tmp <- rv$inputData$indall
    tmp <- tmp %>% dplyr::filter(
      startyear %in% rv$sub$year, 
      commonname %in% rv$sub$species,
      cruise %in% rv$sub$cruise,
      platformname %in% rv$sub$platform,
      serialnumber %in% rv$sub$serialnumber,
      gear %in% rv$sub$gear,
      longitudestart >= rv$sub$lon[1],
      longitudestart <= rv$sub$lon[2], 
      latitudestart >= rv$sub$lat[1],
      latitudestart <= rv$sub$lat[2]
    )
    
    rv$indall <- tmp
    
    tmp <- rv$inputData$mission
    tmp <- tmp %>% dplyr::filter(
      missionid %in% unique(rv$stnall$missionid)
    )
    
    rv$mission <- tmp
    
    tmp <- rv$inputData$fishstation
    tmp <- tmp %>% dplyr::filter(
      startyear %in% unique(rv$stnall$startyear),
      serialnumber %in% unique(rv$stnall$serialnumber)
    )
    
    rv$fishstation <- tmp
    
    tmp <- rv$inputData$catchsample
    tmp <- tmp %>% dplyr::filter(
      startyear %in% unique(rv$stnall$startyear),
      serialnumber %in% unique(rv$stnall$serialnumber),
      catchsampleid %in% unique(rv$stnall$catchsampleid),
      catchpartnumber %in% unique(rv$stnall$catchpartnumber),
      commonname %in% unique(rv$stnall$commonname)
    )
    
    rv$catchsample <- tmp
    
    tmp <- rv$inputData$individual
    tmp <- tmp %>% dplyr::filter(
      missiontype %in% unique(rv$stnall$missiontype),
      startyear %in% unique(rv$stnall$startyear),
      platform %in% unique(rv$stnall$platform),
      missionnumber %in% unique(rv$stnall$missionnumber),
      serialnumber %in% unique(rv$stnall$serialnumber),
      catchsampleid %in% unique(rv$stnall$catchsampleid),
      specimenid %in% unique(rv$indall$specimenid)
    )
    
    rv$individual <- tmp
    
    tmp <- rv$inputData$agedetermination
    tmp <- tmp %>% dplyr::filter(
      missiontype %in% unique(rv$stnall$missiontype),
      startyear %in% unique(rv$stnall$startyear),
      platform %in% unique(rv$stnall$platform),
      missionnumber %in% unique(rv$stnall$missionnumber),
      serialnumber %in% unique(rv$stnall$serialnumber),
      catchsampleid %in% unique(rv$stnall$catchsampleid),
      specimenid %in% unique(rv$indall$specimenid)
    )
    
    rv$agedetermination <- tmp
  })
  
  ##..............
  ## Download ####
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      if (length(input$downloadDataType) == 1 & !"original" %in% input$downloadDataType) {
        paste0(input$downloadDataType, input$downloadFileType)
      } else if (input$downloadFileType == ".rda") {
        "BioticExplorer_data.rda"
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
        
      } else if (sapply(strsplit(file, "\\."), "[", 2) == "rda") {
        
        biotic <- lapply(input$downloadDataType, function(k) {
          eval(parse(text = paste("rv", k, sep = "$")))
        })
        
        names(biotic) <- input$downloadDataType
        
        save(biotic, file = file)
        
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
  
  
  ##....................
  ## Overview stats ####
  
  observeEvent(req(input$file1), {
    
    output$nStationsBox <- renderValueBox({
      valueBox(
        value = tags$p(nrow(rv$fishstation), style = "font-size: 80%;"),
        subtitle = "Stations"
      )
    })
    
    output$nYearsBox <- renderValueBox({
      valueBox(
        length(unique(rv$stnall$startyear)),
        "Unique years"
      )
    })
    
    output$nSpeciesBox <- renderValueBox({
      valueBox(
        length(unique(rv$stnall$commonname)),
        "Unique species"
      )
    })
    
    output$DateStartBox <- renderValueBox({
      valueBox(
        value = tags$p(as.Date(min(rv$stnall$stationstartdate, na.rm = TRUE)),
                       style = "font-size: 80%;"),
        subtitle = "First date"
      )
    })
    
    output$DateEndBox <- renderValueBox({
      valueBox(
        value = tags$p(as.Date(max(rv$stnall$stationstartdate, na.rm = TRUE)),
                       style = "font-size: 80%;"),
        subtitle = "Last date"
      )
    })
    
    output$nMeasuredBox <- renderValueBox({
      valueBox(
        value = tags$p(nrow(rv$indall), style = "font-size: 60%;"),
        subtitle = "Measured specimen"
      )
    })
    
    output$nCruisesBox <- renderValueBox({
      valueBox(
        nrow(rv$mission),
        "Cruises"
      )
    })
    
    output$nGearsBox <- renderValueBox({
      valueBox(
        length(unique(rv$stnall$gear)),
        "Gear types"
      )
    })
    
  })
  
  ##.................s
  ## Data tables ####
  
  output$stnall <- DT::renderDataTable({
    DT::datatable(rv$stnall, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) %>% formatRound(c("longitudestart", "latitudestart", "distance", "catchweight", "lengthsampleweight"))
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
    )# %>% formatRound(c("longitudestart", "latitudestart"))
  })
  
  output$fishstation <- DT::renderDataTable({
    DT::datatable(rv$fishstation, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) %>% formatRound(c("longitudestart", "latitudestart", "distance"))
  })
  
  output$catchsample <- DT::renderDataTable({
    DT::datatable(rv$catchsample, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) %>% formatRound(c("catchweight", "lengthsampleweight"))
  })
  
  output$individualTable <- DT::renderDataTable({
    DT::datatable(rv$individual, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) 
  })
  
  output$agedeterminationTable <- DT::renderDataTable({
    DT::datatable(rv$agedetermination, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) 
  })
  
  
  #..................
  ## Station map ####
  
  observeEvent(req(input$file1), {
    
    if (!input$performanceMode) {
      output$stationMap <- renderLeaflet({
        
        leaflet::leaflet(
          rv$stnall[!is.na(rv$stnall$longitudestart) & !is.na(rv$stnall$latitudestart),]) %>% 
          fitBounds(lng1 = min(rv$stnall$longitudestart, na.rm = TRUE),
                    lng2 = max(rv$stnall$longitudestart, na.rm = TRUE),
                    lat1 = min(rv$stnall$latitudestart, na.rm = TRUE),
                    lat2 = max(rv$stnall$latitudestart, na.rm = TRUE)) %>% 
          addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
                   attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
          addRectangles(
            lng1 = input$subLon[1], lat1 = input$subLat[1], lng2 = input$subLon[2], lat2 = input$subLat[2],
            fillColor = "transparent") %>% 
          addCircles(lat = ~ latitudestart, lng = ~ longitudestart, 
                     weight = 1, radius = 2, 
                     popup = ~as.character(platformname), 
                     label = ~as.character(serialnumber), 
                     color = "red", fillOpacity = 0.5
          )
      })
    } else {
      output$stationMap <- NULL
    }
    
  })
  
  ##..........................
  ## Stn data figures ####
  
  observeEvent(c(req(input$file1), input$Subset), {
    
    ## Number of stations containing the species plot
    
    nStn <- rv$stnall %>% dplyr::group_by(commonname) %>% dplyr::summarise(n = length(unique(paste(startyear, serialnumber))))
    nStn <- nStn[order(-nStn$n),]
    nStn$commonname <- factor(nStn$commonname, nStn$commonname)
    
    output$speciesCompositionPlot <- renderPlot({
      
      ggplot(nStn, aes(y = n, x = commonname)) + 
        geom_col() +
        ylab("Number of stations containing the species") +
        xlab("Species database name") +
        coord_cartesian(expand = FALSE, ylim = range(pretty(nStn$n))) + 
        theme_bw(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    ## Summed catch weight plot
    
    catchW <- rv$stnall[!is.na(rv$stnall$catchweight),]
    
    catchS <- catchW %>% dplyr::group_by(commonname) %>% dplyr::summarise(mean = mean(catchweight, na.rm = TRUE), se = se(catchweight), max = max(catchweight, na.rm = TRUE), min = min(catchweight, na.rm = TRUE), sum = sum(catchweight, na.rm = TRUE))
    catchS <- catchS[order(-catchS$sum),]
    catchS$commonname <- factor(catchS$commonname, catchS$commonname)
    catchS$se[is.na(catchS$se)] <- 0
    catchW$commonname <- factor(catchW$commonname, catchS$commonname)
    
    output$catchweightSumPlot <- renderPlotly({
      
      p <- ggplot(catchS, aes(x = commonname, y = sum)) + 
        geom_col() + 
        scale_y_log10("Summed catch weight [log10(kg)]") +
        xlab("Species database name") +
        coord_cartesian() + 
        theme_bw(base_size = 12) +
        annotate("text", x = Inf, y = Inf, label = paste("Total catch\n all species\n", round(sum(catchS$sum), 0), "kg"), vjust = 1, hjust = 1, size = 5) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
      ggplotly(p) %>% plotly::layout(annotations = list(x = 1, y= 1, xref = "paper", yref = "paper", text = paste("Total catch\n all species\n", round(sum(catchS$sum), 0), "kg"), showarrow = FALSE, font = list(size = 12)))
      
    })
    
    ## Mean catch weight plot
    
    output$catchweightMeanPlot <- renderPlot({
      
      ggplot(catchS, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
        geom_linerange() +
        geom_point() +
        ylab("Mean catch weight (kg; +/- SE)") +
        xlab("Species database name") +
        coord_cartesian() + 
        theme_bw(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    ## Catch weight range plot
    
    output$catchweightRangePlot <- renderPlot({
      
      ggplot() + 
        geom_linerange(data = catchS, aes(x = commonname, ymax = max, ymin = min), color = "red") +
        geom_point(data = catchW, aes(x = commonname, y = catchweight), size = 1, shape = 21) +
        scale_y_log10("Catch weight range [log10(kg)]") +
        xlab("Species database name") +
        coord_cartesian() + 
        theme_bw(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    ## Mean weight of fish in catch
    
    meanW <- rv$stnall %>% filter(!is.na(catchweight) & catchweight > 0 & !is.na(catchcount) & catchcount > 0) %>% group_by(commonname, cruise, startyear, serialnumber) %>% summarise(weight = sum(catchweight), n = sum(catchcount), indw = weight/n)
    meanW <- droplevels(meanW)
    meanW <- meanW %>% group_by(commonname) %>% summarise(mean = mean(indw), min = min(indw), max = max(indw), sd = sd(indw), se = se(indw)) %>% arrange(-mean)
    meanW$commonname <- factor(meanW$commonname, meanW$commonname)
    
    output$catchIndMeanWeightPlot <- renderPlot({
      
      ggplot(meanW, aes(x = commonname, y = mean, ymin = min, ymax = max)) + 
        geom_pointrange() +
        scale_y_log10("Mean specimen weight (kg +/- range)", labels = scales::number_format(accuracy = 0.001)) +
        xlab("Species database name") +
        theme_bw(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    ## Mean n in catch plot
    
    catchN <- rv$stnall[!is.na(rv$stnall$catchcount) & rv$stnall$catchcount > 0,]
    
    meanN <- catchN %>% group_by(commonname) %>% summarise(mean = mean(catchcount), se = se(catchcount), max = max(catchcount), min = min(catchcount), Nstn = length(unique(paste(cruise, startyear, serialnumber))))
    meanN <- meanN[order(-meanN$mean),]
    meanN$se[is.na(meanN$se)] <- 0
    meanN$commonname <- factor(meanN$commonname, meanN$commonname)
    catchN$commonname <- factor(catchN$commonname, meanN$commonname)
    
    output$catchcountMeanPlot <- renderPlot({
      
      ggplot(meanN, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) + 
        geom_linerange() +
        geom_point() +
        ylab("Mean number in catch (+/- SE)") +
        xlab("Species database name") +
        theme_bw(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    ## N range in catch plot
    
    output$catchcountRangePlot <- renderPlot({
      
      ggplot() + 
        geom_linerange(data = meanN, 
                       aes(x = commonname, ymax = max, ymin = min), color = "red") +
        geom_point(data = catchN,
                   aes(x = commonname, y = catchcount), size = 1, shape = 21) +
        scale_y_log10("Range for number in catch (log10)") +
        xlab("Species database name") +
        coord_cartesian() + 
        theme_bw(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    
    
    ## Catch in gear plot
    
    catchGBase <- rv$stnall[!is.na(rv$stnall$catchweight),]
    catchG <- catchGBase %>% group_by(gear, commonname) %>% 
      summarise(sum = sum(catchweight))
    catchG$commonname <- factor(catchG$commonname, catchS$commonname)
    
    output$gearcatchPlot <- renderPlot({
      
      ggplot(catchG, aes(x = commonname, y = as.factor(gear), 
                         size = sum, color = sum)) +
        geom_point() + 
        scale_color_distiller(name = "Total catch [log10(kg)]", 
                              palette = "Spectral", trans = "log10", 
                              breaks = c(1 %o% 10^(-4:4))
        ) +
        scale_size(name = "Total catch [log10(kg)]", trans = "log10", 
                   breaks = c(1 %o% 10^(-4:4), range = c(1,8))
        ) +
        ylab("Gear code") +
        xlab("Species database name") +
        theme_bw(base_size = 14) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    ## Station depth plot
    
    stnD <- rv$stnall %>% group_by(cruise, startyear, serialnumber) %>% summarise(bdepth = unique(bottomdepthstart), fdepth = unique(fishingdepthmin))
    stnD <- melt(stnD, id = 1:3)
    stnD$variable <- recode_factor(stnD$variable, "bdepth" = "Bottom depth (start)", "fdepth" = "Minimum fishing depth")
    
    output$stationDepthPlot <- renderPlot({
      
      ggplot(stnD, aes(x = value)) +
        geom_histogram(binwidth = 100, color = "black", fill = "grey") +
        facet_wrap(~variable) +
        scale_y_continuous("Count", expand = c(0, 0)) +
        scale_x_continuous("Depth (m)", expand = c(0,0.05)) +
        expand_limits(x = 0) +
        theme_classic(base_size = 14) +
        theme(strip.background = element_blank())
      
    })
    
    # stnG <- catchGBase %>% group_by(gear) %>%
    #   summarise(n = length(unique(paste(cruise, startyear, serialnumber))))
    
  })
  
  ## Stn data maps ####
  
  observeEvent(c(req(input$file1), input$Subset), {
    
    compDat <- rv$stnall %>% filter(!is.na(catchweight)) %>% group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname) %>% summarise(catchweight = sum(catchweight)) 
    
    sumCompDat <- compDat %>% group_by(commonname) %>% summarise(sum = sum(catchweight)) %>% arrange(-sum)
    
    compDat$commonname <- factor(compDat$commonname, sumCompDat$commonname)
    
    if (length(sumCompDat$commonname) > 6) {
      levels(compDat$commonname)[!levels(compDat$commonname) %in% sumCompDat$commonname[1:6]] <- "Andre arter"
    }
    
    levels(compDat$commonname) <- gsub("(^[[:alpha:]])", "\\U\\1", levels(compDat$commonname), perl = TRUE)    
    
    compDat <- compDat %>% group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname, .drop = FALSE) %>% summarise(catchweight = sum(catchweight)) %>% arrange(cruise, startyear, serialnumber, commonname)
    
    compDatW <- dcast(compDat, cruise + startyear + serialnumber + longitudestart + latitudestart + fishingdepthmin ~ commonname, value.var = "catchweight", fill = 0)
    
    compDatW$total <- rowSums(compDatW[levels(compDat$commonname)]) 
    
    ## The map
    
    output$catchCompMap <- renderLeaflet({
      
      leaflet::leaflet() %>% 
        fitBounds(lng1 = min(rv$stnall$longitudestart, na.rm = TRUE),
                  lng2 = max(rv$stnall$longitudestart, na.rm = TRUE),
                  lat1 = min(rv$stnall$latitudestart, na.rm = TRUE),
                  lat2 = max(rv$stnall$latitudestart, na.rm = TRUE)) %>% 
        addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
                 attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
        addMinicharts(
          compDatW$longitudestart, compDatW$latitudestart,
          type = "pie", chartdata = compDatW[,levels(compDat$commonname)],
          colorPalette = ColorPalette,
          width = 40 * log(compDatW$total) / log(max(compDatW$total)), 
          transitionTime = 0
        )
    })
    ## Catch - fishing depth plot
    
    output$catchSpeciesWeightPlot <- renderPlot({
      
      ggplot(compDat[compDat$commonname != "Andre arter",], aes(x = fishingdepthmin, y = catchweight, group = commonname)) +
        geom_smooth(se = FALSE) +
        geom_point() +
        ylab("Catch weight (kg)") + 
        xlab("Minimum fishing depth (m)") +
        facet_wrap(~commonname, scales = "free_y") + 
        theme_bw(base_size = 14)
    })
  })
  
  
  observeEvent(c(req(input$file1), input$Subset, input$catchMapSpecies), {
    
    if (input$catchMapSpecies == "All") {
      sps <- unique(rv$stnall$commonname)
    } else {
      sps <- input$catchMapSpecies
    }
    
    ## Catch map ####
    
    tmp <- rv$stnall %>% 
      filter(commonname %in% sps & !is.na(longitudestart) & !is.na(latitudestart)) %>% 
      group_by(startyear, serialnumber, longitudestart, 
               latitudestart, gear, bottomdepthstart, stationstartdate) %>% 
      summarize(catchsum = round(sum(catchweight, na.rm = TRUE), 2))
    
    tmp2 <- rv$stnall %>% filter(!is.na(longitudestart) & !is.na(latitudestart))
    
    tmp2 <- tmp2[!paste(tmp2$startyear, tmp2$serialnumber, sep = "_") %in% paste(tmp$startyear, tmp$serialnumber, sep = "_"), !names(tmp2) %in% c("catchsampleid", "commonname", "catchcategory", "catchpartnumber", "catchweight", "catchcount", "lengthsampleweight", "lengthsamplecount")]
    
    if (nrow(tmp2) > 0) tmp2$catchsum <- 0
    
    output$catchMap <- renderLeaflet({
      
      p <- leaflet::leaflet(tmp, options = leafletOptions(zoomControl = FALSE)) %>% 
        fitBounds(lng1 = min(rv$stnall$longitudestart, na.rm = TRUE),
                  lng2 = max(rv$stnall$longitudestart, na.rm = TRUE),
                  lat1 = min(rv$stnall$latitudestart, na.rm = TRUE),
                  lat2 = max(rv$stnall$latitudestart, na.rm = TRUE)) %>% 
        addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
                 attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>%
        addCircles(lat = ~ latitudestart, lng = ~ longitudestart, 
                   weight = 4, radius = 5e4*(tmp$catchsum/max(tmp$catchsum)), 
                   label = paste0(tmp$serialnumber, "; ", tmp$catchsum, " kg"), 
                   popup = paste("Serial number:", tmp$serialnumber, "<br>",
                                 "Date:", tmp$stationstartdate, "<br>",
                                 "Gear code:", tmp$gear, "<br>",
                                 "Bottom depth:", round(tmp$bottomdepthstart, 0), "m",
                                 "<br>", input$catchMapSpecies, "catch:", tmp$catchsum, 
                                 "kg"), 
                   color = "red", fill = NA
        ) 
      
      if (nrow(tmp2) > 0) {
        p %>% 
          addCircles(lat = tmp2$latitudestart, lng = tmp2$longitudestart, 
                     weight = 4, radius = 1, 
                     label = paste0(tmp2$serialnumber, "; ", tmp2$catchsum, " kg"), 
                     popup = paste("Serial number:", tmp2$serialnumber, "<br>",
                                   "Date:", tmp2$stationstartdate, "<br>",
                                   "Gear code:", tmp2$gear, "<br>",
                                   "Bottom depth:", round(tmp2$bottomdepthstart, 0), "m",
                                   "<br>", input$catchMapSpecies, "catch:", tmp2$catchsum, 
                                   "kg"), 
                     color = "black"
          )
      } else {
        p
      }
      
    })
    
  }) 
  
  
  
  ##..........................................
  ## Ind data overview figures and tables ####
  
  observeEvent(c(req(input$file1), input$Subset), {
    
    indSumTab <- rv$indall %>% 
      dplyr::group_by(commonname) %>% 
      dplyr::summarise(Total = length(commonname), 
                       Length = sum(!is.na(length)), 
                       Weight = sum(!is.na(individualweight)), 
                       Sex = sum(!is.na(sex)), 
                       Maturationstage = sum(!is.na(maturationstage)), 
                       Specialstage = sum(!is.na(specialstage)), 
                       Age = sum(!is.na(age))
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
  
  observeEvent(c(req(input$file1), input$Subset, input$indSpecies, input$laPlotSexSwitch), {
    
    if (input$tabs == "indallSpecies" & input$indSpecies != "Select a species to generate the plots") {
      
      ## Length-weight plot
      
      tmpBase <- rv$indall %>% filter(commonname == input$indSpecies)
      
      if (input$indSpecies == "blåkveite") {
        
        tmpTab <- data.table::dcast(tmpBase, cruise + startyear + serialnumber + longitudestart + latitudestart ~ catchpartnumber, fun.aggregate = length, value.var = "length")
        
        if(all(c(1, 2) %in% names(tmpTab))) {
          
          tmpTab$EggaSystem <- tmpTab$`1` > 0 & tmpTab$`2` > 0
          
          tmpBase <- left_join(tmpBase, tmpTab[!names(tmpTab) %in% 1:10], by = c("startyear", "serialnumber", "cruise", "longitudestart", "latitudestart"))  
          
          tmpBase$sex <- ifelse(!is.na(tmpBase$sex), tmpBase$sex, ifelse(is.na(tmpBase$sex) & tmpBase$EggaSystem & tmpBase$catchpartnumber == 1, 1, ifelse(is.na(tmpBase$sex) & tmpBase$EggaSystem & tmpBase$catchpartnumber == 2, 2, NA)))
          
          tmpBase <- tmpBase[names(tmpBase) != "EggaSystem"]
        }
      }
      
      lwDat <- tmpBase %>% filter(!is.na(length) & !is.na(individualweight))
      
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
      
      ## Age - length plot
      
      laDat <- tmpBase %>% filter(!is.na(age) & !is.na(length))
      
      if (nrow(laDat) > 0) {
        
        if (input$laPlotSexSwitch) {
          
          laDat <- laDat %>% filter(!is.na(sex))
          
          laModF <- fishmethods::growth(age = laDat[laDat$sex == 1,]$age, size = laDat[laDat$sex == 1,]$length, Sinf = max(laDat[laDat$sex == 1,]$length), K = 0.1, t0 = 0, graph = FALSE)
          
          laModM <- fishmethods::growth(age = laDat[laDat$sex == 2,]$age, size = laDat[laDat$sex == 2,]$length, Sinf = max(laDat[laDat$sex == 2,]$length), K = 0.1, t0 = 0, graph = FALSE)
          
          laDat$sex <- as.factor(laDat$sex)
          laDat$sex <- recode_factor(laDat$sex, "1" = "Female", "2" = "Male")
          
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
      
      ## L50 maturity plot  ####
      
      l50Dat <- tmpBase %>% filter(!is.na(sex) & !is.na(maturationstage))
      
      if(nrow(l50Dat) > 0) {
        
        l50Dat$sex <- factor(l50Dat$sex)
        l50Dat$sex <- recode_factor(l50Dat$sex, "1" = "Female", "2" = "Male", "3" = "Unidentified")
        
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
      
      ## Sex ratio map ####
      
      srDat <- tmpBase %>% filter(!is.na(sex)) %>% group_by(cruise, startyear, serialnumber, longitudestart, latitudestart) %>% summarise(Female = sum(sex == 1), Male = sum(sex == 2), total = length(sex))
      
      if (nrow(srDat) > 0) {
        
        output$sexRatioMap <- renderLeaflet({
          
          leaflet::leaflet() %>% 
            fitBounds(lng1 = min(srDat$longitudestart, na.rm = TRUE),
                      lng2 = max(srDat$longitudestart, na.rm = TRUE),
                      lat1 = min(srDat$latitudestart, na.rm = TRUE),
                      lat2 = max(srDat$latitudestart, na.rm = TRUE)) %>% 
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
      
      ## Size distribution map ####
      
      sdDat <- tmpBase %>% filter(!is.na(length)) %>% dplyr::select(cruise, startyear, serialnumber, longitudestart, latitudestart, length) %>% mutate(interval = ggplot2::cut_interval(length, n = 5)) %>% group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, interval, .drop = FALSE) %>% summarise(count = n())
      
      if(nrow(sdDat) > 0) {
        
        sdDatW <- dcast(sdDat, cruise + startyear + serialnumber + longitudestart + latitudestart ~ interval, value.var = "count", fill = 0)
        sdDatW$total <- rowSums(sdDatW[,levels(sdDat$interval)])
        
        output$sizeDistributionMap <- renderLeaflet({
          
          leaflet::leaflet() %>% 
            fitBounds(lng1 = min(sdDatW$longitudestart, na.rm = TRUE),
                      lng2 = max(sdDatW$longitudestart, na.rm = TRUE),
                      lat1 = min(sdDatW$latitudestart, na.rm = TRUE),
                      lat2 = max(sdDatW$latitudestart, na.rm = TRUE)) %>% 
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
  
})

##........................
## Compile to the app ####

shinyApp(ui, server)
