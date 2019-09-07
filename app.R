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

if (os == "Linux") {
  required.packages <- c("shiny", "shinyFiles", "shinydashboard", "DT", "tidyverse", "devtools", "leaflet", "openxlsx", "dplyr", "data.table", "Cairo", "scales")
  options(shiny.usecairo = T)
} else {
  required.packages <- c("shiny", "shinyFiles", "shinydashboard", "DT", "tidyverse", "devtools", "leaflet", "openxlsx", "dplyr", "data.table", "scales")
}

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

## Header

header <- dashboardHeader(title = "Biotic Explorer")

##_____________
## Sidebar ####

sidebar <- 
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      
      menuItem("Information", tabName = "info", icon = icon("info-circle")),
      menuItem("Upload & filter", tabName = "upload", icon = icon("arrow-circle-up")),
      menuItem("Stations & catches", icon = icon("ship"),
               menuSubItem("Overview", tabName = "stnallOverview"),
               menuSubItem("Examine data", tabName = "stnallExamine")
      ),
      menuItem("Individuals & ages", icon = icon("fish"),
               menuSubItem("Overview", tabName = "indallOverview"),
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
      tabItem("info", 
              
              fluidRow(
                column(width = 12,
                       h1("Welcome to the Biotic Explorer", align = "center"),
                       br(),
                       p("This is a", a("Shiny app", href = "http://shiny.rstudio.com"), "allowing examination and manipulation of the Norwegian Maritime Data-center (NMD) standard xml files, which are used within the Institute of Marine Research database. To start with the app, click the", strong("'Upload & filter'"), "tab on the side panel."),
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
                       h5("(c) Institute of Marine Research, Norway, acknowledging the", a("RStudio team and Shiny developers", href = "https://www.rstudio.com/about/"), align = "left"),
                       br(),
                       br(),
                       h5("Version 0.1.8 (alpha), 2019-09-06", align = "right")
                )
              )
      ),
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
                         
                         radioButtons("lenghtUnit", "Fish length unit:",
                                      c("Millimeter" = "mm",
                                        "Centimeter" = "cm",
                                        "Meter" = "m"),
                                      selected = "cm",
                                      inline = TRUE),
                         
                         radioButtons("weigthUnit", "Fish weight unit:",
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
                         
                         selectInput(inputId = "subYear", label = "Year:", choices = NULL, multiple = TRUE),
                         selectInput(inputId = "subSpecies", label = "Species:", choices = NULL, multiple = TRUE),
                         #selectInput(inputId = "subCruise", label = "Cruise number:", choices = NULL, multiple = TRUE),
                         selectInput(inputId = "subPlatform", label = "Platform name:", choices = NULL, multiple = TRUE),
                         selectInput(inputId = "subSerialnumber", label = "Serial number:", choices = NULL, multiple = TRUE),
                         selectInput(inputId = "subGear", label = "Gear code:", choices = NULL, multiple = TRUE),
                         sliderInput(inputId = "subLon", label = "Longitude:", min = -180, max = 180, value = c(-180, 180)),
                         sliderInput(inputId = "subLat", label = "Latitude:", min = -90, max = 90, value = c(-90, 90)),
                         actionButton(inputId = "Subset", label = "Subset")
                         #verbatimTextOutput("test")
                       ),
                       
                       box(title = "Station locations", status = "primary", width = NULL,
                           conditionalPanel(
                             condition = "input.performanceMode == false",
                             leafletOutput(outputId = "stationMap")
                           ),
                           
                           conditionalPanel(
                             condition = "input.performanceMode == true",
                             p("Performance mode. No map. Subset data and use 'Stations & catches' -> 'Overview' to examine station locations.", align = "center")
                           )       
                       )
                )
                
                ## End columns ###
                
              )
      ),
      
      
      tabItem("stnallOverview",
              fluidRow(
                box(title = "Species composition", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("speciesCompositionPlot")
                ),
                
                box(title = "Catch weight mean and standard error", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("catchweightMeanPlot")
                ),
                
                box(title = "Catch weight range", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("catchweightRangePlot")
                ),
                
                box(title = "Mean number in catch and standard error", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("catchcountMeanPlot")
                ),
                
                box(title = "Range of number in catch", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("catchcountRangePlot")
                ),
                
                box(title = "Total (summed) catch by gear type", width = 12, status = "info", 
                    solidHeader = TRUE,
                    plotOutput("gearcatchPlot", height = "600px")
                ),
                
                box(title = "Map of catches (in kg)", width = 12, status = "info", 
                    solidHeader = TRUE,
                    selectInput("catchMapSpecies", "Species", 
                                c("All", unique(rv$stnall$commonname))),
                    leafletOutput(outputId = "catchMap")
                )
              )
      ),
      
      tabItem("stnallExamine", DT::dataTableOutput("stnall")),
      
      tabItem("indallOverview", 
              fluidRow(
                box(title = "Individual sample overview", width = 12, status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("individualSummaryTable")
                )
              )
      ),
      
      tabItem("indallExamine", DT::dataTableOutput("indall")),
      
      tabItem("missionExamine", DT::dataTableOutput("missionTable")),
      tabItem("fishstationExamine", DT::dataTableOutput("fishstation")),
      tabItem("catchsampleExamine", DT::dataTableOutput("catchsample")),
      tabItem("individualExamine", DT::dataTableOutput("individualTable")),
      tabItem("agedeterminationExamine", DT::dataTableOutput("agedeterminationTable")),
      
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
                  #verbatimTextOutput("test")
                ) 
              )
      )
    )
  )

## Dashboard page  

ui <- dashboardPage(header, sidebar, body)

##............
## Server ####

server <- shinyServer(function(input, output, session) {
  
  ## Options
  
  options(shiny.maxRequestSize = 1000*1024^2) ## This sets the maximum file size for upload. 1000 = 1 Gb. 
  
  ## Read data
  
  rv <- reactiveValues()
  
  observeEvent(req(input$file1), {
    
    tryCatch({
      dat <- processBioticFile(file = input$file1$datapath, lengthUnit = input$lenghtUnit, weightUnit = input$weigthUnit, removeEmpty = input$removeEmpty, coreDataOnly = input$coreDataOnly, dataTable = FALSE, convertColumns = TRUE)
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
  ## Test output
  
  # output$test <- renderText({
  #   paste(input$file1[1], collapse = ", ")
  # })
  
  ##................
  ## Subsetting ####
  
  observeEvent(req(input$file1), {
    updateSelectInput(session, "subYear", choices = sort(unique(rv$stnall$startyear)))
    updateSelectInput(session, "subSpecies", choices = sort(unique(rv$stnall$commonname)))
    #updateSelectInput(session, "subCruise", choices = sort(unique(rv$stnall$cruise)))
    updateSelectInput(session, "subPlatform", choices = sort(unique(rv$stnall$platformname)))
    updateSelectInput(session, "subSerialnumber", choices = sort(unique(rv$stnall$serialnumber)))
    updateSelectInput(session, "subGear", choices = sort(unique(rv$stnall$gear)))
    
    min.lon <- floor(min(rv$stnall$longitudestart, na.rm = TRUE))
    max.lon <- ceiling(max(rv$stnall$longitudestart, na.rm = TRUE))
    updateSliderInput(session, "subLon", min = min.lon, max = max.lon, value = c(min.lon, max.lon), step = 0.1)
    
    min.lat <- floor(min(rv$stnall$latitudestart, na.rm = TRUE))
    max.lat <- ceiling(max(rv$stnall$latitudestart, na.rm = TRUE))
    updateSliderInput(session, "subLat", min = min.lat, max = max.lat, value = c(min.lat, max.lat), step = 0.1)
  })
  
  
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
  
  ################
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
  
  
  ######################
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
  
  ###################
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
          addTiles() %>% 
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
  
  ############################
  ## Station data figures ####
  
  observeEvent(c(req(input$file1), input$Subset), {
    
    tmp <- rv$stnall %>% dplyr::group_by(commonname) %>% dplyr::summarise(n = length(unique(paste(startyear, serialnumber))))
    tmp <- tmp[order(-tmp$n),]
    tmp$commonname <- factor(tmp$commonname, tmp$commonname)
    
    output$speciesCompositionPlot <- renderPlot({
      ggplot(tmp, aes(y = n, x = commonname)) + 
        geom_col() +
        ylab("Number of stations containing the species") +
        xlab("Species database name") +
        coord_cartesian(expand = FALSE, ylim = range(pretty(tmp$n))) + 
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    })
    
  })
  
  observeEvent(c(req(input$file1), input$Subset), {
    
    tmp <- rv$stnall[!is.na(rv$stnall$catchweight),]
    
    tmp2 <- tmp %>% dplyr::group_by(commonname) %>% dplyr::summarise(mean = mean(catchweight), se = se(catchweight), max = max(catchweight), min = min(catchweight))
    tmp2 <- tmp2[order(-tmp2$mean),]
    tmp2$commonname <- factor(tmp2$commonname, tmp2$commonname)
    tmp$commonname <- factor(tmp$commonname, tmp2$commonname)
    
    output$catchweightMeanPlot <- renderPlot({
      ggplot(tmp2, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) + 
        geom_linerange() +
        geom_point() +
        ylab("Mean catch weight (kg; +/- SE)") +
        xlab("Species database name") +
        coord_cartesian() + 
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    output$catchweightRangePlot <- renderPlot({
      ggplot() + 
        geom_linerange(data = tmp2, aes(x = commonname, ymax = max, ymin = min), color = "red") +
        geom_point(data = tmp, aes(x = commonname, y = catchweight), size = 0.1) +
        scale_y_log10("Catch weight range [log10(kg)]") +
        xlab("Species database name") +
        coord_cartesian() + 
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
  })
  
  
  observeEvent(c(req(input$file1), input$Subset), {
    
    tmp <- rv$stnall[!is.na(rv$stnall$catchcount) & rv$stnall$catchcount > 0,]
    
    tmp2 <- tmp %>% group_by(commonname) %>% summarise(mean = mean(catchcount), se = se(catchcount), max = max(catchcount), min = min(catchcount))
    tmp2 <- tmp2[order(-tmp2$mean),]
    tmp2$commonname <- factor(tmp2$commonname, tmp2$commonname)
    tmp$commonname <- factor(tmp$commonname, tmp2$commonname)
    
    output$catchcountMeanPlot <- renderPlot({
      ggplot(tmp2, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) + 
        geom_linerange() +
        geom_point() +
        ylab("Mean number in catch (+/- SE)") +
        xlab("Species database name") +
        coord_cartesian() + 
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    output$catchcountRangePlot <- renderPlot({
      ggplot() + 
        geom_linerange(data = tmp2, aes(x = commonname, ymax = max, ymin = min), color = "red") +
        geom_point(data = tmp, aes(x = commonname, y = catchweight), size = 0.1) +
        scale_y_log10("Range for number in catch (log10)") +
        xlab("Species database name") +
        coord_cartesian() + 
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    })
    
    observeEvent(c(req(input$file1), input$Subset), {
      tmp <- rv$stnall[!is.na(rv$stnall$catchweight),]
      tmp2 <- tmp %>% group_by(gear, commonname) %>% summarise(sum = sum(catchweight))
      
      output$gearcatchPlot <- renderPlot({
        ggplot(tmp2, aes(x = commonname, y = as.factor(gear), 
                         size = sum, color = sum)) +
          geom_point() + 
          scale_color_distiller(name = "Total catch [log10(kg)]", 
                                palette = "Spectral", trans = "log10", 
                                breaks = c(1 %o% 10^(-4:4))
          ) +
          scale_size(name = "Total catch [log10(kg)]", trans = "log10", 
                     breaks = c(1 %o% 10^(-4:4))
          ) +
          ylab("Gear code") +
          xlab("Species database name") +
          theme_bw(base_size = 14) + 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      })
      
      observeEvent(c(req(input$file1), input$Subset, input$catchMapSpecies), {
        
        if (input$catchMapSpecies == "All") {
          sps <- unique(rv$stnall$commonname)
        } else {
          sps <- input$catchMapSpecies
        }
        
        tmp <- rv$stnall %>% 
          filter(commonname %in% sps & !is.na(longitudestart) & !is.na(latitudestart)) %>% 
          group_by(startyear, serialnumber, longitudestart, 
                   latitudestart, gear, bottomdepthstart) %>% 
          summarize(catchsum = round(sum(catchweight, na.rm = TRUE), 1))
        
        
        output$catchMap <- renderLeaflet({
          
          leaflet::leaflet(tmp, options = leafletOptions(zoomControl = FALSE)) %>% 
            fitBounds(lng1 = min(tmp$longitudestart, na.rm = TRUE),
                      lng2 = max(tmp$longitudestart, na.rm = TRUE),
                      lat1 = min(tmp$latitudestart, na.rm = TRUE),
                      lat2 = max(tmp$latitudestart, na.rm = TRUE)) %>% 
            addTiles() %>%
            addCircles(lat = ~ latitudestart, lng = ~ longitudestart, 
                       weight = 2, radius = ~ catchsum*100, 
                       label = ~as.character(catchsum), 
                       popup = paste("Serial number:", tmp$serialnumber, "<br>",
                                     "Gear code:", tmp$gear, "<br>",
                                     "Bottom depth:", round(tmp$bottomdepthstart, 0), "m",
                                     "<br>", input$catchMapSpecies, "catch:", tmp$catchsum, 
                                     "kg"), 
                       color = "red", fill = NA
            ) 
        })
        
      }) 
      
    })
    
  })
  
  ##########################################
  ## Individual data figures and tables ####
  
  observeEvent(c(req(input$file1), input$Subset), {
    
    tmp <- rv$indall %>% dplyr::group_by(commonname) %>% dplyr::summarise(Total = length(commonname), Length = sum(!is.na(length)), Weight = sum(!is.na(individualweight)), Sex = sum(!is.na(sex)), Maturationstage = sum(!is.na(maturationstage)), Specialstage = sum(!is.na(specialstage)), Age = sum(!is.na(age)))
    
    output$individualSummaryTable <- DT::renderDataTable({
      DT::datatable(tmp, options = list(searching = FALSE))
    })
  })
  #output$species <- unique(inputData()$stnall$commonname)
  
  # output$agePlot <- renderPlot({
  # 
  #   if (is.null(inputData()$age)) {
  # 
  #     return(NULL)
  # 
  #     } else {
  # 
  #       ind <-inputData()$ind
  #       x <- ind[!is.na(ind$length) & !is.na(ind$weight),]
  #       mod <- lm(log(weight) ~ log(length), data = x)
  # 
  #       p <- ggplot() +
  #         geom_point(data = x, aes(x = length, y = weight)) +
  #         stat_function(data = data.frame(x = c(5, 55)), aes(x),
  #                       fun = function(a, b, x) {a*x^b},
  #                       args = list(a = exp(coef(mod)[1]), b = coef(mod)[2]),
  #                       color = "blue", size = 1) +
  #         xlab("Total length (cm)") +
  #         ylab("Weigth (g)") +
  #         annotate("text", x = 1, y = Inf,
  #                  label = paste0("a = ", round(exp(coef(mod)[1]), 3), "\n",
  #                                 "b = ", round(coef(mod)[2], 3)),
  #                  vjust = 1, hjust = 0) +
  #         theme_classic()
  # 
  #       print(p)
  # 
  #   }
  # })
  
})

###########################
## Compilte to the app ####

shinyApp(ui, server)
