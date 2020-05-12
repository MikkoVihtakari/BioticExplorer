#' @title Update subset selectors

updateSelectors <- function() {
  
  # Update selectors
  rv$all <- list()
  rv$all$startyear <- rv$mission %>% lazy_dt() %>% select(startyear) %>% distinct() %>% pull() %>% sort()
  rv$all$commonname <- rv$stnall %>% lazy_dt() %>% select(commonname) %>% distinct() %>% pull() %>% sort()
  rv$all$cruise <- rv$mission %>% lazy_dt() %>% select(cruise) %>% distinct() %>% pull() %>% sort()
  rv$all$platformname <- rv$stnall %>% lazy_dt() %>% select(platformname) %>% distinct() %>% pull() %>% sort()
  rv$all$serialnumber <- rv$stnall %>% lazy_dt() %>% select(serialnumber) %>% distinct() %>% pull() %>% sort()
  rv$all$gear <- rv$stnall %>% lazy_dt() %>% select(gear) %>% distinct() %>% pull() %>% sort()
  
  rv$all$indSpecies <- rv$indall %>% lazy_dt() %>% 
    filter(!is.na(length) & !is.na(individualweight)) %>%
    group_by(commonname) %>% tally() %>% filter(n > 1) %>%
    select(commonname) %>% distinct() %>% pull() %>% sort()
  
  lon <- rv$stnall %>% lazy_dt() %>% filter(!is.na(longitudestart)) %>% summarise(min = suppressWarnings(min(longitudestart)), max = suppressWarnings(max(longitudestart))) %>% collect()
  lat <- rv$stnall %>% lazy_dt() %>% filter(!is.na(latitudestart)) %>% summarise(min = suppressWarnings(min(latitudestart)), max = suppressWarnings(max(latitudestart))) %>% collect()
  
  if(nrow(lon) == 0) {
    rv$all$min.lon <- -180
    rv$all$max.lon <- 180
  } else {
    rv$all$min.lon <- floor(lon$min)
    rv$all$max.lon <- ceiling(lon$max)
  }
  
  if(nrow(lat) == 0) {
    rv$all$min.lat <- -90
    rv$all$max.lat <- 90
  } else {
    rv$all$min.lat <- round_any(lat$min, 0.1, floor)
    rv$all$max.lat <- round_any(lat$max, 0.1, ceiling)
  }
  
  rv$all$date <- rv$stnall %>% lazy_dt() %>% summarise(min = min(stationstartdate, na.rm = TRUE), max = max(stationstartdate, na.rm = TRUE)) %>% collect()
}

updateFilterform <- function(db = FALSE) {
  
  if(db) {
    NULL # for now
  } else {
    updateSelectInput(session, "subYear", choices = rv$all$startyear)
    updateSelectInput(session, "subSpecies", choices = rv$all$commonname)
    updateSelectInput(session, "subCruise", choices = rv$all$cruise)
    updateSelectInput(session, "subPlatform", choices = rv$all$platformname)
    updateSelectInput(session, "subSerialnumber", choices = rv$all$serialnumber)
    updateSelectInput(session, "subGear", choices = rv$all$gear)
    updateSelectInput(session, "catchMapSpecies", choices = c("All", rv$stnall$commonname))
    updateSelectInput(session, "indSpecies", choices = c("Select a species to generate the plots", rv$all$indSpecies))
    updateSliderInput(session, "subLon", min = rv$all$min.lon, max = rv$all$max.lon, value = rv$sub$lon, step = 0.1)
    updateSliderInput(session, "subLat", min = rv$all$min.lat, max = rv$all$max.lat, value = rv$sub$lat, step = 0.1) 
  }
}

#' @title Generate the station map

updateMap <- function(db = FALSE) {
  
  # Functions 
  
  stationMap <- function() {
    
    x <- rv$stnall %>% lazy_dt() %>% select(missiontype, startyear, platform, platformname, missionnumber, missionid, serialnumber, latitudestart, longitudestart) %>% 
      filter(!is.na(longitudestart) & !is.na(latitudestart)) %>% distinct() %>% collect()
    
    if(nrow(x) == 0) {
      leaflet::leaflet() %>% 
        addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
                 attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
        addMarkers(lng = 20, lat = 70, label = "No position information")
    } else {
      leaflet::leaflet(x) %>% 
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
    }
  }
  
  # Data
  
  ## Station map ##
  if (!input$performanceMode) {
    if(db) {
      output$stationMapDb <- renderLeaflet(stationMap())
    } else {
      output$stationMap <- renderLeaflet(stationMap())
    }
  } else {
    output$stationMap <- NULL
  }
}

#' @title Populate the quick overview panel and station map

obsPopulatePanel <- function(db = FALSE) {
  
  # Functions
  
  nStationsBox <- function() {
    renderValueBox({
      
      tmp <- rv$stnall %>% lazy_dt() %>% select(missionid, startyear, serialnumber) %>% distinct() %>% count() %>% pull()
      
      valueBox(
        value = tags$p(tmp, style = "font-size: 80%;"),
        subtitle = "Stations"
      )
    })
  }
  
  nYearsBox <- function() {
    renderValueBox({
      valueBox(
        length(rv$all$startyear),
        "Unique years"
      )
    })
  }
  
  nSpeciesBox <- function() {
    renderValueBox({
      valueBox(
        length(rv$all$commonname),
        "Unique species"
      )
    })
  }
  
  DateStartBox <- function() {
    renderValueBox({
      valueBox(
        value = tags$p(rv$all$date$min,
                       style = "font-size: 80%;"),
        subtitle = "First date"
      )
    })
  }
  
  DateEndBox <- function() {
    renderValueBox({
      valueBox(
        value = tags$p(rv$all$date$max,
                       style = "font-size: 80%;"),
        subtitle = "Last date"
      )
    })
  }
  
  nMeasuredBox <- function() {
    renderValueBox({
      valueBox(
        value = tags$p(rv$indall %>% lazy_dt() %>% select(length) %>% count() %>% pull(), style = "font-size: 60%;"),
        subtitle = "Measured specimen"
      )
    })
  }
  
  nCruisesBox <- function() {
    renderValueBox({
      valueBox(
        rv$mission %>% lazy_dt() %>% count() %>% pull(),
        "Cruises"
      )
    })
  }
  
  nGearsBox <- function() {
    renderValueBox({
      valueBox(
        length(unique(rv$all$gear)),
        "Gear types"
      )
    })
  }
  
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
  updateFilterform(db)
  
  # Update stats
  
  if(db) {
    output$nStationsBoxDb <- nStationsBox()
  } else {
    output$nStationsBox <- nStationsBox() 
  }
  
  if(db) {
    output$nYearsBoxDb <- nYearsBox()
  } else {
    output$nYearsBox <- nYearsBox()
  }
  
  if(db) {
    output$nSpeciesBoxDb <- nSpeciesBox()
  } else {
    output$nSpeciesBox <- nSpeciesBox()
  }
  
  if(db) {
    output$DateStartBoxDb <- DateStartBox()
  } else {
    output$DateStartBox <- DateStartBox() 
  }
  
  if(db) {
    output$DateEndBoxDb <- DateEndBox()
  } else {
    output$DateEndBox <- DateEndBox()
  }
  
  if(db) {
    output$nMeasuredBoxDb <- nMeasuredBox()
  } else {
    output$nMeasuredBox <- nMeasuredBox()  
  }
  
  if(db) {
    output$nCruisesBoxDb <- nCruisesBox()
  } else {
    output$nCruisesBox <- nCruisesBox()
  }
  
  if(db) {
    output$nGearsBoxDb <- nGearsBox()
  } else {
    output$nGearsBox <- nGearsBox()
  }
  
  updateMap(db)
}

#' @title Make a filter chain for file-based- and database-data loaded into the memory

makeFilterChain <- function(db = FALSE) {
  
  sub <- list()
  filterChain <- list()
  
  ## Year
  
  if(db) {
    sub$year <- input$selYearDb
  } else {
    sub$year <- input$subYear
  }
  
  if (!is.null(sub$year)) { 
    filterChain <- append(filterChain, paste0("startyear %in% c('", paste0(sub$year, collapse = "', '"), "')"))
  }
  
  ## Species
  
  if(db) {
    sub$species <- input$selSpeciesDb
  } else {
    sub$species <- input$subSpecies    
  }
  
  if (!is.null(sub$species)) {
    filterChain <- append(filterChain, paste0("commonname %in% c('", paste0(sub$species, collapse = "', '"), "')"))
  }
  
  ## Cruise 
  
  if(db) {
    sub$cruise <- input$selCruiseDb
  } else {
    sub$cruise <- input$subCruise
  }
  
  if (!is.null(sub$cruise)) {
    filterChain <- append(filterChain, paste0("cruise %in% c('", paste0(sub$cruise, collapse = "', '"), "')"))
  }
  
  ## Platform
  
  if(db) {
    sub$platform <- input$selPlatformDb
  } else {
    sub$platform <- input$subPlatform 
  }
  
  if (!is.null(sub$platform)) {
    filterChain <- append(filterChain, 
                          paste0("platformname %in% c('", paste0(sub$platform, collapse = "', '"), "')"))
  }
  
  ## Serial number
  
  if(!db) {
    sub$serialnumber <- input$subSerialnumber   
  } 
  
  if (!is.null(sub$serialnumber)) {
    filterChain <- append(filterChain, 
                          paste0("serialnumber %in% c('", paste0(sub$serialnumber, collapse = "', '"), "')"))
  }
  
  ## Gear
  
  if(db) {
    sub$gear <- input$selGearDb
  } else {
    sub$gear <- input$subGear
  }
  
  if (!is.null(sub$gear)) {
    filterChain <- append(filterChain, paste0("gear %in% c('", paste0(sub$gear, collapse = "', '"), "')"))
  }
  
  ## Mission type name
  
  if(db) {
    sub$missiontypename <- input$selMissionTypeDb
  } else {
    sub$missiontypename <- NULL#input$subGear
  }
  
  if (!is.null(sub$missiontypename)) {
    filterChain <- append(filterChain, paste0("missiontypename %in% c('", paste0(sub$missiontypename, collapse = "', '"), "')"))
  }
  
  ## Longitude
  
  if(db) {
    sub$lon <- as.numeric(input$selLonDb)
  } else {
    sub$lon <- as.numeric(input$subLon)
  }
  
  if(all(sub$lon == c(-180, 180))) sub$lon <- NULL
  
  if (!is.null(sub$lon)) {
    filterChain <- append(filterChain, paste0("longitudestart >= ",sub$lon[1], " & longitudestart <= ", sub$lon[2])) 
  }
  
  ## Latitude
  
  if(db) {
    sub$lat <- as.numeric(input$selLatDb)
  } else {
    sub$lat <- as.numeric(input$subLat)
  }
  
  if(all(sub$lat == c(-90, 90))) sub$lat <- NULL
  
  if (!is.null(sub$lat)) {
    filterChain <- append(filterChain, paste0("latitudestart >= ", sub$lat[1], " & latitudestart <= ", sub$lat[2]))
  }
  
  # if (!identical(as.numeric(input$subLon), c(rv$all$min.lon, rv$all$max.lon))) {
  #   sub$lon <- as.numeric(input$subLon)
  #   filterChain <- append(filterChain, paste0("longitudestart >= '", input$subLon[1], "' & longitudestart <= '", input$subLon[2], "'"))
  # } else {
  #   sub$lon <- NULL
  # }
  
  # if (!identical(as.numeric(input$subLat), c(rv$all$min.lat, rv$all$max.lat))) {
  #   sub$lat <- as.numeric(input$subLat)
  #   filterChain <- append(filterChain, paste0("latitudestart >= '", input$subLat[1], "' & latitudestart <= '", input$subLat[2], "'"))
  # } else {
  #   sub$lat <- NULL
  # }
  
  list(filterChain = filterChain, sub = sub)
}

