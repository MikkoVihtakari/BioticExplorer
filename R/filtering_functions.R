#' @title Update subset selectors

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
  
  lon <- rv$stnall %>% summarise(min = min(longitudestart, na.rm = TRUE), max = max(longitudestart, na.rm = TRUE)) %>% collect()
  lat <- rv$stnall %>% summarise(min = min(latitudestart, na.rm = TRUE), max = max(latitudestart, na.rm = TRUE)) %>% collect()
  
  rv$all$min.lon <- floor(lon$min)
  rv$all$max.lon <- ceiling(lon$max)
  rv$all$min.lat <- floor(lat$min)
  rv$all$max.lat <- ceiling(lat$max)
  
  rv$all$date <- rv$stnall %>% summarise(min = min(stationstartdate, na.rm = TRUE), max = max(stationstartdate, na.rm = TRUE)) %>% collect()
}

updateFilterform <- function() {
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

#' @title Generate the station map

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

#' @title Populate the quick overview panel and station map

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
    
    tmp <- rv$stnall %>% select(missionid, startyear, serialnumber) %>% distinct() %>% count()
    
    valueBox(
      value = tags$p(tmp, style = "font-size: 80%;"),
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
      value = tags$p(length(rv$indall$length), style = "font-size: 60%;"),
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

#' @title Make a filter chain for file-based- and database-data loaded into the memory

makeFilterChain <- function() {
  
  sub <- list()
  filterChain <- list()
  
  sub$year <- input$subYear
  if (!is.null(input$subYear)) { 
    filterChain <- append(filterChain, paste0("startyear %in% c('", paste0(input$subYear, collapse = "', '"), "')"))
  }
  
  sub$species <- input$subSpecies    
  if (!is.null(input$subSpecies)) {
    filterChain <- append(filterChain, paste0("commonname %in% c('", paste0(input$subSpecies, collapse = "', '"), "')"))
  }
  
  sub$cruise <- input$subCruise
  if (!is.null(input$subCruise)) {
    filterChain <- append(filterChain, paste0("cruise %in% c('", paste0(input$subCruise, collapse = "', '"), "')"))
  }
  
  sub$platform <- input$subPlatform    
  if (!is.null(input$subPlatform)) {
    filterChain <- append(filterChain, 
                          paste0("platformname %in% c('", paste0(input$subPlatform, collapse = "', '"), "')"))
  }
  
  sub$serialnumber <- input$subSerialnumber   
  if (!is.null(input$subSerialnumber)) {
    filterChain <- append(filterChain, 
                          paste0("serialnumber %in% c('", paste0(input$subSerialnumber, collapse = "', '"), "')"))
  }
  
  sub$gear <- input$subGear
  if (!is.null(input$subGear)) {
    filterChain <- append(filterChain, paste0("gear %in% c('", paste0(input$subGear, collapse = "', '"), "')"))
  }
  
  if (is.null(input$subLon)) {
    sub$lon <- NULL
  } else {
    sub$lon <- as.numeric(input$subLon)
    filterChain <- append(filterChain, paste0("longitudestart >= '", input$subLon[1], "' & longitudestart <= '", input$subLon[2], "'")) 
  }

  if (is.null(input$subLat)) {
    sub$lat <- NULL
  } else {
    sub$lat <- input$subLat
    filterChain <- append(filterChain, paste0("latitudestart >= '", input$subLat[1], "' & latitudestart <= '", input$subLat[2], "'"))
    
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

