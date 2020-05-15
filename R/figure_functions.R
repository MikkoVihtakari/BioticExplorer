##############################
# File version 2020-05-13 ####
# Author and contact: mikko.vihtakari@hi.no

#' @title Station overview map using leaflet
#' @param data data required by the map. See \code{updateMap}

stationMap <- function(data) {
  
  if(nrow(data) == 0) {
    leaflet::leaflet() %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
      addMarkers(lng = 20, lat = 70, label = "No position information")
  } else {
    leaflet::leaflet(data) %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
      addCircleMarkers(lat = ~ latitudestart, lng = ~ longitudestart, 
                       weight = 1, radius = 2, 
                       popup = ~as.character(platformname), 
                       label = ~as.character(serialnumber), 
                       color = "red", fillOpacity = 0.5,
                       clusterOptions = markerClusterOptions()
      )
  }
}



# addRectangles(
# lng1 = input$subLon[1], lat1 = input$subLat[1], lng2 = input$subLon[2], lat2 = input$subLat[2],
# lng1 = rv$sub$lon[1], lat1 = rv$sub$lat[1], lng2 = rv$sub$lon[2], lat2 = rv$sub$lat[2],
# fillColor = "transparent") %>% 


#' @title Generate data for species overview plots
#' @description Generates data required by species overview plots in Biotic Explorer
#' @param data stnall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$stnall}.
#' @return Returns a list of tibbles containing data required by station-based various plots.
#' @import dplyr data.table

speciesOverviewData <- function(data) {
  
  # Number of stations
  
  nStn <- data %>% lazy_dt() %>% dplyr::group_by(commonname) %>% dplyr::summarise(n = length(unique(paste(startyear, serialnumber)))) %>% arrange(-n) %>% collect()
  nStn$commonname <- factor(nStn$commonname, nStn$commonname)
  
  # Catch weight
  
  catchW <- data[!is.na(data$catchweight),]
  
  if(nrow(catchW) > 0) {
    catchS <- catchW %>% lazy_dt() %>% dplyr::group_by(commonname) %>% 
      dplyr::summarise(mean = mean(catchweight, na.rm = TRUE), se = se(catchweight), max = max(catchweight, na.rm = TRUE), min = min(catchweight, na.rm = TRUE), sum = sum(catchweight, na.rm = TRUE)) %>% 
      arrange(-sum) %>% collect()
    catchS$commonname <- factor(catchS$commonname, catchS$commonname)
    catchS$se[is.na(catchS$se)] <- 0
    catchW$commonname <- factor(catchW$commonname, catchS$commonname)
  } else {
    catchS <- tibble(commonname = "chr", mean = 1, se = 1, max = 1, min = 1, sum = 1, .rows = 0)
  }
  
  # Mean weight
  
  meanW <- data %>% lazy_dt() %>% filter(!is.na(catchweight) & catchweight > 0 & !is.na(catchcount) & catchcount > 0) %>% 
    group_by(commonname, cruise, startyear, serialnumber) %>% summarise(weight = sum(catchweight), n = sum(catchcount)) %>% 
    mutate(indw = weight/n) %>% collect()
  
  meanW <- droplevels(meanW)
  meanW <- meanW %>% group_by(commonname) %>% summarise(mean = mean(indw), min = min(indw), max = max(indw), sd = sd(indw), se = se(indw)) %>% arrange(-mean)
  meanW$commonname <- factor(meanW$commonname, meanW$commonname)
  
  # Numbers in catch
  
  catchN <- data[!is.na(data$catchcount) & data$catchcount > 0,]
  
  meanN <- catchN %>% lazy_dt() %>% group_by(commonname) %>% 
    summarise(mean = mean(catchcount), se = se(catchcount), max = max(catchcount), min = min(catchcount), Nstn = length(unique(paste(cruise, startyear, serialnumber)))) %>% 
    arrange(-mean) %>% collect()
  meanN$se[is.na(meanN$se)] <- 0
  meanN$commonname <- factor(meanN$commonname, meanN$commonname)
  catchN$commonname <- factor(catchN$commonname, meanN$commonname)
  
  # Catch by gear
  
  catchGBase <- data[!is.na(data$catchweight),]
  catchG <- catchGBase %>% lazy_dt() %>% group_by(gear, commonname) %>% 
    summarise(sum = sum(catchweight)) %>% collect()
  
  catchG$commonname <- factor(catchG$commonname, catchS$commonname)
  
  # Bottom depth and fishing depth by station
  
  stnD <- data %>% lazy_dt() %>% group_by(cruise, startyear, serialnumber) %>% 
    summarise(bdepth = unique(bottomdepthstart), fdepth = unique(fishingdepthmin)) %>% 
    collect()
  
  stnD <- data.table::melt(data.table::as.data.table(stnD), id.vars = 1:3)
  stnD$variable <- dplyr::recode_factor(stnD$variable, "bdepth" = "Bottom depth (start)", "fdepth" = "Minimum fishing depth")
  
  # catch composition data
  
  compDat <- data %>% lazy_dt() %>% filter(!is.na(catchweight)) %>% 
    group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname) %>%
    summarise(catchweight = sum(catchweight)) %>% collect()
  
  sumCompDat <- compDat %>% group_by(commonname) %>% summarise(sum = sum(catchweight)) %>% arrange(-sum)
  
  compDat$commonname <- factor(compDat$commonname, sumCompDat$commonname)
  
  if (length(sumCompDat$commonname) > 6) {
    levels(compDat$commonname)[!levels(compDat$commonname) %in% sumCompDat$commonname[1:6]] <- "Andre arter"
  }
  
  levels(compDat$commonname) <- gsub("(^[[:alpha:]])", "\\U\\1", levels(compDat$commonname), perl = TRUE)    
  
  compDat <- compDat %>% 
    group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname, .drop = FALSE) %>% 
    summarise(catchweight = sum(catchweight)) %>% 
    arrange(cruise, startyear, serialnumber, commonname)
  
  compDatW <- tidyr::spread(compDat, commonname, catchweight)
  
  compDatW$total <- rowSums(compDatW[,levels(compDat$commonname)]) 
  
  # Return
  
  list(nStn = nStn, catchW = catchW, catchS = catchS, meanW = meanW, catchN = catchN, meanN = meanN, catchG = catchG, stnD = stnD, compDat = compDat, compDatW = compDatW)
}

#' @title Plot Number of stations containing a species
#' @description Plots a species composition in an \link[=processBioticFile]{bioticProcData} object. 
#' @param data data object from \link{speciesOverviewData}. Requires the nStn data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

speciesCompositionPlot <- function(data, base_size = 14) {
  
  x <- data$nStn
  
  ggplot(x, aes(y = n, x = commonname)) + 
    geom_col() +
    ylab("Number of stations containing the species") +
    xlab("Species database name") +
    coord_cartesian(expand = FALSE, ylim = range(pretty(x$n))) + 
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot summed catch weight
#' @description Plots summed catch weights in an \link[=processBioticFile]{bioticProcData} object. 
#' @param data data object from \link{speciesOverviewData}. Requires the catchS data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightSumPlot  <- function(data, base_size = 12) {
  
  x <- data$catchS
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = sum)) +
      annotate("text", x = 1, y = 1, label = "No weight data") +
      ylab("Summed catch weight [log10(kg)]") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = sum)) +
      geom_col() +
      scale_y_log10("Summed catch weight [log10(kg)]") +
      xlab("Species database name") +
      coord_cartesian() +
      theme_bw(base_size = base_size) +
      annotate("text", x = Inf, y = Inf, label = paste("Total catch\n all species\n", round(sum(x$sum), 0), "kg"), vjust = 1, hjust = 1, size = 5) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
}

## 

#' @title Plot mean catch weight
#' @description Plots mean catch weights in an \link[=processBioticFile]{bioticProcData} object. Error bars are standard error of the mean.
#' @param data data object from \link{speciesOverviewData}. Requires the catchS data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightMeanPlot  <- function(data, base_size = 14) {
  
  x <- data$catchS
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No weight data") +
      ylab("Mean catch weight (kg; +/- SE)") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
      geom_linerange() +
      geom_point() +
      ylab("Mean catch weight (kg; +/- SE)") +
      xlab("Species database name") +
      coord_cartesian() +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
}

#' @title Plot range of catch weights
#' @description Plots the range of catch weights in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the catchS and catchW data frames.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightRangePlot  <- function(data, base_size = 14) {
  
  x <- data$catchS
  y <- data$catchW
  
  if(nrow(x) == 0 | nrow(y) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No weight data") +
      scale_y_log10("Catch weight range [log10(kg)]") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot() +
      geom_linerange(data = x, aes(x = commonname, ymax = max, ymin = min), color = "red") +
      geom_point(data = y, aes(x = commonname, y = catchweight), size = 1, shape = 21) +
      scale_y_log10("Catch weight range [log10(kg)]") +
      xlab("Species database name") +
      coord_cartesian() +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  }
}

#' @title Plot mean weight of fish in catch
#' @description Plots mean weight of fish in catch in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanW data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchIndMeanWeightPlot <- function(data, base_size = 14) {
  
  x <- data$meanW
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No weight data") +
      scale_y_log10("Mean specimen weight [log10(kg) +/- range]") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = mean, ymin = min, ymax = max)) +
      geom_pointrange() +
      scale_y_log10("Mean specimen weight [log10(kg) +/- range]", labels = scales::number_format(accuracy = 0.001)) +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  }
}

#' @title Plot mean number in catches
#' @description Plots mean number of fish in catches in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanN data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchcountMeanPlot <- function(data, base_size = 14) {
  
  x <- data$meanN
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No catch data") +
      ylab("Mean number in catch (+/- SE)") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
      geom_linerange() +
      geom_point() +
      ylab("Mean number in catch (+/- SE)") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
}

#' @title Plot range of number in catches
#' @description Plots range of number of fish in catches in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanN and catchN data frames.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchcountRangePlot <- function(data, base_size = 14) {
  
  x <- data$meanN
  y <- data$catchN
  
  if(nrow(x) == 0 | nrow(y) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No catch data") +
      scale_y_log10("Range for number in catch (log10)") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot() +
      geom_linerange(data = x,
                     aes(x = commonname, ymax = max, ymin = min), color = "red") +
      geom_point(data = y,
                 aes(x = commonname, y = catchcount), size = 1, shape = 21) +
      scale_y_log10("Range for number in catch (log10)") +
      xlab("Species database name") +
      coord_cartesian() +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  }
}

#' @title Plot catch by species and gear code
#' @description Plots total catch by fish and gear type in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the catchG data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

gearCatchPlot <- function(data, base_size = 14) {
  
  x <- data$catchG
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = gear)) +
      annotate("text", x = 1, y = 1, label = "No gear information") +
      ylab("Gear code") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = as.factor(gear),
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
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  }
}

#' @title Plot bottom depth and minimum fishing depth distribution for stations
#' @description Plots bottom depth and minimum fishing depth distribution for stations in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the stnD data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

stationDepthPlot <- function(data, base_size = 14) {
  
  x <- data$stnD
  
  if(all(is.na(x$value))) {
    ggplot(x, aes(x = value)) +
      annotate("text", x = 1, y = 1, label = "No depth data") +
      ylab("Count") +
      xlab("Depth (m)") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = value)) +
      geom_histogram(binwidth = 100, color = "black", fill = "grey") +
      facet_wrap(~variable) +
      scale_y_continuous("Count", expand = c(0, 0)) +
      scale_x_continuous("Depth (m)", expand = c(0,0.05)) +
      expand_limits(x = 0) +
      theme_classic(base_size = base_size) +
      theme(strip.background = element_blank())
  }
}

#' @title Plot minimum fishing depth by catch for six most dominant species
#' @description Plots minimum fishing depth by catch for six most dominant species in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the compDat data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchSpeciesWeightPlot <- function(data, base_size = 14) {
  
  x <- data$compDat
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = fishingdepthmin, y = catchweight)) +
      annotate("text", x = 1, y = 1, label = "No depth data") +
      ylab("Catch weight (kg)") + 
      xlab("Minimum fishing depth (m)") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x[x$commonname != "Andre arter",], aes(x = fishingdepthmin, y = catchweight, group = commonname)) +
      geom_smooth(se = FALSE) +
      geom_point() +
      ylab("Catch weight (kg)") + 
      xlab("Minimum fishing depth (m)") +
      facet_wrap(~commonname, scales = "free_y") + 
      theme_bw(base_size = base_size)
  }
  
  
}

#' @title Plot catch composition on a map
#' @description Plots catch composition in an \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' @param data data object from \link{speciesOverviewData}. Requires the compDat data frame.
#' @return Returns a \link[leaflet]{leaflet} object
#' @import leaflet

catchCompMap <- function(data) {
  
  x <- data$compDatW
  y <- data$compDat
  
  if(nrow(x) == 0 | nrow(y) == 0) {
    leaflet::leaflet() %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
      addMarkers(lng = 20, lat = 70, label = "No catch data")
  } else {
    leaflet::leaflet() %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
      addMinicharts(
        x$longitudestart, x$latitudestart,
        type = "pie", chartdata = x[,levels(y$commonname)],
        colorPalette = ColorPalette,
        width = 40 * log(x$total) / log(max(x$total)), 
        transitionTime = 0
      )
  }
}


#' @title Plot total catch of a specific species on a mpa
#' @description Plots total catch of a species in an \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' @param data stnall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$stnall}.
#' @param species NMD commonname for a species. The names are in Norwegian. Use "All" to plot a sum of total catch including all species.
#' @return Returns a \link[leaflet]{leaflet} object
#' @import leaflet

catchMap <- function(data, species) {
  
  ## Definitions
  
  if (species == "All") {
    sps <- unique(data$commonname)
  } else {
    sps <- species
  }
  
  ## Prepare data
  
  tmp2 <- data %>% lazy_dt() %>% filter(!is.na(longitudestart) & !is.na(latitudestart)) %>% collect()
  
  tmp <- tmp2 %>% lazy_dt() %>% 
    filter(commonname %in% sps) %>% 
    group_by(startyear, serialnumber, longitudestart, latitudestart, gear, bottomdepthstart, stationstartdate) %>% 
    summarize(catchsum = round(sum(catchweight, na.rm = TRUE), 2)) %>% collect()
  
  tmp2 <- tmp2[!paste(tmp2$startyear, tmp2$serialnumber, sep = "_") %in% paste(tmp$startyear, tmp$serialnumber, sep = "_"), !names(tmp2) %in% c("catchsampleid", "commonname", "catchcategory", "catchpartnumber", "catchweight", "catchcount", "lengthsampleweight", "lengthsamplecount")]
  
  if (nrow(tmp2) > 0) tmp2$catchsum <- 0
  
  ## Plot
  
  p <- leaflet::leaflet(tmp, options = leafletOptions(zoomControl = FALSE)) %>% 
    addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
             attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>%
    addCircles(lat = ~ latitudestart, lng = ~ longitudestart, 
               weight = 4, radius = 5e4*(tmp$catchsum/max(tmp$catchsum)), 
               label = paste0(tmp$serialnumber, "; ", tmp$catchsum, " kg"), 
               popup = paste("Serial number:", tmp$serialnumber, "<br>",
                             "Date:", tmp$stationstartdate, "<br>",
                             "Gear code:", tmp$gear, "<br>",
                             "Bottom depth:", round(tmp$bottomdepthstart, 0), "m",
                             "<br>", species, "catch:", tmp$catchsum, 
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
                               "<br>", species, "catch:", tmp2$catchsum, 
                               "kg"), 
                 color = "black"
      )
  } else {
    p
  }
  
}

#' @title Plot overview of fish length by species
#' @description Plots overview of fish length by species in an \link[=processBioticFile]{bioticProcData} object.
#' @param indall indall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$indall}.
#' @param nLimit Integer for the minimum number of length measurements / species to be included in the plot.
#' @param unit character giving the measurement unit.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2


indLengthPlot <- function(indall, nLimit = 10, unit = "m", base_size = 14) {
  
  sps <- indall %>% lazy_dt() %>% filter(!is.na(length)) %>% group_by(commonname) %>% count() %>% filter(n > nLimit) %>% pull(commonname) 
  
  if(length(sps) > 0) {
    
    indLen <- indall %>% lazy_dt() %>%
      filter(!is.na(length), commonname %in% sps) %>% collect() %>% as.data.table()
    
    meanIndLen <- indLen %>% lazy_dt() %>%  
      group_by(commonname) %>%
      summarise(medLength = median(length)) %>% 
      arrange(-medLength) %>% collect()
    
    indLen[, commonname := factor(commonname, levels = meanIndLen$commonname)]
    
    ggplot() +
      geom_violin(data = indLen, aes(y = length, x = commonname)) +
      geom_point(data = meanIndLen, aes(y = medLength, x = commonname), shape = 95, size = 5) + 
      ylab(paste0("Length (", unit, ")")) +
      xlab("Species database name") +
      #coord_cartesian(expand = FALSE, ylim = range(pretty(x$n))) +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  } else {
    
    ggplot() +
      annotate("text", x = 1, y = 1, label = paste0("None of the species has\n >", nLimit, " length measured individuals")) +
      ylab(paste0("Length (", unit, ")")) +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  }
}

#' @title Plot overview of fish weight by species
#' @description Plots overview of fish weight by species in an \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' @param indall indall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$indall}.
#' @param nLimit Integer for the minimum number of length measurements / species to be included in the plot.
#' @param unit character giving measurement unit.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

indWeightPlot <- function(indall, nLimit = 10, unit = "kg", base_size = 14) {
  
  sps <- indall %>% lazy_dt() %>% filter(!is.na(individualweight)) %>% group_by(commonname) %>% count() %>% filter(n > nLimit) %>% pull(commonname)
  
  if(length(sps) > 0) {
    
    indWei <- indall %>% lazy_dt() %>%
      filter(!is.na(individualweight), commonname %in% sps) %>% collect() %>% as.data.table()
    
    meanIndWei <- indWei %>% lazy_dt() %>%  
      group_by(commonname) %>%
      summarise(medWeight = median(individualweight)) %>% 
      arrange(-medWeight) %>% collect()
    
    indWei[, commonname := factor(commonname, levels = meanIndWei$commonname)]
    
    ggplot() +
      geom_violin(data = indWei, aes(y = individualweight, x = commonname)) +
      geom_point(data = meanIndWei, aes(y = medWeight, x = commonname), shape = 95, size = 5) + 
      ylab(paste0("Weight (", unit, ")")) +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  } else {
    
    ggplot() +
      annotate("text", x = 1, y = 1, label = paste0("None of the species has\n >", nLimit, " length measured individuals")) +
      ylab(paste0("Weight (", unit, ")")) +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  }
}