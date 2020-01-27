##############################
# File version 2020-02-27 ####
# Author and contact: mikko.vihtakari@hi.no

#' @title Generate data for species overview plots
#' @description Generates data required by species overview plots in Biotic Explorer
#' @param data stnall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$stnall}.
#' @return Returns a list of tibbles containing data required by station-based various plots.
#' @import dplyr data.table

speciesOverviewData <- function(data) {
  
  # Number of stations
  
  nStn <- data %>% dplyr::group_by(commonname) %>% dplyr::summarise(n = length(unique(paste(startyear, serialnumber))))
  nStn <- nStn[order(-nStn$n),]
  nStn$commonname <- factor(nStn$commonname, nStn$commonname)
  
  # Catch weight
  
  catchW <- data[!is.na(data$catchweight),]
  
  catchS <- catchW %>% dplyr::group_by(commonname) %>% 
    dplyr::summarise(mean = mean(catchweight, na.rm = TRUE), se = se(catchweight), max = max(catchweight, na.rm = TRUE), min = min(catchweight, na.rm = TRUE), sum = sum(catchweight, na.rm = TRUE))
  catchS <- catchS[order(-catchS$sum),]
  catchS$commonname <- factor(catchS$commonname, catchS$commonname)
  catchS$se[is.na(catchS$se)] <- 0
  catchW$commonname <- factor(catchW$commonname, catchS$commonname)
  
  # Mean weight
  
  meanW <- data %>% filter(!is.na(catchweight) & catchweight > 0 & !is.na(catchcount) & catchcount > 0) %>% 
    group_by(commonname, cruise, startyear, serialnumber) %>% summarise(weight = sum(catchweight), n = sum(catchcount), indw = weight/n)
  meanW <- droplevels(meanW)
  meanW <- meanW %>% group_by(commonname) %>% summarise(mean = mean(indw), min = min(indw), max = max(indw), sd = sd(indw), se = se(indw)) %>% arrange(-mean)
  meanW$commonname <- factor(meanW$commonname, meanW$commonname)
  
  # Numbers in catch
  
  catchN <- data[!is.na(data$catchcount) & data$catchcount > 0,]
  
  meanN <- catchN %>% group_by(commonname) %>% summarise(mean = mean(catchcount), se = se(catchcount), max = max(catchcount), min = min(catchcount), Nstn = length(unique(paste(cruise, startyear, serialnumber))))
  meanN <- meanN[order(-meanN$mean),]
  meanN$se[is.na(meanN$se)] <- 0
  meanN$commonname <- factor(meanN$commonname, meanN$commonname)
  catchN$commonname <- factor(catchN$commonname, meanN$commonname)
  
  # Catch by gear
  
  catchGBase <- data[!is.na(data$catchweight),]
  catchG <- catchGBase %>% group_by(gear, commonname) %>% 
    summarise(sum = sum(catchweight))
  
  catchG$commonname <- factor(catchG$commonname, catchS$commonname)
  
  # Bottom depth and fishing depth by station
  
  stnD <- data %>% group_by(cruise, startyear, serialnumber) %>% 
    summarise(bdepth = unique(bottomdepthstart), fdepth = unique(fishingdepthmin))
  
  stnD <- data.table::melt(data.table::as.data.table(stnD), id.vars = 1:3)
  stnD$variable <- dplyr::recode_factor(stnD$variable, "bdepth" = "Bottom depth (start)", "fdepth" = "Minimum fishing depth")
  
  # catch composition data
  
  compDat <- data %>% filter(!is.na(catchweight)) %>% 
    group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname) %>%
    summarise(catchweight = sum(catchweight)) 
  
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
#' @return Returns a ggplot object
#' @import ggplot2

speciesCompositionPlot <- function(data) {
  
  x <- data$nStn
  
  ggplot(x, aes(y = n, x = commonname)) + 
    geom_col() +
    ylab("Number of stations containing the species") +
    xlab("Species database name") +
    coord_cartesian(expand = FALSE, ylim = range(pretty(x$n))) + 
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}
  
#' @title Plot summed catch weight
#' @description Plots summed catch weights in an \link[=processBioticFile]{bioticProcData} object. 
#' @param data data object from \link{speciesOverviewData}. Requires the catchS data frame.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightSumPlot  <- function(data) {
  
  x <- data$catchS
  
  ggplot(x, aes(x = commonname, y = sum)) +
    geom_col() +
    scale_y_log10("Summed catch weight [log10(kg)]") +
    xlab("Species database name") +
    coord_cartesian() +
    theme_bw(base_size = 12) +
    annotate("text", x = Inf, y = Inf, label = paste("Total catch\n all species\n", round(sum(x$sum), 0), "kg"), vjust = 1, hjust = 1, size = 5) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

## 

#' @title Plot mean catch weight
#' @description Plots mean catch weights in an \link[=processBioticFile]{bioticProcData} object. Error bars are standard error of the mean.
#' @param data data object from \link{speciesOverviewData}. Requires the catchS data frame.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightMeanPlot  <- function(data) {
  
  x <- data$catchS
  
  ggplot(x, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
    geom_linerange() +
    geom_point() +
    ylab("Mean catch weight (kg; +/- SE)") +
    xlab("Species database name") +
    coord_cartesian() +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot range of catch weights
#' @description Plots the range of catch weights in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the catchS and catchW data frames.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightRangePlot  <- function(data) {
  
  x <- data$catchS
  y <- data$catchW
  
  ggplot() +
    geom_linerange(data = x, aes(x = commonname, ymax = max, ymin = min), color = "red") +
    geom_point(data = y, aes(x = commonname, y = catchweight), size = 1, shape = 21) +
    scale_y_log10("Catch weight range [log10(kg)]") +
    xlab("Species database name") +
    coord_cartesian() +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot mean weight of fish in catch
#' @description Plots mean weight of fish in catch in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanW data frame.
#' @return Returns a ggplot object
#' @import ggplot2

catchIndMeanWeightPlot <- function(data) {
  
  x <- data$meanW
  
  ggplot(x, aes(x = commonname, y = mean, ymin = min, ymax = max)) +
    geom_pointrange() +
    scale_y_log10("Mean specimen weight (kg +/- range)", labels = scales::number_format(accuracy = 0.001)) +
    xlab("Species database name") +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot mean number in catches
#' @description Plots mean number of fish in catches in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanN data frame.
#' @return Returns a ggplot object
#' @import ggplot2

catchcountMeanPlot <- function(data) {
  
  x <- data$meanN
  
  ggplot(x, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
    geom_linerange() +
    geom_point() +
    ylab("Mean number in catch (+/- SE)") +
    xlab("Species database name") +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot range of number in catches
#' @description Plots range of number of fish in catches in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanN and catchN data frames.
#' @return Returns a ggplot object
#' @import ggplot2

catchcountRangePlot <- function(data) {
  
   x <- data$meanN
   y <- data$catchN
   
   ggplot() +
     geom_linerange(data = x,
                    aes(x = commonname, ymax = max, ymin = min), color = "red") +
     geom_point(data = y,
                aes(x = commonname, y = catchcount), size = 1, shape = 21) +
     scale_y_log10("Range for number in catch (log10)") +
     xlab("Species database name") +
     coord_cartesian() +
     theme_bw(base_size = 14) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot catch by species and gear code
#' @description Plots total catch by fish and gear type in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the catchG data frame.
#' @return Returns a ggplot object
#' @import ggplot2

gearCatchPlot <- function(data) {
  
  ggplot(data$catchG, aes(x = commonname, y = as.factor(gear),
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
  
}

#' @title Plot bottom depth and minimum fishing depth distribution for stations
#' @description Plots bottom depth and minimum fishing depth distribution for stations in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the stnD data frame.
#' @return Returns a ggplot object
#' @import ggplot2

stationDepthPlot <- function(data) {
  
  ggplot(data$stnD, aes(x = value)) +
    geom_histogram(binwidth = 100, color = "black", fill = "grey") +
    facet_wrap(~variable) +
    scale_y_continuous("Count", expand = c(0, 0)) +
    scale_x_continuous("Depth (m)", expand = c(0,0.05)) +
    expand_limits(x = 0) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank())
  
}

#' @title Plot minimum fishing depth by catch for six most dominant species
#' @description Plots minimum fishing depth by catch for six most dominant species in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the compDat data frame.
#' @return Returns a ggplot object
#' @import ggplot2

catchSpeciesWeightPlot <- function(data) {
  
  x <- data$compDat
  
  ggplot(x[x$commonname != "Andre arter",], aes(x = fishingdepthmin, y = catchweight, group = commonname)) +
    geom_smooth(se = FALSE) +
    geom_point() +
    ylab("Catch weight (kg)") + 
    xlab("Minimum fishing depth (m)") +
    facet_wrap(~commonname, scales = "free_y") + 
    theme_bw(base_size = 14)
}