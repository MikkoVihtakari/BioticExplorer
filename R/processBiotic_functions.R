#' @title Read and process a NMD Biotic xml file for further use in the BioticExplorer
#' @description A wrapper for \code{\link[RstoxData]{readXmlFile}} to enable further use in the BioticExplorer
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param lengthUnit character string specifying the unit for length output. Alternatives: "mm", "cm" or "m".
#' @param weightUnit character string specifying the unit for weigth output. Alternatives: "g" or "kg". 
#' @param removeEmpty logical indicating whether empty columns should be removed from the output. This option also influences "coreData" columns.
#' @param coreDataOnly logical indicating whether only important core columns should be picked from data. See \code{\link{coreDataList}} for list of core columns for each data type.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function, but leads to problems with non-unicode characters.
#' @param missionidPrefix A prefix for the \code{missionid} identifier, which separates cruises. Used in \code{\link{processBioticFiles}} function when several xml files are put together. \code{NULL} (default) omits the prefix. Not needed in \code{processBioticFile} function.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} data frames. The \code{$stnall} and \code{$indall} data frames are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). 
#' @author Mikko Vihtakari (Institute of Marine Research) 
#' @import RstoxData data.table
#' @export

# Debugging parameters
# lengthUnit = "cm"; weightUnit = "g"; removeEmpty = TRUE; coreDataOnly = FALSE; returnOriginal = TRUE; dataTable = TRUE; convertColumns = TRUE; missionidPrefix = NULL
# lengthUnit = "m"; weightUnit = "g"; removeEmpty = FALSE; coreDataOnly = TRUE; returnOriginal = TRUE; dataTable = TRUE; convertColumns = FALSE; missionidPrefix = NULL
processBioticFile <- function(file, lengthUnit = "cm", weightUnit = "g", removeEmpty = TRUE, coreDataOnly = FALSE, returnOriginal = FALSE, convertColumns = TRUE, missionidPrefix = NULL) {
  
  ## Checks
  
  if(!file.exists(file)) stop("file does not exist. Check your file path.")
  
  ## Read the Biotic file ----
  
  dt <- RstoxData::readXmlFile(file)
  
  ## Mission data ---
  
  if (coreDataOnly) {
    msn <- dt$mission[, coreDataList("mission"), with = FALSE]
  } else {
    msn <- data.table::setDT(dt$mission)
  }
  
  if (convertColumns) {
    date.cols <- grep("date", names(msn), value = TRUE) 
    msn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  } 
  
  if (is.null(missionidPrefix)) {
    msn$missionid <- rownames(msn)
  } else {
    msn$missionid <- paste(missionidPrefix, rownames(msn), sep = "_")
  }
  
  ## Station data ---
  
  if (coreDataOnly) {
    stn <- dt$fishstation[, coreDataList("fishstation"), with = FALSE]
  } else {
    stn <- setDT(dt$fishstation)
  }
  
  stn[is.na(stationstarttime), stationstarttime := "00:00:00.000Z"]
  
  stn[, stationstartdate := as.POSIXct(paste(stn$stationstartdate, stn$stationstarttime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]
  
  stn[, stationstarttime := NULL]
  
  if (!coreDataOnly) {
    stn[is.na(stationstoptime), stationstoptime := "00:00:00.000Z"]
    stn[, stationstopdate := as.POSIXct(paste(stn$stationstopdate, stn$stationstoptime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]
    stn[, stationstoptime := NULL]
  }
  
  if (convertColumns) {
    date.cols <- grep("date", names(stn), value = TRUE) 
    stn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  ##________________
  ## Sample data ---
  
  if (coreDataOnly) {
    cth <- dt$catchsample[, coreDataList("catchsample"), with = FALSE]
  } else {
    cth <- dt$catchsample
  }
  
  ##____________________
  ## Individual data ---
  
  if (coreDataOnly) {
    ind <- dt$individual[, coreDataList("individual"), with = FALSE]
  } else {
    ind <- setDT(dt$individual)
  }
  
  ### Length conversion
  
  if (lengthUnit == "cm") {
    ind[, length := length*100]
  } else if (lengthUnit == "mm") {
    ind[, length := length*1000]
  } 
  
  ### Weigth conversion
  
  if (sum(is.na(ind$individualweight)) != nrow(ind)) {
    
    if (weightUnit == "g") {
      ind[, individualweight := individualweight*1000]
    }  
  }
  
  ## Age data ---
  
  if (coreDataOnly) {
    age <- dt$agedetermination[, coreDataList("agedetermination"), with = FALSE]
  } else {
    age <- setDT(dt$agedetermination)
  }
  
  if (convertColumns) {
    date.cols <- grep("date", names(age), value = TRUE) 
    age[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  # if (nrow(age) == 0) {
  #   age <- rapply(age, as.integer, how = "replace")
  # }
  
  ## Compiled datasets ----
  
  if (coreDataOnly) {
    tmp <- coreDataList("fishstation")
    tmp <- tmp[!tmp %in% "stationstarttime"]
    
    coredat <- merge(msn[, c("missionid", "missiontype", "missionnumber", "startyear", "platform", "platformname", "cruise")], stn[, tmp, with = FALSE], all = TRUE)
    
  } else {
    coredat <- merge(msn, stn, by = names(msn)[names(msn) %in% names(stn)], all = TRUE)
  }
  
  # Stndat
  
  stndat <- merge(coredat, cth, all.y = TRUE, by = c("missiontype", "missionnumber", "startyear", "platform", "serialnumber"))
  
  # Inddat
  
  inddat <- merge(stndat[,!names(stndat) %in% c("purpose", "stationcomment"), with = FALSE], ind, all.y = TRUE, by = names(stndat)[names(stndat) %in% names(ind)]) 
  inddat <- rbindlist(list(inddat,age), fill=TRUE, use.names=TRUE)
  
  # inddat[is.na(inddat$commonname), "commonname"] <- "Merging error due to missing data"
  
  ## Return ----
  
  ### Format
  
  if (returnOriginal) {
    out <- list(mission = msn, fishstation = stn, catchsample = cth, individual = ind, agedetermination = age, stnall = stndat, indall = inddat)
  } else {
    out <- list(mission = msn, stnall = stndat, indall = inddat)
  }
  
  ### Remove empty columns to save space
  
  if (removeEmpty) {
    out <- lapply(out, function(k) {  
      k[, which(unlist(lapply(k, function(x) !all(is.na(x))))), with = FALSE]
    })
  }
  
  ### Class
  
  class(out) <- "bioticProcData"
  
  ### Return 
  
  out
  
}

#' @title Read and process NMD Biotic xml files for further use in the BioticExplorer
#' @description A wrapper for \code{\link{processBioticFile}} allowing processing multiple files simultaneously
#' @param files character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param lengthUnit character string specifying the unit for length output. Alternatives: "mm", "cm" or "m".
#' @param weightUnit character string specifying the unit for weigth output. Alternatives: "g" or "kg". 
#' @param removeEmpty logical indicating whether empty columns should be removed from output. This option also influences "coreData" columns.
#' @param coreDataOnly logical indicating whether only important core columns should be picked from data. See \code{\link{coreDataList}} for list of core columns for each data type.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} data frames. The \code{$stnall} and \code{$indall} data frames are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). 
#' @author Mikko Vihtakari (Institute of Marine Research) 
#' @import RstoxData data.table
#' @export

# Debugging parameters
# files = c("/Users/mvi023/Desktop/biotic_year_1982_species_172930.xml", "/Users/mvi023/Desktop/biotic_year_2016_species_172930.xml")
# lengthUnit = "cm"; weightUnit = "g"; removeEmpty = FALSE; coreDataOnly = TRUE; returnOriginal = TRUE; convertColumns = FALSE; mcCores = 1L
processBioticFiles <- function(files, lengthUnit = "cm", weightUnit = "g", removeEmpty = TRUE, coreDataOnly = FALSE, returnOriginal = FALSE, convertColumns = TRUE) {
  
  # Read xml files
  
  
  # Debug parameters: lengthUnit. = lengthUnit; weightUnit. = weightUnit; coreDataOnly. = coreDataOnly; returnOriginal. = returnOriginal
  out <- lapply(seq_along(files), function(i, lengthUnit. = lengthUnit, weightUnit. = weightUnit, coreDataOnly. = coreDataOnly, returnOriginal. = returnOriginal, convertColumns. = convertColumns) {
    print(paste("i =", i, "file = ", files[i]))
    print(paste(round(100*i/length(files), 0), "%"))
    processBioticFile(files[i], lengthUnit = lengthUnit., weightUnit = weightUnit., removeEmpty = FALSE, 
                      coreDataOnly = coreDataOnly., returnOriginal = returnOriginal., dataTable = TRUE, 
                      convertColumns = convertColumns., missionidPrefix = i
                      )
  })
  
  
  # Combine
  
  out <- do.call(Map, c(f = rbind, out))
  
  # Convert column classes
  
  # if (convertColumns) {
  #   
  #   out <- lapply(out, function(k) {
  #     convertColumnTypes(k)
  #   })
  #   
  # }
  
  # Convert to data.frames and/or remove empty columns
  
  if (!dataTable) {
    out <- lapply(out, function(k) {
      k <- as.data.frame(k)
    })
    
    if (removeEmpty) {
      out <- lapply(out, function(k) {
        if (is.null(k)) {
          NULL
        } else {
          k[apply(k, 2, function(x) sum(is.na(x))) != nrow(k)] 
        }
      })
    }
  } else if (removeEmpty) {
    
    
    out <- lapply(out, function(k) {
      if (is.null(k)) {
        NULL
      } else {
        k[, which(unlist(lapply(k, function(x) !all(is.na(x))))), with = FALSE]
      }
    })
    
    
  }
  
  # Define class
  
  class(out) <- "bioticProcData"
  
  # return
  
  out
  
}

## Core data columns list ----

#' @title List of core data columns by data type in NMD Biotic data
#' @description List of core data types used in the \code{\link{processBioticFile}} function
#' @param type character string specifying the data type. Alternatives: "mission", "fishstation", "individual", "catchsample", or "agedetermination".
#' @return Returns a character vector of core data types for a given data type.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @keywords internal
#' @export

coreDataList <- function(type) {
  switch(type,
         mission = c(c("missiontype", "startyear", "platform", "missionnumber", "missiontypename", "callsignal", "platformname", "cruise", "missionstartdate", "missionstopdate", "purpose")),
         fishstation = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "station", "stationstartdate", "stationstarttime", "longitudestart", "latitudestart", "bottomdepthstart", "fishingdepthmin", "gear", "distance"),
         individual = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid", "sex", "maturationstage", "specialstage", "length", "individualweight"),
         catchsample = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "commonname", "catchcategory", "catchpartnumber", "catchweight", "catchcount", "lengthsampleweight", "lengthsamplecount"),
         agedetermination = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid", "age", "readability"),
         stop("Undefined type argument"))
}

## Convert column types ----

#' @title Converts column types in a data frame to (hopefully) correct types
#' @description Converts column types in a data frame to (hopefully) correct types
#' @param df a data.table
#' @return Returns a data.table with corrected column types. Also corrects misinterpreted Norwegian letters and dates.
#' @import data.table
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @export

convertColumnTypes <- function(df) {
  
  ## Conversion function
  
  convertFun <- function(k) {
    
    if (all(is.na(k))) { # no conversion if all NA
      k
    } else if (any(grepl("POSIX", class(k)))) { # no conversion if k is already time class
      k
    } else if (all(!unlist(tryCatchWE(as.Date(k))))) { # k is a date
      as.Date(k)
    } else if (tryCatchWE(as.numeric(k))$warning) { # k is a character
      trimws(k)
    } else if (all(stats::na.omit(as.numeric(k) == as.integer(k)))) { # k is an integer
      as.integer(k)
    } else if (all(!unlist(tryCatchWE(as.numeric(k))))) { # k is numeric
      as.numeric(k)
    } else {
      stop("column type conversion failed.")
    }
  }
  
  ## Conversion
  
  df[, lapply(.SD, convertFun)]
  
}



## Warning/Error catcher ----

#' @title tryCatch both warnings (with value) and errors
#' @param expr an \R expression to evaluate
#' @return List of logicals indicating whether the \code{expr} produces a warning or error.
#' @author Martin Maechler; Copyright (C) 2010-2012 The R Core Team, Mikko Vihtakari (Institute of Marine Research)
#' @export

tryCatchWE <- function(expr) {
  W <- NULL
  
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  
  er <- withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler)
  
  list(error = "error" %in% class(er), warning = !is.null(W))
  
}


# Print method for bioticProcData ----

#' @title Print processed NMD Biotic data (\code{bioticProcData}) objects
#' @description \code{\link{print}} function for \code{\link[=processBioticFile]{bioticProcData}} objects
#' @param x \code{bioticProcData} object to be printed.
#' @param ... further arguments passed to \code{\link{print}}.
#' @method print bioticProcData
#' @author Mikko Vihtakari
#' @seealso \code{\link{processBioticFile}} \code{\link{processBioticFiles}}
#' @export

print.bioticProcData <- function(x, ...) {
  
  cat("Processed Biotic Data object")
  cat(paste(" of class", class(x)), sep = "\n")
  cat(NULL, sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("$mission: ", nrow(x$mission), " rows and ", ncol(x$mission), " columns"), sep = "\n")
  cat(paste0("$fishstation: ", nrow(x$fishstation), " rows and ", ncol(x$fishstation), " columns"), sep = "\n")
  cat(paste0("$catchsample: ", nrow(x$catchsample), " rows and ", ncol(x$catchsample), " columns"), sep = "\n")
  cat(paste0("$individual: ", nrow(x$individual), " rows and ", ncol(x$individual), " columns"), sep = "\n")
  cat(paste0("$agedetermination: ", nrow(x$agedetermination), " rows and ", ncol(x$agedetermination), " columns"), sep = "\n")
  cat(paste0("$stnall: ", nrow(x$stnall), " rows and ", ncol(x$stnall), " columns"), sep = "\n")
  cat(paste0("$indall: ", nrow(x$indall), " rows and ", ncol(x$indall), " columns"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Object size: ", sep = "")
  print(utils::object.size(x), unit = "auto")
  cat("Years: ", sep = "")
  cat(unique(x$mission$startyear), sep = ", ")
  cat(NULL, sep = "\n")
  cat(paste0(length(unique(x$mission$cruise)), " cruises, ", length(unique(paste(x$stnall$startyear, x$stnall$serialnumber))), " separate stations and ", nrow(x$indall), " measured fish."), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("Geographic range: ", round(min(x$stnall$longitudestart, na.rm = TRUE), 1), "-", round(max(x$stnall$longitudestart, na.rm = TRUE), 1), " degrees longitude and ", round(min(x$stnall$latitudestart, na.rm = TRUE), 1), "-", round(max(x$stnall$latitudestart, na.rm = TRUE), 1), " latitude."), sep = "\n")
  cat("Number of missing station coordinates: ", sep = "")
  cat(sum(is.na(x$stnall$longitudestart) | is.na(x$stnall$latitudestart)))
  cat(NULL, sep = "\n")
  cat("Unique species: ", sep = "")
  cat(sort(unique(x$stnall$commonname)), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  
}
