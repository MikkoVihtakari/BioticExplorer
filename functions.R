#' @title Read and process a NMD Biotic xml file for further use in the BioticExplorer
#' @description A wrapper for \code{\link[RstoxData]{readXmlFile}} to enable further use in the BioticExplorer
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param lengthUnit character string specifying the unit for length output. Alternatives: "mm", "cm" or "m".
#' @param weightUnit character string specifying the unit for weigth output. Alternatives: "g" or "kg". 
#' @param removeEmpty logical indicating whether empty columns should be removed from the output. This option also influences "coreData" columns.
#' @param coreDataOnly logical indicating whether only important core columns should be picked from data. See \code{\link{coreDataList}} for list of core columns for each data type.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @param dataTable logical indicating whether the output should be returned as \link[data.table]{data.table}s instead of \link{data.frame}s. Setting this to \code{TRUE} speeds up further calculations using the data (but requires the \link[data.table]{data.table} syntax).
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function, but leads to problems with non-unicode characters.
#' @param missionidPrefix A prefix for the \code{missionid} identifier, which separates cruises. Used in \code{\link{processBioticFiles}} function when several xml files are put together. \code{NULL} (default) omits the prefix. Not needed in \code{processBioticFile} function.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} data frames. The \code{$stnall} and \code{$indall} data frames are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). 
#' @author Mikko Vihtakari (Institute of Marine Research) 
#' @import RstoxData data.table

# Debugging parameters
# file = "/Users/mvi023/Dropbox/Workstuff/Meetings/2019 Data Limited SA course/Vassild SA/Data/biotic_year_1994_species_162064.xml"
# lengthUnit = "cm"; weightUnit = "g"; removeEmpty = FALSE; coreDataOnly = TRUE; returnOriginal = TRUE; convertColumns = FALSE; missionidPrefix = NULL
# file = "C:\\Users\\a22357\\Dropbox\\Workstuff\\Meetings\\2019 Data Limited SA course\\Vassild SA\\Data\\biotic_year_1989_species_162064.xml"
processBioticFile <- function(file, lengthUnit = "cm", weightUnit = "g", removeEmpty = FALSE, coreDataOnly = TRUE, returnOriginal = TRUE, dataTable = TRUE, convertColumns = TRUE, missionidPrefix = NULL) {
  
  ## Read the Biotic file ----
  
  dt <- RstoxData::readXmlFile(file)
  
  ## Mission data ---
  
  if (coreDataOnly) {
    msn <- dt$mission[, coreDataList("mission"), with = FALSE]
  } else {
    msn <- setDT(dt$mission)
  }
  
  if (convertColumns) {
    msn <- convertColumnTypes(msn)
  } # add else here to fix the Norwegian letters
  
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
    stn <- convertColumnTypes(stn)  
  }
  
  ##________________
  ## Sample data ---
  
  if (coreDataOnly) {
    cth <- dt$catchsample[, coreDataList("catchsample"), with = FALSE]
  } else {
    cth <- dt$catchsample
  }
  
  if (convertColumns) {
    cth <- convertColumnTypes(cth) 
  }
  
  ##____________________
  ## Individual data ---
  
  if (coreDataOnly) {
    ind <- dt$individual[, coreDataList("individual"), with = FALSE]
  } else {
    ind <- setDT(dt$individual)
  }
  
  if (convertColumns) {
    ind <- convertColumnTypes(ind)
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
    age <- convertColumnTypes(age)
  }
  
  if (nrow(age) == 0) {
    age <- rapply(age, as.integer, how = "replace")
  }
  
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
  
  inddat <- merge(stndat, ind, all.y = TRUE, by = names(stndat)[names(stndat) %in% names(ind)]) 
  inddat <- merge(inddat, age, all = TRUE, by = names(inddat)[names(inddat) %in% names(age)])
  
  ## Return ----
  
  if (returnOriginal) {
    out <- list(mission = msn, fishstation = stn, catchsample = cth, individual = ind, agedetermination = age, stnall = stndat, indall = inddat)
  } else {
    out <- list(stnall = stndat, indall = inddat)
  }
  
  out <- lapply(out, function(k) {
    if (nrow(k) == 0) {
      NULL 
    } else {
      k
    }
  })
  
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
  
  class(out) <- "bioticProcData"
  out
  
}

#' @title Read and process NMD Biotic xml files for further use in the BioticExplorer
#' @description A wrapper for \code{\link{processBioticFile}} allowing processing multiple files simultaneously
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param lengthUnit character string specifying the unit for length output. Alternatives: "mm", "cm" or "m".
#' @param weightUnit character string specifying the unit for weigth output. Alternatives: "g" or "kg". 
#' @param removeEmpty logical indicating whether empty columns should be removed from output. This option also influences "coreData" columns.
#' @param coreDataOnly logical indicating whether only important core columns should be picked from data. See \code{\link{coreDataList}} for list of core columns for each data type.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @param dataTable logical indicating whether the output should be returned as \link[data.table]{data.table}s instead of \link{data.frame}s. Setting this to \code{TRUE} speeds up further calculations using the data (but requires the \link[data.table]{data.table} syntax).
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} data frames. The \code{$stnall} and \code{$indall} data frames are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). 
#' @author Mikko Vihtakari (Institute of Marine Research) 
#' @import RstoxData data.table

# Debugging parameters
# files = c("/Users/mvi023/Desktop/biotic_year_1982_species_172930.xml", "/Users/mvi023/Desktop/biotic_year_2016_species_172930.xml")
# lengthUnit = "cm"; weightUnit = "g"; removeEmpty = FALSE; coreDataOnly = TRUE; returnOriginal = TRUE; convertColumns = FALSE
processBioticFiles <- function(files, lengthUnit = "cm", weightUnit = "g", removeEmpty = FALSE, coreDataOnly = TRUE, returnOriginal = TRUE, dataTable = TRUE, convertColumns = TRUE) {
  
  # Read xml files
  
  out <- lapply(seq_along(files), function(i, lengthUnit. = lengthUnit, weightUnit. = weightUnit, removeEmpty. = removeEmpty, coreDataOnly. = coreDataOnly, returnOriginal. = returnOriginal) {
    print(paste("i =", i, "file = ", files[i]))
    print(paste(round(100*i/length(files), 0), "%"))
    processBioticFile(files[i], lengthUnit = lengthUnit., weightUnit = weightUnit., removeEmpty = removeEmpty., coreDataOnly = coreDataOnly., returnOriginal = returnOriginal., dataTable = TRUE, convertColumns = FALSE, missionidPrefix = i)
  })
  
  # Combine
  
  out <- do.call(Map, c(f = rbind, out))
  
  # Convert column classes
  
  if (convertColumns) {
    out <- lapply(out, function(k) {
      convertColumnTypes(k)
    })
  }
  
  # Convert to data.frames and/or remove empty columns
  
  if (!dataTable) {
    out <- lapply(out, function(k) {
      k <- as.data.frame(k)
    })
    
    if (removeEmpty) {
      out <- lapply(out, function(k) {
        k[apply(k, 2, function(x) sum(is.na(x))) != nrow(k)] 
      })
    }
  } else if (removeEmpty) {
    out <- lapply(out, function(k) {
      k[,which(unlist(lapply(k, function(x)!all(is.na(x))))),with = FALSE]
    })
  }
  
  # Define class
  
  class(out) <- "bioticProcData"
  
  # return
  
  out
  
}

## Core data columns list ----

#' @title List of core data columns by data type in NMD Biotic data
#' @description List of core data types used in \code{\link{processBioticFile}} and \code{\link{processBioticData}} functions
#' @param type character string specifying the data type. Alternatives: "mission", "fishstation", "individual", "catchsample", or "agedetermination".
#' @return Returns a character vector of core data types for a given data type.
#' @author Mikko Vihtakari (Institute of Marine Research)

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
#' @param k a data.table
#' @return Returns a data.table with corrected column types. Also corrects misinterpreted Norwegian letters and dates.
#' @import data.table
#' @author Mikko Vihtakari (Institute of Marine Research)

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
      trimws(correctNorwegianLetters(k))
    } else if (all(na.omit(as.numeric(k) == as.integer(k)))) { # k is an integer
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

tryCatchWE <- function(expr) {
  W <- NULL
  
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  
  er <- withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler)
  
  list(error = "error" %in% class(er), warning = !is.null(W))
  
}

## Correct Norwegian letters ----

#' @title Replace misinterpreted Norwegian letters by correct ones
#' @description Replaces various misinterpretations of å, æ, and ø by the correct letters
#' @param x character vector
#' @return Returns a character vector with corrected letters
#' @author Mikko Vihtakari (Institute of Marine Research) 

correctNorwegianLetters <- function(x) {
  
  if(!any(class(x) %in% c("factor", "character"))) stop("x has to be factor or character")
  
  if(class(x) == "factor") {
    FAC <- TRUE
    levs <- levels(x)
    x <- as.character(x)
  } else {
    FAC  <- FALSE
  }
  
  x <- gsub("Ã¦", "æ", gsub("Ã¥|\xe5", "å", gsub("Ã¸|xe6|\xf8", "ø", gsub("\xed", "i", gsub("\xc5", "Å", gsub("\xd8", "Ø", x))))))
  x[x == ""] <- NA
  
  if(FAC) {
    levs <- gsub("Ã¦", "æ", gsub("Ã¥|\xe5", "å", gsub("Ã¸|xe6|\xf8", "ø", gsub("\xed", "i", gsub("\xc5", "Å", gsub("\xd8", "Ø", levs))))))
    factor(x, levels = levs)
  } else {
    x
  }
}

# Print method for bioticProcData ----

#' @title Print processed NMD Biotic data (\code{bioticProcData}) objects
#' @description \code{\link{print}} function for \code{\link[=processBioticFile]{bioticProcData}} objects
#' @param x \code{bioticProcData} object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print bioticProcData
#' @author Mikko Vihtakari
#' @seealso \code{\link{processBioticFile}} \code{\link{processBioticData}}

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

#' @title Standard error of mean
#' @param x numeric vector

se <- function(x) {
  sd(x, na.rm = T)/sqrt(sum(!is.na(x)))}

## Loading logo, from https://stackoverflow.com/a/32854387/1082004

loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)")
    ),
    tags$a(href = href,
           div(class = "busy",  
               img(src = loadingsrc, height = height, width = width, alt = alt)),
           div(class = 'notbusy',
               img(src = src, height = height, width = width, alt = alt))
    )
  )
}

#' @title Back-transform predictor variables from a logit model

unlogit <- function(p, model) { 
  mean <- unname((log(p/(1 - p)) - coef(model)[1])/coef(model)[2])
  
  tmp.cis <- suppressMessages(confint(model))
  
  ci.max <- unname((log(p/(1 - p)) - tmp.cis[1])/tmp.cis[2])
  ci.min <- unname((log(p/(1 - p)) - tmp.cis[3])/tmp.cis[4])
  
  data.frame(mean = mean, ci.min = ci.min, ci.max = ci.max)
}


## Custom colour palette

ColorPalette <- c("#449BCF", "#82C893", "#D696C8", "#FF5F68", "#FF9252", "#FFC95B", "#056A89")
