#' @title Read and process NMD Biotic xml files for further use in the BioticExplorer
#' @description A wrapper for \code{\link[RNMDAPI]{readNMDxmlFile}} to enable further use in the BioticExplorer
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param lengthUnit character string specifying the unit for length output. Alternatives: "mm", "cm" or "m".
#' @param weightUnit character string specifying the unit for weigth output. Alternatives: "g" or "kg". 
#' @param removeEmpty logical indicating whether empty columns should be removed from output. This option also influences "coreData" columns.
#' @param coreDataOnly logical indicating whether only important core columns should be picked from data. See \code{\link{coreDataList}} for list of core columns for each data type.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$fishstation}, \code{$individual}, \code{$catchsample} and \code{$agedetermination} data frames. See the help file in the Shiny app for explanations of the data types.
#' @author Mikko Vihtakari (Institute of Marine Research) 

# Debugging parameters
# file = "/Users/mvi023/Dropbox/Workstuff/Meetings/2019 Data Limited SA course/Vassild SA/Data/biotic_year_1994_species_162064.xml"
# lengthUnit = "cm"; weightUnit = "g"; removeEmpty = FALSE; coreDataOnly = TRUE
# file = "C:\\Users\\a22357\\Dropbox\\Workstuff\\Meetings\\2019 Data Limited SA course\\Vassild SA\\Data\\biotic_year_1989_species_162064.xml"
processBioticFile <- function(file, lengthUnit = "cm", weightUnit = "g", removeEmpty = FALSE, coreDataOnly = TRUE) {
  
 ## Read the Biotic file ----
  
  dt <- RNMDAPI::readNMDxmlFile(file)
  
  dt <- lapply(dt, function(k) {
    k <- as.data.frame(k)
  })
  
 ## Mission data ---
  
  if(coreDataOnly) {
      msn <- dt$mission[coreDataList("mission")]
    } else {
      msn <- dt$mission
  }
  
  msn <- convertColumnTypes(msn)
  msn$missionid <- rownames(msn)
  
 ## Station data ---
  
  if(coreDataOnly) {
     stn <- dt$fishstation[coreDataList("fishstation")]
  } else {
     stn <- dt$fishstation
  }
 
  stn$stationstartdate <- strptime(paste(stn$stationstartdate, stn$stationstarttime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")
  stn <- stn[!names(stn) %in% "stationstarttime"]
 
  if(!coreDataOnly) {
    stn$stationstopdate <- strptime(paste(stn$stationstopdate, stn$stationstoptime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")
    stn <- stn[!names(stn) %in% "stationstoptime"]
  }
 
  stn <- convertColumnTypes(stn)
 
  ## Sample data ---
  
  if(coreDataOnly) {
     cth <- dt$catchsample[coreDataList("catchsample")]
  } else {
     cth <- dt$catchsample
  }
  
  # Fix weigth and length
  
  cth <- convertColumnTypes(cth)
  
  ## Individual data ---
  
  if(coreDataOnly) {
     ind <- dt$individual[coreDataList("individual")]
  } else {
     ind <- dt$individual
  }
  
  ind <- convertColumnTypes(ind)
  
  ### Length conversion
  ind$length <- 
    if (lengthUnit == "cm") {
    ind$length * 100
  } else if (lengthUnit == "mm") {
    ind$length * 1000
  } else {
    ind$length
  }
  
  ### Weigth conversion
  
  if(sum(is.na(ind$individualweight)) != nrow(ind)) {
    ind$individualweight <- 
      if (weightUnit == "g") {
        ind$individualweight * 1000
      } else {
        ind$individualweight
      }  
  }

  ## Age data ---
  
  if(coreDataOnly) {
     age <- dt$agedetermination[coreDataList("agedetermination")]
  } else {
     age <- dt$agedetermination
  }
  
  age <- convertColumnTypes(age)
  
  ## Compiled datasets ----
  
  tmp <- coreDataList("fishstation")
  tmp <- tmp[!tmp %in% "stationstarttime"]
  
  coredat <- merge(msn[c("missiontype", "startyear", "platform", "missionnumber", "missionid", "platformname")], stn[tmp], all = TRUE)
  
  stndat <- merge(coredat, cth[coreDataList("catchsample")], all.y = TRUE)
  rownames(stndat) <- 1:nrow(stndat)
  
  
  inddat <- merge(stndat[c("missiontype", "startyear", "platform", "missionnumber", "missionid", "serialnumber", "catchsampleid", "platformname", "longitudestart", "latitudestart", "gear", "commonname")], ind[coreDataList("individual")], all.y = TRUE)
  inddat <- merge(inddat, age[coreDataList("agedetermination")], all = TRUE)
  
  rownames(inddat) <- 1:nrow(inddat)
  
  ## Return ----
  
  out <- list(mission = msn, fishstation = stn, catchsample = cth, individual = ind, agedetermination = age, stnall = stndat, indall = inddat)
  
  out <- lapply(out, function(k) {
    if(nrow(k) == 0) {
      NULL 
    } else {
      k
    }
  })
  
  if(removeEmpty) {
    out <- lapply(out, function(k) {
      k[apply(k, 2, function(x) sum(is.na(x))) != nrow(k)] 
    })
  }
      
  class(out) <- "bioticProcData"
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
#' @param df a data.frame
#' @return Returns a data.frame with corrected column types. Also corrects misinterpreted Norwegian letters and dates.
#' @author Mikko Vihtakari (Institute of Marine Research)

convertColumnTypes <- function(df) {
  
  out <- lapply(df, function(k) {
    
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
    
  })
  
  as.data.frame(out, stringsAsFactors = FALSE)
}

## Warning/Error catcher ----

#' @title tryCatch both warnings (with value) and errors
#' @param expr an \R expression to evaluate
#' @return List of logical indicating whether the \code{expr} produces a warning or error.
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
  
  x <- gsub("Ã¦", "æ", gsub("Ã¥", "å", gsub("Ã¸", "ø", gsub("\xed", "i", gsub("\xc5", "Å", gsub("\xd8", "Ø", gsub("\xe6", "ø", gsub("\xe5", "å", gsub("\xf8", "ø", x)))))))))
  x[x == ""] <- NA
  
  if(FAC) {
    levs <- gsub("Ã¦", "æ", gsub("Ã¥", "å", gsub("Ã¸", "ø", gsub("\xed", "i", gsub("\xc5", "Å", gsub("\xd8", "Ø", gsub("\xe6", "ø", gsub("\xe5", "å", gsub("\xf8", "ø", levs)))))))))
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
  cat(sum(is.na(x$fishstation$longitudestart) | is.na(x$fishstation$latitudestart)))
  cat(NULL, sep = "\n")
  cat("Unique fish species: ", sep = "")
  cat(unique(x$stnall$commonname), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  
}

#' @title Standard error of mean
#' @param x numeric vector

se <- function (x){
  sd(x, na.rm = T)/sqrt(sum(!is.na(x)))}

