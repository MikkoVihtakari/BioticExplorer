##############################
# File version 2020-01-10 ####
# Author and contact: mikko.vihtakari@hi.no
# Load required packages:
# required.packages <- c("tidyverse", "dplyr", "data.table", "scales", "RstoxData")
# sapply(required.packages, require, character.only = TRUE)

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

#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#' @keywords internal
#' @author Hadley Wickham
#' @export
#'
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @title Process range text input

processRangeInput <- function(inp, idx) {
  # Sanitize input
  spl <- trimws(unlist(strsplit(inp, "[,]")))
  toEval <- grep("^\\d+:\\d+$", spl, perl = TRUE, value = TRUE)
  gearRange <- try(eval(parse(text = paste0("c(", paste(toEval, collapse = ","), ")"))))
  if(class(gearRange ) != "try-error")
    ret <- intersect(idx, gearRange)
  else
    ret <- NULL
  return(ret)
}

