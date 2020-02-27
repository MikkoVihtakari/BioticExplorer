# Create a DT object from DBI (SQL) connection

DTfromSQL <- function(session, con, tableName, id) {
  
  cleanDataFrame = function(x) {
    x = unname(x)  # remove column names
    if (!is.data.frame(x)) return(x)
    for (j in seq_len(ncol(x))) {
      xj = x[, j]
      xj = unname(xj)  # remove names
      dim(xj) = NULL  # drop dimensions
      if (is.table(xj)) xj = c(xj)  # drop the table class
      x[[j]] = xj
    }
    unname(x)
  }
  
  # Get table
  dat <- tbl(con, tableName)
  
  # Count
  n <- dat %>% count() %>% pull()
  
  # Initial data
  cn <- colnames(dat)
  dat_init <- data.frame(matrix(ncol = length(cn)))
  colnames(dat_init) <- cn
  
  # Options
  options <- list()
  options$serverSide = TRUE
  options$autoWidth = TRUE
  options$sDom  = '<"top">lrt<"bottom">ip'
  options$scrollX = TRUE
  options$pageLength = 20
  options$ajax = list(url = dataTableAjax(session, dat_init, 
                                          filter = function(data, params) {
                                            #print(params)
                                            
                                            # Get current data (based on calculation)
                                            fr <- as.numeric(params$start) + 1
                                            ut <- as.numeric(params$start) + as.numeric(params$length)
                                            part <- fr:ut
                                            
                                            # doesn't work with SQL
                                            #newdata <- dat %>% collect() %>% slice(part)
                                            
                                            # For sql
                                            sql <- dbplyr::build_sql( "SELECT * FROM ( SELECT row_number() OVER () AS row, * FROM ", sql(tableName), " ) AS t WHERE row >=", sql(as.character(fr)), " AND row <= ", sql(as.character(ut)), con = con)
                                            #print(sql)
                                            newdata <- DBI::dbGetQuery(con, sql)
                                            
                                            # Remove row column
                                            newdata <- newdata %>% select(-row)
                                            
                                            # Add new column in front
                                            newdata <- cbind(" " = part, newdata)
                                            
                                            #print(newdata)
                                            #print(data)
                                            
                                            #print(ncol(newdata))
                                            #print(ncol(data))
                                            
                                            #print(colnames(newdata))
                                            #print(colnames(data))
                                            
                                            
                                            # the data argument in this case may be meaningless 
                                            return(list(
                                              draw = as.integer(params$draw),
                                              recordsTotal = n,
                                              recordsFiltered = n,
                                              data = cleanDataFrame(newdata)
                                              #,
                                              #DT_rows_all =  seq_len(10),
                                              #DT_rows_current = part
                                            )) 
                                          }, outputId = id))
  
  # create a widget using an Ajax URL created above
  widget = datatable(dat_init, options = options)
  
  return(widget)
}

con_db <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "/Users/a22357/Desktop/IMR_db.monetdb")

# Process data from database
processBioticDB <- function(con_db, lengthUnit = "cm", weightUnit = "g", removeEmpty = TRUE, coreDataOnly = FALSE, returnOriginal = TRUE, dataTable = TRUE, convertColumns = TRUE, missionidPrefix = NULL) {
  
  ## Check
  if(!class(con_db)[1] == "MonetDBEmbeddedConnection") stop("Connection is unavailable")
  
  ## create connection
  msn <- tbl(con_db, "mission")
  stn <- tbl(con_db, "fishstation")
  cth <- tbl(con_db, "catchsample")
  ind <- tbl(con_db, "individual")
  age <- tbl(con_db, "agedetermination")
  stndat <- dplyr::tbl(con_db, "stndat")
  system.time(inddat <- dplyr::tbl(con_db, "inddat") %>% filter(commonname == "blåkveite"))
  system.time(inddat <- DBI::dbReadTable(con_db, "inddat"))
  
  data.table::as.data.table(con_db)
  
  bla <- rquery::db_td(con_db, "inddat")
  rqdatatable::ex_data_table(bla)
  
  
  inddat <- local_td(con_db)
  
  useDate <- function(datasrc) {
    # TODO Here: Do we need to use TS (combined date time)?
    # 
    remove_suffix <- function(x) gsub(".asDate", "", x)
    datasrc <- datasrc %>% select(-ends_with("date", ignore.case=FALSE)) %>% rename_at(vars(ends_with("Date", ignore.case = FALSE)), remove_suffix)
    return(datasrc)
  }
  
  # Convert columns
  if (convertColumns) {
    msn <- useDate(msn)
    stn <- useDate(stn)
    age <- useDate(age)
    stndat <- useDate(stndat)
    inddat <- useDate(inddat)
  }
  
  if (!is.null(missionidPrefix)) {
    msn <- msn %>% mutate(missionid = paste(missionidPrefix, missionid, sep = "_"))
    stndat <- stndat %>% mutate(missionid = paste(missionidPrefix, missionid, sep = "_"))
    inddat <- inddat %>% mutate(missionid = paste(missionidPrefix, missionid, sep = "_"))
  }
  
  ### Length conversion
  if (lengthUnit == "cm") {
    ind <- ind %>% select(-(length)) %>% rename_at(vars(matches("length.cm")), function(x) return("length"))
  } else if (lengthUnit == "mm") {
    ind <- ind %>% select(-(length)) %>% rename_at(vars(matches("length.mm")), function(x) return("length"))
  } 
  
  ### Weigth conversion
  if (weightUnit == "g") {
    ind <- ind %>% select(-(individualweight)) %>% rename_at(vars(matches("individualweight.gram")), function(x) return("individualweight"))
  }
  
  # DB snapshot info
  ts <- tbl(con_db, "metadata") %>% select(year, timestamp) %>% pull()
  info <- paste("NOTE: Snapshot data are downloaded between", head(ts,1), "and", tail(ts,1), "(UTC time)")
  
  ## Return ----
  
  ### Format  
  if (returnOriginal) {
    out <- list(info = info, mission = msn, fishstation = stn, catchsample = cth, individual = ind, agedetermination = age, stnall = stndat, indall = inddat)
  } else {
    out <- list(info = info, mission = msn, stnall = stndat, indall = inddat)
  }
  
  class(out) <- "bioticProcDataDB"
  
  ### Return
  return(out)
}