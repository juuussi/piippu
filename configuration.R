library(testthat)
library(methods)

source(base_dir %>% paste0("common_funcs.R"))

# TODO: Create hidden list of global variables?

setClass(Class="Configuration", 
         slots = list(values="list"))

#Constructor
Configuration <- function(analysis_start_date, analysis_end_date, other_values=list(), replace=FALSE) {
  if(exists(".config") && replace == FALSE) {
    stop("Configuration object already exists. To replace it, use the `replace=TRUE` argument.")
  } else {
    if(valid_arg(analysis_start_date, stop_on_false=TRUE) && valid_arg(analysis_end_date, stop_on_false=TRUE)) {
      analysis_start <- to_unix_time(analysis_start_date)
      analysis_end   <- to_unix_time(analysis_end_date)
      if(analysis_end <= analysis_start) stop("Analysis end date cannot be before or the same as the analysis start date.")
      .config <<- new("Configuration", values=c(list(starttime=analysis_start, endtime=analysis_end), other_values))
    }
  }
}

getAnalysisStart <- function() {
  if(!exists(".config")) stop("Configuration object does not exist. Use the function `Configuration` to initialize one.")
  return(.config@values$starttime)
}
getAnalysisEnd <- function() {
  if(!exists(".config")) stop("Configuration object does not exist. Use the function `Configuration` to initialize one.")
  return(.config@values$endtime)
}
getFiles <- function() {
  if(!exists(".config")) stop("Configuration object does not exist. Use the function `Configuration` to initialize one.")
  return(list(subjects_file = .config@values$subjects_file,
              drugs_file = .config@values$drugs_file,
              events_file = .config@values$events_file,
              blackbox_file = .config@values$blackbox_file,
              static_covariates_file = .config@values$static_covariates_file)
         )
}
getOption <- function (option) {
  if(!exists(".config")) stop("Configuration object does not exist. Use the function `Configuration` to initialize one.")
  valid_arg(option, expected_class="character", expected_length=1, stop_on_false=TRUE)
  
  opt <- .config@values[[option]]
  if(!is.null(opt)) return(opt)
  else stop(paste0("No such option `", option, "` in configuration."))
}
test_that("Configuration: Arguments are validated correctly", 
          {
            expect_error(Configuration("11.10.2013", "10.10.2013", replace=TRUE))
            expect_error(Configuration("11.13.2013", "10.10.2013", replace=TRUE))
          })
test_that("Configuration: deny accidental replacement", 
          {
            Configuration("11.10.2013", "10.11.2013", replace=TRUE)
            expect_error(Configuration("11.10.2013", "10.11.2013", replace=FALSE))
          })

# Remove configuration created by tests
rm(.config)

# Reading config from file
readConfigFile <- function(path, replace=FALSE) {
  
  valid_arg(path, expected_class="character", expected_length=1, stop_on_false=TRUE)
  
  strs <- readLines(path)
  strs <- strsplit(strs, "=", fixed=TRUE)
  opts <- lapply(strs, function(x) x[2])
  opts <- lapply(opts, function(str) strsplit(str, ",", fixed=TRUE)[[1]])
  
  names(opts) <- lapply(strs, function(x) x[1])
  
  Configuration(opts$analysis_start_date, 
                opts$analysis_end_date,
                opts[base::setdiff(names(opts), c("analysis_start_date", "analysis_end_date"))], replace=replace)
  
}