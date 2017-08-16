library(strict)
library(dplyr)
library(tibble)
library(stringr)
library(futile.logger)
library(lubridate)
library(survival)
library(Rcpp)

Rcpp::sourceCpp(base_dir %>% paste0("aux_funcs.cpp"))

# R wrapper for C++ function Cpp_add_missing_intervals
make_all_intervals <- function(data, idcolnum, config=NULL){
  
  if(!is.matrix(data)) stop("Input data must be a matrix")
  
  # Function Cpp_add_missing_intervals assumes id as first column
  data <- data[,c(idcolnum, base::setdiff(1:ncol(data), idcolnum))]
  # Sort by idcol
  data <- data[order(data[,1]),]
  
  first_all_NA_row <- function(matr) {
    nas <- which(unlist(apply(matr, MARGIN=1, FUN=function(x) all(is.na(x)))))
    return (ifelse(length(nas) > 0, min(nas), Inf))
  }
  result <- Cpp_add_missing_intervals(data[,1:3], ifelse(!is.null(config), config$analysis_start, 0))
  cut_row <- first_all_NA_row(result)
  # test_that("No non-all-NA-rows are removed", 
  #   expect_true(all(is.na(result[-1:-(cut_row-1), ])))
  # )
 
  if(cut_row < Inf) 
    result <- result[1:(cut_row-1), ]
  
  colnames(result) <- colnames(data)[1:3]
  
  return (result)
  
}

# REDUNDANT function if marking NA's while reading csv's?
reinsert_NAs <- function(data, cols=1:ncol(data), ...) {
  args <- unlist(list(...))
  print(args)
  if(any(!is.character(args))) stop("Some NAs are not character strings.")
  args <- vapply(FUN.VALUE=character(1), args, function(arg) str_trim(arg))
  
  #TODO: Inefficient
  for(n in 1:nrow(data))
    for(m in cols)
      if(data[n,m] %in% args)
        data[n,m] <- NA
  
  return (data)
}


to_unix_time <- function(date_strings, tz="UTC") {
  if(is.vector(date_strings))
    return (as.integer(parse_date_time(date_strings, tz=tz, orders=c("%m/%d/%Y","%d.%m.%Y"), exact=TRUE)))
  if(is.data.frame(date_strings))
    return (vapply(date_strings, FUN.VALUE=integer(nrow(date_strings)), 
                   function(strs) return (as.integer(parse_date_time(strs, tz=tz, orders=c("%m/%d/%Y","%d.%m.%Y"), exact=TRUE)))))
}

