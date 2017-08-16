library(strict)
library(dplyr)
library(tibble)
library(stringr)
library(futile.logger)
library(lubridate)
library(survival)
library(Rcpp)

Rcpp::sourceCpp(base_dir %>% paste0("aux_funcs.cpp"))

# Combine data and put it into narrow format
# Assumes data_object input
make_narrow <- function(timeindep_data, drug_data, event_data, min.date, max.date) {
  
  drug_data <- rename_column(drug_data, "dose", "value")
  drug_data <- rename_column(drug_data, "class", "name")
  #colnames(drug_data$data_matrix)[colnames(drug_data$data_matrix) == "dose"] <- "value"
  #colnames(drug_data$data_matrix)[colnames(drug_data$data_matrix) == "class"] <- "name"
  drug_frame <- data.frame(drug_data$data_matrix, type=rep("drug"), stringsAsFactors=FALSE)
  event_frame <- data.frame(event_data$data_matrix, type=rep("event"), stringsAsFactors=FALSE)
  timedep_data <- as.data.frame(rbind(drug_frame,drug_frame),stringsAsFactors=TRUE)
  timedep_data <- as.data_object(timedep_data)
  
  narrow_data <- make_all_intervals(timedep_data$data_matrix, idcolnum = 1)#, config=config) #TODO fix config usage
  # Add people who have no events or drug usages to data
  extra_lines <- cbind(person_id = base::setdiff(timeindep_data$data_matrix[,"person_id"], narrow_data[,1]), start = min.date, end = max.date)
  narrow_data <- rbind(narrow_data, extra_lines)
  
  # Add event indicators
  narrow_data <- cbind(narrow_data, add_event_indicators(narrow_data, events$data_matrix[,c(1,2,4)], idcolnum = 1))
  
  # Add drug columns
  narrow_data <- cbind(narrow_data, make_drug_columns(narrow_data, drug_data$data_matrix))
  
  # Add time-independent variables
  # TODO
  
  return (narrow_data)
}

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
# R wrapper for function Cpp_add_event_indicators
# Event_data is assumed to consist of an ID-column, time column and event type column.
# The function returns a matrix of indicators with a column for each event type.
add_event_indicators <- function(time_matrix, event_data, idcolnum, permanent=FALSE) {
  
  if(!is.matrix(time_matrix)) stop("Input time_matrix must be a matrix")
  
  # Function Cpp_add_event_indicators assumes id as first column
  time_matrix <- time_matrix[,c(idcolnum, base::setdiff(1:ncol(time_matrix), idcolnum))]
  event_data <- event_data[,c(idcolnum, base::setdiff(1:ncol(event_data), idcolnum))]
  # Sort by idcol
  time_matrix <- time_matrix[order(time_matrix[,1]),]
  event_data <- event_data[order(event_data[,1]),]
  
  events <- list()
  for(type in unique(event_data[,3])) {
    events <- c(events, list(event_data[event_data[,3] == type,1:2,drop=FALSE]))
  }
  
  
  indic_matrix <- (vapply(FUN.VALUE=matrix(nrow=nrow(time_matrix), ncol=ncol(event_data)-2, 0), 
                         seq_along(events), 
                         function(x) Cpp_add_event_indicators(time_matrix, events[[x]], permanent)
  ))
  #colnames(indic_matrix) <- colnames(event_data)[2:ncol(event_data)]
  
  return (indic_matrix)
}


# R wrapper for function Cpp_add_med_col.
make_drug_columns <- function(narrow_data, drug_data) {
  
  # TODO: Do proper sanitization for drug_data
  drugs <- list()
  drugs <- c(drugs, lapply(unique(drug_data[,4]), 
                           function(x) drug_data[drug_data[,4]==x,c(1:3,5),drop=FALSE]
  ))
  
  return (cbind(vapply(FUN.VALUE=numeric(nrow(narrow_data)), drugs, 
         function(x) Cpp_add_med_col(narrow_data, x)
  )))
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

