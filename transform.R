#library(strict)
library(dplyr)
#library(tibble)
library(stringr)
library(futile.logger)
library(lubridate)
library(survival)
library(Rcpp)
library(testthat)

source(base_dir %>% paste0("common_funcs.R"))

Rcpp::sourceCpp(base_dir %>% paste0("aux_funcs.cpp"))

#TODO: use dplyr more

make_long <- function(timeindep_data, drug_data, event_data, static_covariate_data) {
  
  valid_arg(timeindep_data, expected_class="DataObject", stop_on_false=TRUE)
  valid_arg(drug_data, expected_class="DataObject", stop_on_false=TRUE)
  valid_arg(event_data, expected_class="DataObject", stop_on_false=TRUE)
  valid_arg(static_covariate_data, expected_class="DataObject", stop_on_false=TRUE)
  
  
  # Data objects must have correct variable names to avoid mistakes with misordered columns
  if(!colnames(event_data) %>% base::setequal(c("person_id", "start", "stop", "value")))
    stop("Event data must have columns called `person_id`, `start`, `stop` and `value`.")
  if(!colnames(drug_data) %>% base::setequal(c("person_id", "start", "stop", "class", "dose")))
  #if(!all(colnames(drug_data@data_matrix) %in% c("person_id", "start", "stop", "class")))
    stop("Drug data must have columns called `person_id`, `start`, `stop`, `class`.")#, and optionally `dose`.")
  if(!colnames(static_covariate_data) %>% base::setequal(c("person_id", "name", "value")))
    stop("Static covariate data must have columns called `person_id`, `name` and `value`.")
  
  # When doing rbind, must have same column names
  #if("dose" %in% colnames(drug_data)) {
    drug_data <- rename_column(drug_data, "class", "value")
  #} else {
    #drug_data <- add_column(drug_data, "value", NA)
  #}
  
  event_data <- reorder_columns(event_data, c("person_id", "start", "stop", "value")) 
  drug_data  <- reorder_columns(drug_data, c("person_id", "start", "stop", "value", "dose"))
  static_covariate_data  <- reorder_columns(static_covariate_data, c("person_id", "name", "value"))
  
  timedep_data <- rbind(event_data@data_matrix[,c("person_id", "start", "stop")], drug_data@data_matrix[,c("person_id", "start", "stop")])#asDataObject(timedep_data)
  
  num_uniq_events <- length(unique(event_data$value))
  num_uniq_drugs <- length(unique(drug_data$value))
  
  long_data <- make_all_intervals(timedep_data, idcolnum = 1)
  # Add people who have no events or drug usages to data
  extra_lines <- cbind(person_id = timeindep_data$person_id %>% base::setdiff(long_data[,1]), 
                       start = getAnalysisStart(), 
                       end = getAnalysisEnd())
  long_data <- rbind(long_data, extra_lines)
  
  # Add event indicators
  event_inds <- add_event_indicators(long_data, event_data@data_matrix[,c(1,3,4)], idcolnum = 1)
  long_data <- cbind(long_data, event_inds)
  colnames(long_data)[3 + 1:num_uniq_events] <- getLevels(event_data)$value
  
  # Add drug columns
  drug_cols <- make_drug_columns(long_data, drug_data@data_matrix)
  long_data <- data.frame(cbind(long_data, drug_cols), row.names=NULL)
  colnames(long_data)[(4+num_uniq_events):(4+num_uniq_events+num_uniq_drugs-1)] <- levels(drug_data$value)
  
  # Add time-independent variables
  
  # Check for duplicate or contradictory rows
  if(NROW(unique(cbind(static_covariate_data@data_matrix[,c("person_id","name")]))) != NROW(static_covariate_data@data_matrix)) {
    stop("Each static covariate must have only one value for each person.")
  }
  
  covars <- getLevels(static_covariate_data)$name
  vars <- split(static_covariate_data@data_matrix, static_covariate_data@data_matrix[,"person_id"], drop=TRUE)
  vars <- lapply(vars, function(dat) return (matrix(dat, ncol=ncol(static_covariate_data@data_matrix), dimnames=dimnames(static_covariate_data@data_matrix))))
  
  rows <- lapply(vars, function(id_data) {
    tmp <- rep(NA,length(covars))
    names(tmp) <- covars
    tmp[ id_data[,"name", drop=TRUE] ] <- id_data[,"value", drop=TRUE]
    return (c(id_data[1,"person_id",drop=TRUE], tmp))
    })
  rows <- do.call(rbind,rows)
  static_covs_long <- rows[match(long_data$person_id, rows[,"person_id",drop=TRUE]),base::setdiff(colnames(rows), "person_id")]
  #static_covs_long[,] <- getLevels(static_covariate_data)$value[static_covs_long]
  if(!is.null(getLevels(static_covariate_data)$value))
    static_covs_long <- do.call(cbind, lapply(1:NCOL(static_covs_long), function(col) {
      l <- getLevels(static_covariate_data)$value
      if(!is.null(l)) {
        return (l[static_covs_long[,col,drop=TRUE] ])
      }
    }))
  #static_covs_long <- getLevels(static_covariate_data)
  rownames(static_covs_long) <- NULL
  colnames(static_covs_long) <- getLevels(static_covariate_data)$name
  
  long_data <- cbind(long_data, static_covs_long)
  
  return (long_data)
}

#Function for putting data into "within-analysis" format, i.e. resetting time on event
reset_on_event <- function(data_object, event){
  
}

# R wrapper for C++ function Cpp_add_missing_intervals
make_all_intervals <- function(data, idcolnum){
  
  if(!is.matrix(data)) stop("Input data must be a matrix")
  
  # Function Cpp_add_missing_intervals assumes id as first column
  data <- data[,c(idcolnum, base::setdiff(1:ncol(data), idcolnum))]
  # Sort by idcol
  data <- data[order(data[,1]),]
  
  first_all_NA_row <- function(matr) {
    nas <- which(unlist(apply(matr, MARGIN=1, FUN=function(x) all(is.na(x)))))
    return (ifelse(length(nas) > 0, min(nas), Inf))
  }
  result <- Cpp_add_missing_intervals(data[,1:3], getAnalysisStart())
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
add_event_indicators <- function(time_matrix, event_data, idcolnum, permanent=rep(FALSE, times=ncol(event_data)-2)) {
  
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
  
  indic_matrix <- lapply(seq_along(events),
                         function(x) Cpp_add_event_indicators(time_matrix, events[[x]], permanent[x])  
                         )
  indic_matrix <- do.call(cbind, indic_matrix)
  
  return (indic_matrix)
}


# R wrapper for function Cpp_add_med_col.
make_drug_columns <- function(narrow_data, drug_data) {
  
  # TODO: Do proper sanitization for drug_data
  drugs <- list()
  drugs <- c(drugs, lapply(unique(drug_data[,4]), 
                           function(x) drug_data[drug_data[,4]==x,,drop=FALSE]
  ))
  drug_cols <- cbind(vapply(FUN.VALUE=numeric(nrow(narrow_data)), drugs, 
                      function(x) Cpp_add_med_col(narrow_data, x)
                    ))
  
  return (drug_cols)
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




