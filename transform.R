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

make_wide <- function(timeindep_data, drug_data, event_data, static_covariate_data, right_censoring_data, left_censoring_data) {
  
  valid_arg(timeindep_data, expected_class="DataObject", stop_on_false=TRUE)
  valid_arg(drug_data, expected_class="DataObject", stop_on_false=TRUE)
  valid_arg(event_data, expected_class="DataObject", stop_on_false=TRUE)
  valid_arg(static_covariate_data, expected_class="DataObject", stop_on_false=TRUE)
  #valid_arg(right_censoring_data, expected_class="data.frame", stop_on_false=TRUE)
  #valid_arg(left_censoring_data, expected_class="data.frame", stop_on_false=TRUE)
  
  if(any(right_censoring_data$person_id != left_censoring_data$person_id)) {
    stop("Censoring data must have the same id's (in the same order)")
  }
  
  
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
  
  right_censoring_vars <- as.list(right_censoring_data %>% drop_col("person_id")) %>% na.as(.Machine$integer.max)
  left_censoring_vars  <- as.list(left_censoring_data %>% drop_col("person_id")) %>% na.as(-.Machine$integer.max)
  mins <- do.call(pmin, right_censoring_vars)
  maxs <- do.call(pmax, left_censoring_vars)
  censoring_data <- matrix(NA, nrow=nrow(right_censoring_data), ncol=3)
  censoring_data[,1] <- right_censoring_data[,"person_id"]
  censoring_data[,2] <- maxs
  censoring_data[,3] <- mins
  
  timedep_data <- rbind(event_data@data_matrix[,c("person_id", "start", "stop")], 
                        drug_data@data_matrix[,c("person_id", "start", "stop")],
                        censoring_data)
  
  num_uniq_events <- length(unique(event_data$value))
  num_uniq_drugs <- length(unique(drug_data$value))
  
  wide_data <- make_all_intervals(timedep_data, idcolnum = 1)
  
  # extra_lines <- cbind(person_id = extra_people, 
  #                      start = maxs, 
  #                      end = mins)
  # wide_data <- rbind(wide_data, extra_lines)
  
  # Add event indicators
  event_inds <- add_event_indicators(wide_data, event_data@data_matrix[,c(1,3,4)], idcolnum = 1)
  wide_data <- cbind(wide_data, event_inds)
  colnames(wide_data)[3 + 1:num_uniq_events] <- getLevels(event_data)$value
  
  # Add drug columns
  drug_cols <- make_drug_columns(wide_data, drug_data@data_matrix)
  wide_data <- data.frame(cbind(wide_data, drug_cols), row.names=NULL)
  colnames(wide_data)[(4+num_uniq_events):(4+num_uniq_events+num_uniq_drugs-1)] <- levels(drug_data$value)
  
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
  static_covs_wide <- rows[match(wide_data$person_id, rows[,"person_id",drop=TRUE]),base::setdiff(colnames(rows), "person_id")]
  #static_covs_wide[,] <- getLevels(static_covariate_data)$value[static_covs_wide]
  if(!is.null(getLevels(static_covariate_data)$value))
    static_covs_wide <- do.call(cbind, lapply(1:NCOL(static_covs_wide), function(col) {
      l <- getLevels(static_covariate_data)$value
      if(!is.null(l)) {
        return (l[static_covs_wide[,col,drop=TRUE] ])
      }
    }))
  #static_covs_wide <- getLevels(static_covariate_data)
  rownames(static_covs_wide) <- NULL
  colnames(static_covs_wide) <- getLevels(static_covariate_data)$name
  
  wide_data <- cbind(wide_data, static_covs_wide)
  rownames(wide_data) <- NULL
  
  return (wide_data)
}

# Function for putting data into "within-analysis" format, i.e. resetting time on event; to be run on wide-format data.
# The reset type-option uses nomenclature from "Regression modeling of time to event data, second edition" by Hosmer, Lemeshow and May,
# as used on the page https://stats.idre.ucla.edu/sas/faq/how-can-i-model-repeated-events-survival-analysis-in-proc-phreg/ .
reset_on_event <- function(data, event, event_occurrence_marker=1, reset_type="Conditional model A") {
  
  cols <- c("person_id", "start","stop",event)
  if(! all(cols %in% colnames(data))) 
    stop("Data must have columns `person_id`, `start`, `stop` and a column named by the event parameter.")
  
  if(! reset_type %in% c("Conditional model A", "Conditional model B") ) 
    stop("Reset type must be `Conditional model A` or `Conditional model B`.")
  
  # Add stratum column
  data <- cbind(data, stratum=rep(NA, NROW(data)))
  
  # Reorder
  data <- data[,c(cols, "stratum", base::setdiff(colnames(data), c(cols,"stratum"))), drop=FALSE]
  
  id_data <- split(data, data$person_id)
  reset <- lapply(id_data, function(d) {
    
    d <- d[order(d$start), ,drop=FALSE]
    
    # Reset first interval to start from 0
    d[,2:3] <- d[,2:3,drop=FALSE] - min(d[,2,drop=TRUE])
    
    # Stratum is set to correspond to the "ordinal" of the event, e.g. first, second ...
    event.num <- 1
    
    for(r in 1:(max(1,NROW(d)-1))) {
      
      # Event occurence -> reset next interval to start from 0
      elem <- d[r,4,drop=TRUE]
      
      d[,5] <- event.num
      if(elem == event_occurrence_marker) {
        
        if(reset_type == "Conditional model B") {
          # In these two models, time is reset to 0 after each event. 
          d[(r+1):NROW(d), 2:3] <- d[(r+1):NROW(d), 2:3] - d[r, 3, drop=TRUE]
        }
        event.num <- event.num + 1
        
      }
      
    }
    return (d)
  })
  data <- do.call(rbind, reset)
  return (data)
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
  result <- Cpp_add_missing_intervals(data[,1:3])
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
add_event_indicators <- function(time_matrix, event_data, idcolnum, permanent=rep(FALSE, times=length(unique(event_data[,3])))) {
  
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




