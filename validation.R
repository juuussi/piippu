library(strict)
library(dplyr)
library(tibble)
library(stringr)
library(futile.logger)


validation <- function(unique_values = list(), factors=logical(), dates=logical(), numerics=logical()) 
  structure(list(unique_values = unique_values, factors = factors,dates=dates, numerics=numerics), class="validation")

print.validation <- function(x) {
  cat("\tUnique values:\n")
  
  
  padded_colnames <- vapply(FUN.VALUE= character(1), 
                            seq_along(x$unique_values),
                            function(row) str_pad(paste0(names(x$unique_values)[row], ":"), 24, side="right"))
                            
  for(row in seq_along(x$unique_values)) {
    cat(paste0(padded_colnames[row], paste(x$unique_values[[row]], collapse=", "), "\n"))# %>% invisible
  }
  
  
  cat("\n\tGuessed column types:\n")
  types <- guess_types(x)
  
  for(row in seq_along(x$unique_values)) {
    cat(paste0(padded_colnames[row], types[row], "\n"))# %>% invisible
  }
}

guess_types <- function(x) {
  col_types <-  with(x, 
    vapply(FUN.VALUE=character(1), seq_along(numerics), function(var) { 
      
        type <- which(c(numerics[var], factors[var], dates[var]))
        
        # If more than one class fits; cannot guess which it is, cannot continue
        if(length(type) > 1) 
          stop(paste0("Column ", names(unique_values)[var], " is of ambiguous type, guessed to be one of ", c("numeric", "factor", "date")[type]))
        
        # If none of guessed classes, maybe just strings. 
        if(length(type) == 0)
          return ("character")
        
        #Otherwise, just return the guessed type
        return (c("numeric", "factor", "date")[type])
        
      })
  )
  names(col_types) <- names(x$unique_values)
    
  return(col_types)
  
}


  
full_validate <- function (data_df, date_type="default") {
  # Check if column looks like dates, complain if ambiguous
  
  # Find non-numeric unique values in each column
  unique_values <- 1:ncol(data_df) %>% lapply(function(col) {
                                nonnum <- suppressWarnings(as.numeric(data_df[[col]]))
                                nonnum <- nonnum %>% is.na %>% which
                                data_df[nonnum,col] %>% unique %>% unlist %>% return
                              }) 
                            
  for (u in seq_along(unique_values)) names(unique_values[[u]]) <- NULL
  names(unique_values) <- colnames(data_df)
  
  # Try to guess which columns were meant to be which type
  numerics <- guess_if_numeric(data_df)
  factors <- guess_if_factor(data_df)
  dates <- guess_if_date(data_df)
  
  
  
  #names(factors) <- colnames(data_df)
  
  
  
  return (validation(unique_values, factors, dates, numerics))
}

guess_if_factor <- function(data_df, threshold=0.5){
  # Compute proportion of unique values of all values in column
  freq_uniques <- vapply(FUN.VALUE=numeric(1), 1:ncol(data_df), 
                         function(col) {
                           return ( length(unique(data_df[[col]])) / nrow(data_df) )
                         })
  return (freq_uniques < threshold)
}
guess_if_date <- function(data_df, threshold=0.5) {
  freq_dates <- vapply(FUN.VALUE=numeric(1), seq_along(data_df), function(col) {
    b <- grepl("[[:space:]]*[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+[[:space:]]*", data_df[[col]]) 
    b <- grepl("[[:space:]]*[[:digit:]]+/[[:digit:]]+/[[:digit:]]+[[:space:]]*", data_df[[col]]) | b
    b <- grepl("[[:space:]]*[[:digit:]]+-[[:digit:]]+-[[:digit:]]+[[:space:]]*", data_df[[col]]) | b
    b <- sum(b) / length(b)
    return (b)
  })
  return (freq_dates > threshold)
}

guess_if_numeric <- function(data_df, threshold=0.9) {
  
  freq_numeric <- colSums(!is.na(suppressWarnings(vapply(data_df, as.numeric, FUN.VALUE=numeric(nrow(data_df)))))) / nrow(data_df)
  return (freq_numeric > threshold)
}
