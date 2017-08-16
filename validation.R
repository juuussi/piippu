# TODO:
# - Force selection of ID column and require uniqueness of values
# - Force selection of "categorical" or "ordinal" for factors
# - Check factors for low ratio of edit distance to length of level to find possible typos

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
  types <-  with(x, 
      lapply(seq_along(numerics), function(var) { 
        
          type <- which(c(numerics[var], factors[var], dates[var]))
          
          # If none of guessed classes, maybe just a column of strings. 
          if(length(type) == 0)
            return ("character")
          
          #Otherwise, just return the guessed type
          return (c("numeric", "factor", "date")[type])
          
        }
    ))
  names(types) <- names(x$unique_values)
   
  for(row in seq_along(x$unique_values)) {
    cat(paste0(padded_colnames[row], paste(types[[row]], collapse=" OR "), "\n"))# %>% invisible
  }
  
}


is_valid <- function(validation) { 
  
  # Any non-numeric values in mostly numeric columns?
  nonnum_uniqs <- vapply(FUN.VALUE=integer(1), seq_along(validation$unique_values), 
                  function(i) validation$numerics[i] && (length(na.omit(validation$unique_values[[i]])) > 0))
  
  nonnum_compl <- vapply(FUN.VALUE=character(1), seq_along(validation$unique_values),
                  function(i) {
                    if(nonnum_uniqs[i]) 
                      return(paste0("Numeric column ", i, " (", names(validation$unique_values)[i], ") contains also non-numeric values."))
                    else NA_character_ 
                  })
  
  # Any non-date values in mostly date-filled columns?
  nondate_uniqs <- vapply(FUN.VALUE=integer(1), seq_along(validation$unique_values), 
                  function(i) validation$dates[i] && (length(na.omit(validation$unique_values[[i]])) > 0))
  
  nondate_compl <- vapply(FUN.VALUE=character(1), seq_along(validation$unique_values),
                  function(i) {
                    if(nondate_uniqs[i]) 
                      return(paste0("Date column ", i, " (", names(validation$unique_values)[i], ") contains also non-date values."))
                    else NA_character_
                  })
  
  # Check factors for values that can be interpreted as numerics. 
  # TODO: If there are only numerics in a factor, make user choose either categorical or ordinal.
  numerics_in_factor <- vapply(FUN.VALUE=logical(1), seq_along(validation$unique_values),
                         function(i) {
                           return (validation$factors[i] && any(!is.na(suppressWarnings(as.numeric(na.omit(validation$unique_values[[i]]))))))
                         })
  
  mixed_compl <- character(0)
  if(any(with(validation, dates & numerics | dates & factors | numerics & factors))) {
    mixed_compl <- "There are columns with ambiguous types."
  }
  complaints <- na.omit(c(nonnum_compl, nondate_compl, mixed_compl))
  if(length(complaints) > 0) {
    for(compl in complaints) print(compl)
    return (FALSE)
  } else 
    return(TRUE)
  
}

  
validate <- function (data_df, date_type="default") {
  
  # Try to guess which columns were meant to be which type
  numerics <- guess_if_numeric(data_df)
  factors <- guess_if_factor(data_df)
  dates <- guess_if_date(data_df)
  
  # Find non-numeric unique values in each column (and all for factors)
  unique_values <- 1:ncol(data_df) %>% lapply(function(col) {
    
      if(factors[col]) {
         return (unlist(unique(data_df[,col])))
      }
      else  if(dates[col]) {
        nondate <- !is_date(data_df[[col]])
        data_df[nondate,col] %>% unique %>% unlist %>% return
      }
      else { #numeric
        nonnum <- suppressWarnings(as.numeric(data_df[[col]]))
        nonnum <- nonnum %>% is.na
        b <- nonnum
        data_df[b,col] %>% unique %>% unlist %>% return
      }
    }) 
                            
  for (u in seq_along(unique_values)) names(unique_values[[u]]) <- NULL
  names(unique_values) <- colnames(data_df)
  
  
  
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

is_date <- function(column) {
  
  b <- grepl("[[:space:]]*[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+[[:space:]]*", column) 
  b <- grepl("[[:space:]]*[[:digit:]]+/[[:digit:]]+/[[:digit:]]+[[:space:]]*", column) | b
  b <- grepl("[[:space:]]*[[:digit:]]+-[[:digit:]]+-[[:digit:]]+[[:space:]]*", column) | b
  return (b)
  
}
guess_if_date <- function(data_df, threshold=0.5) {
  
  freq_dates <- vapply(FUN.VALUE=numeric(1), seq_along(data_df), function(col) {
    b <- is_date(data_df[[col]])
    b <- sum(b) / length(b)
    return (b)
  })
  
  return (freq_dates > threshold)
}

guess_if_numeric <- function(data_df, threshold=0.5) {
  
  freq_numeric <- colSums(!is.na(suppressWarnings(vapply(data_df, as.numeric, FUN.VALUE=numeric(nrow(data_df)))))) / nrow(data_df)
  return (freq_numeric > threshold)
  
}
