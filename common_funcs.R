library("lubridate")

# Check argument for zero length, NULL, NaN or NA value
valid_arg <- function(arg, expected_class=NULL, expected_length=NULL, stop_on_false=TRUE) {
  if(is.null(arg)) {
    if(stop_on_false) {
      stop("Argument is NULL.")
    }
    return (FALSE)
  }
  if(!is.null(expected_length) && length(arg) != expected_length) {
    if(stop_on_false) {
      stop(paste0("Argument is of length ", length(arg), " instead of expected length ", expected_length, "."))
    }
    return (FALSE)
  }
  if(!isS4(arg)) {
    if(any(is.na(arg))) {
      if(stop_on_false) {
        stop("Argument is or contains NA.")
      }
      return (FALSE)
    }
    # if(any(is.nan(arg))) {
    #   if(stop_on_false) {
    #     stop("Argument is or contains NaN.")
    #   }
    #   return (FALSE)
    # }
  }
  if(!is.null(expected_class) && class(arg) != expected_class) {
    if(stop_on_false) {
      stop(paste0("Argument is of class `", class(arg), "` instead of expected class `", expected_class, "`."))
    }
    return(FALSE)
  }
  
  return (TRUE)
}

replace_in <- function(matr, index, value) {
  matr[index] <- value
  return (matr)
}
na.as <- function(x, val) {
  if(is.matrix(x))
    is.na(x) <- val
  if(is.data.frame(x) || is.list(x)) {
    x[] <- lapply(x, function(column) {
      column[is.na(column)] <- val
      return (column)
    })
  }

  return (x)
}
drop_col <- function(dataframe, column) {
  if(is.character(column))
    return (dataframe[,base::setdiff(colnames(dataframe), column),drop=FALSE])
  if(is.integer(column))
    return (dataframe[,-column,drop=FALSE])
}

to_unix_time <- function(date_strings, tz="UTC") {
  if(is.vector(date_strings))
    return (as.integer(as.integer(parse_date_time(date_strings, tz=tz, orders=c("%m/%d/%Y","%d.%m.%Y"), exact=TRUE))/86400))
  if(is.data.frame(date_strings))
    return (vapply(date_strings, FUN.VALUE=integer(nrow(date_strings)), 
                   function(strs) return (as.integer(as.integer(parse_date_time(strs, tz=tz, orders=c("%m/%d/%Y","%d.%m.%Y"), exact=TRUE))/86400))))
}