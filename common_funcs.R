
# Check argument for zero length, NULL, NaN or NA value
valid_arg <- function(arg, expected_class=NULL, expected_length=1, stop_on_false=TRUE) {
  if(length(arg) != expected_length) {
    if(stop_on_false) {
      stop(paste0("Argument is of length ", length(arg), " instead of expected length ", expected_length, "."))
    }
    return (FALSE)
  }
  if(is.na(arg)) {
    if(stop_on_false) {
      stop("Argument is NA.")
    }
    return (FALSE)
  }
  if(is.null(arg)) {
    if(stop_on_false) {
      stop("Argument is NULL.")
    }
    return (FALSE)
  }
  if(is.nan(arg)) {
    if(stop_on_false) {
      stop("Argument is NULL.")
    }
    return (FALSE)
  }
  if(!is.null(expected_class) && class(arg) != expected_class) {
    if(stop_on_false) {
      stop(paste0("Argument is of class `", class(arg), "` instead of expected class `", expected_class, "`."))
    }
    return(FALSE)
  }
  
  return (TRUE)
}