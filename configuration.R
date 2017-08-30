
# TODO: Create hidden list of global variables?
configuration <- function(analysis_start, analysis_end, replace=FALSE) {
  if(exists(".config") && replace == FALSE) {
    stop("Configuration object already exists. To replace it, use the `replace=TRUE` argument.")
  } else {
    if(valid_arg(analysis_start, stop_on_false=TRUE) && valid_arg(analysis_end, stop_on_false=TRUE)) {
      .config <<- structure(list(analysis_start = to_unix_time(analysis_start),
                     analysis_end   = to_unix_time(analysis_end)),
                class="configuration")
    }
  }
}
