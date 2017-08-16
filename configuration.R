
#Default configuration
configuration <- function(analysis_start=integer(0), analysis_end=integer(0)) 
  structure(list(analysis_start = to_unix_time(analysis_start),
                 analysis_end   = to_unix_time(analysis_end)),
            class="configuration")

