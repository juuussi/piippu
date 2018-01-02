rm(list=ls(all.names=TRUE))

#library(strict)
library(dplyr)
library(readr)
library(survival)
library(futile.logger)
library(tidyr)
library(tables)

options(warn=1)
piippu.TESTING <- TRUE

base_dir <- "~/projects/piippu/"
#source(base_dir %>% paste0("validation.R"))
source(base_dir %>% paste0("common_funcs.R"))
source(base_dir %>% paste0("data_object.R"))
source(base_dir %>% paste0("configuration.R"))
source(base_dir %>% paste0("descr_stats.R"))
source(base_dir %>% paste0("transform.R"))

flog.info("Reading configuration file.")

# Set configuration
readConfigFile(base_dir %>% paste0("piippu_config"))

if(!getConfOption("analysis_type") %in% c("First event", "Resetting on event", "Counting process")) {
  stop("Option `analysis_type` must be one of `First event`, `Resetting on event`, `Counting process`.")
}
if(getConfOption("analysis_type") == "Resetting on event" && !getConfOption("reset_type") %in% c("First event", "Resetting on event", "Counting process")) {
  stop("Option `reset_type` must be either `Conditional model A` or `Conditional model B`.")
}

# Set logging file
flog.appender(appender.file(getConfOption("log_file")))
flog.threshold(TRACE)

flog.info("Starting piippu run.")

# Read files
data <- lapply(getFiles(), 
               function(x) read_delim(x, delim=";", col_names=TRUE, na=c("NA","", " ")))

flog.info("Loading files:")
invisible(lapply(getFiles(), flog.info))
flog.info("Done.")

# Transform units to day-precision Unix time (days after 1.1.1970)
flog.info("Transforming dates to Unix time: ")
data$subjects_file[,c("dob", "deathdate", "cohort_entry")] <- do.call(cbind, lapply(data$subjects_file[,c("dob", "deathdate", "cohort_entry"),drop=FALSE], to_unix_time))
data$events_file[,c("date")] <- do.call(cbind, lapply(data$events_file[,c("date"),drop=FALSE], to_unix_time))
data$drugs_file[,c("start","stop")] <- do.call(cbind, lapply(data$drugs_file[,c("start","stop")], to_unix_time))
data$blackbox_file[,c("start")] <- data$blackbox_file[,"start"] %>% to_unix_time 
flog.info("Done.")

#validations <- lapply(data, validate)

# if(!all(vapply(validations, is_valid, FUN.VALUE=logical(1))))
#   stop("Validation failed")

outcome <- getConfOption("outcome")
predictor <- unlist(str_split(getConfOption("predictor"), ":"))
covars <- str_split(getConfOption("adjust_for"), ":")

# Encode data for transformation
subjects <- asDataObject(data$subjects_file)
static_covariates <- asDataObject( data$static_covariates_file[,c("person_id", "name", "value")] )
drugs    <- asDataObject(data$drugs_file)
blackbox <- asDataObject(data$blackbox_file)

events <- asDataObject(data$events_file[,c("person_id", "date", "date", "event")])
events <- rename_column(events, "event", "value")
events <- rename_column(events, "date", "start")
events <- rename_column(events, "date.1", "stop")

if(!"dose" %in% colnames(drugs)) {
  drugs <- add_column(drugs, "dose", rep(1L, nrow(data$drugs_file)))
}

remove_unrelated <- function(x, y, column, x_name, y_name) {
  
  x_in_y <- getColumn(x,column) %in% getColumn(y,column)
  if(!all(x_in_y)) {
    num_orig_rows <- nrow(x)
    x <- x[x_in_y, ]
    flog.warn(paste0("Data in ", x_name, " includes persons not listed in ", y_name))
    flog.warn(paste0("Removed ", (num_orig_rows - nrow(x)), "/", num_orig_rows, " rows."))
  }
  return (x)
}

drugs             <- remove_unrelated(drugs, subjects, "person_id", "drugs", "subjects")
static_covariates <- remove_unrelated(static_covariates, subjects, "person_id", "static_covariates", "subjects")
events            <- remove_unrelated(events, subjects, "person_id", "events", "subjects")
blackbox          <- remove_unrelated(blackbox, subjects, "person_id", "blackbox", "subjects")

flog.info("Filtering out blackboxed persons:")
blackboxfilters <- getConfOption("blackbox_filters")
if(length(blackboxfilters) == 1 && blackboxfilters == "ALL") 
  blackboxfilters <- unique(blackbox_markers$type)

blackbox_markers <- blackbox[is.na(blackbox$start), , drop=FALSE] 
blackbox_markers <- drop_col(blackbox_markers, "start")
blackbox_markers <- blackbox_markers[blackbox_markers$type %in% blackboxfilters, ,drop=FALSE]
flog.info(paste0("Blackbox types: ", blackboxfilters))

{
  subjnum           <- length(unique(subjects$person_id))
  subjects          <- subjects[!subjects$person_id %in% blackbox_markers$person_id, ]
  events            <- events[!events$person_id %in% blackbox_markers$person_id, ]
  drugs             <- drugs[!drugs$person_id %in% blackbox_markers$person_id, ]
  static_covariates <- static_covariates[!static_covariates$person_id %in% blackbox_markers$person_id, ]
  
  flog.info(paste0("Done. Removed ", (subjnum - length(unique(subjects$person_id))), "/", subjnum, " ids."))
}

flog.info("Censoring")

produce_censor_data <- function(vars) {
  bboxdata <- NULL
  # These censoring types are from the blackbox file
  bbvar <- base::intersect(unique(blackbox$type), vars)
  
  # These censoring types are from the subjects file
  sbvar <- base::intersect(colnames(subjects), vars)
  person_id <- unique(c(subjects$person_id, blackbox$person_id))
  basedata <- data.frame(person_id,
                         matrix(NA_integer_, nrow=length(person_id), ncol=length(sbvar)+length(bbvar))
              )
  
  if(length(bbvar) > 0) {
    bboxdata <- as.data.frame(blackbox[!is.na(blackbox$start) & blackbox$type %in% bbvar, , drop=FALSE])
    bboxdata <- spread_(bboxdata, key_col="type", value_col="start")
    ind <- match(bboxdata$person_id, basedata$person_id)
    basedata[na.omit(ind), (1:length(bbvar))+1] <- drop_col(as.data.frame(bboxdata[!is.na(ind),]), "person_id")
  }
  if(length(sbvar) > 0) {
    subjdata <- as.data.frame(subjects[,c("person_id", sbvar)])
    ind <- match(subjdata$person_id, basedata$person_id)
    basedata[na.omit(ind), (length(bbvar)+2):ncol(basedata)] <- drop_col(as.data.frame(subjdata[!is.na(ind),]), "person_id")
  } 
  colnames(basedata) <- c("person_id", base::setdiff(c(colnames(bboxdata), colnames(subjdata)), "person_id"))
  return (basedata)
}

right_censor_data <- produce_censor_data(getConfOption("censor_right"))
left_censor_data  <- produce_censor_data(getConfOption("censor_left"))

right_censor_data <- cbind(right_censor_data, getAnalysisEnd())
left_censor_data <- cbind(left_censor_data, getAnalysisStart())

# Filter out people not in `subjects`; this could be done at an earlier stage, TODO
right_censor_data <- right_censor_data[right_censor_data$person_id %in% subjects$person_id, ]
left_censor_data <- left_censor_data[left_censor_data$person_id %in% subjects$person_id, ]

# for(var in c(getConfOption("censor_right"), getConfOption("censor_left"))) {
#   subjects <- drop_col(subjects, var)
#   blackbox <- drop_col(blackbox, var)
# }

# # Add id's that are in subjects but not blackboxed
# subjects_without_censoring <- base::setdiff(subjects$person_id, c(right_censor_data$person_id, left_censor_data$person_id))
# if(length(subjects_without_censoring) > 0) {
#   
#   extra_rows <- matrix(NA, ncol=NCOL(censoring_data), nrow=length(subjects_without_censoring))
#   colnames(extra_rows) <- colnames(censoring_data)
#   extra_rows[,"person_id"] <- subjects_without_censoring
#   censoring_data <- rbind(censoring_data, extra_rows)
#   
# }

#censoring_data <- cbind(censoring_data, analysis_stop=getAnalysisEnd(), deathdate=subjects$deathdate)

censor <- function(timeline_data, right_censoring_data, left_censoring_data) {
  
  if(!all(c("person_id", "start", "stop") %in% colnames(timeline_data))) 
    stop("timeline_data must have columns `person_id`, `start`, `stop`.")
  
  right_censoring_vars <- as.list(right_censoring_data %>% drop_col("person_id")) %>% na.as(.Machine$integer.max) # If NA, put integer max value
  left_censoring_vars  <- as.list(left_censoring_data %>% drop_col("person_id")) %>% na.as(-.Machine$integer.max) # If NA, put integer min value
  mins <- do.call(pmin, right_censoring_vars)
  maxs <- do.call(pmax, left_censoring_vars)
  
  right_ind <- match(timeline_data$person_id, unique(right_censoring_data$person_id))
  left_ind  <- match(timeline_data$person_id, unique( left_censoring_data$person_id))
  right_censor_date <- mins[right_ind]
  left_censor_date  <- maxs[ left_ind]
  
  # Get rid of everything that is before left censor or after right censor
  right_ind <- timeline_data$start <= right_censor_date
  left_ind <- timeline_data$stop >= left_censor_date
  flog.info(paste0("Removed ", sum(!(right_ind & left_ind)), "/", length(right_ind), " time intervals"))# including ", sum(timeline_data[!(right_ind & left_ind),outcome] == 1), " ", outcome, " events."))
  timeline_data <- timeline_data[right_ind & left_ind, ]
  left_censor_date <- left_censor_date[right_ind & left_ind]
  right_censor_date <- right_censor_date[right_ind & left_ind]
  
  # Truncate stops to right censoring date
  right_censored_stop <- pmin(timeline_data$stop, right_censor_date)
  flog.info(paste0("Truncated ", sum(right_censored_stop < timeline_data$stop), "/", length(right_censored_stop), " intervals."))
  timeline_data$stop <- right_censored_stop
  
  # Truncate starts to left censoring date
  left_censored_start <- pmax(timeline_data$start, left_censor_date)
  flog.info(paste0("Truncated ", sum(left_censored_start > timeline_data$start), "/", length(left_censored_start), " intervals."))
  timeline_data$start <- left_censored_start
  
  return (timeline_data)
}

{
  flog.info(paste0("Censoring events:"))
  orignumevents <- sum(events[,"value",drop=TRUE] == outcome)
  events <- censor(events, right_censor_data, left_censor_data)
  flog.info(paste0("Removed ", orignumevents - sum(events[,"value",drop=TRUE] == outcome), "/", orignumevents, " events."))
  
  #orignumdrugs <- nrow(drugs)
  flog.info(paste0("Censoring drugs:"))
  drugs <- censor(drugs, right_censor_data, left_censor_data)
  #flog.info(paste0("Removed ", orignumdrugs - nrow(drugs), "/", orignumdrugs, " drugs."))
}

flog.info("Converting to counting process format:")
wide_data <- make_wide(subjects, drugs, events, static_covariates, right_censor_data, left_censor_data)
wide_data <- data.frame(wide_data)
flog.info("Done.")

# Add static cov. columns from subject data
wide_data <- cbind(wide_data, 
      drop_col(as.data.frame(subjects)[match(wide_data$person_id, subjects$person_id),], "person_id")
)

count_person_years <- function(data) {
  intvals <- data$stop - data$start
  return (sum(intvals) / 365)
}

flog.info(paste0("Number of unique subject ids in data: ", length(unique(wide_data[,"person_id", drop=TRUE]))))
flog.info(paste0("Total person-years: ", count_person_years(wide_data)))
flog.info(paste0("Person-years with ", predictor[1], ": ", count_person_years(wide_data[wide_data[[predictor[1] ]]==1,])))
flog.info(paste0("Person-years without ", predictor[1], ": ", count_person_years(wide_data[wide_data[[predictor[1] ]]!=1,])))

write_stats <- function(data) {
  
  fname <- paste0(getConfOption("output_folder"), "/", c("stats.txt"))
  lns <- c(paste0("Total events: \t", sum(data[[outcome]] == 1)),
           paste0("Total subjects\t:", length(unique(data$person_id))),
           paste0("Total person-years:\t",count_person_years(data)),
           paste0("Person-years with ", predictor[1], ": ", count_person_years(data[data[[predictor[1] ]]==1,])),
           paste0("Person-years without ", predictor[1], ": ", count_person_years(data[data[[predictor[1] ]]!=0,])))
  writeLines(lns, fname, "\r\n")
  
}

# tabl <- build_table(outcome, predictor[1], wide_data)
# print_table(tabl)

write_stats(wide_data)
  
flog.info(paste0("Analysis type is selected as ", 
                 c("`first event only`", "`normal`", paste0("resetting with ", getConfOption("reset_type")))[as.integer(getConfOption("analysis_type"))]))
flog.info(paste0("Starting analysis: "))

summary_frame <- function(model) {
  s <- summary(model)
  d <- data.frame(s$coefficients, confint(model), exp(confint(model)))
  colnames(d) <- c("Coef", "Exp. coef", "coef SE", "Robust SE", "z-value", "p-value", "Haz. 2.5%", "Haz. 97.5%", "Exp. 2.5%", "Exp. 97.5%")
  return(d)
}

write_results <- function(model, prefix) {
  
  fname <- paste0(getConfOption("output_folder"), "/", prefix, c("_summary.csv"))
  write.table(summary_frame(model), fname, sep=";", row.names=TRUE, col.names=NA)
  
}
# Fix column types to match config
for(C in covars) {
  # Make sure categorical variables are encoded as factors, even if represented as numbers
  if(C[2] == "categorical") {
    wide_data[,C[1]] <- as.factor(wide_data[,C[1]])
  }
}
covars <- vapply(FUN.VALUE=character(1), covars, function(x) x[1])

# Actual analysis
                  
if(getConfOption("analysis_type") == "First event") {
  
  # Censor after first event
  split_outcomes <- split(wide_data, wide_data$person_id)
  split_outcomes <- lapply(split_outcomes, function(x) { 
    stops <- x$stop
    e <- x[[outcome]] == 1
    # If no events, censor nothing 
    if(all(!e)) return (x)
    
    # Otherwise get rid of all time intervals past first event
    subseq_events <- stops > min(stops[e])
    x <- x[!subseq_events,,drop=FALSE]# <- 0
    return (x)
  })
  
  wide_data <- do.call(rbind, split_outcomes)
  
  # Unadjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",outcome,") ~ ", predictor[1], " + cluster(person_id)")), wide_data) 
  #)
  
  write_results(model, "unadjusted")
  # Adjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",outcome,") ~ ", predictor[1], " + cluster(person_id) +", paste0(covars,collapse="+"))), wide_data) 
  #)

  write_results(model, "adjusted")
  
} else if (getConfOption("analysis_type") == "Counting process") {
  # Multiple events allowed
  
  # Unadjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",outcome,") ~ ", predictor[1], " + cluster(person_id)")), wide_data) 
  #)
  write_results(model, "unadjusted")
  # Adjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ", outcome,") ~ ", predictor[1], " + cluster(person_id) +", paste0(covars, collapse="+"))), wide_data) 
  #)
  write_results(model, "adjusted")
  
} else if (getConfOption("analysis_type") == "Resetting on event") {
  
  # Resetting at event
  resetted_data <- reset_on_event(wide_data, event=outcome, event_occurrence_marker=1, reset_type=getConfOption("reset_type"))
  
  # Unadjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",outcome,") ~ ", predictor[1], " + cluster(person_id) + cluster(stratum)")), resetted_data) 
  #)
  write_results(model, "unadjusted")
  # Adjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ", outcome,") ~ ", predictor[1], " + cluster(person_id) + cluster(stratum) + ", paste0(covars,collapse="+"))), resetted_data) 
  #)
  write_results(model, "adjusted")
}
flog.info("Done.")

