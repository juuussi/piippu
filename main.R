rm(list=ls(all.names=TRUE))

#library(strict)
library(dplyr)
#library(readr)
library(survival)
#library(tables)
library(futile.logger)
library(tidyr)

options(warn=1)
piippu.TESTING <- TRUE

base_dir <- "~/projects/piippu/"
source(base_dir %>% paste0("validation.R"))
source(base_dir %>% paste0("common_funcs.R"))
source(base_dir %>% paste0("data_object.R"))
source(base_dir %>% paste0("configuration.R"))
source(base_dir %>% paste0("descr_stats.R"))
source(base_dir %>% paste0("transform.R"))

flog.info("Reading configuration file.")

# Set configuration
readConfigFile(base_dir %>% paste0("piippu_config"))

# Set logging file
flog.appender(appender.file(getConfOption("log_file")))
flog.threshold(TRACE)

flog.info("Starting piippu run.")

# Read files
data <- lapply(getFiles(), 
               function(x) read.table(x, sep=";",stringsAsFactors=FALSE, header=TRUE,
                                      na.strings=c("NA","", " ")))

flog.info("Loading files:")
invisible(lapply(getFiles(), flog.info))
flog.info("Done.")

# Transform units to day-precision Unix time (days after 1.1.1970)
flog.info("Transforming dates to Unix time: ")
data$events_file[c("date")] <- do.call(cbind, lapply(data$events_file[,c("date"),drop=FALSE], to_unix_time))
data$drugs_file[,c("start","stop")] <- do.call(cbind, lapply(data$drugs_file[,c("start","stop")], to_unix_time))
data$blackbox_file[,c("start")] <- data$blackbox_file[,"start",drop=TRUE] %>% to_unix_time 
flog.info("Done.")

#validations <- lapply(data, validate)

# if(!all(vapply(validations, is_valid, FUN.VALUE=logical(1))))
#   stop("Validation failed")

outcome <- getConfOption("outcome")
predictor <- getConfOption("predictor")


subjects <- asDataObject(data$subjects_file[c("person_id","sex", "dob", "deathdate")])
static_covariates <- asDataObject(data$static_covariates_file[,c("person_id", "name", "value")])
drugs    <- asDataObject(data$drugs_file)
events <- asDataObject(data$events_file[,c("person_id", "date", "date", "event")])
events <- rename_column(events, "event", "value")
events <- rename_column(events, "date", "start")
events <- rename_column(events, "date.1", "stop")

if(!"dose" %in% colnames(drugs))
  drugs <- add_column(drugs, "dose", rep(1, nrow(data$drugs_file)))

# Remove drug uses that are not linked to subjects
drug_users_in_subjects <- drugs$person_id %in% subjects$person_id
if(!all(drug_users_in_subjects)) {
  orig_numrows <- nrow(drugs)
  drugs <- drugs[drug_users_in_subjects, ]
  flog.warn("Drug data includes persons not listed in subject data.")
  flog.warn(paste0("Removed ", (orig_numrows - nrow(drugs)), "/", orig_numrows, " rows."))
}


# Remove static covariates that are not linked to subjects
statcovar_ids_in_subjects <- static_covariates$person_id %in% subjects$person_id
if(!all(statcovar_ids_in_subjects)) {
  orig_numrows <- nrow(static_covariates)
  static_covariates <- static_covariates[statcovar_ids_in_subjects, ]
  flog.warn("Static covariate data includes persons not listed in subject data.")
  flog.warn(paste0("Removed ", (orig_numrows - nrow(static_covariates)), "/", orig_numrows, " rows."))
}

# Remove events not linked to subjects
events_in_subjects <- events$person_id %in% subjects$person_id
if(!all(events_in_subjects)) {
  orig_numrows <- nrow(events)
  events <- events[events_in_subjects, ]
  flog.warn("Event data includes persons not listed in subject data.")
  flog.warn(paste0("Removed ", (orig_numrows - nrow(events)), "/", orig_numrows, " rows."))
}

blackbox <- data$blackbox_file
# Remove blackboxes not linked to subjects
blackbox_in_subjects <- blackbox$person_id %in% subjects$person_id
if(!all(blackbox_in_subjects)) {
  orig_numrows <- nrow(blackbox)
  blackbox <- blackbox[blackbox_in_subjects, ,drop=FALSE]
  flog.warn("Event data includes persons not listed in subject data.")
  flog.warn(paste0("Removed ", (orig_numrows - nrow(blackbox)), "/", orig_numrows, " rows."))
}
# Filter out blackboxed people

flog.info("Filtering out blackboxed persons:")

blackbox_markers <- blackbox[is.na(blackbox$start), , drop=FALSE] 
blackbox_markers <- drop_col(blackbox_markers, "start")
blackbox_markers <- blackbox_markers[blackbox_markers$type %in% getConfOption("blackbox_filters"), ,drop=FALSE]
flog.info(paste0("Blackbox types: ", unique(blackbox_markers$type)))

{
  subjnum           <- length(unique(subjects$person_id))
  subjects          <- subjects[!subjects$person_id %in% blackbox_markers$person_id, ]
  events            <- events[!events$person_id %in% blackbox_markers$person_id, ]
  drugs             <- drugs[!drugs$person_id %in% blackbox_markers$person_id, ]
  static_covariates <- static_covariates[!static_covariates$person_id %in% blackbox_markers$person_id, ]
  
  flog.info(paste0("Done. Removed ", (subjnum - length(unique(subjects$person_id))), "/", subjnum, " ids."))
}

flog.info("Censoring")
censor_dates <- blackbox[!is.na(blackbox$start), , drop=FALSE]

censoring_data <- spread_(censor_dates, key_col="type", value_col="start")
# Add id's that are in subjects but not blackboxed
subjects_without_censoring <- base::setdiff(subjects$person_id, censoring_data$person_id)
if(length(subjects_without_censoring) > 0) {
  extra_rows <- matrix(NA, ncol=NCOL(censoring_data), nrow=length(subjects_without_censoring))
  colnames(extra_rows) <- colnames(censoring_data)
  extra_rows[,"person_id"] <- subjects_without_censoring
  censoring_data <- rbind(censoring_data, extra_rows)
}

censoring_data <- cbind(censoring_data, analysis_stop=getAnalysisEnd())

censor <- function(timeline_data, censoring_data) {
  if(!all(c("person_id", "start", "stop") %in% colnames(timeline_data))) 
    stop("timeline_data must have columns `person_id`, `start`, `stop`.")
  
  censoring_vars <- as.list(censoring_data %>% drop_col("person_id")) %>% na.as(Inf)
  mins <- do.call(pmin, censoring_vars)
  
  ind <- match(timeline_data$person_id, unique(censoring_data$person_id))
  censor_date <- mins[ind]
  # Get rid of everything that starts after censoring
  ind <- timeline_data$start > censor_date
  flog.info(paste0("Removed ", sum(ind), "/", length(ind), " time intervals including ", sum(timeline_data[ind,outcome] == 1), " ", outcome, " events."))
  timeline_data <- timeline_data[!ind, ]
  censor_date <- censor_date[!ind]
  # Truncate stops to censoring date
  censored_stop <- pmin(timeline_data$stop, censor_date)
  flog.info(paste0("Truncated ", sum(censored_stop == timeline_data$stop), "/", length(censored_stop), " intervals."))
  timeline_data$stop <- censored_stop
  
  return (timeline_data)
  
}

{
  orignumevents <- nrow(events)
  events <- censor(events, censoring_data)
  flog.info(paste0("Removed ", orignumevents - nrow(events), "/", orignumevents, " events."))
  
  orignumdrugs <- nrow(drugs)
  drugs <- censor(drugs, censoring_data)
  flog.info(paste0("Removed ", orignumdrugs - nrow(drugs), "/", orignumdrugs, " drugs."))
}

#flog.info(
wide_data <- make_wide(subjects, drugs, events, static_covariates)
wide_data <- data.frame(wide_data)

if(getConfOption("analysis_type") == 1) {
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
    coxph(as.formula(paste0("Surv(start, stop, ",getConfOption("outcome"),") ~ ", getConfOption("predictor"), " + cluster(person_id)")), wide_data) 
  #)
  summary(model)
  confint(model)
  exp(confint(model))
  
  # Adjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",getConfOption("outcome"),") ~ ", getConfOption("predictor"), " + cluster(person_id) +", paste(getConfOption("adjust_for"),collapse="+"))), wide_data) 
  #)
  summary(model)
  confint(model)
  exp(confint(model))
} else if (getConfOption("analysis_type") == 2) {
  # Multiple events allowed
  
  # Unadjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",getConfOption("outcome"),") ~ ", getConfOption("predictor"), " + cluster(person_id)")), wide_data) 
  #)
  summary(model)
  confint(model)
  exp(confint(model))
  
  # Adjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",getConfOption("outcome"),") ~ ", getConfOption("predictor"), " + cluster(person_id) +", paste(getConfOption("adjust_for"),collapse="+"))), wide_data) 
  #)
  summary(model)
  confint(model)
  exp(confint(model))
  
} else if (getConfOption("analysis_type") == 3) {
  
  # Resetting at event
  resetted_data <- reset_on_event(wide_data, event=outcome, event_occurrence_marker=1, reset_type=getConfOption("reset_type"))
  
  # Unadjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",getConfOption("outcome"),") ~ ", getConfOption("predictor"), " + cluster(person_id) + cluster(stratum)")), resetted_data) 
  #)
  summary(model)
  confint(model)
  exp(confint(model))
  
  # Adjusted model
  model <- #lax( 
    coxph(as.formula(paste0("Surv(start, stop, ",getConfOption("outcome"),") ~ ", getConfOption("predictor"), " + cluster(person_id) + cluster(stratum) + ", paste(getConfOption("adjust_for"),collapse="+"))), resetted_data) 
  #)
  summary(model)
  confint(model)
  exp(confint(model))
  
}


count_person_years <- function(data) {
  intvals <- data$stop - data$start
  return (sum(intvals) / 365)
}

count_person_years(wide_data)
count_person_years(wide_data[wide_data$ANYSTROKE==0,])
count_person_years(wide_data[wide_data$ANYSTROKE==1,])

length(unique(wide_data[,"person_id", drop=TRUE]))

#tabl <- build_table(outcome, predictor, wide_data)
#print_table(tabl)

