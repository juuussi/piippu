rm(list=ls(all.names=TRUE))

#library(strict)
library(dplyr)
library(readr)
library(survival)

options(warn=2)

base_dir <- "~/projects/piippu/"
source(base_dir %>% paste0("validation.R"))
source(base_dir %>% paste0("transform.R"))
source(base_dir %>% paste0("configuration.R"))
source(base_dir %>% paste0("data_object.R"))
source(base_dir %>% paste0("common_funcs.R"))

# Set configuration
readConfigFile(base_dir %>% paste0("piippu_config"))
data <- lapply(getFiles(), 
               function(x) read_delim(x, delim=";"))

# TODO: bubblegum solution for current test data
data$subjects_file[c("dob","deathdate","cohort_entry")] <- do.call(cbind, lapply(data$subjects_file[c("dob","deathdate","cohort_entry")], to_unix_time))
data$events_file[c("date")] <- do.call(cbind, lapply(data$events_file[c("date")], to_unix_time))
data$drugs_file[c("start","stop")] <- do.call(cbind, lapply(data$drugs_file[c("start","stop")], to_unix_time))
#validations <- lapply(data, validate)

# if(!all(vapply(validations, is_valid, FUN.VALUE=logical(1))))
#   stop("Validation failed")

filter_ids <- data$blackbox_file %>% select("person_id", "type") %>% 
              dplyr::filter(.data$type %in% getOption("blackbox_filters")) %>% 
              pull("person_id")
length(unique(filter_ids))

subjects <- asDataObject(data$subjects_file[c("person_id", "dob", "deathdate")])

drugs <- asDataObject(data$drugs_file)
events <- filter_out_data(drugs, "person_id", 
                          drugs@data_matrix[!(drugs@data_matrix[,"person_id", drop=TRUE] %in% subjects@data_matrix[,"person_id", drop=TRUE]),"person_id", drop=TRUE])

static_covariates <- asDataObject(data$static_covariates_file[,c("person_id", "name", "value")])
static_covariates <- filter_out_data(static_covariates, "person_id", 
                          static_covariates@data_matrix[!(static_covariates@data_matrix[,"person_id", drop=TRUE] %in% subjects@data_matrix[,"person_id", drop=TRUE]),"person_id", drop=TRUE])

events <- asDataObject(data$events_file[,c("person_id", "date", "date", "event")])
events <- rename_column(events, "event", "value")
events <- rename_column(events, "date", "start")
events <- rename_column(events, "date.1", "stop")
events <- filter_out_data(events, "person_id", 
                          events@data_matrix[!(events@data_matrix[,"person_id", drop=TRUE] %in% subjects@data_matrix[,"person_id", drop=TRUE]),"person_id", drop=TRUE])

subjects <- filter_out_data(subjects, "person_id", filter_ids)
events <- filter_out_data(events, "person_id", filter_ids)
#drugs <- filter_out_data(drugs, "person_id", which(!(drugs@data_matrix[,"person_id", drop=TRUE] %in% subjects@data_matrix[,"person_id", drop=TRUE])))
drugs <- filter_out_data(drugs, "person_id", filter_ids)
static_covariates <- filter_out_data(static_covariates, "person_id", filter_ids)

# Remove any events or drug usage periods past analysis end date
drugs@data_matrix <- drugs@data_matrix[drugs@data_matrix[,"start"] <= getAnalysisEnd(),]
events@data_matrix <- events@data_matrix[events@data_matrix[,"start"] <= getAnalysisEnd(),]
# Truncate drug usages to analysis end date
drugs@data_matrix[drugs@data_matrix[,"stop"] > getAnalysisEnd(),] <- getAnalysisEnd()

# Should I remove people  in `events` but not `subjects`? 

long_data <- make_long(subjects, drugs, events, static_covariates)
long_data <- data.frame(long_data)
length(unique(long_data[,"person_id", drop=TRUE]))

model <- #lax( 
  coxph(as.formula(paste0("Surv(start, stop, ",getOption("outcome"),") ~ ", getOption("predictor"), " + cluster(person_id)")), long_data) 
#)
plot(survfit(model))
summary(model)
confint(model)
exp(confint(model))