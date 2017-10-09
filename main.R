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

drugs <- asDataObject(data$drugs_file)
subjects <- asDataObject(data$subjects_file[c("person_id", "dob", "deathdate")])

covariates <- asDataObject(data$covariates_file[,c("person_id", "name", "value")])

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
# Should I remove people  in `events` but not `subjects`? 

wide_data <- make_wide(subjects, drugs, events)
wide_data <- data.frame(wide_data)
length(unique(wide_data[,"person_id", drop=TRUE]))
#wide_data <- wide_data %>% dplyr::filter(!(.data$person_id %in% filter_ids))

model <- #lax( 
  coxph(as.formula(paste0("Surv(start, stop, ",getOption("outcome"),") ~ ", getOption("predictor"), " + cluster(person_id)")), wide_data) 
#)
plot(survfit(model))
