rm(list=ls(all.names=TRUE))

library(strict)
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


events <- as.data_object(data$events_file)
drugs <- as.data_object(data$drugs_file)
subjects <- as.data_object(data$subjects_file[c("person_id", "dob", "deathdate")])

wide_data <- make_wide(subjects, drugs, events)
wide_data <- data.frame(wide_data)

model <- lax( coxph(Surv(start, end, Hipfracture) ~ A + cluster(person_id), wide_data) )
plot(survfit(model))
