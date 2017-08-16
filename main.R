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

config <- configuration(analysis_start = "19.1.2007", analysis_end = "1.1.2014")

# subjects <- read_csv(base_dir %>% paste0("subjects.csv"))
# validation_subjects <- validate(subjects)
# print(validation_subjects)
# print(is_valid(validation_subjects))

subjects <- read_csv(base_dir %>% paste0("subjects_fixed.csv"))
validation_subjects <- validate(subjects)
print(validation_subjects)

subjects[,validation_subjects$dates] <- to_unix_time(subjects[,validation_subjects$dates])

drugs <- read_delim(base_dir %>% paste0("drugs.csv"), delim=";")
validation_drugs <- validate(drugs)
print(validation_drugs)
drugs[,validation_drugs$dates] <- to_unix_time(drugs[,validation_drugs$dates])

events <- read_delim(base_dir %>% paste0("events.csv"), delim=";")
events$value[is.na(events$value)] <- "default" 
validation_events <- validate(events)
print(validation_events)

if( !is_valid(validation_subjects) || !is_valid(validation_drugs) || !is_valid(validation_events) )
  stop("Validation failed")

events[,validation_events$dates] <- to_unix_time(events[,validation_events$dates])

#timedep_data <- as.data_object(timedep_data)
events <- as.data_object(events)
drugs <- as.data_object(drugs)
subjects <- as.data_object(subjects)

narrow_data <- make_narrow(subjects, drugs, events, config$analysis_start, config$analysis_end)
narrow_data <- data.frame(narrow_data)

model <- lax( coxph(Surv(start, end, V5) ~ V6, narrow_data))
