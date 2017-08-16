library(strict)
library(dplyr)
library(readr)

options(warn=2)

base_dir <- "~/projects/piippu/"
source(base_dir %>% paste0("validation.R"))
source(base_dir %>% paste0("transform.R"))
source(base_dir %>% paste0("configuration.R"))

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

colnames(drugs)[colnames(drugs) == "dose"] <- "value"
colnames(drugs)[colnames(drugs) == "class"] <- "name"
drugs <- cbind(drugs, type=rep("drug"))
events <- cbind(events, type=rep("event"))
timedep_data <- rbind(drugs,events)
timedep_data$name <- as.factor(timedep_data$name)
namelevels <- unique(data.frame(levels = as.character(timedep_data$name), values=unclass(timedep_data$name), stringsAsFactors=FALSE))
timedep_data$type <- as.factor(timedep_data$type)
typelevels <- unique(data.frame(levels = as.character(timedep_data$type), values=unclass(timedep_data$type), stringsAsFactors=FALSE))
timedep_data$value <- as.factor(timedep_data$value)
valuelevels <- unique(data.frame(levels = as.character(timedep_data$value), values=unclass(timedep_data$value), stringsAsFactors=FALSE))
timedep_data$name <- unclass(timedep_data$name)
timedep_data$type <- unclass(timedep_data$type)
timedep_data$value <- unclass(timedep_data$value)
timedep_data <- data.matrix(timedep_data)

narrow_data <- make_all_intervals(timedep_data, idcolnum = which(colnames(timedep_data) == "person_id"), config=config)
# Add people who have no events or drug usages to data
extra_lines <- cbind(person_id = base::setdiff(subjects$person_id, narrow_data[,1]), start = config$analysis_start, end = config$analysis_end)
narrow_data <- rbind(narrow_data, extra_lines)
