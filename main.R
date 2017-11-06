rm(list=ls(all.names=TRUE))

library(strict)
library(dplyr)
library(readr)
library(survival)
library(tables)
library(futile.logger)

options(warn=1)

base_dir <- "~/projects/piippu/"
source(base_dir %>% paste0("validation.R"))
source(base_dir %>% paste0("transform.R"))
source(base_dir %>% paste0("configuration.R"))
source(base_dir %>% paste0("data_object.R"))
source(base_dir %>% paste0("common_funcs.R"))
source(base_dir %>% paste0("descr_stats.R"))

# Set configuration
readConfigFile(base_dir %>% paste0("piippu_config"))
data <- lapply(getFiles(), 
               function(x) read_delim(x, delim=";"))

# TODO: bubblegum solution for current test data
data$subjects_file[c("dob","deathdate","cohort_entry")] <- do.call(cbind, lapply(data$subjects_file[c("dob","deathdate","cohort_entry")], to_unix_time))
data$events_file[c("date")] <- do.call(cbind, lapply(data$events_file[c("date")], to_unix_time))
data$drugs_file[c("start","stop")] <- do.call(cbind, lapply(data$drugs_file[c("start","stop")], to_unix_time))
data$blackbox_file[c("start")] <- data$blackbox_file  %>% to_unix_time 
#validations <- lapply(data, validate)

# if(!all(vapply(validations, is_valid, FUN.VALUE=logical(1))))
#   stop("Validation failed")
outcome <- getOption("outcome")
predictor <- getOption("predictor")

filter_ids <- data$blackbox_file %>% select("person_id", "type") %>% 
              dplyr::filter(.data$type %in% getOption("blackbox_filters")) %>% 
              pull("person_id")
length(unique(filter_ids))

subjects <- asDataObject(data$subjects_file[c("person_id","sex", "dob", "deathdate")])

drugs <- asDataObject(data$drugs_file)
events <- filter_out_data(drugs, "person_id", 
                          drugs[!(drugs[,"person_id", drop=TRUE] %in% subjects[,"person_id", drop=TRUE]),"person_id", drop=TRUE])

static_covariates <- asDataObject(data$static_covariates_file[,c("person_id", "name", "value")])
static_covariates <- filter_out_data(static_covariates, "person_id", 
                          static_covariates[!(static_covariates[,"person_id", drop=TRUE] %in% 
                                                            subjects[,"person_id", drop=TRUE]),"person_id", drop=TRUE]
                          )

events <- asDataObject(data$events_file[,c("person_id", "date", "date", "event")])
events <- rename_column(events, "event", "value")
events <- rename_column(events, "date", "start")
events <- rename_column(events, "date.1", "stop")
events <- filter_out_data(events, "person_id", 
                          events[!(events[,"person_id", drop=TRUE] %in% subjects[,"person_id", drop=TRUE]),"person_id", drop=TRUE])

subjects <- filter_out_data(subjects, "person_id", filter_ids)
events <- filter_out_data(events, "person_id", filter_ids)
#drugs <- filter_out_data(drugs, "person_id", which(!(drugs@data_matrix[,"person_id", drop=TRUE] %in% subjects@data_matrix[,"person_id", drop=TRUE])))
drugs <- filter_out_data(drugs, "person_id", filter_ids)
static_covariates <- filter_out_data(static_covariates, "person_id", filter_ids)



# Add followup end date
# Follow_up ends at analysis end at the latest
followup_end <- rep(getAnalysisEnd(),nrow(subjects))
ind <- match(subjects$person_id,data$blackbox_file[,"person_id"] %>% pull)
followup_end[!is.na(ind) & (data$blackbox_file[ind, "type"] == "LAITOS90PV" %>% na.as(FALSE))] <- (data$blackbox_file %>% pull("start"))[ind[!is.na(ind)]]
followup_end <- pmin(subjects$deathdate, followup_end, na.rm=TRUE)

max_stop <- split(drugs$stop, match(drugs$person_id, unique(drugs$person_id))) %>% lapply(max) %>% do.call(rbind,.)
ind <- match(subjects$person_id, unique(drugs$person_id))
followup_end <- pmin(max_stop[ind], followup_end, na.rm=TRUE)
subjects <- add_column(subjects, "followup_end", followup_end)


# bbox_to_subject_ids <- na.omit(match(data$blackbox_file[,"person_id"], subjects$person_id))
# followup_end <- data$blackbox_file[bbox_to_subject_ids,]
# followup_end[followup_end[,1] == "LAITOS90PV",] 
# subjects <- add_column(subjects, "followup_end", )

# Remove any events or drug usage periods past analysis end date
drugs <- drugs[drugs[,"start"] <= getAnalysisEnd(),,drop=FALSE]
events <- events[events[,"start"] <= getAnalysisEnd(),,drop=FALSE]
# Truncate drug usages to analysis end date
drugs[drugs[,"stop"] > getAnalysisEnd(),] <- matrix(getAnalysisEnd(), ncol=1,nrow=nrow(drugs))

long_data <- make_long(subjects, drugs, events, static_covariates)
long_data <- data.frame(long_data)
length(unique(long_data[,"person_id", drop=TRUE]))

tabl <- build_table(outcome, predictor, long_data)
print_table(tabl)

model <- lax( 
  coxph(as.formula(paste0("Surv(start, stop, ",getOption("outcome"),") ~ ", getOption("predictor"), " + cluster(person_id)")), long_data) 
)
model <- lax( 
  coxph(as.formula(paste0("Surv(start, stop, ",getOption("outcome"),") ~ ", getOption("predictor"), " + cluster(person_id) +", paste(getOption("adjust_for"),collapse="+"))), long_data) 
)
plot(survfit(model))
summary(model)
confint(model)
exp(confint(model))
