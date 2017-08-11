library(strict)
library(dplyr)
library(readr)

options(warn=2)

base_dir <- "~/projects/piippu/"
source(base_dir %>% paste0("validation.R"))
source(base_dir %>% paste0("transform.R"))

subjects <- read_csv(base_dir %>% paste0("subjects.csv"))

validation_subjects <- validate(subjects)

print(validation_subjects)
print(is_valid(validation_subjects))

subjects_dates <- subjects[,validation_subjects$dates]

