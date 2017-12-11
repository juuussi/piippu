
rm(list=ls(all.names=TRUE))

#library(strict)
library(dplyr)
library(readr)
library(survival)
#library(tables)
library(futile.logger)

options(warn=1)

base_dir <- "~/projects/piippu/"
source(base_dir %>% paste0("validation.R"))
source(base_dir %>% paste0("common_funcs.R"))
source(base_dir %>% paste0("data_object.R"))
source(base_dir %>% paste0("configuration.R"))
source(base_dir %>% paste0("descr_stats.R"))
source(base_dir %>% paste0("transform.R"))


# Set configuration
readConfigFile(base_dir %>% paste0("piippu_testconfig"))

### Tests

#Test data
subject_testdata <- data.frame(
  person_id=c(1,2,3,4,18,14,28,41,42,43),#,44,52,59,60,63),#,64,71,73,78,79,81,82,90,96,90),
  sex=c(0,0,1,1,0,1,0,1,0,1),#,0,1,1,0,1),#,1,1,0,0,0,0,1,0,1,0),
  dob=c(-17188,-7306,-3448,-6940,-16617,-4386,-16471,-8037,-17768,-19765),#,-10750,-14356,-17367,-8227,-9562),#,-2705,-2741,-19381,-16847,-15136,-12960,-15643,-2970,-681,-6754),
  cohort_entry=c(5154,5921,14464,8225,27455,5565,30,12706,20513,16500),#,9515,445,25002,5658,17933),#,7031,9648,25058,21366,12151,18953,15573,4698,3261,13596),
  deathdate=c(9740,30146,18194,28876,49747,8490,-522,23763,38411,16045),#,11288,12637,24956,34239,31746),#,34830,14493,29410,41916,26719,46788,43416,9770,13643,20141),
  stringsAsFactors=FALSE
)

drug_testdata <- data.frame(
  person_id=c(90,2,3,4,18,14,14,41,42,43),#,44,14,59,60,63),#,64,71,73,78,79,82,82,82,90,90),
  start=c(5182,8613,6420,338,2665,3474,9682,493,3515,5429),#,7545,8808,6223,8643,5170),#,3597,4462,3287,741,9583,3722,666,7639,9035,9456),
  stop=c(5181,14498,11428,2543,10757,6861,17245,2468,12245,8249),#,9374,11134,11661,17831,5761),#,11042,7076,12607,9617,15383,4404,9449,17224,16008,11660),
  class=c("Drug","Drug","Drug_two","Drug_two","Drug_two","Drug_3","Drug","Drug","Drug","Drug"),#,"Drug_two","Drug","Drug_3","Drug_two","Drug"),
      #"Drug_3", "Drug","Drug","Drug","Drug_two","Drug","Drug_3","Drug","Drug_3","Drug_two"),
  dose= (1:10) - 0.5,
  stringsAsFactors=FALSE
)
event_testdata <- data.frame(
  person_id=c(90,2,3,4,18,14,14,41,42,43),#,44,14,59,60,63),#,64,71,73,78,79,82,82,82,90,90),
  start=c(622,411,304,276,383,994,1052,719,872,337),#,1093,680,382,565,1037),#,5,35,556,635,731,61,16,821,1040,1053),
  stop=c(622,411,304,276,384,994,1052,720,872,338),#,1093,680,382,565,1037),#,5,35,556,635,731,61,16,821,1041,1053),
  value=c("Event1", "Event2", "Event3")[c(1,3,2,3,3,2,1,2,3,3)],#,2,2,2,2,3)],#,1,2,3,2,3,1,2,3,1,1)],
  stringsAsFactors=FALSE
)
expect_error({
  
  static_testdata <- data.frame(
    person_id = c(63,41,59,44,60,44,63,18,63,4,4,59,2,42,2,41,63,43,63,2,2,18,59,41,18,60,59,41,43,41,63,2,63,44,2,41,42,2,43,4,42,4,63,18,42,42,42,63,2,18),
      #c(4,79,79,44,90,14,42,73,82,82,43,14,90,71,64,90,41,71,82,82,42,90,90,14,18,90,4,82,42,59,4,82,82,18,18,82,82,44,43,90,14,2,82,14,82,90,60,82,14,63),
    name=c("Var3","Var1","Var1","Var1","Var2","Var1","Var2","Var1","Var3","Var2","Var2","Var3","Var1","Var1","Var2","Var1","Var3","Var2","Var2","Var2",
           "Var2","Var3","Var2","Var2","Var1","Var1","Var2","Var1","Var1","Var1","Var2","Var2","Var3","Var3","Var3","Var3","Var2","Var1","Var3","Var1",
           "Var2","Var1","Var2","Var1","Var1","Var2","Var3","Var3","Var3","Var2"),
    value=c("No","Yes","Yes","No","No","Yes","Yes","Yes","No","Yes","Yes","No","No","No","Yes","No","Yes","No","No","No","No","No","Yes","Yes","Yes",
            "No","Yes","No","No","Yes","Yes","Yes","Yes","Yes","No","No","No","Yes","Yes","No","Yes","No","No","No","Yes","Yes","No","Yes","No","No"),
    stringsAsFactors=FALSE
    
  )
  
  wide_testdata <- make_wide(asDataObject(subject_testdata), asDataObject(drug_testdata), asDataObject(event_testdata), asDataObject(static_testdata))
  
}, message="Each static covariate must have only one value for each person")

expect_silent({
  
  static_testdata <- data.frame(
    person_id = c(63,41,59,44,60,18,4,2,42,43),
      #c(4,79,79,44,90,14,42,73,82,82,43,14,90,71,64,90,41,71,82,82,42,90,90,14,18,90,4,82,42,59,4,82,82,18,18,82,82,44,43,90,14,2,82,14,82,90,60,82,14,63),
    name=c("Var3","Var1","Var1","Var1","Var2","Var1","Var2","Var1","Var1","Var3"),
    value=c("No", "Yes","Yes","No", "No", "Yes","Yes","No", "No", "No"),
    stringsAsFactors=FALSE
    
  )
  
  wide_testdata <- make_wide(asDataObject(subject_testdata), asDataObject(drug_testdata), asDataObject(event_testdata), asDataObject(static_testdata))
  wide_testdata <- wide_testdata[with(wide_testdata, order(person_id, start,stop)),, drop=FALSE]
  row.names(wide_testdata) <- NULL #for comparing to manual_data
})
manual_data <- data.frame(
  person_id=c(1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 14, 14, 14, 14, 14, 14, 18, 18, 18, 18, 28, 41, 41, 41, 41, 42, 42, 42, 43, 43, 43, 43, 90, 90, 90),
  start=c(10957, 411, 8613, 10957, 304, 6420, 10957, 276, 338, 2543, 994, 1052, 3474, 6861, 9682, 10957, 383, 384, 2665, 10757, 10957, 493, 719, 720, 2468, 872, 3515, 10957, 337, 338, 5429, 8249, 622, 5181, 5182),
  stop=c(15705, 8613, 10957, 14498, 6420, 10957, 11428, 338, 2543, 10957, 1052, 3474, 6861, 9682, 10957, 17245, 384, 2665, 10757, 10957, 15705, 719, 720, 2468, 10957, 3515, 10957, 12245, 338, 5429, 8249, 10957, 5181, 5182, 10957),
  Event1=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  Event2=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  Event3=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Drug=c(0, 0, 1.5, 1.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6.5, 6.5, 0, 0, 0, 0, 0, 7.5, 7.5, 7.5, 0, 0, 8.5, 8.5, 0, 0, 9.5, 0, 0, 0, 0),
  Drug_3=c(0, 0, 0, 0, 0, 2.5, 2.5, 0, 3.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Drug_two=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Var1=c(NA, "No", "No", "No", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Yes", "Yes", "Yes", "Yes", NA, "Yes", "Yes", "Yes", "Yes", "No", "No", "No", NA, NA, NA, NA, NA, NA, NA),
  Var2=c(NA, NA, NA, NA, NA, NA, NA, "Yes", "Yes", "Yes", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA),
  Var3=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "No","No","No","No", NA, NA, NA),
  stringsAsFactors=TRUE
)
#rownames(manual_data) <- NULL

  
expect_equal(wide_testdata, manual_data)

resetted_testdata <- reset_on_event(wide_testdata, "Event1")
#manual_reset_data <- data.frame(
