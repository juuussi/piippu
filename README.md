# Piippu

Piippu is an R pipeline for running survival analysis on drug usage period data.  It is designed to be used on data produced with the [PRE2DUP-method](https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/s12911-015-0140-z) from drug purchase register data. Most of the code is focused on doing required data transformations relatively efficiently. 

## Installation

The code is currently not formatted as a package, and so requires the user to perform some steps themselves.

### Required packages

The script requires the following packages to be installed:

- stringr
- lubridate
- survival
- Rcpp
- testthat
- dplyr
- readr
- futile.logger
- tidyr
- tables

### With `git`

Set your working directory to where you want to "install" the scripts and then run

`git clone git://github.com/juuussi/piippu.git`

### Otherwise 

Download the code as a zip file by clicking `Clone or download` and `Download zip` on the [Github page](https://github.com/juuussi/piippu). Unpack the folder.

## Configuration

Next create a configuration file.

Every line consists is of the form `name=value`, e.g. `analysis_start_date=1.1.2012`.
The following values must be set:

- `analysis_start_date`: Analysis start date in format `DD.MM.YYYY`.
- `analysis_end_date`: Analysis end date in format `DD.MM.YYYY`.

Paths to data files (see section [Data files](#datafiles)):

- `subjects_file`
- `static_covariates_file`
- `drugs_file`
- `events_file`
- `blackbox_file`

- `output_folder`: Path to where results should be saved. The folder must exist.
- `log_file`: Path to where log file should be generated

Analysis parameters:

- `blackbox_filters`: Which blackboxing identifiers should be used separated with `,`. Set `ALL` to avoid having to write everything manually
- `outcome`: Name of outcome variable
- `analysis_type`: one of the following strings: `First event`, `Counting process`, `Reset on event`. See section [Analysis types](#analysistypes).
- `reset_type`: either `Conditional model A` or `Conditional model B`.

The next two options have slightly more structure, i.e. `name=value:type,...,value:type`. `type` can be either `numeric` or `categorical`. E.g. for predicting with a drug with continuously measured dosage `predictor=DRUG1:numeric`.

- `predictor`: Name of predictor variable
- `adjust_for`: Names and types for covariates.

The code can be run in an R session with 

`source("PATH_TO_FOLDER/piippu/main.R", chdir=TRUE)`

Or directly from the command line

`Rscript PATH_TO_FOLDER/piippu/main.R`

<h2 id="datafiles"> Data files </h2>



<h2 id="analysistypes"> Analysis types </h2>

There are currently 3 implemented analysis types.

### Multiple events

This analysis is the "basic" form: it allows each outcome event to occur any number of times, including zero. 

### First event

The first event analysis censors after the first event. 

### Resetting at event

Here we reset time for an individual to zero at each event, following either "Conditional model" A or B as described in
https://stats.idre.ucla.edu/sas/faq/how-can-i-model-repeated-events-survival-analysis-in-proc-phreg/ .