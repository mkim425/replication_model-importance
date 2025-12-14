# Example use:
# Rscript --vanilla submit_cluster_jobs_shapley.R single_day_forecastdata

library(covidHubUtils)
library(hubEnsembles)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr)) 
suppressPackageStartupMessages(library(lubridate))

#library(abind)
path <- "~/project" # please change to your own path
source(paste0(path,"/data/calculate-importance/importance_score_lomo.R"))
source(paste0(path,"/data/calculate-importance/build_array_lomo.R"))

args <- commandArgs(trailingOnly = TRUE)
file_name <- args[1]

forecast_data <- readRDS(paste0(path,"/data/forecast-data/", file_name)) 

truth <- readRDS(paste0(path,"/data-raw/truth_data_upto2022.rds"))

build_array(forecast_data, truth) 
