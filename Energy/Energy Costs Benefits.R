setwd("~/Desktop/LAC Energy Tableaus")
library(reshape)
library("readxl")
library(dplyr)
source("CBUtil_energy.R")
source("IMF_externality_multipliers.R")
source("Country Specific Multipliers.R")
source("SSP Utilities.R")


#INPUT OUTPUT FILE NAMES
filename<-'summary_energy_results.csv'
output.file<-read.csv(filename)
data<-output.file
run_attributes<-ssp_merge_run_attributes()
merged_data<-merge(data, run_attributes[,1:2], by=c('primary_id'), x.all=TRUE)
data<-merged_data


#outfilename<-paste0("CB_", filename)
#outfilename_defs<-paste0("CB_definitions_", filename)
#definition_file<-'energy_cb_definitions_5_9.csv'

#Use these for cost blender
definition_file<-'Cost Blender Testing/CB_definitions_cost_blender_SCOE_test.csv'
outfilename<-paste0("Cost Blender Testing/cost_blender_", data_filename)
outfilename_defs<-paste0("Cost Blender Testing/cost_blender_definitions_", data_filename)

#trim to just the necessary columns
cb_results<-calculate_costs_and_benefits(data, definition_file, outfilename, outfilename_defs)







