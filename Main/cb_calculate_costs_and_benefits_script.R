#This script will calculate all the costs and benefits defined in 
#a cost_benefit_definitions_main.csv file. 
#To run this scirpt, please update thew workding directory and the paths in cb_config.R and update any filenames here

#-------------SOURCE LIBRARIES AND CODE-----
source('cb_config.R')
source('cb_utilities.R')
source('cb_strategy_specific_functions.R')
source('general_ssp_utilities.R')

#-------------SET WORKING DIRECTORY----------
setwd('~/Desktop/LAC_Decarb_Git/ssp_cost_benefits/Main/')

#-------------PATHS to KEY FILES-----------

path_to_model_results<-'/Users/nidhi/Desktop/LAC Visualizations/'
data_filename<-paste0(path_to_model_results, 'sisepuede_results.csv') #path to model output runs
primary_filename<-paste0(path_to_model_results, 'ATTRIBUTE_PRIMARY.csv') #path to model output primary filename
strategy_filename<-paste0(path_to_model_results, 'ATTRIBUTE_STRATEGY.csv') #path to model output strategy filename
cb_main_definitions_filename<-paste0(ssp_costs_benefits_git_path, 'cost_benefit_definitions_main.csv') #path to the main cost benefit definitions file
cb_strategy_specific_definitions_filename<-paste0(ssp_costs_benefits_git_path, 'cost_benefit_definitions_strategy_specific.csv') #path to strategy-specific file
cb_output_filename<-paste0(ssp_costs_benefits_git_path,'cost_benefit_results.csv') #path to write results

#-------------PREPARE THE DATA--------------
#Read data
output.file<-read.csv(data_filename)
data<-output.file

#Merge model output with strategy attributes (mainly the strategy_code)
run_attributes<-ssp_merge_run_attributes(primary_filename, strategy_filename)
merged_data<-merge(data, run_attributes[,c('primary_id', 'strategy_code')], by=c('primary_id'), x.all=TRUE)
data<-merged_data

#Clean the data of rows with variables that are buggy or extraneous
orig_vars<-unique(data$variable)
c1<-orig_vars[grepl('^totalvalue_enfu_fuel_consumed_.*_fuel_electricity$', orig_vars)] #Skip electricity costs because they are duplicative of other costs
c2<-orig_vars[grepl('totalvalue.*furnace_gas', orig_vars)] #Skip furnace gas because it is crazy buggy and extraneous
exclude_list<-c(c1, 
                c2, 
                'totalvalue_enfu_fuel_consumed_entc_fuel_crude' #Skip crude because it is duplicative
)

rows_to_keep<-!grepl(paste(exclude_list, collapse="|"), data$variable)
data_cleaned<-data[rows_to_keep,]
new_vars<-unique(data_cleaned$variable)
data<-data_cleaned

#-------------READ OTHER FILES IN----------
cb_main_definitions<-read.csv(cb_main_definitions_filename)
cb_strategy_specific_definitions<-read.csv(cb_strategy_specific_definitions_filename)
cb_strategy_attributes<-read.csv(strategy_filename)

#-------------CALCULATE COSTS AND BENEFITS----------
#read the definition
#run the code
#NIDHI: note to self -- how will we keep track of the definitions that are in cost factor files?
#want to add the following information to each row? output_variable_name	output_display_name	natural.multiplier.units	display_notes	internal_notes
#Nidhi (Monday Night -- check fgas code)
start_time <- Sys.time()
results<-calculate_costs_and_benefits(data, 
                                      cb_main_definitions[cb_main_definitions$strategy_code=='IPPU:DEC_DEMAND',]
                                      ,
                                      #cb_strategy_specific_definitions[cb_strategy_specific_definitions$strategy_code=='IPPU:DEC_CLINKER',]
                                      )

end_time <- Sys.time()
runtime<-(end_time - start_time)

#Test a single cost factor file
#test_cost_factors<-read.csv('~/Desktop/LAC_Decarb_Git/ssp_cost_benefits/cost_factors/ghg_effects_factors.csv')
#list_of_vars<-unique(data$variable)
#test<-calculate_costs_and_benefits_from_cost_factors(data, cb_definitions[cb_definitions$strategy_code=='TRNS:MODE_SHIFT_FREIGHT',], test_cost_factors, list_of_vars)

#Test the entc transmission loss
#definition<-cb_strategy_specific_definitions[cb_strategy_specific_definitions$strategy_code=='ENTC:REDUCE_LOSSES',]
#loss_cost<-cb_entc_reduce_losses(data, definition)


#-------------WRITE THE RESULTS---------------------
write.csv(results, file=cb_output_filename)



