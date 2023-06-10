source('cb_config.R')
source('cb_utilities.R')
source('cb_strategy_specific_functions.R')

setwd('~/Desktop/LAC_Decarb_Git/ssp_cost_benefits/Main/')

factors_filename<-'cost_factors/trns_air_pollution_cost_factors.csv'
data_filename<-'~/Desktop/LAC Energy Tableaus/summary_energy_results.csv'
cb_definitions_filename<-'~/Desktop/LAC_Decarb_Git/ssp_cost_benefits/Energy/CB_definitions_TRNS.csv'
primary_filename<-'~/Desktop/LAC Energy Tableaus/ATTRIBUTE_PRIMARY.csv'
strategy_filename<-'~/Desktop/LAC Energy Tableaus/ATTRIBUTE_STRATEGY.csv'


#-------Prepare the Data!------
#Read data
output.file<-read.csv(data_filename)
data<-output.file

#Merge results with strategy attributes (mainly the strategy_code)
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

#establish the new list of unique variables in the dataset
list_of_variables<-new_vars

#Read definitions
x<-1 #for now, calculate air quality for light duty road transport
cb_definitions<-read.csv(cb_definitions_filename)
electrify_light_duty<-cb_definitions[cb_definitions$strategy_code=='TRNS:FUEL_SWITCH_LIGHT_DUTY' & 
                                       cb_definitions$output_variable_name=='technical_cost_electrification_LDV',]


#Read cost factors
cost_factors<-read.csv(factors_filename)


test<-calculate_costs_and_benefits_from_cost_factors_file(data, electrify_light_duty, factors_filename, list_of_variables)
