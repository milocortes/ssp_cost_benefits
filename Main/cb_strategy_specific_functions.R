# This file contains cost and benefit functions for 
# transformation specific cost and benefit calculations
# that aren't handled by one of the general cost-benefit utility functions


#the economic effect of reduced transportation demand is equal to
#the transport-driven GDP (from input output tables)
#and the fraction of that transport that is avoided relative to a baseline
cb_economic_effect_of_reduced_transport_demand<-function(data, definition){

  #Get the LAC gdp data and format it for time_period, region, and gdp columns
  path_to_gdp_data<-paste0(sisepuede_data_git_path, 'SocioEconomic/gdp_mmm_usd/input_to_sisepuede/projected/gdp_mmm_usd.csv')
  gdp_data<-read.csv(path_to_gdp_data)
  lac_iso3_codes<-ssp_LAC_iso3()
  gdp_data<-gdp_data[gdp_data$iso_code3 %in% lac_iso3_codes, ]
  
  #time period
  gdp_data$time_period<-gdp_data$Year-2015
  
  #country codes
  colnames(gdp_data)[1]<-'ISO3'
  path_to_country_codes<-paste0(sisepuede_data_git_path, 'Energy/nemomod_entc_residual_capacity_pp_gas_gw/raw_data/iso3_all_countries.csv')
  country_codes<-read.csv(path_to_country_codes)
  gdp_data<-merge(gdp_data, country_codes, by=c('ISO3'))
  colnames(gdp_data)[colnames(gdp_data)=='REGION']<-'region'
  gdp_data<-gdp_data[,c('region', 'time_period', 'gdp_mmm_usd')]
  
  #Create the demand reduction data frame
  time_period<-seq(from=0, to=35, by=1)
  demand_reduction<-c(rep(0,10), seq(from = 0, to = 0.25, by = (0.25)/25))
  demand_reduction_transformation<-data.frame(time_period, demand_reduction)
  
  #merge with demand reduction
  gdp_data<-merge(gdp_data, demand_reduction_transformation, by='time_period')
  gdp_data$cost_of_demand_reduction<-gdp_data$gdp_mmm_usd*gdp_data$demand_reduction*definition$multiplier
  
  #put these in the output data
  data_strategy<-subset(data, strategy_code==definition$test_id & variable==definition$difference_variable)
  data_output<-merge(data_strategy, gdp_data, by=c('region', 'time_period'))
  
  data_output$variable<-definition$output_variable_name
  data_output$value<-data_output$cost_of_demand_reduction*10^9 #convert out of mmm
  data_output$difference_variable<-'trns_frac_demand_reduced'
  colnames(data_output)[colnames(data_output) == 'demand_reduction']<-'difference_value'
  
  data_output <- subset(data_output, select = -c(gdp_mmm_usd, cost_of_demand_reduction, primary_id))
}
