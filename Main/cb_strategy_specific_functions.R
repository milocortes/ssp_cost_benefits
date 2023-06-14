# This file contains cost and benefit functions for 
# transformation specific cost and benefit calculations
# that aren't handled by one of the general cost-benefit utility functions

#----------IPPU:CLINKER------------------
cb_ippu_clinker<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                          output_mults, change_in_multiplier, country_specific_multiplier,
                          scale_impact, scaling, list_of_variables){
  
  #Find strategy IDs
  base_code<-cb_strategy_attributes$strategy_id[cb_strategy_attributes$strategy_code==strategy_code_base]
  strategy_code<-cb_strategy_attributes$strategy_id[cb_strategy_attributes$strategy_code==strategy_code_tx]
  
  #read the fraction of clinker from the repository and find the difference
  chile_ippu_base<-read_xlsx(paste0(lac_decarbonization_git_path, "/ref/ingestion/calibrated/chile/model_input_variables_chile_ip_calibrated.xlsx"), sheet=paste0('strategy_id-', base_code))
  chile_ippu_tx<-read_xlsx(paste0(lac_decarbonization_git_path, "/ref/ingestion/calibrated/chile/model_input_variables_chile_ip_calibrated.xlsx"), sheet=paste0('strategy_id-', strategy_code))
  base_frac_clinker<-chile_ippu_base[chile_ippu_base$variable=='frac_ippu_cement_clinker',]
  tx_frac_clinker<-chile_ippu_tx[chile_ippu_base$variable=='frac_ippu_cement_clinker',]
  
  
  #calculate the amount of avoided clinker
  frac_clinker_avoided<-t(base_frac_clinker - tx_frac_clinker)
  data_strategy<-subset(data, strategy_code==strategy_code_tx & variable==diff_var)
  data_strategy$difference_variable<-'avoided_clinker'
  data_strategy(merge, data_strategy, frac_clinker_avoided, by='time_period')
  data_strategy$difference_value <-data_strategy$value/data_strategy
  
  #clinker_in
  
  #apply multiplier
  
  
  
}


#----------IPPU:FGASES-------------------
cb_ippu_florinated_gases<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                   output_mults, change_in_multiplier, country_specific_multiplier,
                                   scale_impact, scaling, list_of_variables){
  #get all the variables with florinated gases
  #use nomenclature "emission_co2e_NAMEOFGAS_ippu_" where name of gas contains an "f"
  emissions_vars<-list_of_variables[grep('emission_co2e_', list_of_variables)]
  exclude<-"_co2_|_n2o_|_ch4_|_subsector_"
  fgases<-emissions_vars[!str_detect(emissions_vars, exclude)]
    
  #sum up for both strategies
  data_strategy<-subset(data, strategy_code==strategy_code_tx & variable %in% fgases)
  data_strategy_summarized<-data_strategy %>% 
    group_by(region, time_period, strategy_code) %>% 
    summarise(difference_value = sum(value))
  data_strategy_summarized$difference_variable<-'emission_co2e_all_fgases_ippu'
  data_strategy_summarized$variable<-output_vars
  
  data_strategy_base<-subset(data, strategy_code==strategy_code_base & variable %in% fgases)
  data_strategy_base_summarized<-data_strategy_base %>% 
    group_by(region, time_period, strategy_code) %>% 
    summarise(difference_value = sum(value))
  data_strategy_base_summarized$difference_variable<-'emission_co2e_all_fgases_ippu'
  data_strategy_base_summarized$variable<-output_vars
  
  #take difference and multiply by cost / CO2e
  data_fgases_merged<-merge(data_strategy_summarized, data_strategy_base_summarized, by=c('region', 'time_period'), suffixes = c("", ".base"))
  data_fgases_merged$difference_value<-data_fgases_merged$difference_value - data_fgases_merged$difference_value.base
  data_fgases_merged$value<-data_fgases_merged$difference_value * output_mults

  data_fgases_merged<-data_fgases_merged[, c("region","time_period","strategy_code",
                                             "difference_variable", "difference_value","variable","value")]
  
  #return result
  return(data_fgases_merged)
}

#----------ENTC:REDUCE_LOSSES: Technical cost of maintaining grid ----------

cb_entc_reduce_losses<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
  output_mults, change_in_multiplier, country_specific_multiplier,
  scale_impact, scaling, list_of_variables){
  
  #get the loss file
  path_to_loss_reduction_costs<-paste0(ssp_costs_benefits_git_path, 'strategy_specific_cb_files/ENTC_REDUCE_LOSSES_cost_file.xlsx')
  cb_transmission_loss_costs<-read_xlsx(path_to_loss_reduction_costs, sheet = "Annual Loss Reduction Cost")
  
  #map ISO3 to the reigons
  path_to_country_codes<-paste0(sisepuede_data_git_path, 'Energy/nemomod_entc_residual_capacity_pp_gas_gw/raw_data/iso3_all_countries.csv')
  country_codes<-read.csv(path_to_country_codes)
  cb_transmission_loss_costs<-merge(cb_transmission_loss_costs, country_codes, by=c('ISO3'))
  colnames(cb_transmission_loss_costs)[colnames(cb_transmission_loss_costs)=='REGION']<-'region'
  
  #assign fixed losses to each country from 2025 to 2050
  data_strategy<-subset(data, strategy_code==strategy_code_tx & variable==diff_var)
  data_output<-merge(data_strategy, cb_transmission_loss_costs, by=c('region'))
  data_output$variable<-output_vars
  data_output$value<-data_output$annual_investment_USD
  data_output$difference_variable<-'N/A (constant annual cost)'
  data_output$difference_value<-data_output$annual_investment_USD

  data_output <- subset(data_output, select = -c(primary_id, ISO3, Country, annual_investment_USD, Code, Category.Name))
  
  return(data_output)
  
}

#----------TRNS:DEC_DEMAND: Economic impact of reducing transport demand----------
#the economic effect of reduced transportation demand is equal to
#the transport-driven GDP (from input output tables)
#and the fraction of that transport that is avoided relative to a baseline
#THIS WILL NOT WORK BECAUSE DO.CALL IS PASSING OTHER ARGUMENTS
#NOT FIXING BECAUSE WE AREN"T USING THIS TRANSFORMATON RIGHT NOW
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
