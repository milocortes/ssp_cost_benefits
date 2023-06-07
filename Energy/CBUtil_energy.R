#Cost Benefit Utility Functions

library(reshape)





#-------calculate an LHS sample of futures related to strategy effectiveness, costs, and benefits
create_futures<-function(filename, num_futures){
  
  #Read the file
  futures_definitions<-read.csv(filename)

  #generate the uncertainty scalars and store as a dataframe
  num_uncertainties<-length(futures_definitions)-1
  lhs_matrix <- t(randomLHS(num_futures, num_uncertainties))
  futures_scalars<-lhs_matrix*(futures_definitions$scalar_max - futures_definitions$scalar_min) + futures_definitions$scalar_min
  futures_scalars<-t(futures_scalars)
  futures_df<-as.data.frame(futures_scalars)
  
  #add strategy and uncertainty as variable names to unpack later
  variable_names<-paste0(futures_definitions$strategy_id,":",futures_definitions$variable)
  colnames(futures_df)<-variable_names
  
  #add a column for baseline
  baseline_future<-integer(num_uncertainties)+1
  futures_df<-rbind(baseline_future, futures_df)
  
  #create a future number
  futures_df$future_id<-c(1:nrow(futures_df))
  
  #create a column for the strategy and a column for the cost and effects scalars for each
  futures_df<-futures_df %>% pivot_longer(cols=variable_names,
                                          names_to='variable',
                                          values_to='value')
  futures_df<-separate(futures_df, col=variable, into=c('strategy_id', 'variable'), sep=':')
  
  futures_df<-futures_df %>% pivot_wider(names_from = variable, values_from = value)
  
  #any NAs mean there's no exploration over the variable, so treat as 1.
  futures_df[is.na(futures_df)] <- 1
  
  return(futures_df)
}

#-------calculate a blended cost of a strategy based on contributing strategies-----------
#portfolio_emissions_effect is the rows of cost benefit data with portfolio effects
#cost_effectiveness_results is the data on abatement cost effectiveness and relative effects of the strategies in the portfolio
#sum_costs is a flag indcating whether costs should be reported individually or as a total weight
blend_costs<-function(portfolio_data, cost_effectiveness_results){

  #merge the two data sets so that we get one full set of cost results per contributing strategy
  merged_data<-merge(cost_effectiveness_results, portfolio_data, by=c('region', 'time_period'), all.x=TRUE)
  
  #calculate (1) the difference in GHG attributable to each strategy and (2) the abatement cost of that strategy
  merged_data$rel_difference_value<-merged_data$difference_value*merged_data$relative_effect
  merged_data$rel_value<-merged_data$rel_difference_value*merged_data$abatement_cost_effectiveness
  
  #rename columns and select only those 
  merged_data$difference_variable<-paste0('change_in_emissions:', merged_data$strategy_id.x)
  merged_data$difference_value<-merged_data$rel_difference_value
  merged_data$value<-merged_data$rel_value
  colnames(merged_data)[colnames(merged_data)=='strategy_id.y']<-'strategy_id'
  
  if ('future_id' %in% colnames(cost_effectiveness_results)){
    results<-merged_data[,c(colnames(portfolio_data), 'future_id')]
  }else{
    results<-merged_data[,colnames(portfolio_data)]
  }
  return(results)
}

#-------calculate the abatement cost effectiveness and relative effect of each strategy in a dataset-----------
cost_effectiveness<-function(data){
  
  local_data<-data
  
  #Calculate the abatement cost effectiveness
  local_data$abatement_cost_effectiveness <- local_data$cost/local_data$change_in_emissions
  
  #Calculate the relative contributions and scaled costs based on the difference variable cost*(difference/total_difference)
 abatement_cost_effectiveness<-local_data %>% group_by(region, time_period) %>% 
    mutate(total_emissions_effect=sum(change_in_emissions), relative_effect=change_in_emissions/total_emissions_effect)

 #replace NaNs adn Infs with 0
 abatement_cost_effectiveness<-abatement_cost_effectiveness %>% 
   mutate(
     across(everything(), ~replace_na(.x, 0))
   )
 
 abatement_cost_effectiveness[sapply(abatement_cost_effectiveness, is.infinite)] <- 0
 
 
 
  return(abatement_cost_effectiveness)
   
}


#-------calculate cost or benefit from difference between 2 policies in long form-----------

#Data is a dataframe with id_vars region, time_period, and policy
#policy_tx: string identifier of transformation we are calculating CB for
#primary_id_base: string identifier of transformation we are comparing to
#the difference is base-tx
#diff_var: string name of variable in data to difference
#output_var: vector of names of the output variables
#output_mults: mutpliers to apply to the difference
#change_in_multiplier: how the multiplier changes over time, e.g., to reflect costs
#country_specific_multiplier: a flag indicatingw hether output_mults is a data frame
#with output_mults$region and output_mults$multiplier
#subtractbase: flag that, if set, subtracts the baseline run from the transformed run


cb_long<-function(data, strategy_id_tx, strategy_id_base, diff_var, output_vars, 
                  output_mults, change_in_multiplier, country_specific_multiplier,
                  scale_impact, scaling){
  
  id_vars <-c('region','time_period', 'strategy_id') #primary_id->strategy_id
  
  #create output table
  if (diff_var == "FIXED_ANNUAL_COST"){
    cat(r, "...........Dealing with Fixed annual costs...\n")
    diff_var<-"nemomod_entc_annual_production_by_technology_pp_solar" #use this as a dummy variable
    diff_var_name<-"No Difference Variable"
    datap_base<-subset(data, strategy_id==strategy_id_base & variable==diff_var) #primary_id->strategy_id
    datap_tx<-subset(data,strategy_id==strategy_id_tx & variable==diff_var) #primary_id->strategy_id
    datap_base$variable<-"Fixed Annual Cost"
    datap_tx$variable<-"Fixed Annual Cost"
    datap_base$value<-0
    datap_tx$value<-1
    datap_tx$value[datap_tx$time_period<10]<-0 #Don't start the fixed annual cost until 2025
  }else{
    diff_var_name<-paste0("diff_", diff_var)
    datap_base<-subset(data, strategy_id==strategy_id_base & variable==diff_var) #primary_id->strategy_id
    datap_tx<-subset(data,strategy_id==strategy_id_tx & variable==diff_var) #primary_id->strategy_id
  }
  
  datap_base$region<-as.character(datap_base$region)
  datap_base$variable<-as.character(datap_base$variable)
  datap_tx$region<-as.character(datap_tx$region)
  datap_tx$variable<-as.character(datap_tx$variable)
  datap_base$strategy_id<-NULL #primary_id->strategy_id
  datap_base$primary_id<-NULL
  
  colnames(datap_base)<-gsub('value', 'value_base', colnames(datap_base))
  data_merged<-Reduce(function(...) merge(...,), list( datap_tx,datap_base))

  #Calculate the differene in variables and then apply the multiplier, which may change over time
  #Assume cost change only begins NOW
  #Assume costs are 2020.
  data_merged$difference<-data_merged$value-data_merged$value_base
  data_merged$time_period_for_multiplier_change<-data_merged$time_period
  
  if (country_specific_multiplier){
    matched_multipliers<-as.numeric(merge(data_merged, output_mults, by="region", all.x=TRUE)$multiplier)
    data_merged$values<-t(t(data_merged$difference)*matched_multipliers*change_in_multiplier^data_merged$time_period_for_multiplier_change)
  }else{
    data_merged$values<-t(t(data_merged$difference)*output_mults*change_in_multiplier^data_merged$time_period_for_multiplier_change)  
  }

  #Scale the effects of this cost by the relative effects of other transformations
  if (scale_impact == 1){

    #prepare the two datasets we are comparing
    tmp_ids<-as.numeric(unlist(strsplit(scaling, ":")))
    numerator_id<-tmp_ids[1]
    denomenator_id<-tmp_ids[2]
    comp_id<-tmp_ids[3]
    
    datap_num<-subset(data, strategy_id==numerator_id & variable==diff_var) #primary_id->strategy_id
    datap_denom<-subset(data,strategy_id==denomenator_id & variable==diff_var) #primary_id->strategy_id
    datap_comp<-subset(data,strategy_id==comp_id & variable==diff_var) #primary_id->strategy_id
    
    datap_comp$region<-as.character(datap_comp$region)
    datap_comp$variable<-as.character(datap_comp$variable)
    datap_num$region<-as.character(datap_num$region)
    datap_num$variable<-as.character(datap_num$variable)
    datap_denom$region<-as.character(datap_denom$region)
    datap_denom$variable<-as.character(datap_denom$variable)
    datap_comp$region<-as.character(datap_comp$region)
    datap_comp$variable<-as.character(datap_comp$variable)
    datap_num$strategy_id<-NULL #primary_id->strategy_id
    datap_denom$strategy_id<-NULL #primary_id->strategy_id
    
    #create a merged dataset
    colnames(datap_num)<-gsub('value', 'value_num', colnames(datap_num))
    colnames(datap_denom)<-gsub('value', 'value_denom', colnames(datap_denom))
    colnames(datap_comp)<-gsub('value', 'value_comp', colnames(datap_comp))
    data_scaling_merged<-Reduce(function(...) merge(...,), list(datap_comp,datap_num,datap_denom))
    
    #Calculate the relative effect of each of the contributing function over baseline
    data_scaling_merged$numerator<-data_scaling_merged$value_num-data_scaling_merged$value_comp
    data_scaling_merged$denominator <- (data_scaling_merged$numerator + 
                                          (data_scaling_merged$value_denom-data_scaling_merged$value_comp))
    data_scaling_merged$scaling_factor<-data_scaling_merged$numerator/data_scaling_merged$denominator
    
    #Scale the output by this fraction
    data_merged_test<- merge(data_merged, data_scaling_merged, by=c('region','time_period'), all.x=TRUE)
    data_merged_test$scaled_difference<-data_merged_test$difference*data_merged_test$scaling_factor
    data_merged_test$scaled_values<-data_merged_test$values*data_merged_test$scaling_factor
    data_merged$difference<-data_merged_test$scaled_difference
    data_merged$values<-data_merged_test$scaled_values
  }
  
  
  tmp<-data_merged[,c(id_vars)]
  tmp$difference_variable<-diff_var_name
  tmp$difference_value<-data_merged$difference
  tmp$variable<-output_vars
  tmp$value<-data_merged$values
  output<-tmp
  
  return(output)
}



#--------Calculate costs and benefits defined in an input file ----------
# This code loops through a table where each row defines a cost-benefit calculations
# At its most basic, it finds the difference between variables in a transformed vs. baseline future
# And applies a multiplier to that difference (e.g., the difference in natural gas consumption * externality/unit of consumption)
# The code is flexible to allow for
# -- increases or decreases in the variable over time
# -- matching several variables at a time using a substring of the variable name, e.g, nemomod_entc_discounted_*
# -- summing up the results when multiple variables are matched
# -- using country specific-multipliers
# -- omitting some lines in the table


calculate_costs_and_benefits<-function(data, definition_filename, outfilename, outfilename_defs){
  

  list_of_variables<-unique(data$variable)
  
   
  #-------Columns in the definitions csv table-----------
  #transformation_name: name of the transformation, for human readability
  #type: flag for which function to call, currently unused
  #strategy_id: not used
  #primary_id: primary id of target transformation
  #comparison_id: primary id against which to compare
  #difference_variable:  variable to compare between the two runs
  #   may be a single string
  #   may be a substring of multiple variable names that are matched, e.g., for multiple categories
  #multiplier: multiplier to apply to the difference
  #   may be a single number
  #   may be the name of the variable containing a dataframe of multipliers
  #annual change: compounding change in the multiplier over time
  #multiplier unit: human readable units
  #output_variable_name: variable for the results
  #variable type: human readable classification of cost or benefit or whatever
  #include: for debugging -- include=0 skips this line
  # summarize_energy_costs -- if 1 or 2, automatically generates a line into calculate energy costs.
  #   when 1, comparison is transformation 0
  #   when 2, comparison is whatever is specified in comparison_id
  #sum: f the difference_variable is a substring, should the results be summed over the matching variables?
  #natural multiplier units: human readable units of the units of the original data source
  #country_specific_multiplier: flag 0 or 1 to indicate whether multipliers are defined per country
  #   when this is 1, it is assumed that the multiplier is the name of the variable containing the country-specific data frame
  
  
  #--------Read a cost-benefit definition file and output all the transformation results--------
  cb_definitions<-read.csv(definition_filename, stringsAsFactors = FALSE)
  num_rows<-nrow(cb_definitions)
  
  #--------Add automatic fuel cost calculations----------
  #Go through each row in the cost benefit definition file. 
  #If it is flagged to calculate costs, automatically generate a
  #new line in the file to include capital and operational electricty
  #production costs, and fuel costs
  new_cost_lines<-list()
  for (r in 1:num_rows){
    if (cb_definitions$summarize_energy_costs[r] > 0) {
      cat(r, ": ",cb_definitions$strategy[r], " - adding energy cost calculations... \n")
      cb_elec_cap_cost_line<-cb_definitions[r,]
      cb_elec_cap_cost_line$include<-1
      cb_elec_cap_cost_line$summarize_energy_costs<-0
      cb_elec_cap_cost_line$summarize_air_pollution_costs<-0
      cb_elec_cap_cost_line$test_id<-cb_definitions$strategy_id[r]
      cb_elec_cap_cost_line$output_display_name<-"Electricity Prod."
      
      cb_elec_cap_cost_line$comparison_id<-cb_definitions$comparison_id[r]
      cb_elec_cap_cost_line$difference_variable<-"nemomod_entc_discounted_capital*"
      cb_elec_cap_cost_line$multiplier<-10^6
      cb_elec_cap_cost_line$multiplier.unit<-"$"
      cb_elec_cap_cost_line$annual.change<-1
      cb_elec_cap_cost_line$output_variable_name<-"marginal_CAPEX_electricity"
      cb_elec_cap_cost_line$sum=0
      cb_elec_cap_cost_line$country_specific_multiplier=0
      cb_elec_cap_cost_line$scale_impact=0
      cb_elec_cap_cost_line$scaling=""
      cb_elec_cap_cost_line$natural.multiplier.units="total $"
      cb_elec_cap_cost_line$internal_notes="automatically generated cost definition"
      cb_elec_cap_cost_line$display_notes="Costs of electricity and fuels are endogenously calcuated in NemoMod"
      
      cb_elec_op_cost_line<-cb_elec_cap_cost_line
      cb_elec_op_cost_line$difference_variable<-"nemomod_entc_discounted_operating*"
      cb_elec_op_cost_line$output_variable_name<-"marginal_OPEX_electricity"
      cb_elec_op_cost_line$output_display_name<-"Electricity Prod."
      
      
      cb_fuel_cost_line<-cb_elec_cap_cost_line
      cb_fuel_cost_line$difference_variable<-"totalvalue_enfu_fuel_consumed_*"
      cb_fuel_cost_line$output_variable_name<-"marginal_fuel_cost"
      cb_fuel_cost_line$output_display_name<-"Fuels"
      
      
      new_cost_lines<-append(new_cost_lines, list(cb_elec_cap_cost_line, cb_elec_op_cost_line, cb_fuel_cost_line))
    }
  }
  new_cost_lines<-do.call("rbind", new_cost_lines)
  cb_definitions<-rbind(cb_definitions, new_cost_lines)
  
  
  
  #--------Add automatic air quality calculations----------
  #Go through each row in the cost benefit definition file. 
  #If it is flagged to calculate air quality, automatically generate a
  #new line in the file to include air quality
  new_cost_lines<-list()
  for (r in 1:num_rows){
    if (cb_definitions$summarize_air_pollution_costs[r] > 0) {
      cat(r, ": ",cb_definitions$strategy[r], " - adding pollution cost calculations... \n")
      
      #Transportation diesel
      cb_pollution_cost_diesel_line<-cb_definitions[r,]
      cb_pollution_cost_diesel_line$include<-1
      cb_pollution_cost_diesel_line$summarize_energy_costs<-0
      cb_pollution_cost_diesel_line$summarize_air_pollution_costs<-0
      cb_pollution_cost_diesel_line$test_id<-cb_definitions$strategy_id[r]
      cb_pollution_cost_diesel_line$output_display_name<-"Local Air Pollution"
      
      cb_pollution_cost_diesel_line$difference_variable<-"energy_demand_enfu_subsector_total_pj_trns_fuel_diesel"
      cb_pollution_cost_diesel_line$multiplier<-"cb_diesel_externalities"
      cb_pollution_cost_diesel_line$multiplier.unit<-"$/PJ"
      cb_pollution_cost_diesel_line$annual.change<-1
      cb_pollution_cost_diesel_line$output_variable_name<-"benefit_avoided_air_pollution"
      cb_pollution_cost_diesel_line$sum=0
      cb_pollution_cost_diesel_line$country_specific_multiplier=1
      cb_pollution_cost_diesel_line$scale_impact=0
      cb_pollution_cost_diesel_line$scaling=""
      cb_pollution_cost_diesel_line$natural.multiplier.units="varies"
      cb_pollution_cost_diesel_line$internal_notes="automatically generated pollution definition"
      cb_pollution_cost_diesel_line$display_notes="We use the IMF’s fossil fuel subsidies database (2021) to calculate the avoided air pollution costs of industrial fuel consumption."
      
      #Transportation gas
      cb_pollution_cost_gasoline_line<-cb_pollution_cost_diesel_line
      cb_pollution_cost_gasoline_line$difference_variable<-"energy_demand_enfu_subsector_total_pj_trns_fuel_gasoline"
      cb_pollution_cost_gasoline_line$output_display_name<-"Local Air Pollution"
      cb_pollution_cost_gasoline_line$multiplier<-"cb_gasoline_externalities"
      
      #Industry natural gas
      cb_pollution_cost_industry_natgas_line<-cb_pollution_cost_diesel_line
      cb_pollution_cost_industry_natgas_line$difference_variable<-"energy_demand_enfu_subsector_total_pj_inen_fuel_natural_gas"
      cb_pollution_cost_industry_natgas_line$output_display_name<-"Local Air Pollution"
      cb_pollution_cost_industry_natgas_line$multiplier<-"cb_nat_gas_ind_externalities"
      
      #Industry coal
      cb_pollution_cost_industry_coal_line<-cb_pollution_cost_diesel_line
      cb_pollution_cost_industry_coal_line$difference_variable<-"energy_demand_enfu_subsector_total_pj_inen_fuel_coal"
      cb_pollution_cost_industry_coal_line$output_display_name<-"Local Air Pollution"
      cb_pollution_cost_industry_coal_line$multiplier<-"cb_coal_ind_externalities"
      
      #Power natural gas
      cb_pollution_cost_power_natgas_line<-cb_pollution_cost_diesel_line
      cb_pollution_cost_power_natgas_line$difference_variable<-"nemomod_entc_annual_production_by_technology_pp_gas"
      cb_pollution_cost_power_natgas_line$output_display_name<-"Local Air Pollution"
      cb_pollution_cost_power_natgas_line$multiplier<-"cb_nat_gas_externalities"
      
      #Power coal
      cb_pollution_cost_power_coal_line<-cb_pollution_cost_diesel_line
      cb_pollution_cost_power_coal_line$difference_variable<-"nemomod_entc_annual_production_by_technology_pp_coal"
      cb_pollution_cost_power_coal_line$output_display_name<-"Local Air Pollution"
      cb_pollution_cost_power_coal_line$multiplier<-"cb_coal_externalities"
      
      #Power oil
      cb_pollution_cost_power_oil_line<-cb_pollution_cost_diesel_line
      cb_pollution_cost_power_oil_line$difference_variable<-"nemomod_entc_annual_production_by_technology_pp_oil"
      cb_pollution_cost_power_oil_line$output_display_name<-"Local Air Pollution"
      cb_pollution_cost_power_oil_line$multiplier<-"cb_oil_externalities"
      
      
      new_cost_lines<-append(new_cost_lines, list(cb_pollution_cost_diesel_line, 
                                                  cb_pollution_cost_gasoline_line,
                                                  cb_pollution_cost_industry_natgas_line,
                                                  cb_pollution_cost_industry_coal_line,
                                                  cb_pollution_cost_power_natgas_line,
                                                  cb_pollution_cost_power_coal_line,
                                                  cb_pollution_cost_power_oil_line))
    }
  }
  new_cost_lines<-do.call("rbind", new_cost_lines)
  cb_definitions<-rbind(cb_definitions, new_cost_lines)
  
  
  
  
  #--------Add automatic emissions calculations----------
  #Go through each row in the cost benefit definition file. 
  #If it is flagged to calculate costs, automatically generate a
  #new line in the file to include capital and operational electricty
  #production costs, and fuel costs
  new_cost_lines<-list()
  for (r in 1:num_rows){
    if (cb_definitions$summarize_ghg_effects[r] > 0) {
      cat(r, ": ",cb_definitions$strategy[r], " - adding ghg effects... \n")
      cb_ghg_effects_line<-cb_definitions[r,]
      cb_ghg_effects_line$include<-1
      cb_ghg_effects_line$summarize_energy_costs<-0
      cb_ghg_effects_line$summarize_air_pollution_costs<-0
      cb_ghg_effects_line$summarize_ghg_effects<-0
      
      cb_ghg_effects_line$test_id<-cb_definitions$test_id[r]
      cb_ghg_effects_line$output_display_name<-"Change in total emissions"
      
      cb_ghg_effects_line$comparison_id<-cb_definitions$comparison_id[r]
      cb_ghg_effects_line$difference_variable<-"emission_co2e_subsector_total_*"
      cb_ghg_effects_line$multiplier<-1
      cb_ghg_effects_line$multiplier.unit<-"N/A"
      cb_ghg_effects_line$annual.change<-1
      cb_ghg_effects_line$output_variable_name<-"change_in_emissions"
      cb_ghg_effects_line$sum=1
      cb_ghg_effects_line$country_specific_multiplier=0
      cb_ghg_effects_line$scale_impact=0
      cb_ghg_effects_line$scaling=""
      cb_ghg_effects_line$natural.multiplier.units="N/A"
      cb_ghg_effects_line$internal_notes="automatically generated ghg effects definition"
      cb_ghg_effects_line$display_notes="This is the total change in emissions (Co2e) for this strategy"
      
      new_cost_lines<-append(new_cost_lines, list(cb_ghg_effects_line))
    }
  }
  new_cost_lines<-do.call("rbind", new_cost_lines)
  cb_definitions<-rbind(cb_definitions, new_cost_lines)
  
  
  #---------Calculate Costs and Benefits----------
  #Go through each row in the cost benefit definition file. 
  #If it is flagged to be included, find all the variables that match the
  #variable in the definitions file and loop through cb_long for each
  cb_results<-list()
  strategy_id_list<-unique(data$strategy_id)
  num_rows<-nrow(cb_definitions)
  for (r in 1:num_rows){
    #Execute (1) or Skip (0) this line in the table
    if (cb_definitions$include[r] == 1){
      cat(r, ": ",cb_definitions$strategy[r], " - ",cb_definitions$output_variable_name[r], "\n")
      
      if (!cb_definitions$test_id[r] %in% strategy_id_list){
        cat("WARNING: test_id ", cb_definitions$test_id[r], " not found in dataset...moving on...\n")
        next
      }
      if (!cb_definitions$comparison_id[r] %in% strategy_id_list){
        cat("WARNING: comparison_id ", cb_definitions$comparison_id[r], " not found in dataset...moving on...\n")
        next
      }
      
      #Check for flags in the difference variable
      #FIXED_ANNUAL_COST means that a constant will be applied.
      if (cb_definitions$difference_variable[r] == "FIXED_ANNUAL_COST"){
        diff_var_list<-c(cb_definitions$difference_variable[r])
      }else{
        diff_var_list<-as.character(list_of_variables[grep(glob2rx(cb_definitions$difference_variable[r]), list_of_variables)])
      }
      result_tmp<-list()
      
      #For each variable that matches the substring...
      for (v in diff_var_list){
        #print(paste0('...difference_variable:', v))
        
        #Skip electricity costs because they are duplicative of other costs
        if (length(grep('^totalvalue_enfu_fuel_consumed_.*_fuel_electricity$', v))>0){
          cat("...Skipping ", v, "\n")
          next
        }
        
        #Skip crude because it is duplicative
        if (length(grep('totalvalue_enfu_fuel_consumed_entc_fuel_crude', v))>0){
          cat("...Skipping ", v, "\n")
          next
        }
        
        #Skip furnace gas because it is crazy buggy and extraneous
        if (length(grep(glob2rx('totalvalue*furnace_gas'), v))>0){
          cat("...Skipping ", v, "\n")
          next
        }
        
        #Skip fugitive emissions because it is crazy buggy and extraneous
        if (length(grep(glob2rx('emission_co2e_subsector_total_fgtv'), v))>0){
          cat("...Skipping ", v, "\n")
          next
        }
        
        
        #Call the function cb_long differently depending upon whether
        #the multipliers are a single number or a dataframe of country-specific multipliers
        if (cb_definitions$country_specific_multiplier[r]==1){
          #get the name of the table that has the country specific multipliers
          cat("...Country specific multiplier ", cb_definitions$multiplier[r], "\n")
          country_specific_multiplier<-eval(parse(text=cb_definitions$multiplier[r]))
          result<-cb_long(data, 
                          cb_definitions$test_id[r], 
                          cb_definitions$comparison_id[r], 
                          v,
                          cb_definitions$output_variable_name[r],
                          country_specific_multiplier,
                          cb_definitions$annual.change[r],
                          TRUE, cb_definitions$scale_impact[r], cb_definitions$scaling[r])
        }else{
          result<-cb_long(data, 
                          cb_definitions$test_id[r], 
                          cb_definitions$comparison_id[r], 
                          v,
                          cb_definitions$output_variable_name[r],
                          as.numeric(cb_definitions$multiplier[r]),
                          cb_definitions$annual.change[r],
                          FALSE,
                          cb_definitions$scale_impact[r], cb_definitions$scaling[r])
        }
        result$region<-as.character(result$region)
        #the function will give us the results using the test_id as the identifier
        #so we have to replace it by the primary id manually
        result$strategy_id<-cb_definitions$strategy_id[r]
        
        #If we are supposed to sum up all the variables later (sum==1)
        #Then store them in a temporary list for now
        #else add them to the main list now
        if (cb_definitions$sum[r]==1){
          result_tmp<-append(result_tmp, list(result))
        } else {
          cb_results<-append(cb_results, list(result))
        }
        
      }
      
      #If flagged, sum up the variables in value and difference_value columns
      #Create a new output data frame and append it to the existing list
      #Note that the difference variable may be garbage if we are summing across different comparison variables
      if (cb_definitions$sum[r]==1){
        print('...Combining prior variables')
        result_tmp<-do.call("rbind", result_tmp)
        result_tmp_diff_agg<-aggregate(result_tmp$difference_value, list(result_tmp$region, result_tmp$time_period, result_tmp$strategy_id), sum)
        result_tmp_agg<-aggregate(result_tmp$value, list(result_tmp$region, result_tmp$time_period, result_tmp$strategy_id), sum)
        colnames(result_tmp_diff_agg)<-c('region', 'time_period', 'strategy_id', 'difference_value')
        colnames(result_tmp_agg)<-c('region', 'time_period', 'strategy_id', 'value')
        
        aggregated_result<-result_tmp_diff_agg[,1:3]
        
        aggregated_result$difference_variable<-cb_definitions$difference_variable[r]
        aggregated_result$difference_value<-result_tmp_diff_agg$difference_value
        aggregated_result$variable<-cb_definitions$output_variable_name[r]
        aggregated_result$value<-result_tmp_agg$value
        cb_results<-append(cb_results, list(aggregated_result))
        
      }
      
    }
  }
  
  
  #----------- Append it all together in a dataframe and write the output ---------------
  cb_results<-do.call("rbind", cb_results)
  write.csv(cb_results, outfilename)
  write.csv(cb_definitions, outfilename_defs)
  
  return(cb_results)
}
