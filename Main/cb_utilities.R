#Cost Benefit Utility Functions

library(reshape)
library(stringr)


#-------calculate cost or benefit as a scalar applied to a variable in a single policy------------
#This function calculates costs and benefits as just a scalar applied to a variable within
# a single strategy. It uses code from cb_difference_between_two_strategies, so the use of
# data_merged, for example, is holdover from that function.
cb_scale_variable_in_strategy<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                        output_mults, change_in_multiplier, country_specific_multiplier,
                                        scale_impact, scaling, list_of_variables){
  
  
  id_vars <-c('region','time_period', 'strategy_code') #primary_id->strategy_code
  
  diff_var_name<-paste0("diff_", diff_var)
  datap_tx<-subset(data,strategy_code==strategy_code_tx & variable==diff_var) #primary_id->strategy_code
  
  datap_tx$region<-as.character(datap_tx$region)
  datap_tx$variable<-as.character(datap_tx$variable)
  
  #This is code copied over from another function, so data_merged is just datap_tx
  data_merged<-datap_tx
  data_merged$difference<-data_merged$value
  data_merged$time_period_for_multiplier_change<-data_merged$time_period
  data_merged$values<-t(t(data_merged$difference)*output_mults*change_in_multiplier^data_merged$time_period_for_multiplier_change)  
  
  tmp<-data_merged[,c(id_vars)]
  tmp$difference_variable<-diff_var_name
  tmp$difference_value<-data_merged$difference
  tmp$variable<-output_vars
  tmp$value<-data_merged$values
  output<-tmp
  
  return(output)
  
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


cb_difference_between_two_strategies<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                                               output_mults, change_in_multiplier, country_specific_multiplier,
                                               scale_impact, scaling, list_of_variables){
  
  id_vars <-c('region','time_period', 'strategy_code') #primary_id->strategy_code
  
  #create output table
  if (diff_var == "FIXED_ANNUAL_COST"){
    cat(r, "...........Dealing with Fixed annual costs...\n")
    diff_var<-"nemomod_entc_annual_production_by_technology_pp_solar" #use this as a dummy variable
    diff_var_name<-"No Difference Variable"
    datap_base<-subset(data, strategy_code==strategy_code_base & variable==diff_var) #primary_id->strategy_code
    datap_tx<-subset(data,strategy_code==strategy_code_tx & variable==diff_var) #primary_id->strategy_code
    datap_base$variable<-"Fixed Annual Cost"
    datap_tx$variable<-"Fixed Annual Cost"
    datap_base$value<-0
    datap_tx$value<-1
    datap_tx$value[datap_tx$time_period<10]<-0 #Don't start the fixed annual cost until 2025
  }else{
    diff_var_name<-paste0("diff_", diff_var)
    datap_base<-subset(data, strategy_code==strategy_code_base & variable==diff_var) #primary_id->strategy_code
    datap_tx<-subset(data,strategy_code==strategy_code_tx & variable==diff_var) #primary_id->strategy_code
  }
  
  datap_base$region<-as.character(datap_base$region)
  datap_base$variable<-as.character(datap_base$variable)
  datap_tx$region<-as.character(datap_tx$region)
  datap_tx$variable<-as.character(datap_tx$variable)
  datap_base$strategy_code<-NULL #primary_id->strategy_code
  datap_base$primary_id<-NULL
  
  colnames(datap_base)<-gsub('value', 'value_base', colnames(datap_base))
  data_merged<-Reduce(function(...) merge(...,), list( datap_tx,datap_base))
  
  #Calculate the differene in variables and then apply the multiplier, which may change over time
  #Assume cost change only begins NOW
  #Assume costs are 2020.
  data_merged$difference<-data_merged$value-data_merged$value_base
  data_merged$time_period_for_multiplier_change<-data_merged$time_period
  
  
  data_merged$values<-t(t(data_merged$difference)*output_mults*change_in_multiplier^data_merged$time_period_for_multiplier_change)  
  
  tmp<-data_merged[,c(id_vars)]
  tmp$difference_variable<-diff_var_name
  tmp$difference_value<-data_merged$difference
  tmp$variable<-output_vars
  tmp$value<-data_merged$values
  output<-tmp
  
  return(output)
}


#-------wrapper for cblong that calls cblong on each variable that matches--------

cb_wrapper<-function(cb_function, data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                     output_mults, change_in_multiplier, country_specific_multiplier,
                     scale_impact, scaling, sum_results, list_of_variables){
  

  result_tmp<-list()  
  #Get all the variables that match the difference variable
  diff_var_list<-as.character(list_of_variables[grep(glob2rx(diff_var), list_of_variables)])
  
  #For each variable that matches the substring, calculate the costs and benefits
  for (v in diff_var_list){
    
    do_call_args<-list(data, 
                       strategy_code_tx, 
                       strategy_code_base, 
                       v,
                       output_vars,
                       as.numeric(output_mults),
                       change_in_multiplier,
                       country_specific_multiplier,
                       scale_impact, scaling, list_of_variables)
    result<-do.call(cb_function, do_call_args)
    
    result$region<-as.character(result$region)
    result$strategy_code<-strategy_code_tx
    result_tmp<-append(result_tmp, list(result))
  }
  
  #If flagged, sum up the variables in value and difference_value columns
  #Create a new output data frame and append it to the existing list
  #Note that the difference variable may be garbage if we are summing across different comparison variables
  if (sum_results==1){
    print('...Combining prior variables')
    
    #create one long dataset
    result_tmp<-do.call("rbind", result_tmp)
    
    #summarize the results by region, time period, and strategy code
    #while summing up hte values and difference values
    results_summarized<-result_tmp %>% 
      group_by(region, time_period, strategy_code) %>% 
      summarise(value = sum(value), difference_value = sum(difference_value))
    
    #readd the difference variable and variable fields
    results_summarized$difference_variable<-paste0('diff_', diff_var)
    results_summarized$variable<-output_vars
    return(results_summarized)
    
  }else{
    appended_results<-do.call("rbind", result_tmp)
    return(appended_results)
  }
}


#-------calculate costs and benefits specified in a file of cost factors-------------
calculate_costs_and_benefits_from_cost_factors<-function(data, definition, cost_factors, list_of_variables_in_dataset){
  
  cb_results<-list()
  num_rows<-nrow(cost_factors)
  
  #loop through each row in the factors file and calculate the costs and benefits specified
  for (r in 1:num_rows){
    cost_line<-cost_factors[r,]  
    message(paste0('---------Costs for: ', cost_line$output_variable_name))
    result<-cb_wrapper('cb_difference_between_two_strategies',
                       data, 
                       definition$strategy_code, 
                       definition$comparison_code, 
                       cost_line$difference_variable,
                       cost_line$output_variable_name,
                       as.numeric(cost_line$multiplier),
                       cost_line$annual.change,
                       FALSE,
                       0, 0, 0,
                       list_of_variables_in_dataset)
    
    cb_results<-rbind(cb_results, result)
  }
  return(cb_results)
}





#--------Calculate costs and benefits defined in an input file-------------------
# This code loops through a table where each row defines a cost-benefit calculations
# At its most basic, it finds the difference between variables in a transformed vs. baseline future
# And applies a multiplier to that difference (e.g., the difference in natural gas consumption * externality/unit of consumption)
# The code is flexible to allow for
# -- increases or decreases in the variable over time
# -- matching several variables at a time using a substring of the variable name, e.g, nemomod_entc_discounted_*
# -- summing up the results when multiple variables are matched
# -- using country specific-multipliers
# -- omitting some lines in the table

#-------Columns in the definitions csv table THIS IS OLD INFORMATION-----------
#transformation_name: name of the transformation, for human readability
#type: flag for which function to call, currently unused
#strategy_code: strategy code associated with this row
#test_id: strategy code associated with
#comparison_id: strategy_code id against which to compare
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



#--------------Calculate Costs and Benefits--------------

#This function calculates costs and benefits from main and strategy specific definitions
calculate_costs_and_benefits<-function(data, main_definitions, strategy_specific_definitions){
  
  list_of_variables<-unique(data$variable)
  list_of_strategies_in_data<-unique(data$strategy_code)
  results<-list()
  
  if (!missing(main_definitions)){
    ndefs<-nrow(main_definitions)
    
    #read the cost factors
    cost_factors<-colnames(main_definitions)[str_detect(colnames(main_definitions), 'calculation')]
    cost_factor_filenames<-paste0(ssp_costs_benefits_git_path, 'cost_factors/', str_replace(cost_factors, 'calculation', 'factors.csv'))
    ncost_factors<-length(cost_factors)
    cost_factors_list<-lapply(cost_factor_filenames,fread)
    
    #Part 1: get the cost factors results in the main definition file by...
    #...for each strategy (defined in the definitions file)
    for (d in 1:ndefs){
      
      #...if that strategy should be evaluated and is in the results dataset...
      #(skip if not)
      if ((main_definitions$include[d] != 1) & 
          (!main_definitions$strategy_code[d] %in% list_of_strategies_in_data)){
        next
      }
        
        
      
      #...and for each cost_factor column in that file
      for (c in 1:ncost_factors){
        
        #....if the cost factor is supposed to be evaluated
        if (main_definitions[d,cost_factors[c]]==1){
          message(paste0('Cost Factor Loop --- strategy: ', main_definitions$strategy_code[d], '  cost factor: ', cost_factors[c]))
          
          #....evaluate it and append the results
          r<-calculate_costs_and_benefits_from_cost_factors(data, 
                                                            main_definitions[d,], 
                                                            data.frame(cost_factors_list[c]), 
                                                            list_of_variables)
          results<-append(results, list(r))
        }
      }
    }
  }
  
  #Part 2: get the strategy_specific results in the strategy_specific definition file by... 
  if (!missing(strategy_specific_definitions)){
    nstrat<-nrow(strategy_specific_definitions)
    
    #looping through each row
    for (s in 1:nstrat){
      
      #if it is to be included and is in the data....
      if ((strategy_specific_definitions$include[s]==1) & 
          (strategy_specific_definitions$strategy_code[s] %in% list_of_strategies_in_data)){
        message(paste0('Strategy Specific Costs --- strategy: ', 
                       strategy_specific_definitions$strategy_code[s], 
                       '  variable: ', 
                       strategy_specific_definitions$output_variable_name[s]))
        
        #call the wrapper function and append
        r<-cb_wrapper(strategy_specific_definitions$cb_function[s],
                      data,
                      strategy_specific_definitions$test_id[s],
                      strategy_specific_definitions$comparison_id[s],
                      strategy_specific_definitions$difference_variable[s],
                      strategy_specific_definitions$output_variable_name[s],
                      as.numeric(strategy_specific_definitions$multiplier[s]),
                      strategy_specific_definitions$annual.change[s],
                      FALSE,
                      strategy_specific_definitions$scale_impact[s],
                      strategy_specific_definitions$scaling[s],
                      strategy_specific_definitions$sum[s],
                      list_of_variables)
        results<-append(results, list(r))
      }
    }
  }
  #append all the outputs
  cb_results<-do.call("rbind", results)
  
  #boom. done.
  return(cb_results)
}











