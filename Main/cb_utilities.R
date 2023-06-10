#Cost Benefit Utility Functions

library(reshape)
library(stringr)

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


cb_long<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                  output_mults, change_in_multiplier, country_specific_multiplier,
                  scale_impact, scaling){
  
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

cb_wrapper<-function(data, strategy_code_tx, strategy_code_base, diff_var, output_vars, 
                     output_mults, change_in_multiplier, country_specific_multiplier,
                     scale_impact, scaling, sum_results, list_of_variables){
  
  result_tmp<-list()
  
  #Get all the variables that match the difference variable
  diff_var_list<-as.character(list_of_variables[grep(glob2rx(diff_var), list_of_variables)])
  
  #For each variable that matches the substring, calculate the costs and benefits
  for (v in diff_var_list){
    
    result<-cb_long(data, 
                    strategy_code_tx, 
                    strategy_code_base, 
                    v,
                    output_vars,
                    as.numeric(output_mults),
                    change_in_multiplier,
                    country_specific_multiplier,
                    scale_impact, scaling)
    
    result$region<-as.character(result$region)
    result$strategy_code<-strategy_code_tx
    result_tmp<-append(result_tmp, list(result))
  }
  
  #If flagged, sum up the variables in value and difference_value columns
  #Create a new output data frame and append it to the existing list
  #Note that the difference variable may be garbage if we are summing across different comparison variables
  if (sum_results==1){
    print('...Combining prior variables')
    result_tmp<-do.call("rbind", result_tmp)
    result_tmp_diff_agg<-aggregate(result_tmp$difference_value, list(result_tmp$region, result_tmp$time_period, result_tmp$strategy_code), sum)
    result_tmp_agg<-aggregate(result_tmp$value, list(result_tmp$region, result_tmp$time_period, result_tmp$strategy_code), sum)
    colnames(result_tmp_diff_agg)<-c('region', 'time_period', 'strategy_code', 'difference_value')
    colnames(result_tmp_agg)<-c('region', 'time_period', 'strategy_code', 'value')
    
    aggregated_result<-result_tmp_diff_agg[,1:3]
    
    aggregated_result$difference_variable<-cost_line$difference_variable
    aggregated_result$difference_value<-result_tmp_diff_agg$difference_value
    aggregated_result$variable<-cost_line$output_variable_name[s]
    aggregated_result$value<-result_tmp_agg$value
    return(aggregated_result)
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
    result<-cb_wrapper(data, 
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

#-------Columns in the definitions csv table-----------
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



calculate_costs_and_benefits_old<-function(data, definition_filename, outfilename, outfilename_defs){
  
  
  list_of_variables<-unique(data$variable)
  
  
  
  
  #--------Read a cost-benefit definition file and output all the transformation results--------
  cb_definitions<-read.csv(definition_filename, stringsAsFactors = FALSE)
  num_rows<-nrow(cb_definitions)
  
  #---------Calculate Costs and Benefits----------
  #Go through each row in the cost benefit definition file. 
  #If it is flagged to be included, find all the variables that match the
  #variable in the definitions file and loop through cb_long for each
  cb_results<-list()
  strategy_code_list<-unique(data$strategy_code)
  num_rows<-nrow(cb_definitions)
  for (r in 1:num_rows){
    #Execute (1) or Skip (0) this line in the table
    if (cb_definitions$include[r] == 1){
      cat(r, ": ",cb_definitions$strategy[r], " - ",cb_definitions$output_variable_name[r], "\n")
      
      if (!cb_definitions$test_id[r] %in% strategy_code_list){
        cat("WARNING: test_id ", cb_definitions$test_id[r], " not found in dataset...moving on...\n")
        next
      }
      if (!cb_definitions$comparison_id[r] %in% strategy_code_list){
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
        result$strategy_code<-cb_definitions$strategy_code[r]
        
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
        result_tmp_diff_agg<-aggregate(result_tmp$difference_value, list(result_tmp$region, result_tmp$time_period, result_tmp$strategy_code), sum)
        result_tmp_agg<-aggregate(result_tmp$value, list(result_tmp$region, result_tmp$time_period, result_tmp$strategy_code), sum)
        colnames(result_tmp_diff_agg)<-c('region', 'time_period', 'strategy_code', 'difference_value')
        colnames(result_tmp_agg)<-c('region', 'time_period', 'strategy_code', 'value')
        
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


#--------------Calculate Costs and Benefits--------------

#This function calculates costs and benefits from main and strategy specific definitions
calculate_costs_and_benefits<-function(data, main_definitions, strategy_specific_definitions){
  
  list_of_variables<-unique(data$variable)
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
      
      #...if that strategy should be evaluated...
      if (main_definitions$include[d] == 0)
        next
      
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
      
      #if it is to be included....
      if (strategy_specific_definitions$include[s]==1){
        message(paste0('Strategy Specific Costs --- strategy: ', 
                       strategy_specific_definitions$strategy_code[s], 
                       '  variable: ', 
                       strategy_specific_definitions$output_variable_name[s]))
        
        #call the wrapper function and append
        r<-cb_wrapper(data,
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











