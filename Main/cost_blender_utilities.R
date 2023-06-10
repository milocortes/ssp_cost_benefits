#This file contains all the for cost blending but has not yet been ported
#from nidhi's computer

#-------calculate an LHS sample of futures related to strategy effectiveness, costs, and benefits-----------
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
  variable_names<-paste0(futures_definitions$strategy_code,":",futures_definitions$variable)
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
  futures_df<-separate(futures_df, col=variable, into=c('strategy_code', 'variable'), sep=':')
  
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
  merged_data$difference_variable<-paste0('change_in_emissions:', merged_data$strategy_code.x)
  merged_data$difference_value<-merged_data$rel_difference_value
  merged_data$value<-merged_data$rel_value
  colnames(merged_data)[colnames(merged_data)=='strategy_code.y']<-'strategy_code'
  
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