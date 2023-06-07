#This code fully demonstrates how we can calculate the costs
#and benefits of a portfolio of multiple strategies under uncertainty
#by calculating the costs, benefits, and emissions effects of individual strategies
#and creating a weighted cost of the portfolio

#It requires the following files:
#R files with code
#1. This file Energy Cost Effectiveness and Relative Effect.R which is the master script
#2. CBUtil_energy.R contains several utility functions
#3. SSP_utilities.R contains several utility functions

#Model output files that James provides with every run
#4. A file that contains the model results (e.g., summary_energy_results.csv)
#5. ATTRIBUTE_PRIMARY.csv 
#6. ATTRIBUTE_STRATEGY.csv

#Cost benefit definition file
#7. A file that defines how costs and benefits should be calculated (e.g., CB_definitions_cost_blender_SCOE_test.csv)
#8. A file that defines the futures (e.g., )

#The code below illustrates this for the technical costs of three SCOE strategies (3025, 3027, and 3029)
#and calculates a blended technical cost for the ALL strategy (3023) based on the results and a simple experimental design

#Note that the function "calculate_costs_and_benefits" can be used to calculate any number of different costs and benefits
#and has special features/hacks to do constant costs and benefits, costs and benefits that chagne over time, etc.
#that function probably needs to be cleaned up a bunch as right now it is too damn big.

setwd(r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\Cost Benefit Files for Edmundo 5-23\Cost Benefit Files for Edmundo 5-23\)")
library(reshape)
library("readxl")
library(dplyr)
library("stringr")
library(lhs)
library(tidyr)

source("CBUtil_energy.R")
source("SSP Utilities.R")

#Files and file names
filename<-'summary_energy_results.csv' #james latest 
definition_file<-'CB_definitions_cost_blender_SCOE_test.csv'
futures_filename<-'SCOE LHS Test.csv'

output.file<-read.csv(filename)
data<-output.file
run_attributes<-ssp_merge_run_attributes()
merged_data<-merge(data, run_attributes[,1:2], by=c('primary_id'), x.all=TRUE)
data<-merged_data


#Variables
emissions_variable <- "change_in_emissions"
effect_variable<-"technical_effect"
cost_variable<-"cost"
integrated_strategy_id<-3023
list_of_strategies_to_blend <- c(3025, 3027, 3029)


#-----------Calculate costs and benefits of the full set of individual strategies---------
outfilename<-paste0("cost_blender_", filename)
outfilename_defs<-paste0("cost_blender_definitions_", filename)
cb_data<-calculate_costs_and_benefits(data, definition_file, outfilename, outfilename_defs)

#-------------Calculate the cost effectiveness and relative effect of each contributing strategy-----------------
#Get the costs and emissions data of the contributing strategies 
technical_cost<-cb_data[cb_data$variable!=emissions_variable & cb_data$strategy_id %in% list_of_strategies_to_blend,][c(1:3, 5, 7)]
emissions<-cb_data[cb_data$variable==emissions_variable & cb_data$strategy_id %in% list_of_strategies_to_blend,][c(1:3,5)]

#Rename the columns so they are sensible
colnames(technical_cost)[colnames(technical_cost)=='difference_value']<-effect_variable
colnames(technical_cost)[colnames(technical_cost)=='value']<-cost_variable
colnames(emissions)[colnames(emissions)=='difference_value'] <- emissions_variable


#merge the costs and emissions datasets
cost_and_emissions<-merge(technical_cost, emissions, by=c('region', 'time_period', 'strategy_id'), all.x=TRUE)

#calculate the cost effectiveness of the individual strategies 
cost_effectiveness_results<-cost_effectiveness(cost_and_emissions)

#downselect only relevant columns
cost_effectiveness_results<-subset(cost_effectiveness_results, select = c(region,time_period,strategy_id,
                                                                          abatement_cost_effectiveness,relative_effect))



#------------Calculate costs of the integrated portfolio strategy-----------
integrated_strategy<-cb_data[cb_data$strategy_id==integrated_strategy_id & cb_data$variable==emissions_variable,]
blended_costs<-blend_costs(integrated_strategy, cost_effectiveness_results)
blended_costs$variable<-cost_variable

#-------------Calculate costs of the integrated strategy under uncertainty--------------
num_futures<-10
futures<-create_futures(futures_filename, num_futures)

#create the dataset wtih baseline cost effectiveness and relative effects, and scalars
cost_effectiveness_with_futures<-merge(cost_effectiveness_results, futures, by='strategy_id')

#rescale the cost effectiveness (1/cost_scalar)
cost_effectiveness_with_futures$rescaled_cost_effectiveness<-cost_effectiveness_with_futures$abatement_cost_effectiveness/cost_effectiveness_with_futures$cost_scalar


#rescale the effect size and normalize
cost_effectiveness_with_futures$rescaled_effect<-cost_effectiveness_with_futures$relative_effect*cost_effectiveness_with_futures$effect_scalar
cost_effectiveness_with_futures<-cost_effectiveness_with_futures %>% group_by(region, time_period, future_id) %>%
  mutate(total_rescaled_effect=sum(rescaled_effect), rescaled_relative_effect=rescaled_effect/total_rescaled_effect)


#columns to keep
columns_to_keep<-c('strategy_id', 'time_period', 'region', 'future_id',
                                                             'rescaled_cost_effectiveness', 'rescaled_relative_effect')
cost_effectiveness_future_results<-cost_effectiveness_with_futures[, columns_to_keep]
colnames(cost_effectiveness_future_results)[colnames(cost_effectiveness_future_results)=='rescaled_relative_effect']<-'relative_effect'
colnames(cost_effectiveness_future_results)[colnames(cost_effectiveness_future_results)=='rescaled_cost_effectiveness']<-'abatement_cost_effectiveness'

#Blend with futures
blended_costs_with_futures<-blend_costs(integrated_strategy, cost_effectiveness_future_results)
blended_costs_with_futures$variable<-cost_variable
write.csv(file = 'blended_costs_with_futures_SCOE_EXAMPLE.csv', blended_costs_with_futures)


