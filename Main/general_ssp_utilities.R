#General Si-SePuede Utilities

ssp_merge_run_attributes<-function(primary_filename, attribute_filename){
  primary_attributes<-read.csv(primary_filename)
  strategy_attributes<-read.csv(attribute_filename)
  merged_attributes<-merge(primary_attributes, strategy_attributes, by="strategy_id", all.x=TRUE)
  return(merged_attributes)
}

ssp_calculate_gdp_per_capita<-function(){
  gdp_mmm_usd<-read.csv("~/Desktop/LAC_Decarb_Git/sisepuede_data/SocioEconomic/gdp_mmm_usd/input_to_sisepuede/historical/gdp_mmm_usd.csv")
  rural_pop<-read.csv("~/Desktop/LAC_Decarb_Git/sisepuede_data/SocioEconomic/population_gnrl_rural/input_to_sisepuede/historical/population_gnrl_rural.csv")
  urban_pop<-read.csv("~/Desktop/LAC_Decarb_Git/sisepuede_data/SocioEconomic/population_gnrl_urban/input_to_sisepuede/historical/population_gnrl_urban.csv")
  merge_data<-merge(rural_pop, urban_pop, by=colnames(urban_pop)[1:3], all.x=TRUE, all.y=TRUE)
  merge_data<-merge(merge_data, gdp_mmm_usd, by=colnames(merge_data)[1:3])
  merge_data$gdp_per_cap<-merge_data$gdp_mmm_usd*10^9/(merge_data$population_gnrl_rural+merge_data$population_gnrl_urban)
  return(merge_data)
}

ssp_LAC_iso3<-function(){
  a<-c("ARG", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL",
           "CRI", "ECU", "SLV", "GTM", "GUY", "HTI", "HND", "JAM",
           "MEX", "NIC", "PAN", "PER", "PRY", "DOM", "SUR", "TTO", "URY", "VEN")
  return(a)
}


#Remove the discount rate from a levelized capital cost assuming
#(1) all capital cost C is incurred in the first year
#(2) the same amount of output S is produced each year for the lifetime of the plant
#(3) costs are not discounted in the first year, i.e., the first time period is 0, not 1
#Then, 
#let L be the stated levelized cost including discount rate
#let d be the discount rate
#let n be the number of time periods
#The result is C/nS

ssp_undiscount_levelized_capital_cost<-function(L, d, n){
  r<-1/(1+d)
  r_series<-(1-r^n)/(1-r)
  return(L/n*r_series)
}

#This function calculates the decline in  levelized cost of a
#pool of technology, given that the newest version of that technology
#declines to fraction_of_initial after time_periods steps.
#That is, suppose that the cost of new heat pumps declines over time.
#Then, the levelized cost of heat provided by those brand new heat pumps will
#decline by that same rate, but the levelized cost of the entire pool of heat pumps
#will be the geometric average of the costs of new technology over time.
ssp_calculate_average_decay_term<-function(time_periods, fraction_of_initial){
  
  #relative_cost_of_new_technology = decay_term^(time_window-1)
  #we know time window and relative cost, so solve for decay term
  decay_term<-fraction_of_initial^(1/(time_window-1))

  #But, that is the cost of new technology in the year time_window
  #the average cost of technology in that period is higher to include
  # the cost of older technology.
  # We solve for this with the average cost of the time_window terms
  # of the geometric progression, 
  # S_n = [(r^n – 1)/(r – 1)] if r ≠ 1 and r > 1
  # where S_n is the sum of the first n terms
  # r is the decay term
  # n is the time period
                                   
  sum_n <- (decay_term^time_window - 1)/(decay_term-1)
  average_as_fraction_of_initial <- sum_n/time_window
  
  
  #Then we find the decay term that would result in the average cost above
  # at the end of our window
  decay_term_average <- average_as_fraction_of_initial^(1/(time_window-1))
  
  return(decay_term_average)
  
}

#energy_consumption_inen_cement
#marginal_fuel_cost
ssp_spot_check_inen<-function(data, strategy, variable){
  a<-data[data$region=='brazil' & data$time_period==35 & data$strategy_code==strategy & data$variable==variable,]
  return(a)
}
    
  
