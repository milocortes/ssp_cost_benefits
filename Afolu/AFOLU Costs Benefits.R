setwd("/Users/nidhi/Downloads/AFOLU Tableaus/")
library(reshape)
library("readxl")
library(dplyr)
source("CBUtil.R")

output.file<-read.csv("output_afolu_all_long.csv")
data<-output.file
datacolnames<-colnames(data)
nations <- unique(data$Nation)
numnations <-length(nations)
id_vars <-c('Nation','time_period','policy', 'policy_name')
variable_names<-variable_names<-unique(data$variable)
emissions_vars <- as.character(variable_names[grepl('emission_co2e_subsector', variable_names)])
target_var_placeholder<-'pop_lvst_buffalo'


#--------- 0. Baseline NYE -------
#Calculate crop and livestock values and ecosystem services
policy_tx<-0
cb0_baseline<-data[data$policy==policy_tx & data$variable=='pop_lvst_buffalo',id_vars]
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
cb0_baseline<-cb_crop_livestock_value(livestock_crop_var_list, 'value_crop_lvst_',
                                      data, policy_tx, cb0_baseline, 1)



#--------- 1. Rice Cultivation --------
#Multiply the hectares of rice cultivated with rice management by the cost factor
print('1. Rice Cultivation')
policy_tx<-1
cb1_rice_management<-cb_rice_management(data, policy_tx)

#--------- 2. Accelerate Crop Productivity NEED COST OMIT -----
#Calculate the additional crops produced and multiply by price
# print('2. Accelerate Crop Productivity')
# policy_tx <-2
# policy_base<-0
# cb_crop_prod<-data[data$policy==policy_tx & data$variable==target_var,id_vars]
# variable_names<-unique(data$variable)
# livestock_crop_var_list <- as.character(variable_names[grepl('yield_agrc', variable_names)])
# cb2_accelerated_crops<-cb_crop_livestock(livestock_crop_var_list, 'benefit_addedrevenue_', 
#                                             data, policy_tx, policy_base, cb_crop_prod,1,1)
# cb2_accelerated_crops$cost_technical_improve_crop_productivity<-10*10^6 #annual productivity cost placeholder

#--------- 3. Accelerate Livestock Productivity NEED COST OMIT -----
# print('3.Accelerate Livestock Productivity ')
# policy_tx <-3
# policy_base<-0
# cb_lvst_prod<-data[data$policy==policy_tx & data$variable==target_var,id_vars]
# variable_names<-unique(data$variable)
# livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names)])
# cb3_accelerated_livestock<-cb_crop_livestock(livestock_crop_var_list, 'benefit_addedrevenue_', 
#                                    data, policy_tx, policy_base, cb_lvst_prod, 1, 1)
# cb3_accelerated_livestock$cost_technical_improve_lvst_productivity<-10*10^6 #annual cost placeholder

#--------- 4. New Diets -----
print('4. New Diets')
#calculate the number of additional people using better diets
#for each such person, there is a $370 cost savings in groceries and 
#$1000/yr cost savings in health

policy_tx <-4 #New Diets
pop<-read.csv('pop_all_future_with_urban_interpolation.csv')
pop$total<-pop$population_gnrl_urban+pop$population_gnrl_rural
#frac_veg<-seq(0, 30, length.out=36)/100
#rep(frac_veg, numnations)
veg_tx_def<-read.csv("transformation_definitions/Improving_Diets.csv")
veg_tx_def<-veg_tx_def[veg_tx_def$TransformationName=='full',]
veg_tx_def$frac_vegetarians<-(1-veg_tx_def$frac_gnrl_eating_red_meat)

pop_veg<-pop$total*veg_tx_def$frac_vegetarians


target_var <-'area_agrc_crops_rice' #use a random variable

diet_data<-data[data$policy==policy_tx & data$variable==target_var, id_vars]
cb4_diet<-diet_data[,id_vars]
cb4_diet$diff_popvegetarians<-pop_veg
cb4_diet$benefit_grocerysavings<-pop_veg*370
cb4_diet$benefit_healthsavings<-pop_veg*1000


#--------- 5. Enteric Fermentation ------
print('5. Enteric Fermentation')
policy_tx<-5
cb5_ent_ferm<-cb_enteric_fermentation(data, policy_tx)

#--------- 6. Conservation Agriculture ------------
print('6. Cons Ag')
policy_tx <-6
cb6_cons_ag<-cb_conservation_agriculture(data, policy_tx)

#--------- 7. Reducing Food Loss -----
print('7. Food Loss')
#(1) Change in export times value of food
policy_tx <-7
policy_base<-0
variable_names<-unique(data$variable)
livestock_crop_var_list<- as.character(variable_names[grepl('net_imports_agrc', variable_names) | grepl('net_imports_change_lvst', variable_names)])
cb7_foodloss <-data[data$policy==policy_tx & data$variable==variable_names[1], id_vars]
value_multiplier<-(-1)
cb7_foodloss<-cb_crop_livestock(livestock_crop_var_list, 'change_revenue_croplvst_', 
                               data, policy_tx, policy_base, cb7_foodloss, -1,1)

cb7_foodloss$cost_technical<-cb7_foodloss$diff_livestock_crop_total*1200*(-1)


#--------- 8. Manure Management OMIT -----
#--------- 9. Silvopasture -----
print('9. Silvopasture')
policy_tx <-9
policy_base<-0

#(1) Ecosystem services of new secondary forest
diff_var <-'area_lndu_forests_secondary'
output_var_name<-('benefit_ecosystemservices')
multiplier<-(2500)
cb9_silvopasture<-cb(data, policy_tx, policy_base, diff_var, 
             output_var_name, multiplier)

#(2) Cost of creating new secondary forest
cb9_silvopasture$cost_technical<-cb9_silvopasture$diff_area_lndu_forests_secondary*(31)

#(3) Annual benefit of additional livestock (only - no crops)
variable_names<-unique(data$variable)
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names)])
cb9_silvopasture<-cb_crop_livestock(livestock_crop_var_list, 'change_revenue_croplvst', 
                                   data, policy_tx, policy_base, cb9_silvopasture,1,1)

#--------- 10. Rehabilitating Land -------------
print('10. Rehab Land')
policy_tx <-10
policy_base<-0

#(1) Ecosystem services of new secondary forest
diff_var <-'area_lndu_forests_secondary'
output_var_name<-paste('benefit_ecosystemservices_', diff_var, sep='')
multiplier<-(2500)
cb10_rehab<-data[data$policy==policy_tx & data$variable==target_var_placeholder,id_vars]
cb10_rehab<-cb(data, policy_tx, policy_base, diff_var, 
             output_var_name, multiplier)

#(2) Technical cost of rehabilitation
cb10_rehab$cost_technical<-cb10_rehab$diff_area_lndu_forests_secondary*80

#(3) Annual cost of foregone livestock
variable_names<-unique(data$variable)
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])

cb10_rehab<-cb_crop_livestock(livestock_crop_var_list, 'change_revenue_croplvst', 
                                    data, policy_tx, policy_base, cb10_rehab,1,1)





#--------- 11. Reduce Excess Fertilizer Use -----------
#Calculate difference in demand for synthetic fertilizer relative to baseline
#Apply costs and benefits
print('11. Reduce Excess Fert')
policy_tx <-11
policy_name<-'reduce_excess_fertilizer'
policy_base<-0
diff_var <-'demand_soil_synthetic_fertilizer_n_kt'


#output_var_name<-paste('benefit_fertilizersaved_', policy_name, sep='')
output_var_name<-'benefit_fertilizersaved'
multiplier<-(-325*1000) #cost per kiloton
cb11_excess_fertilizer<-data[data$policy==policy_tx & data$variable==target_var_placeholder,id_vars]
cb11_excess_fertilizer<-cb(data, policy_tx, policy_base, 
                         diff_var, output_var_name, multiplier)

#benefit_var<-paste('benefit_avoidednitrate_', policy_name, sep='')
cb11_excess_fertilizer$benefit_avoidednitrate<-cb11_excess_fertilizer$diff_demand_soil_synthetic_fertilizer_n_kt*(-101*1000) #benefit per kiloton


#--------- 12. Improve Fertilizer Application -------

#Calculate difference in demand for synthetic fertilizer relative to baseline
#Apply costs and benefits
print('12. Improve Fert')

policy_tx <-12
policy_name<-'improve_fertilizer_app'
policy_base<-0
diff_var <-'demand_soil_synthetic_fertilizer_n_kt'
#output_var_name<-paste('benefit_fertilizersaved_', policy_name, sep='')
output_var_name<-'benefit_fertilizersaved'
multiplier<-(-325)*1000

cb12_better_fertilizer<-data[data$policy==policy_tx & data$variable==target_var_placeholder,id_vars]
cb12_better_fertilizer<-cb(data, policy_tx, policy_base, 
                         diff_var, output_var_name, multiplier)
#cost_var<-paste('cost_technical_', policy_name, sep='')
#benefit_var<-paste('benefit_avoidednitrate_', policy_name, sep='')
cb12_better_fertilizer['cost_technical']<-cb12_better_fertilizer$diff_demand_soil_synthetic_fertilizer_n_kt*(-23)*1000
cb12_better_fertilizer['benefit_avoidednitrate']<-cb12_better_fertilizer$diff_demand_soil_synthetic_fertilizer_n_kt*(-101)*1000



#--------- 14. Ending Deforestation --------

print('14. End Deforestation')
policy_tx <-14
policy_base<-0
target_var<-'pop_lvst_buffalo'
#set up the database
cb14_deforestation<-data[data$policy==policy_tx & data$variable==target_var, id_vars]
#get the forest outputs
cb14_deforestation<-cb_forest_change('benefit_ecosystemservices_', data, 
                                    policy_tx, policy_base, cb14_deforestation, 1)

#get the crop impacts
variable_names<-unique(data$variable)
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
cb14_deforestation<-cb_crop_livestock(livestock_crop_var_list, 'change_revenue_croplvst_', 
                                          data, policy_tx, policy_base, cb14_deforestation,1,1)


#----------15. Accelerating Crop Productivity with LUGAINS NEED COST OMIT --------
##Cost is the difference in food produced and its value
##Benefit is the ecosystem services of land
# 
# print('15. Accelerate Crop Productivity with LUGains')
# policy_tx <-15
# policy_base<-0
# target_var <-'area_agrc_crops_rice'
# cb15_accelerated_crops_wlugains<-data[data$policy==policy_tx & data$variable==target_var,id_vars]
# 
# ##Calculate the forest impacts
# cb15_accelerated_crops_wlugains<-cb_forest_change('benefit_ecosystemservices_', data, 
#                                          policy_tx, policy_base, cb15_accelerated_crops_wlugains, 1)
# ##Calculate the additional crops produced and multiply by price
# variable_names<-unique(data$variable)
# livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
# cb15_accelerated_crops_wlugains<-cb_crop_livestock(livestock_crop_var_list, 
#                                                    'cost_lostrevenue_cropslvst', 
#                                           data, policy_tx, policy_base, cb15_accelerated_crops_wlugains,1,1)


#----------16. Accelerating Livestock Productivity with LUGAINS NEED COST OMIT --------
# ##Cost is the difference in food produced and its value
# ##Benefit is the ecosystem services of land
# 
# print('16. Accelerate Livestock Productivity with LUGains')
# policy_tx <-16
# policy_base<-0
# target_var <-'area_agrc_crops_rice'
# cb16_accelerated_lvst_wlugains<-data[data$policy==policy_tx & data$variable==target_var,id_vars]
# 
# ##Calculate the forest impacts
# cb16_accelerated_lvst_wlugains<-cb_forest_change('benefit_ecosystemservices_', data, 
#                                                   policy_tx, policy_base, cb16_accelerated_lvst_wlugains, 1)
# ##Calculate the additional crops produced and multiply by price
# variable_names<-unique(data$variable)
# livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
# cb16_accelerated_lvst_wlugains<-cb_crop_livestock(livestock_crop_var_list, 
#                                                    'cost_lostrevenue_cropslvst', 
#                                                    data, policy_tx, policy_base, cb16_accelerated_lvst_wlugains,1,1)

#----------17. New Diets with LUGAINS --------
#Cost is the difference in food produced and its value
#Benefit is the ecosystem services of land

print('17. New Diets with LUGains')
policy_tx <-17
policy_base<-0
target_var <-'area_agrc_crops_rice'
cb17_diet_wlugains<-data[data$policy==policy_tx & data$variable==target_var,id_vars]

#Calculate the forest impacts
cb17_diet_wlugains<-cb_forest_change('benefit_ecosystemservices_', data, 
                                                 policy_tx, policy_base, cb17_diet_wlugains, 1)
#Calculate the additional crops produced and multiply by price
variable_names<-unique(data$variable)
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
cb17_diet_wlugains<-cb_crop_livestock(livestock_crop_var_list, 
                                                  'change_revenue_croplvst', 
                                                  data, policy_tx, policy_base, cb17_diet_wlugains,1,1)
#Tack on other costs and benefits of diets
cb17_diet_wlugains$diff_popvegetarians<-cb4_diet$diff_popvegetarians_new_diets
cb17_diet_wlugains$benefit_grocerysavings<-cb4_diet$benefit_grocerysavings_new_diets
cb17_diet_wlugains$benefit_healthsavings<-cb4_diet$benefit_healthsavings_new_diets



#----------18. Food Loss with LUGAINS --------
#Cost is the difference in food produced and its value
#Benefit is the ecosystem services of land

print('18. Food Loss with LUGains')
policy_tx <-18
policy_base<-0
target_var <-'area_agrc_crops_rice'
cb18_foodloss_wlugains<-data[data$policy==policy_tx & data$variable==target_var,id_vars]

#Calculate the forest impacts
cb18_foodloss_wlugains<-cb_forest_change('benefit_ecosystemservices_', data, 
                                     policy_tx, policy_base, cb18_foodloss_wlugains, 1)
#Calculate the additional crops produced and multiply by price
variable_names<-unique(data$variable)
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
cb18_foodloss_wlugains<-cb_crop_livestock(livestock_crop_var_list, 
                                      'change_revenue_croplvst', 
                                      data, policy_tx, policy_base, cb18_foodloss_wlugains,1,1)

#Tack on costs
cb18_foodloss_wlugains$cost_technical<-cb7_foodloss$cost_technical_reducing_food_losses


#----------19. Silvopasture with LUGAINS --------
#Cost is the difference in food produced and its value
#Benefit is the ecosystem services of land

print('1. Silvopasture with LUGains')
policy_tx <-19
policy_base<-0
target_var <-'area_agrc_crops_rice'
cb19_silvopasture_wlugains<-data[data$policy==policy_tx & data$variable==target_var,id_vars]

#Calculate the forest impacts
cb19_silvopasture_wlugains<-cb_forest_change('benefit_ecosystemservices_', data, 
                                         policy_tx, policy_base, cb19_silvopasture_wlugains, 1)
#Calculate the additional crops produced and multiply by price
variable_names<-unique(data$variable)
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
cb19_silvopasture_wlugains<-cb_crop_livestock(livestock_crop_var_list, 
                                          'change_revenue_croplvst', 
                                          data, policy_tx, policy_base, cb19_silvopasture_wlugains,1,1)

#Tack on silvopasture costs
cb19_silvopasture_wlugains$cost_technical<-cb9_silvopasture$cost_technical




#----------20. Baseline with LUGAINS --------
#Cost is the difference in food produced and its value
#Benefit is the ecosystem services of land

print('20. Baseline with LUGains')
policy_tx <-20
policy_base<-0
target_var <-'area_agrc_crops_rice'
cb20_baseline_wlugains<-data[data$policy==policy_tx & data$variable==target_var,id_vars]

#Calculate the forest impacts
cb20_baseline_wlugains<-cb_forest_change('benefit_ecosystemservices_', data, 
                                             policy_tx, policy_base, cb20_baseline_wlugains, 1)
#Calculate the additional crops produced and multiply by price
variable_names<-unique(data$variable)
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
cb20_baseline_wlugains<-cb_crop_livestock(livestock_crop_var_list, 
                                              'change_revenue_croplvst', 
                                              data, policy_tx, policy_base, cb20_baseline_wlugains,1,1)

#----------21. All Ag Lvst and Forest Mgmt NYE--------
#----------22. All with LUGAINS NYE --------
#----------23. All Ag Lvst Mgmt --------

#combine the costs and benefits of the followig policies
#1. Rice Management
#2. Conservation agriculture
#3. Reduce Excess Fertilizer
#4. Improve Fertilizer Application
#5. Enteric Fermentation

print('23. All Ag Lvst Mgmt')
policy_tx <-23
policy_base<-0
target_var<-'pop_lvst_buffalo'
#set up the database
cb23_all_ag_lvst_mgmt<-data[data$policy==policy_tx & data$variable==target_var, id_vars]

#Add the ones you would do first that don't interfere
list_of_tx<-list(cb1_rice_management, cb5_ent_ferm, cb6_cons_ag, cb11_excess_fertilizer)
for (tx in list_of_tx){
  policy_name<-unique(as.character(tx$policy_name[1]))
  cols_in_tx <- colnames(tx)
  cols_to_add <- cols_in_tx[5: length(cols_in_tx)]
  cols_to_add_names <- paste(cols_to_add, "_", policy_name, sep='')
  cb23_all_ag_lvst_mgmt[cols_to_add_names] <- tx[cols_to_add]
}


#Calculate difference in demand for synthetic fertilizer with improved
#application, relative to excess fertilizer scenario
#Apply costs and benefits
policy_tx <-23
policy_name<-'improve_fertilizer_app'
policy_base<-11
diff_var <-'demand_soil_synthetic_fertilizer_n_kt'
output_var_name<-paste('benefit_fertilizersaved_', policy_name, sep='')
multiplier<-(-325)*1000

cb23_tmp<-cb(data, policy_tx, policy_base, 
            diff_var, output_var_name, multiplier)
cost_var<-paste('cost_technical_', policy_name, sep='')
benefit_var<-paste('benefit_avoidednitrate_', policy_name, sep='')
cb23_all_ag_lvst_mgmt$diff_demand_soil_synthetic_fertilizer_n_kt_improve_fertilizer_app<-cb23_tmp$diff_demand_soil_synthetic_fertilizer_n_kt
cb23_all_ag_lvst_mgmt$benefit_fertilizersaved_improve_fertilizer_app<-cb23_tmp$benefit_fertilizersaved_improve_fertilizer_app
cb23_all_ag_lvst_mgmt[cost_var]<-cb23_tmp$diff_demand_soil_synthetic_fertilizer_n_kt*(-23)*1000
cb23_all_ag_lvst_mgmt[benefit_var]<-cb23_tmp$diff_demand_soil_synthetic_fertilizer_n_kt*(-101)*1000




#----------24. All Ag Lvst Mgmt no Deforestation NYE--------

#----------17A. New Diets with LUGAINS MARGINAL--------
#Cost is the difference in food produced and its value
#Benefit is the ecosystem services of land

# print('17A. New Diets with LUGains Marginal')
# policy_tx <-17
# policy_base<-20
# target_var <-'area_agrc_crops_rice'
# cb17a_diet_wlugains_marginal<-data[data$policy==policy_tx & data$variable==target_var,id_vars]
# 
# #What was our difference in emissions?
# emissions_diff<-cb_emissions_differences(data, policy_tx, policy_base)
# cb17a_diet_wlugains_marginal[emissions_vars,]<-emissions_diff[emissions_vars,]
# 
# #How much more forest do we have and what are its benefits
# cb17a_diet_wlugains_marginal<-cb_forest_change('benefit_ecosystemservices_', data, 
#                                      policy_tx, policy_base, cb17a_diet_wlugains_marginal, 1)
# 
# #How much less crop did we produce and what was its value
# variable_names<-unique(data$variable)
# livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
# cb17_diet_wlugains<-cb_crop_livestock(livestock_crop_var_list, 
#                                       'change_revenue_croplvst', 
#                                       data, policy_tx, policy_base, cb17_diet_wlugains,-1,1)
# 
# #Tack on other costs and benefits of diets
# cb17a_diet_wlugains_marginal$diff_popvegetarians<-cb4_diet$diff_popvegetarians
# cb17a_diet_wlugains_marginal$benefit_grocerysavings<-cb4_diet$benefit_grocerysavings
# cb17a_diet_wlugains_marginal$benefit_healthsavings<-cb4_diet$benefit_healthsavings
# 



#----------25. All Policies (No Production) with LUGains--------
#A. Calculate the costs and benefits of agriculture practices
print('25. All Policies (No Production) with LUGains')
policy_tx <-25
policy_base<-0
target_var <-'area_agrc_crops_rice'
cb25_all_wlugains<-data[data$policy==policy_tx & data$variable==target_var,id_vars]


#A.1 Rice
print('25.rice')
tx<-cb_rice_management(data, policy_tx)
orig_colnames<-colnames(tx)
new_colnames<-paste(orig_colnames, "_rice_manegement", sep='')
colnames(tx)<-new_colnames
cols_in_tx <- colnames(tx)
cols_to_add <- cols_in_tx[5: length(cols_in_tx)]
cb25_all_wlugains<-cbind(cb25_all_wlugains, tx[,cols_to_add])

#A.5 Enteric Fermentation
print('25.enteric fermentation')
tx<-cb_enteric_fermentation(data, policy_tx)
orig_colnames<-colnames(tx)
new_colnames<-paste(orig_colnames, "_enteric_fermentation", sep='')
colnames(tx)<-new_colnames
cols_in_tx <- colnames(tx)
cols_to_add <- cols_in_tx[5: length(cols_in_tx)]
cb25_all_wlugains<-cbind(cb25_all_wlugains, tx[,cols_to_add])


#A.6 Conservation Agriculture
print('25.cons ag')
tx<-cb_conservation_agriculture(data, policy_tx)
orig_colnames<-colnames(tx)
new_colnames<-paste(orig_colnames, "_conservation_aggriculture", sep='')
colnames(tx)<-new_colnames
cols_in_tx <- colnames(tx)
cols_to_add <- cols_in_tx[5: length(cols_in_tx)]
cb25_all_wlugains<-cbind(cb25_all_wlugains, tx[,cols_to_add])


#A.11 and A.12 are too small to count
print('25.fertilizer')
diff_var <-'demand_soil_synthetic_fertilizer_n_kt'
output_var_name<-'benefit_avoidednitrate'
multiplier<-(-101)*1000
tx<-cb(data, policy_tx, policy_base, 
                      diff_var, output_var_name, multiplier)
cols_in_tx <- colnames(tx)
cols_to_add <- cols_in_tx[5: length(cols_in_tx)]
cb25_all_wlugains<-cbind(cb25_all_wlugains, tx[,cols_to_add])

#B. Tack on diet benefits
print('25.diets')
cb25_all_wlugains$diff_popvegetarians<-cb4_diet$diff_popvegetarians
cb25_all_wlugains$benefit_grocerysavings<-cb4_diet$benefit_grocerysavings
cb25_all_wlugains$benefit_healthsavings<-cb4_diet$benefit_healthsavings

#C. Calculate ecosystem service effects
#Calculate the forest impacts
print('25. ecosystem services')
cb25_all_wlugains<-cb_forest_change('benefit_ecosystemservices_', data, 
                                    policy_tx, policy_base, cb25_all_wlugains, 1)

#D. Calculate crop and livestock revenue chanages
print('25. crop livestock revenues')
variable_names<-unique(data$variable)
livestock_crop_var_list <- as.character(variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)])
cb25_all_wlugains<-cb_crop_livestock(livestock_crop_var_list, 
                                    'change_revenue_croplvst', 
                                     data, policy_tx, policy_base, cb25_all_wlugains,1,1)


#E. Allocate technical costs for silvopasture, afforestation, and food loss costs
print('25. other technical costs')
cb25_all_wlugains$cost_technical_silvopasture<-cb9_silvopasture$cost_technical*0.85
cb25_all_wlugains$cost_technical_food_loss<-cb7_foodloss$cost_technical*0.85
cb25_all_wlugains$cost_technical_rehabilitate_land<-cb10_rehab$cost_technical*0.85



#--------- A. Post Process To Long Form ---------

list_of_outputs<-list(cb1_rice_management, 
                      # #cb2_accelerated_crops,
                      # #cb3_accelerated_livestock, 
                      cb4_diet,
                       cb5_ent_ferm, cb6_cons_ag, cb7_foodloss,
                       cb9_silvopasture, 
                      cb10_rehab, 
                      cb11_excess_fertilizer,
                       cb12_better_fertilizer,
                       cb14_deforestation,
                      cb20_baseline_wlugains,
                       cb17_diet_wlugains,
                      cb18_foodloss_wlugains,
                       cb23_all_ag_lvst_mgmt,
                      cb25_all_wlugains
                      )


#merge all
CostBenefits_all<- Reduce(function(...) merge(..., all=T), list_of_outputs)
dim(CostBenefits_all)
CostBenefits_all[is.na(CostBenefits_all)]<-0
dim(CostBenefits_all)

#reshape

#CosfBenefit file
library(data.table)

#set unique identifiers
#id_vars <-c('nation','time_period','policy') #,'Future_ID')

data2<-data.table(CostBenefits_all)
DT.m2 = melt(data2, id.vars = id_vars,
             measure.vars = subset(colnames(data2),!(colnames(data2)%in%id_vars)),
)


#dir_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\IPPU\Tableau\)"
write.csv(DT.m2,paste0("AFOLU_CostBenefitAll_long.csv"),row.names=FALSE)

