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

#--------- 1. Rice Cultivation --------
#Multiply the hectares of rice cultivated with rice management by the cost factor

policy_tx <-1 #rice management
target_var <-'area_agrc_crops_rice'
multiplier<-(-31)
output_var_name <-c('cost_technical_rice_management')

#implementation rate is from 0 to 100 in years 0 to 35
level_of_implementation<-seq(0, 1, length.out=(36))

rice_management_data<-data[data$policy==policy_tx & data$variable==target_var, ]
rice_management_CB_output<-rice_management_data[,id_vars]
rice_management_CB_output$variable<-output_var_name
rice_management_CB_output$value<-rice_management_data[, c('value')]*level_of_implementation*multiplier


#--------- 2. Accelerate Crop Productivity NYE -----
#Calculate the additional crops produced and multiply by price
policy_tx <-2
policy_base<-0
cb_crop_prod<-data[data$policy==policy_tx & data$variable==target_var,id_vars]
variable_names<-unique(data$variable)
livestock_crop_var_list <- variable_names[grepl('yield_agrc', variable_names)]
cb_accelerated_crops<-cb_crop_livestock(livestock_crop_var_list, 'benefit_addedrevenue_improve_crop_productivity_', 
                                            data, policy_tx, policy_base, cb_crop_prod,1)

#--------- 3. Accelerate Livestock Productivity NYE -----
policy_tx <-3
policy_base<-0
cb_lvst_prod<-data[data$policy==policy_tx & data$variable==target_var,id_vars]
variable_names<-unique(data$variable)
livestock_crop_var_list <- variable_names[grepl('pop_lvst', variable_names)]
cb_accelerated_livestock<-cb_crop_livestock(livestock_crop_var_list, 'benefit_addedrevenue_improve_lvst_productivity_', 
                                   data, policy_tx, policy_base, cb_lvst_prod, 1)



#--------- 4. New Diets -----

#calculate the number of additional people using better diets
#for each such person, there is a $370 cost savings in groceries and 
#$1000/yr cost savings in health

policy_tx <-4 #New Diets
pop<-read.csv('pop_all_future_with_urban_interpolation.csv')
pop$total<-pop$population_gnrl_urban+pop$population_gnrl_rural
frac_veg<-seq(0, 30, length.out=36)/100
rep(frac_veg, numnations)
pop_veg<-pop$total*frac_veg

target_var <-'area_agrc_crops_rice' #use a random variable

diet_data<-data[data$policy==policy_tx & data$variable==target_var, id_vars]
cb_diet<-diet_data[,id_vars]
cb_diet$diff_popvegetarians_new_diets<-pop_veg
cb_diet$benefit_grocerysavings_new_diets<-pop_veg*370
cb_diet$benefit_healthsavings_new_diets<-pop_veg*1000


#--------- 5. Enteric Fermentation ------
#Multiply the number of ruminants receiving this transformation by the cost factor
ent_ferm_tx_dat<-read.csv("reducing_enteric_fermentation_tx.csv")
policy_tx<-5
target_var <-ent_ferm_tx_dat$Category
multiplier <-76 #dollars per head
  
cb_ent_ferm <-data[data$policy==policy_tx & data$variable==target_var[1], id_vars]
  for (l in target_var){
    #l<-ent_ferm_tx_dat[3,1] #for testing
    ruminants<-data[data$policy==policy_tx & data$variable==l,]
    frac_affected<-seq(0, ent_ferm_tx_dat[ent_ferm_tx_dat$Category==l,2], length.out=(36))
    frac_affected_rep<-rep(frac_affected, numnations)
    ruminants_affected<-ruminants[ruminants$variable==l,7]
    ruminants_affected<-ruminants_affected*frac_affected_rep
    ruminants_affected_var<-paste('enteric_fermentation_numaffected_', l)
    ruminants_cost_var<-paste('cost_technical_enteric_fermentation_', l)
    cb_ent_ferm[ruminants_affected_var] <- ruminants_affected
    cb_ent_ferm[ruminants_cost_var]<-ruminants_affected*multiplier
  }


#--------- 6. Conservation Agriculture ------------
ca_areas_tx<-read.csv("CA_fraction_areas_by_nation.csv")
policy_tx <-6
frac_ag<-c()
for (i in nations){
  #i<-'argentina' #for testing
  beginning_frac<-round(100*(ca_areas_tx[ca_areas_tx$Nation==i, 2]))
  ending_frac<-round(100*(ca_areas_tx[ca_areas_tx$Nation==i, 3]))
  change_in_ca<-seq(0, ending_frac-beginning_frac, length.out=(36))/100
  frac_ag<-c(frac_ag, change_in_ca)
}

#calculate the number of additional acres under CA
cb_cons_ag <-data[data$policy==policy_tx & data$variable=='area_lndu_croplands', id_vars]
crop_acres<-data[data$policy==policy_tx & data$variable=='area_lndu_croplands',7]
acres_under_ca<-crop_acres*frac_ag
cb_cons_ag$frac_change_in_ca<-frac_ag
cb_cons_ag$diff_acres_under_conservation_agriculture<-acres_under_ca
cb_cons_ag$tcost_technical_conservation_agriculture<-acres_under_ca*(-41)
cb_cons_ag$benefit_landvalue_conservation_agriculture<-acres_under_ca*(10)


#--------- 7. Reducing Food Loss -----
#(1) Change in export times value of food
policy_tx <-7
policy_base<-0
variable_names<-unique(data$variable)
livestock_crop_var_list<- variable_names[grepl('net_imports_agrc', variable_names) | grepl('net_imports_change_lvst', variable_names)]
cb_foodloss <-data[data$policy==policy_tx & data$variable==variable_names[1], id_vars]
value_multiplier<-(-1)
cb_foodloss<-cb_crop_livestock(livestock_crop_var_list, 'benefit_addedrevenue_reducing_food_losses_', 
                               data, policy_tx, policy_base, cb_foodloss, value_multiplier)
#(2) Total tonnage of food saved minus the cost per ton of doing so.
food_loss_vars<-colnames(cb_foodloss)
import_diff_vars<-food_loss_vars[grepl('diff', food_loss_vars)]
tons<- rowSums(cb_foodloss[,import_diff_vars])
cb_foodloss$cost_technical_reducing_food_losses<-tons*1200

#(3) Extra column of total benefit
benefit_vars<-food_loss_vars[grepl('addedrevenue', food_loss_vars)]
cb_foodloss$benefit_addedrevenue_reducing_food_losses_total<- rowSums(cb_foodloss[,benefit_vars])

#--------- 8. Manure Management OMIT -----
#--------- 9. Silvopasture -----
policy_tx <-9
policy_base<-0

#(1) Ecosystem services of new secondary forest
diff_var <-'area_lndu_forests_secondary'
output_var_name<-('benefit_ecosystemservices_silvopasture')
multiplier<-(2500)
cb_silvopasture<-cb(data, policy_tx, policy_base, diff_var, 
             output_var_name, multiplier)

#(2) Cost of creating new secondary forest
cb_silvopasture$cost_technical_silvopasture<-cb_silvopasture$diff_area_lndu_forests_secondary*(31)

#(3) Annual benefit of additional livestock (only - no crops)
variable_names<-unique(data$variable)
livestock_crop_var_list <- variable_names[grepl('pop_lvst', variable_names)]
cb_silvopasture<-cb_crop_livestock(livestock_crop_var_list, 'benefit_addedrevenue_silvopasture_', 
                                   data, policy_tx, policy_base, cb_silvopasture,1)

#--------- 10. Rehabilitating Land -------------

rehab_policy_tx <-10
rehab_policy_base<-0

#(1) Ecosystem services of new secondary forest
diff_var <-'area_lndu_forests_secondary'
output_var_name<-('benefit_ecosystemservices_rehabilitating_land')
multiplier<-(2500)
cb_rehab<-cb(data, rehab_policy_tx, rehab_policy_base, diff_var, 
             output_var_name, multiplier)

#(3) Annual cost of foregone livestock
variable_names<-unique(data$variable)
livestock_crop_var_list <- variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)]

#create the cost vector
livestock_crop_unit_values<-read.csv('livestock_and_crop_values.csv')

for (l in livestock_crop_var_list){
  diff_var <-l
  diff_var_name<-gsub(" ", "", paste('diff_', diff_var))
  output_var_name<-paste('cost_foregonerevenue_rehabilitating_land_', l)
  #multiply the unit cost by -1 since a cost is applied to the loss of crops/stock
  multiplier<-livestock_crop_unit_values[livestock_crop_unit_values$category==l, 2]*-1
  cb_foregonerevenue_rehab_tmp<-cb(data, deforestation_policy_tx,
                                   deforestation_policy_base, diff_var,
                                   output_var_name, multiplier)
  cb_rehab[diff_var_name]<-cb_foregonerevenue_rehab_tmp[,4]
  cb_rehab[output_var_name]<-cb_foregonerevenue_rehab_tmp[,5]
}

#--------- 11. Reduce Excess Fertilizer Use -----------
#Calculate difference in demand for synthetic fertilizer relative to baseline
#Apply costs and benefits

excessN_policy_tx <-11
excessN_policy_name<-'reduce_excess_fertilizer'
excessN_policy_tx_base<-0
diff_var <-'demand_soil_synthetic_fertilizer_n_kt'

output_var_name<-paste('cost_fertilizersaved_', excessN_policy_name, sep='')
multiplier<-(325)
cb_excess_fertilizer<-cb(data, excessN_policy_tx, excessN_policy_tx_base, 
                         diff_var, output_var_name, multiplier)

benefit_var<-paste('benefit_avoidednitrate_', excessN_policy_name, sep='')
cb_excess_fertilizer[benefit_var]<-cb_excess_fertilizer$diff_demand_soil_synthetic_fertilizer_n_kt*(-101)


#--------- 12. Improve Fertilizer Application -------

#Calculate difference in demand for synthetic fertilizer relative to baseline
#Apply costs and benefits

betterN_policy_tx <-12
betterN_policy_name<-'improve_fertilizer_app'
betterN_policy_base<-0
diff_var <-'demand_soil_synthetic_fertilizer_n_kt'

output_var_name<-paste('cost_fertilizersaved_', betterN_policy_name, sep='')
multiplier<-(325)
cb_better_fertilizer<-cb(data, betterN_policy_tx, betterN_policy_base, 
                         diff_var, output_var_name, multiplier)
cost_var<-paste('cost_technical_', betterN_policy_name, sep='')
benefit_var<-paste('benefit_avoidednitrate_', betterN_policy_name, sep='')
cb_better_fertilizer[cost_var]<-cb_excess_fertilizer$diff_demand_soil_synthetic_fertilizer_n_kt*(-23)
cb_better_fertilizer[benefit_var]<-cb_excess_fertilizer$diff_demand_soil_synthetic_fertilizer_n_kt*(-101)

#alternatively, calculate how much fertilizer was applied
#determine how much higher it would have bee without this transformation
#apply costs and benefits to that value

#level_of_implementation<-seq(0, 1, length.out=(36))

policy_tx <-12
policy_name<-'improve_fertilizer_app'
policy_base<-0
target_var<-'demand_soil_synthetic_fertilizer_n_kt'

reduction_in_fertilizer<-read.csv('improve_fertilizer_app.csv')
reduction_in_fertilizer<-reduction_in_fertilizer[reduction_in_fertilizer$TransformationName=="full",]

improve_fertilizer_cb<-data[data$policy==policy_tx & data$variable==target_var, id_vars]

synth_fertilizer_applied_kt<-data[data$policy==policy_tx & data$variable==target_var,7]
synth_fertilizer_avoided<-synth_fertilizer_applied_kt/reduction_in_fertilizer$demscalar_soil_fertilizer_n_per_area - synth_fertilizer_applied_kt
improve_fertilizer_cb$diff_fertilizeravoidedkt_improve_fertilizer_app <- synth_fertilizer_avoided

##BORROWED AND FIX NIDHI

improve_fertilizer_cb$benefit_avoidedNapplication_improve_fertilizer_app<- synth_fertilizer_avoided*(325)
improve_fertilizer_cb$benefit_avoidednitrate_improve_fertilizer_app<- synth_fertilizer_avoided*(325)

output_var_name<-paste('cost_fertilizersaved_', betterN_policy_name, sep='')
multiplier<-(325)
cb_better_fertilizer<-cb(data, betterN_policy_tx, betterN_policy_base, 
                         diff_var, output_var_name, multiplier)
cost_var<-paste('cost_technical_', betterN_policy_name, sep='')
benefit_var<-paste('benefit_avoidednitrate_', betterN_policy_name, sep='')
cb_better_fertilizer[cost_var]<-cb_excess_fertilizer$diff_demand_soil_synthetic_fertilizer_n_kt*(-23)
cb_better_fertilizer[benefit_var]<-cb_excess_fertilizer$diff_demand_soil_synthetic_fertilizer_n_kt*(-101)



acres_under_ca<-crop_acres*frac_ag
cb_cons_ag$frac_change_in_ca<-frac_ag
cb_cons_ag$diff_acres_under_conservation_agriculture<-acres_under_ca
cb_cons_ag$tcost_technical_conservation_agriculture<-acres_under_ca*(-41)
cb_cons_ag$benefit_landvalue_conservation_agriculture<-acres_under_ca*(10)



#--------- 14. Ending Deforestation --------
deforestation_policy_tx <-14
deforestation_policy_base<-0

#(1) One-time cost of foregone timber
diff_var <-'area_lndu_forests_primary'
output_var_name<-('cost_foregonerevenue_deforestation_primary')
multiplier<-(900/35)
cb_foregonerevenue_deforestation_primary<-cb(data, deforestation_policy_tx, 
                                               deforestation_policy_base, diff_var, 
                                               output_var_name, multiplier)

#(2) One-time cost of foregone timber
diff_var <-'area_lndu_forests_secondary'
output_var_name<-('cost_foregonerevenue_deforestation_secondary')
multiplier<-(900/35)
cb_foregonerevenue_deforestation_secondary<-cb(data, deforestation_policy_tx, 
                                               deforestation_policy_base, diff_var, 
                                               output_var_name, multiplier)

#combine 1 and 2 into a single data frame
cb_deforestation<-cb_foregonerevenue_deforestation_primary
cb_deforestation$diff_area_lndu_forests_secondary<-cb_foregonerevenue_deforestation_secondary$diff_area_lndu_forests_secondary
cb_deforestation$cost_foregonerevenue_deforestation_secondary<-cb_foregonerevenue_deforestation_secondary$cost_foregonerevenue_deforestation_secondary

#(3) Annual cost of foregone livestock
variable_names<-unique(data$variable)
livestock_crop_var_list <- variable_names[grepl('pop_lvst', variable_names) | grepl('yield_agrc', variable_names)]

#create the cost vector
livestock_crop_unit_values<-read.csv('livestock_and_crop_values.csv')

for (l in livestock_crop_var_list){
  diff_var <-l
  diff_var_name<-gsub(" ", "", paste('diff_', diff_var))
  output_var_name<-paste('cost_foregonerevenue_deforestation_', l)
  #multiply the unit cost by -1 since a cost is applied to the loss of crops/stock
  multiplier<-livestock_crop_unit_values[livestock_crop_unit_values$category==l, 2]*-1
  cb_foregonerevenue_deforestation_tmp<-cb(data, deforestation_policy_tx,
                                             deforestation_policy_base, diff_var,
                                             output_var_name, multiplier)
  cb_deforestation[diff_var_name]<-cb_foregonerevenue_deforestation_tmp[,4]
  cb_deforestation[output_var_name]<-cb_foregonerevenue_deforestation_tmp[,5]
}

#Annual benefit of ecosystem services
tmp<-cb_deforestation$diff_area_lndu_forests_primary*5000
cb_deforestation$benefit_ecosystemservices_deforestation_primary<-tmp

tmp<-cb_deforestation$diff_area_lndu_forests_secondary*2500
cb_deforestation$benefit_ecosystemservices_deforestation_secondary<-tmp


diff_var <-'area_lndu_forests_mangroves'
output_var_name<-'benefit_ecosystemservices_deforestation_mangroves'
multiplier<-(10000) #since it is base-tx the difference will be negative
benefit_ecosystemservices_deforestation_tmp<-cb(data, deforestation_policy_tx, 
                                             deforestation_policy_base, diff_var, 
                                             output_var_name, multiplier)
cb_deforestation['diff_area_lndu_forests_mangroves']<-benefit_ecosystemservices_deforestation_tmp[,4]
cb_deforestation[output_var_name]<-benefit_ecosystemservices_deforestation_tmp[,5]





















