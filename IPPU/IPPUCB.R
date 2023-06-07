output.file<-read.csv("output_ippu_all_2022_06_24.csv")
data<-output.file


#how we identify each individual run
id_vars <-c('nation','time_period','policy')
#how many policies we are comparing
policies <- unique(data$policy)
#how many nations we are working with
nations <- unique(data$nation)
numnations <-length(nations)
#how many time periods do we have
numtimesteps <-length(unique(data$time_period))

#---------- CCS ---------
#Cost of CCS is the difference in CO2 emissions between policy 0 and policy 3
#CO2 Column Names
ccs_vars <-c('emission_co2e_co2_ippu_production_cement', 
             'emission_co2e_co2_ippu_production_chemicals', 
             'emission_co2e_co2_ippu_production_glass',
             'emission_co2e_co2_ippu_production_lime_and_carbonite',
             'emission_co2e_co2_ippu_production_metals',
             'emission_co2e_co2_ippu_production_paper',
             'emission_co2e_co2_ippu_production_plastic')
ccs_cost_per_tco2<-c(27, 55, 55, 55, 55, 55, 55)
ccs_categories<-substring(ccs_vars, 35)
ccs_cost_vars<-paste0('cost_USD_M_ccs_', ccs_categories)
ccs_diff_vars<-paste0('difference_ccs_', ccs_vars)

ccs_output_rownames<-c(id_vars, ccs_cost_vars, ccs_diff_vars)

#EDMUNDO: the ccs output will be in this data frame at the end of the for loop
ccs_output<-data.frame(matrix(ncol=length(ccs_cost_output_rownames), nrow=numtimesteps*numnations))
colnames(ccs_output)<- ccs_output_rownames

rowstart<-1
rowend<-numtimesteps
for (i in nations){
  #datap<-subset(data,nation=='brazil')
  datap<-subset(data,nation==i)
  datap0<-subset(datap,policy==0)[,c(id_vars,ccs_vars)]
  datap3<-subset(datap,policy==3)[,c(id_vars,ccs_vars)]
  emissions_differences<-datap0[,c(ccs_vars)]-datap3[,c(ccs_vars)]
  costs<-t(t(emissions_differences)*ccs_cost_per_tco2)
  ccs_output[rowstart:rowend,c(id_vars)]<-datap3[,c(id_vars)]
  ccs_output[rowstart:rowend,c(ccs_cost_vars)]<-costs
  ccs_output[rowstart:rowend,c(ccs_diff_vars)]<-emissions_differences
  rowstart<-rowend+1
  rowend<-rowend+numtimesteps
}
#write.csv(ccs_output, 'ccs_outputs.csv')


#---------- FGAS ---------

#Cost of FGAS is the difference in FGAS emissions between policy 0 and policy 4
#FGAS Column Names
fgas_vars <-c(
  'emission_co2e_hfcs_ippu_product_use_product_use_ods_other',
  'emission_co2e_hfcs_ippu_product_use_product_use_ods_refrigeration',
  'emission_co2e_hfcs_ippu_production_chemicals',
  'emission_co2e_hfcs_ippu_production_electronics',
  'emission_co2e_hfcs_ippu_production_metals',
  'emission_co2e_pfcs_ippu_product_use_product_use_ods_other',
  'emission_co2e_pfcs_ippu_production_chemicals',
  'emission_co2e_pfcs_ippu_production_electronics',
  'emission_co2e_pfcs_ippu_production_other_product_manufacturing',
  'emission_co2e_sf6_ippu_chemicals',
  'emission_co2e_sf6_ippu_electronics',
  'emission_co2e_sf6_ippu_other_product_manufacturing',
  'emission_co2e_other_fcs_ippu_production_electronics')
  
  
  
fgas_cost_per_tco2<-100
fgas_cost_vars<-paste0('cost_USD_M_fgas_', fgas_vars)
fgas_diff_vars<-paste0('difference_fgas_', fgas_vars)

fgas_output_rownames<-c(id_vars, fgas_cost_vars, fgas_diff_vars)

#EDMUNDO: the fgas output will be in this data frame at the end of the for loop
fgas_output<-data.frame(matrix(ncol=length(fgas_output_rownames), nrow=numtimesteps*numnations))
colnames(fgas_output)<- fgas_output_rownames

rowstart<-1
rowend<-numtimesteps
for (i in nations){
  #datap<-subset(data,nation=='brazil')
  datap<-subset(data,nation==i)
  datap0<-subset(datap,policy==0)[,c(id_vars,fgas_vars)]
  datap4<-subset(datap,policy==4)[,c(id_vars,fgas_vars)]
  emissions_differences<-datap0[,c(fgas_vars)]-datap4[,c(fgas_vars)]
  costs<-emissions_differences*fgas_cost_per_tco2
  fgas_output[rowstart:rowend,c(id_vars)]<-datap4[,c(id_vars)]
  fgas_output[rowstart:rowend,c(fgas_cost_vars)]<-costs
  fgas_output[rowstart:rowend,c(fgas_diff_vars)]<-emissions_differences
  rowstart<-rowend+1
  rowend<-rowend+numtimesteps
}
write.csv(fgas_output, 'fgas_outputs.csv')





#---------- N2O ---------

#Cost of N2O is the difference in N2O emissions between policy 0 and policy 5
#N2O Column Names
n2o_vars <-c('emission_co2e_n2o_ippu_chemicals')

n2o_cost_per_tco2<-2
n2o_cost_vars<-paste0('cost_USD_M_n2o_', n2o_vars)
n2o_diff_vars<-paste0('difference_n2o_', n2o_vars)

n2o_output_rownames<-c(id_vars, n2o_cost_vars, n2o_diff_vars)

#EDMUNDO: the n2o output will be in this data frame at the end of the for loop
n2o_output<-data.frame(matrix(ncol=length(n2o_output_rownames), nrow=numtimesteps*numnations))
colnames(n2o_output)<- n2o_output_rownames

rowstart<-1
rowend<-numtimesteps
for (i in nations){
  #datap<-subset(data,nation=='brazil')
  datap<-subset(data,nation==i)
  datap0<-subset(datap,policy==0)[,c(id_vars,n2o_vars)]
  datap4<-subset(datap,policy==5)[,c(id_vars,n2o_vars)]
  emissions_differences<-datap0[,c(n2o_vars)]-datap4[,c(n2o_vars)]
  costs<-emissions_differences*n2o_cost_per_tco2
  n2o_output[rowstart:rowend,c(id_vars)]<-datap5[,c(id_vars)]
  n2o_output[rowstart:rowend,c(n2o_cost_vars)]<-costs
  n2o_output[rowstart:rowend,c(n2o_diff_vars)]<-emissions_differences
  rowstart<-rowend+1
  rowend<-rowend+numtimesteps
}
write.csv(n2o_output, 'n2o_outputs.csv')


#---------- Clinker ---------

#Cost of Clinker is the difference in cement substitution between policy 0 and policy 2
#Clinker Column Names
clinker<-read.csv('clinker.csv')
clinker_remaining<-clinker[clinker$TransformationName == 'full', c('frac_ippu_cement_clinker')]

clinker_vars <-c('prod_ippu_cement_tonne')

clinker_cost_per_tonne<- -60
clinker_ben_per_tonne<- 50
clinker_diff_vars<-paste0('clinker_substituted_tons_', clinker_vars)
clinker_cost_vars<-paste0('clinker_substituted_cost_USD_', clinker_vars)
clinker_ben_vars<-paste0('clinker_substituted_benefit_USD_', clinker_vars)

clinker_output_rownames<-c(id_vars, clinker_diff_vars, clinker_cost_vars, clinker_ben_vars)

#EDMUNDO: the clinker output will be in this data frame at the end of the for loop
clinker_output<-data.frame(matrix(ncol=length(clinker_output_rownames), nrow=numtimesteps*numnations))
colnames(clinker_output)<- clinker_output_rownames

rowstart<-1
rowend<-numtimesteps
for (i in nations){
  #datap<-subset(data,nation=='brazil')
  datap<-subset(data,nation==i)
  datap0<-subset(datap,policy==0)[,c(id_vars, clinker_vars)]
  datap2<-subset(datap,policy==2)[,c(id_vars, clinker_vars)]
  clinker_substituted_tons<-(datap2$prod_ippu_cement_tonne*(1-clinker_remaining))
  costs<-clinker_substituted_tons*clinker_cost_per_tonne                                                 
  benefits<-clinker_substituted_tons*clinker_ben_per_tonne                                                 
  clinker_output[rowstart:rowend,c(id_vars)]<-datap2[,c(id_vars)]
  clinker_output[rowstart:rowend,c(clinker_cost_vars)]<-costs
  clinker_output[rowstart:rowend,c(clinker_ben_vars)]<-benefits
  clinker_output[rowstart:rowend,c(clinker_diff_vars)]<-clinker_substituted_tons
  rowstart<-rowend+1
  rowend<-rowend+numtimesteps
}
write.csv(clinker_output, 'clinker_outputs.csv')


#---------- Demand Management ---------

#Cost of DM is the difference in amount of cement and steel produced in policies 0 and 1
#DM Column Names

dm_vars <-c('prod_ippu_cement_tonne', 'prod_ippu_metals_tonne')

dm_cost_per_tonne<-c(-100, -500)
dm_waste_ben_per_ton<-c(0,390)
dm_mining_ben_per_ton<-c(0,3)
dm_air_ben_per_ton<-c(45,0)

dm_diff_vars<-paste0('dm_avoided_tons_', dm_vars)
dm_cost_vars<-paste0('dm_cost_USD_', dm_vars)
dm_ben_vars_avoided_waste<-paste0('dm_benefit_avoided_waste_byproducts_USD_', dm_vars)
dm_ben_vars_avoided_mining<-paste0('dm_benefit_avoided_mining_USD_', dm_vars)
dm_ben_vars_avoided_air<-paste0('dm_benefit_avoided_airpollution_USD_', dm_vars)

dm_output_rownames<-c(id_vars, dm_diff_vars, 
                      dm_cost_vars, 
                      dm_ben_vars_avoided_mining,
                      dm_ben_vars_avoided_waste,
                      dm_ben_vars_avoided_air)

#EDMUNDO: the dm output will be in this data frame at the end of the for loop
dm_output<-data.frame(matrix(ncol=length(dm_output_rownames), nrow=numtimesteps*numnations))
colnames(dm_output)<- dm_output_rownames

rowstart<-1
rowend<-numtimesteps
for (i in nations){
  #datap<-subset(data,nation=='brazil')
  datap<-subset(data,nation==i)
  datap0<-subset(datap,policy==0)[,c(id_vars, dm_vars)]
  datap1<-subset(datap,policy==1)[,c(id_vars, dm_vars)]
  materials_differences<-datap0[,c(dm_vars)]-datap1[,c(dm_vars)]
  costs<-t(t(materials_differences)*dm_cost_per_tonne)/10^6
  benefits_waste<-t(t(materials_differences)*dm_waste_ben_per_ton)/10^6
  benefits_mining<-t(t(materials_differences)*dm_mining_ben_per_ton)/10^6
  benefits_air<-t(t(materials_differences)*dm_air_ben_per_ton)/10^6
  
  dm_output[rowstart:rowend,c(id_vars)]<-datap1[,c(id_vars)]
  dm_output[rowstart:rowend,c(dm_diff_vars)]<-materials_differences
  dm_output[rowstart:rowend,c(dm_cost_vars)]<-costs
  dm_output[rowstart:rowend,c(dm_ben_vars_avoided_waste)]<-benefits_waste
  dm_output[rowstart:rowend,c(dm_ben_vars_avoided_mining)]<-benefits_mining
  dm_output[rowstart:rowend,c(dm_ben_vars_avoided_air)]<-benefits_air
  rowstart<-rowend+1
  rowend<-rowend+numtimesteps
}
#write.csv(dm_output, 'dm_outputs.csv')




#---------- Recyclables ---------

#Cost of rec is in circular economy. Benefit is value of materials, but will be endognized later
rec_vars <-c('qty_ippu_recycled_glass_used_in_production_tonne',
            'qty_ippu_recycled_metals_used_in_production_tonne',
            'qty_ippu_recycled_paper_used_in_production_tonne',
            'qty_ippu_recycled_plastic_used_in_production_tonne',
            'qty_ippu_recycled_rubber_and_leather_used_in_production_tonne',
            'qty_ippu_recycled_textiles_used_in_production_tonne')

rec_ben_per_ton<-c(25,1307, 80, 100, 0, 0)

rec_ben_vars<-paste0('recycle_benefit_savings_', rec_vars)
rec_output_rownames<-c(id_vars, 
                      rec_ben_vars)

#EDMUNDO: the recycling output will be in this data frame at the end of the for loop
rec_output<-data.frame(matrix(ncol=length(rec_output_rownames), nrow=numtimesteps*numnations))
colnames(rec_output)<- rec_output_rownames

rowstart<-1
rowend<-numtimesteps
for (i in nations){
  #datap<-subset(data,nation=='brazil')
  datap<-subset(data,nation==i)
  datap6<-subset(datap,policy==6)[,c(id_vars, rec_vars)]
  benefits<-t(t(datap6[,c(rec_vars)])*rec_ben_per_ton)/10^6
  
  rec_output[rowstart:rowend,c(id_vars)]<-datap6[,c(id_vars)]
  rec_output[rowstart:rowend,c(rec_ben_vars)]<-benefits
  rowstart<-rowend+1
  rowend<-rowend+numtimesteps
}
write.csv(rec_output, 'rec.csv')





#---------All Transformations ----
# CEMENT-------
# Cement transformations are implemented in this order.
#(1) demand management: dm
#(2) clinker substitution: csub 
#(3) ccs
# So, the costs and benefits of each subsequent transformation can only be
# applied to the production or emissions that remian after the prior transformation
# has had its effect.

#the costs and benefits of dm are in the columns named:
# dm_cost_USD_prod_ippu_cement_tonne
# dm_benefit_avoided_waste_byproducts_USD_prod_ippu_cement_tonne
# dm_benefit_avoided_mining_USD_prod_ippu_cement_tonne
# dm_benefit_avoided_airpollution_USD_prod_ippu_cement_tonne

#the costs and benefits of csub are in the columns named:
#clinker_substituted_cost_USD_prod_ippu_cement_tonne
#clinker_substituted_benefit_USD_prod_ippu_cement_tonne

#the costs of ccs are in the columns named (no co-benefits of CCS)
#cost_USD_M_ccs_prod_ippu_cement_tonne

# Let dm_costs_t, dm_benefits_t, csub_costs_t, 
# and csub_benefits_t, ccs_costs_t, ccs_benefit_t
# represent the costs and benefits above of each transformation 
# in time t when the transformation is implemented alone. These are calculated above.


# Let dm_frac_t be the fraction of demand in this future relative to baseline_in time t
# Let csub_frac_t be the fraction of clinker that is portland cement relative to baseline

#For example, from the documentation of full implementation
#dm_frac_2050 = 0.7, meaning demand for cement is 70% of baseline demand in 2050
#csub_frac_2050 = 1*(0.5) meaning 50% of clinker is still portland cement 

#Then the costs and benefits of dm in the ALL transformation are the same
# as when it is implemented individually
#all_costs_dm_t <- dm_costs_t
#all_benefits_dm_t <- dm_benefits_t

#the costs and benefits of csub are multiplied by dm_frac_t
#all_cost_csub_t <- dm_frac_t*csub_cost_t
#all_benefits_csub_t <- dm_frac_t*csub_benefits_t

#the costs and benefits of ccs are multiplied by dm_frac_t and csub_frac_t
#all_cost_ccs_t <- dm_frac_t*csub_frac_t*ccs_cost_t
#all_benefits_ccs_t <- dm_frac_t*csub_frac_t*ccs_benefits_t


