#Set root directory
#root<- r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\)"
#root<- r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\)"
root<- r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\)"

#read supporting files
#ouputfile
output.file<-r"(outputs\output_waste_all_2023_03_05.csv)"
#output.file<-r"(outputs\output_waste_all_2023_02_16.csv)"
#output.file<-r"(outputs\output_waste_all_2022_12_08.csv)"
#output.file<-r"(outputs\output_waste_all_2022_11_22.csv)"
#output.file<-r"(outputs\output_waste_all_2022_06_13_beta.csv)"
#output.file<-r"(outputs\output_waste_all_2022_06_13.csv)"
#output.file<-r"(outputs\output_waste_all_2022_05_24.csv)"
data<-read.csv(paste0(root,output.file))
data$total_co2e <-data$emission_co2e_subsector_total_trww+data$emission_co2e_subsector_total_wali+data$emission_co2e_subsector_total_waso
data$Future_ID <- 0
data$X<-NULL

#sanitation costs and benefits supporting files
data_tr<-r"(calibration\CircularEconomy\Transformations\)"
St<-read.csv(paste0(root,data_tr,'SanitationTransformation_2023_03_03_paraguay_test.csv'))
#St<-read.csv(paste0(root,data_tr,'SanitationTransformation_2023_03_03_paraguay.csv'))
#St<-read.csv(paste0(root,data_tr,'SanitationTransformation_2023_02_16_paraguay.csv'))
#St<-read.csv(paste0(root,data_tr,'SanitationTransformation_2022_11_25.csv'))
Exp<-read.csv(paste0(root,data_tr,'transformations_experiment_new_paraguay.csv'))

#load population tables
data_pop<-r"(calibration\CircularEconomy\SupportData\)"
#pop_all<-read.csv(paste0(root,data_pop,"pop_all_future.csv"))
pop_all<-read.csv(paste0(root,data_pop,"pop_all_future_with_urban_interpolation.csv"))
#pop_all$nation<-tolower(pop_all$nation)
pop_all$nation<-gsub(" ","_",pop_all$nation)


##code begins
#how we identify each individual run
id_vars <-c('nation','time_period','policy','Future_ID')

#how many policies we are comparing
policies <- unique(data$policy)

#how many nations we are working with
nations <- unique(data$nation)



#COSTS
#Cost of Treatment
#Industrial and Domestic Urban Wastewater Treatment
#volume vars
vol_vars<-subset(colnames(data),grepl("vol_trww_ww_",colnames(data))==TRUE)

#original subdivision, whcih was wrong
#TerciaryAerobic <- 'vol_trww_ww_treated_aerobic_m3'
#TerciaryAnaerobic <-'vol_trww_ww_treated_anaerobic_m3'
#Secondary <- c('vol_trww_ww_treated_latrine_improved_m3','vol_trww_ww_treated_septic_m3')
#Primary <- 'vol_trww_ww_treated_latrine_unimproved_m3'
#NoTreatment <- c('vol_trww_ww_untreated_no_sewerage_m3','vol_trww_ww_untreated_with_sewerage_m3')
#treatment_vars <- c(TerciaryAerobic,TerciaryAnaerobic,Secondary,Primary,NoTreatment)

#new subdivision, which is correct

TerciaryAerobic <- 'vol_trww_ww_treated_advanced_aerobic_m3'
TerciaryAnaerobic <-'vol_trww_ww_treated_advanced_anaerobic_m3'
Secondary <- c('vol_trww_ww_treated_secondary_aerobic_m3','vol_trww_ww_treated_secondary_anaerobic_m3')
Primary <- 'vol_trww_ww_treated_primary_m3'
NoTreatment <- c('vol_trww_ww_untreated_no_sewerage_m3','vol_trww_ww_untreated_with_sewerage_m3')
treatment_vars <- c(TerciaryAerobic,TerciaryAnaerobic,Secondary,Primary,NoTreatment)




Cost_Treatment<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap<-datap[,c(id_vars,treatment_vars)]



#datap[,paste0(TerciaryAerobic,'_CostTreatment')]<-datap[,TerciaryAerobic]*2.42
#datap[,paste0(TerciaryAnaerobic,'_CostTreatment')]<-datap[,TerciaryAnaerobic]*2.32
#datap[,paste0(Secondary,'_CostTreatment')]<-datap[,Secondary]*2.12
#datap[,paste0(Primary,'_CostTreatment')]<-datap[,Primary]*1.99
#datap[,paste0(NoTreatment,'_CostTreatment')]<-datap[,NoTreatment]*1.83

datap[,paste0('Cost_USD_trww_technical_treatment_',TerciaryAerobic)]<-datap[,TerciaryAerobic]*1.54
datap[,paste0('Cost_USD_trww_technical_treatment_',TerciaryAnaerobic)]<-datap[,TerciaryAnaerobic]*1.60
datap[,paste0('Cost_USD_trww_technical_treatment_',Secondary)]<-datap[,Secondary]*0.80
datap[,paste0('Cost_USD_trww_technical_treatment_',Primary)]<-datap[,Primary]*0.64
datap[,paste0('Cost_USD_trww_technical_treatment_',NoTreatment)]<-datap[,NoTreatment]*0.06 #*0.58


datap<-datap[,c(id_vars,paste0('Cost_USD_trww_technical_treatment_',treatment_vars))]

datap[,'Cost_USD_trww_technical_treatment_Total_CostTreatment']<-rowSums(datap[,paste0('Cost_USD_trww_technical_treatment_',treatment_vars)])
tvars<-c(paste0('Cost_USD_trww_technical_treatment_',treatment_vars),'Cost_USD_trww_technical_treatment_Total_CostTreatment')
datap0<-subset(datap,policy==0)[,c(id_vars,tvars)]
datap0[,paste0(tvars,"Ref")] <- datap0 [,tvars]
datap0[,tvars]<-NULL
datap0[,'policy']<-NULL
for (j in policies)
  {
#j<-4
  datapi<-subset(datap,policy==j)[,c(id_vars,tvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(tvars,'_marginal')]<-datapi[,tvars]-datapi[,paste0(tvars,"Ref")]
  Cost_Treatment<-append(Cost_Treatment,list(datapi))
  }
}


#Cost of Solid waste management
OpenBurning<-subset(colnames(data),grepl("qty_waso_incinerated",colnames(data))==TRUE)
OpenDumping<-subset(colnames(data),grepl("qty_waso_open_dump_",colnames(data))==TRUE)
Landfilled<-subset(colnames(data),grepl("qty_waso_landfilled_",colnames(data))==TRUE)
AnaerobicBiogas<-subset(colnames(data),grepl("qty_waso_biogas_",colnames(data))==TRUE)
Recycling<-subset(colnames(data),grepl("qty_waso_recycled_",colnames(data))==TRUE)
Composting<-subset(colnames(data),grepl("qty_waso_compost_",colnames(data))==TRUE)
tvars<-c(OpenBurning,OpenDumping,Landfilled,AnaerobicBiogas,Recycling,Composting)

Cost_SolidWaste<-list()


for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap<-datap[,c(id_vars,tvars)]
datap[,'Cost_USD_waso_technical_management_OpenBurning']<- rowSums(datap[,OpenBurning]*7) # 7 dollars per ton of waste
datap[,'Cost_USD_waso_technical_management_OpenDumping']<- rowSums(datap[,OpenDumping]*7) # 7 dollars per ton of waste
datap[,'Cost_USD_waso_technical_management_Landfilled']<- rowSums(datap[,Landfilled]*31) # 31 dollars per ton of waste
datap[,'Cost_USD_waso_technical_management_AnaerobicBiogas']<- rowSums(datap[,AnaerobicBiogas]*94) # 94 dollars per ton of waste
datap[,'Cost_USD_waso_technical_management_Composting']<- rowSums(datap[,Composting]*28) # 28 dollars per ton of waste
datap[,'Cost_USD_waso_technical_management_Recycling']<- rowSums(datap[,Recycling]*115) # 115 dollars per ton of waste
datap[,'Cost_USD_waso_technical_management_Collection']<- rowSums(datap[,c(Recycling,Landfilled)]*62) # 62 dollars per ton of waste
datap[,'Cost_USD_waso_technical_management_Total']<-datap[,'Cost_USD_waso_technical_management_OpenBurning']+
                                                    datap[,'Cost_USD_waso_technical_management_OpenDumping']+
                                                    datap[,'Cost_USD_waso_technical_management_Landfilled']+
                                                    datap[,'Cost_USD_waso_technical_management_AnaerobicBiogas']+
                                                    datap[,'Cost_USD_waso_technical_management_Composting']+
                                                    datap[,'Cost_USD_waso_technical_management_Recycling']+
                                                    datap[,'Cost_USD_waso_technical_management_Collection']
cvars<-c('Cost_USD_waso_technical_management_OpenBurning',
         'Cost_USD_waso_technical_management_OpenDumping',
         'Cost_USD_waso_technical_management_Landfilled',
         'Cost_USD_waso_technical_management_AnaerobicBiogas',
         'Cost_USD_waso_technical_management_Composting',
         'Cost_USD_waso_technical_management_Recycling',
         'Cost_USD_waso_technical_management_Collection',
         'Cost_USD_waso_technical_management_Total')
datap0<-subset(datap,policy==0)[,c(id_vars,cvars)]
datap0[,paste0(cvars,"Ref")] <- datap0 [,cvars]
datap0[,cvars]<-NULL
datap0[,'policy']<-NULL
for (j in policies)
  {
#j<-0
  datapi<-subset(datap,policy==j)[,c(id_vars,cvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(cvars,'_marginal')]<-datapi[,cvars]-datapi[,paste0(cvars,"Ref")]
  Cost_SolidWaste<-append(Cost_SolidWaste,list(datapi))
  }
}

#Cost of reducing waste [REMOVE SLLUDGE FROM WASTE REDUCTION]
#policy id=2
tvars <-subset(colnames(data),grepl("qty_waso_total",colnames(data))==TRUE)
tvars_food <- subset(tvars,grepl("food",tvars)==TRUE)
tvars_other <- subset (tvars, grepl("food",tvars)==FALSE)
#remove sludge
tvars_other <- subset(tvars_other,tvars_other!="qty_waso_total_sludge_produced_tonne")

Cost_WasteReduction<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap0<-subset(datap,policy==0)[,c(id_vars,tvars)]
datap0[,paste0(tvars,"Ref")] <- datap0 [,tvars]
datap0[,tvars]<-NULL
datap0[,'policy']<-NULL

#for all policies I want to estimate the delta in waste produced and then multiply that for the corresponding cost.
#for policies that are not in waste transformation, MAKE THE SLUDGE MULTIPLICATION ZERO

for (j in policies)
  {
#j<-1
  datapi<-subset(datap,policy==j)[,c(id_vars,tvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0('Cost_USD_waso_technical_wastereduction_',tvars,'_marginal')]<-datapi[,tvars]-datapi[,paste0(tvars,"Ref")]
  datapi[,paste0('Cost_USD_waso_technical_wastereduction_',tvars_food,'_marginal')]<-datapi[,paste0('Cost_USD_waso_technical_wastereduction_',tvars_food,'_marginal')]*40*-1 #row 1 table B.4
  datapi[,paste0('Cost_USD_waso_technical_wastereduction_',tvars_other,'_marginal')]<-datapi[,paste0('Cost_USD_waso_technical_wastereduction_',tvars_other,'_marginal')]*400*-1 #row 2 table B.4 #create an if for sludge
  datapi[,'Cost_USD_waso_technical_wastereduction_Total_marginal']<-datapi[,paste0('Cost_USD_waso_technical_wastereduction_',tvars_food,'_marginal')]+rowSums(datapi[,paste0('Cost_USD_waso_technical_wastereduction_',tvars_other,'_marginal')])
  datapi<-datapi[,c(id_vars,'Cost_USD_waso_technical_wastereduction_Total_marginal')]
  Cost_WasteReduction<-append(Cost_WasteReduction,list(datapi))
  }
}


#cost of Sanitation

#create table for each policy
st_all<-apply(Exp,1,function(x){pivot<-subset(St,TransformationName==x['sanitation']);
                                pivot$policy<-x['Policy_ID'];
                                pivot$TransformationName<-NULL;
                                pivot})
st_all<-do.call('rbind',st_all)
rural_vars<-subset(colnames(st_all),grepl("rural",colnames(st_all))==TRUE)
urban_vars<-subset(colnames(st_all),grepl("urban",colnames(st_all))==TRUE)


#merge with population table

st_all<-Reduce(function(...) merge(..., all=T), list(pop_all,st_all))
st_all$Future_ID<-0


#estimate population proportions
st_all[,rural_vars]<-st_all[,'population_rural']*st_all[,rural_vars]
st_all[,urban_vars]<-st_all[,'population_urban']*st_all[,urban_vars]

Cost_Sanitation<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(st_all,nation==i)
datap<-datap[,c(id_vars,c(rural_vars,urban_vars))]
#separate classes
Unimproved_rural<- c(subset(rural_vars,grepl('latrine_unimproved',rural_vars)==TRUE),subset(rural_vars,grepl('untreated_no_sewerage',rural_vars)==TRUE))
#Improved_rural<-c(subset(rural_vars,grepl('septic',rural_vars)==TRUE),subset(rural_vars,grepl('latrine_improved',rural_vars)==TRUE),subset(rural_vars,grepl('untreated_with_sewerage',rural_vars)==TRUE))
Improved_rural<-c(subset(rural_vars,grepl('latrine_improved',rural_vars)==TRUE),subset(rural_vars,grepl('untreated_with_sewerage',rural_vars)==TRUE))
SafeManaged_rural<-subset(rural_vars,!(rural_vars%in%c(Unimproved_rural,Improved_rural)))
Unimproved_urban<- c(subset(urban_vars,grepl('latrine_unimproved',urban_vars)==TRUE),subset(urban_vars,grepl('untreated_no_sewerage',urban_vars)==TRUE))
Improved_urban<-c(subset(urban_vars,grepl('septic',urban_vars)==TRUE),subset(urban_vars,grepl('latrine_improved',urban_vars)==TRUE),subset(urban_vars,grepl('untreated_with_sewerage',urban_vars)==TRUE))
SafeManaged_urban<-subset(urban_vars,!(urban_vars%in%c(Unimproved_urban,Improved_urban)))
#estimate people under each class
datap[,'People_Unimproved_rural']<-rowSums(datap[,Unimproved_rural])
datap[,'People_Improved_rural']<-rowSums(datap[,Improved_rural])
datap[,'People_SafeManaged_rural']<-rowSums(datap[,SafeManaged_rural])
datap[,'People_Unimproved_urban']<-rowSums(datap[,Unimproved_urban])
datap[,'People_Improved_urban']<-rowSums(datap[,Improved_urban])
datap[,'People_SafeManaged_urban']<-rowSums(datap[,SafeManaged_urban])

#estimate costs
datap[,'Cost_USD_trww_technical_sanitation_Unimproved_rural']<-rowSums(datap[,Unimproved_rural])*6.5 #table B.5
datap[,'Cost_USD_trww_technical_sanitation_Improved_rural']<-rowSums(datap[,Improved_rural])*68.1 #table B.5
datap[,'Cost_USD_trww_technical_sanitation_SafeManaged_rural']<-rowSums(datap[,SafeManaged_rural])*102.1#table B.5
datap[,'Cost_USD_trww_technical_sanitation_Unimproved_urban']<-rowSums(datap[,Unimproved_urban])*6.5 #table B.5
datap[,'Cost_USD_trww_technical_sanitation_Improved_urban']<-rowSums(datap[,Improved_urban])*34.1#table B.5
datap[,'Cost_USD_trww_technical_sanitation_SafeManaged_urban']<-rowSums(datap[,SafeManaged_urban])*66.2 #table B.5
datap[,'Cost_USD_trww_technical_sanitation_Total']<-rowSums(datap[,c('Cost_USD_trww_technical_sanitation_Unimproved_rural',
                                                                     'Cost_USD_trww_technical_sanitation_Improved_rural',
                                                                     'Cost_USD_trww_technical_sanitation_SafeManaged_rural',
                                                                     'Cost_USD_trww_technical_sanitation_Unimproved_urban',
                                                                     'Cost_USD_trww_technical_sanitation_Improved_urban',
                                                                     'Cost_USD_trww_technical_sanitation_SafeManaged_urban')])
cvars<-c(
         c('People_Unimproved_rural','People_Improved_rural','People_SafeManaged_rural','People_Unimproved_urban','People_Improved_urban','People_SafeManaged_urban'),
         c('Cost_USD_trww_technical_sanitation_Unimproved_rural','Cost_USD_trww_technical_sanitation_Improved_rural','Cost_USD_trww_technical_sanitation_SafeManaged_rural','Cost_USD_trww_technical_sanitation_Unimproved_urban','Cost_USD_trww_technical_sanitation_Improved_urban','Cost_USD_trww_technical_sanitation_SafeManaged_urban','Cost_USD_trww_technical_sanitation_Total')
         )
datap0<-subset(datap,policy==0)[,c(id_vars,cvars)]
datap0[,paste0(cvars,"Ref")] <- datap0 [,cvars]
datap0[,cvars]<-NULL
datap0[,'policy']<-NULL
for (j in policies)
  {
#j<-4
  datapi<-subset(datap,policy==j)[,c(id_vars,cvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(cvars,'_marginal')]<-datapi[,cvars]-datapi[,paste0(cvars,"Ref")]
  Cost_Sanitation<-append(Cost_Sanitation,list(datapi))
  }
}


#BENEFITS

#Avoided cost of carbon
co2e_vars <- c('emission_co2e_subsector_total_trww','emission_co2e_subsector_total_wali','emission_co2e_subsector_total_waso','total_co2e')
#co2e_vars <- c('total_co2e')
Benefit_CostCarbon<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap0<-subset(datap,policy==0)[,c(id_vars,co2e_vars)]
datap0[,paste0(co2e_vars,"Ref")] <- datap0 [,co2e_vars]
datap0[,co2e_vars]<-NULL
datap0[,'policy']<-NULL
#for all policies I want to estimate the delta in emissions
for (j in policies)
  {
#j<-0
  datapi<-subset(datap,policy==j)[,c(id_vars,co2e_vars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(co2e_vars,'_marginal')]<-datapi[,co2e_vars]-datapi[,paste0(co2e_vars,"Ref")]
  datapi[,paste0('benefit_USD_trww_marginal_emissions_socialcostcarbon_',co2e_vars,'_marginal')]<-datapi[,paste0(co2e_vars,'_marginal')]*50*-1*1e6 # #social cost of carbon 50 dollars per ton, multiply by 1e6 becase ouput variables are in MT
  datapi<-datapi[,c(id_vars,paste0('benefit_USD_trww_marginal_emissions_socialcostcarbon_',co2e_vars,'_marginal'))]
  Benefit_CostCarbon<-append(Benefit_CostCarbon,list(datapi))
  }
}


#less cod and bod in effluent
#liquid waste

bod_vars<-subset(colnames(data),grepl("qty_trww_bod_",colnames(data))==TRUE)
bod_vars<-c(subset(bod_vars,grepl("aerobic",bod_vars)==TRUE),subset(bod_vars,grepl("septic",bod_vars)==TRUE))
cod_vars<-subset(colnames(data),grepl("qty_trww_cod_",colnames(data))==TRUE)
cod_vars<-c(subset(cod_vars,grepl("aerobic",cod_vars)==TRUE),subset(cod_vars,grepl("septic",cod_vars)==TRUE))
n_vars<-c("qty_trww_n_removed_tonne") #,"qty_trww_n_in_effluent_tonne")
p_vars<-c("qty_trww_p_removed_tonne") #,"qty_trww_p_in_effluent_tonne")

tvars<-c(bod_vars,cod_vars,n_vars,p_vars)

Benefit_ImprovedTreatment<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap0<-subset(datap,policy==0)[,c(id_vars,tvars)]
datap0[,paste0(tvars,"Ref")] <- datap0 [,tvars]
datap0[,tvars]<-NULL
datap0[,'policy']<-NULL
#for all policies I want to estimate the delta in emissions
for (j in policies)
  {
#j<-2
  datapi<-subset(datap,policy==j)[,c(id_vars,tvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
#absolute values
  datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',bod_vars )]<-datapi[,paste0(bod_vars )]*0.1*1000 # multiplied by 1000 because output is in ton
  datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',cod_vars )]<-datapi[,paste0(cod_vars )]*0.2*1000 ## multiplied by 1000 because output is in ton
  datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',n_vars )]<-datapi[,paste0(n_vars )]*36.2*1000 # multiplied by 1000 because output is in ton
  datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',p_vars )]<-datapi[,paste0(p_vars )]*93.9*1000 ## multiplied by 1000 because output is in ton
  datapi[,'benefit_USD_trww_healthandenvironment_improvedtreatmeent_Total']<-rowSums(datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',tvars)])
#marginal
  datapi[,paste0(tvars,'_marginal')]<-datapi[,tvars]-datapi[,paste0(tvars,"Ref")]
  datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',bod_vars,'_marginal')]<-datapi[,paste0(bod_vars,'_marginal')]*0.1*1000 # multiplied by 1000 because output is in ton
  datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',cod_vars,'_marginal')]<-datapi[,paste0(cod_vars,'_marginal')]*0.2*1000 ## multiplied by 1000 because output is in ton
  datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',n_vars,'_marginal')]<-datapi[,paste0(n_vars,'_marginal')]*36.2*1000 # multiplied by 1000 because output is in ton
  datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',p_vars,'_marginal')]<-datapi[,paste0(p_vars,'_marginal')]*93.9*1000 ## multiplied by 1000 because output is in ton
  datapi[,'benefit_USD_trww_healthandenvironment_improvedtreatmeent_Total_marginal']<-rowSums(datapi[,paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',tvars,'_marginal')])
#subset to specific columns
  datapi<-datapi[,c(id_vars,'benefit_USD_trww_healthandenvironment_improvedtreatmeent_Total_marginal','benefit_USD_trww_healthandenvironment_improvedtreatmeent_Total',paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',tvars),paste0('benefit_USD_trww_healthandenvironment_improvedtreatmeent_',tvars,'_marginal'))]
  Benefit_ImprovedTreatment<-append(Benefit_ImprovedTreatment,list(datapi))
  }
}

#benefit waste avoided
#estimate recoverable rates based in per capita and population per nation, 1/3 of wasted food can be avoided.
tvars <-subset(colnames(data),grepl("qty_waso_total",colnames(data))==TRUE)
tvars_food <- subset(tvars,grepl("food",tvars)==TRUE)
tvars_other <- subset (tvars, grepl("food",tvars)==FALSE)
Benefit_WasteReduction<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap0<-subset(datap,policy==0)[,c(id_vars,tvars)]
datap0[,paste0(tvars,"Ref")] <- datap0 [,tvars]
datap0[,tvars]<-NULL
datap0[,'policy']<-NULL
#for all policies I want to estimate the delta in waste produced and then multiply that for the corresponding cost.
for (j in policies)
  {
#j<-1
  datapi<-subset(datap,policy==j)[,c(id_vars,tvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(tvars,'_marginal')]<-datapi[,tvars]-datapi[,paste0(tvars,"Ref")]
  datapi[,paste0('benefit_USD_waso_marginal_economic_wastereduction_',tvars_food,'_marginal')]<-datapi[,paste0(tvars_food,'_marginal')]*3000*-1*(1/5) #row 1 table B.4, we assume that 1/5 of the food wasted is actually the share that can be avoided
  datapi[,paste0('benefit_USD_waso_marginal_economic_wastereduction_',tvars_other,'_marginal')]<-datapi[,paste0(tvars_other,'_marginal')]*500*-1 #row 2 table B.4
  datapi[,'benefit_USD_waso_marginal_economic_wastereduction_Total']<-datapi[,paste0('benefit_USD_waso_marginal_economic_wastereduction_',tvars_food,'_marginal')]+rowSums(datapi[,paste0('benefit_USD_waso_marginal_economic_wastereduction_',tvars_other,'_marginal')])
  datapi<-datapi[,c(id_vars,'benefit_USD_waso_marginal_economic_wastereduction_Total')]
  Benefit_WasteReduction<-append(Benefit_WasteReduction,list(datapi))
  }
}

#benefit recycled materiales
tvars <-subset(colnames(data),grepl("qty_waso_recycled",colnames(data))==TRUE)
tvars_paper<-subset(tvars,grepl("paper",tvars)==TRUE)
tvars_glass<-subset(tvars,grepl("glass",tvars)==TRUE)
tvars_metal<-subset(tvars,grepl("metal",tvars)==TRUE)
tvars_other<-subset(tvars,!(tvars%in%c(tvars_paper,tvars_glass,tvars_metal)))

Benefit_RecycledWaste<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap<-datap[,c(id_vars,tvars)]

#'benefit_USD_waso_absolute_economic_recyclables_total'


datap[,'benefit_USD_waso_absolute_economic_recyclables_RecycledPaper']<-datap[,tvars_paper]*140 # table B.7
datap[,'benefit_USD_waso_absolute_economic_recyclables_RecycledGlass']<-datap[,tvars_glass]*25 #table B.7
datap[,'benefit_USD_waso_absolute_economic_recyclables_RecycledMetal']<-datap[,tvars_metal]*1307 # table B.7
datap[,'benefit_USD_waso_absolute_economic_recyclables_other']<-rowSums(datap[,tvars_other]*mean(c(140,25)))
datap[,'benefit_USD_waso_absolute_economic_recyclables_Total']<-datap[,'benefit_USD_waso_absolute_economic_recyclables_RecycledPaper']+
                                                                datap[,'benefit_USD_waso_absolute_economic_recyclables_RecycledGlass']+
                                                                datap[,'benefit_USD_waso_absolute_economic_recyclables_RecycledMetal']+
                                                                datap[,'benefit_USD_waso_absolute_economic_recyclables_other']
datap<-datap[,c(id_vars,'benefit_USD_waso_absolute_economic_recyclables_Total')]
Benefit_RecycledWaste<-append(Benefit_RecycledWaste,list(datap))
}

#benefit compost
tvars <-subset(colnames(data),grepl("qty_waso_compost",colnames(data))==TRUE)
Benefit_CompostedWaste<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap<-datap[,c(id_vars,tvars)]
datap[,'benefit_USD_waso_absolute_economic_compostables_Total']<-rowSums(datap[,tvars]*2.9) # Table B.7
datap<-datap[,c(id_vars,'benefit_USD_waso_absolute_economic_compostables_Total')]
Benefit_CompostedWaste<-append(Benefit_CompostedWaste,list(datap))
}

#Benefit solid waste management improvement

OpenBurning<-subset(colnames(data),grepl("qty_waso_incinerated",colnames(data))==TRUE)
OpenDumping<-subset(colnames(data),grepl("qty_waso_open_dump_",colnames(data))==TRUE)
Landfilled<-subset(colnames(data),grepl("qty_waso_landfilled_",colnames(data))==TRUE)
AnaerobicBiogas<-subset(colnames(data),grepl("qty_waso_biogas_",colnames(data))==TRUE)
Recycling<-subset(colnames(data),grepl("qty_waso_recycled_",colnames(data))==TRUE)
Composting<-subset(colnames(data),grepl("qty_waso_compost_",colnames(data))==TRUE)
tvars<-c(OpenBurning,OpenDumping,Landfilled,AnaerobicBiogas,Recycling,Composting)

Benefit_ImpWasteMgmnt<-list()
for (i in nations)
{
#i<-'argentina'
#i<-'costa_rica'
datap<-subset(data,nation==i)
datap<-datap[,c(id_vars,tvars)]
datap[,'Total_UnmanagedWaste']<-rowSums(datap[,c(OpenBurning,OpenDumping)])
datap[,'Total_ManagedWaste'] <-rowSums(datap[,c(Landfilled,AnaerobicBiogas,Recycling,Composting)])
datap[,'Percent_UnmanagedWaste']<-datap[,'Total_UnmanagedWaste']/(datap[,'Total_UnmanagedWaste']+datap[,'Total_ManagedWaste'])
datap[,'Percent_ManagedWaste']<-datap[,'Total_ManagedWaste']/(datap[,'Total_UnmanagedWaste']+datap[,'Total_ManagedWaste'])
datap<-datap[,c(id_vars,'Percent_UnmanagedWaste','Percent_ManagedWaste')]
datap<-Reduce(function(...) merge(..., all=T), list(datap,subset(pop_all,nation==i)))
datap[,'Population_UnmanagedWaste'] <- datap[,'Percent_UnmanagedWaste']*(datap[,'population_urban']+datap[,'population_rural'])
datap[,'Population_ManagedWaste'] <- datap[,'Percent_ManagedWaste']*(datap[,'population_urban']+datap[,'population_rural'])
pvars<-c('Population_UnmanagedWaste','Population_ManagedWaste')
datap<-datap[,c(id_vars,pvars)]
datap0<-subset(datap,policy==0)[,c(id_vars,pvars)]
datap0[,paste0(pvars,"Ref")] <- datap0 [,pvars]
datap0[,pvars]<-NULL
datap0[,'policy']<-NULL
#for all policies I want to estimate the delta in waste produced and then multiply that for the corresponding cost.
for (j in policies)
  {
#j<-1
  datapi<-subset(datap,policy==j)[,c(id_vars,pvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(pvars,'_marginal')]<-datapi[,pvars]-datapi[,paste0(pvars,"Ref")]
  datapi[,'benefit_USD_waso_healthandenvironment_improvedmanagement_Total_marginal']<-ifelse(datapi[,'Population_ManagedWaste_marginal']>0,datapi[,'Population_ManagedWaste_marginal']*55,0) # table B.7
  datapi<-datapi[,c(id_vars,'benefit_USD_waso_healthandenvironment_improvedmanagement_Total_marginal')]
  Benefit_ImpWasteMgmnt<-append(Benefit_ImpWasteMgmnt,list(datapi))
  }
}



#Benefit of improved household sanitation
#different approach reading input fractions

#create table for each policy
st_all<-apply(Exp,1,function(x){pivot<-subset(St,TransformationName==x['sanitation']);
                                pivot$policy<-x['Policy_ID'];
                                pivot$TransformationName<-NULL;
                                pivot})
st_all<-do.call('rbind',st_all)
rural_vars<-subset(colnames(st_all),grepl("rural",colnames(st_all))==TRUE)
urban_vars<-subset(colnames(st_all),grepl("urban",colnames(st_all))==TRUE)

st_all<-Reduce(function(...) merge(..., all=T), list(pop_all,st_all))
st_all$Future_ID<-0


#estimate population proportions
st_all[,rural_vars]<-st_all[,'population_rural']*st_all[,rural_vars]
st_all[,urban_vars]<-st_all[,'population_urban']*st_all[,urban_vars]

Benefit_ImpSanitation<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(st_all,nation==i)
datap<-datap[,c(id_vars,c(rural_vars,urban_vars))]
datap0<-subset(datap,policy==0)
datap0[,paste0(c(rural_vars,urban_vars),"Ref")] <- datap0 [,c(rural_vars,urban_vars)]
datap0[,c(rural_vars,urban_vars)]<-NULL
datap0[,'policy']<-NULL
for (j in policies)
  {
  #j<-2
  datapi<-subset(datap,policy==j)[,c(id_vars,c(rural_vars,urban_vars))]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(c(rural_vars,urban_vars),'_Delta')]<-datapi[,c(rural_vars,urban_vars)]-datapi[,paste0(c(rural_vars,urban_vars),"Ref")]
  datapi<-datapi[,c(id_vars,paste0(c(rural_vars,urban_vars),'_Delta'))]
  UnimprovedSanitation<-c(subset(colnames(datapi),grepl("unimproved",colnames(datapi))==TRUE),subset(colnames(datapi),grepl("no_sewerage",colnames(datapi))==TRUE))
  ImprovedSanitation<-subset(colnames(datapi),!(colnames(datapi)%in%UnimprovedSanitation))
  ImprovedSanitation<-subset(ImprovedSanitation,!(ImprovedSanitation%in%id_vars))
  datapi[,'benefit_USD_trww_healthandenvironment_improvedsanitation_Total']<-rowSums(datapi[,ImprovedSanitation])
  datapi[,'benefit_USD_trww_healthandenvironment_improvedsanitation_Total']<-ifelse(datapi[,'benefit_USD_trww_healthandenvironment_improvedsanitation_Total']>0,datapi[,'benefit_USD_trww_healthandenvironment_improvedsanitation_Total'],0)
  datapi[,'benefit_USD_trww_healthandenvironment_improvedsanitation_Total']<-datapi[,'benefit_USD_trww_healthandenvironment_improvedsanitation_Total']*200 #table B.7
  datapi<-datapi[,c(id_vars,'benefit_USD_trww_healthandenvironment_improvedsanitation_Total')]
  Benefit_ImpSanitation<-append(Benefit_ImpSanitation,list(datapi))
  }
}

#merge all
Benefit_WasteReduction<-do.call('rbind',Benefit_WasteReduction)
Benefit_ImprovedTreatment<-do.call('rbind',Benefit_ImprovedTreatment)
Benefit_CostCarbon<-do.call('rbind',Benefit_CostCarbon)
Benefit_RecycledWaste<-do.call('rbind',Benefit_RecycledWaste)
Benefit_CompostedWaste<-do.call('rbind',Benefit_CompostedWaste)
Benefit_ImpWasteMgmnt<-do.call('rbind',Benefit_ImpWasteMgmnt)
Benefit_ImpSanitation<-do.call('rbind',Benefit_ImpSanitation)
Cost_SolidWaste<-do.call('rbind',Cost_SolidWaste)
Cost_Treatment<-do.call('rbind',Cost_Treatment)
Cost_WasteReduction<-do.call('rbind',Cost_WasteReduction)
Cost_Sanitation<-do.call('rbind',Cost_Sanitation)

dim(Benefit_WasteReduction)
dim(Benefit_ImprovedTreatment)
dim(Benefit_CostCarbon)
dim(Benefit_RecycledWaste)
dim(Benefit_CompostedWaste)
dim(Benefit_ImpWasteMgmnt)
dim(Benefit_ImpSanitation)
dim(Cost_Treatment)
dim(Cost_SolidWaste)
dim(Cost_WasteReduction)
dim(Cost_Sanitation)

#merge all
CostBenefits_all<- Reduce(function(...) merge(..., all=T), list(Benefit_WasteReduction,
                                                                Benefit_ImprovedTreatment,
                                                                Benefit_CostCarbon,
                                                                Benefit_RecycledWaste,
                                                                Benefit_CompostedWaste,
                                                                Benefit_ImpWasteMgmnt,
                                                                Benefit_ImpSanitation,
                                                                Cost_SolidWaste,
                                                                Cost_Treatment,
                                                                Cost_WasteReduction,
                                                                Cost_Sanitation
                                                              ))
#append waste reduction

#write
#dir_out<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Output\)"
#dir_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Output\)"
#write.csv(CostBenefits_all,paste0(dir_out,"CostBenefitAll_new.csv"),row.names=FALSE)


#dir_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\outputs\)"
dir_out<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\outputs\)"
#write.csv(CostBenefits_all,paste0(dir_out,"CostBenefitAll_2022_11_28.csv"),row.names=FALSE)
#write.csv(CostBenefits_all,paste0(dir_out,"CostBenefitAll_2023_02_16.csv"),row.names=FALSE)
write.csv(CostBenefits_all,paste0(dir_out,"CostBenefitAll_2023_03_03.csv"),row.names=FALSE)
