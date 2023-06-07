#read file
#data.dir <- r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Output\)"
data.dir <- r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Output\)"
file.name <- r"(output_waste_all.csv)"

#Read file
data<-read.csv(paste0(data.dir,file.name))
data$total_co2e <-data$emission_co2e_subsector_total_trww+data$emission_co2e_subsector_total_wali+data$emission_co2e_subsector_total_waso
data$Future_ID <- 0
data$X<-NULL

#how we identify each individual run
id_vars <-c('nation','time_period','policy','Future_ID')

#how many policies we are comparing
policies <- unique(data$policy)

#how many nations we are working with
nations <- unique(data$nation)

#COSTS
#Cost of Treatment
#Industrial and Domestic Urban Wastewater Treatment
TerciaryAerobic <- 'vol_trww_ww_treated_aerobic_m3'
TerciaryAnaerobic <-'vol_trww_ww_treated_anaerobic_m3'
Secondary <- c('vol_trww_ww_treated_latrine_improved_m3','vol_trww_ww_treated_septic_m3')
Primary <- 'vol_trww_ww_treated_latrine_unimproved_m3'
NoTreatment <- c('vol_trww_ww_untreated_no_sewerage_m3','vol_trww_ww_untreated_with_sewerage_m3')
treatment_vars <- c(TerciaryAerobic,TerciaryAnaerobic,Secondary,Primary,NoTreatment)

Cost_Treatment<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap<-datap[,c(id_vars,treatment_vars)]
datap[,paste0(TerciaryAerobic,'_CostTreatment')]<-datap[,TerciaryAerobic]*2.42
datap[,paste0(TerciaryAnaerobic,'_CostTreatment')]<-datap[,TerciaryAnaerobic]*2.32
datap[,paste0(Secondary,'_CostTreatment')]<-datap[,Secondary]*2.12
datap[,paste0(Primary,'_CostTreatment')]<-datap[,Primary]*1.99
datap[,paste0(NoTreatment,'_CostTreatment')]<-datap[,NoTreatment]*1.83
datap<-datap[,c(id_vars,paste0(treatment_vars,'_CostTreatment'))]
datap[,'Total_CostTreatment']<-rowSums(datap[,paste0(treatment_vars,'_CostTreatment')])
Cost_Treatment<-append(Cost_Treatment,list(datap))
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
datap[,'OpenBurning_Cost']<- rowSums(datap[,OpenBurning]*7) # 7 dollars per ton of waste
datap[,'OpenDumping_Cost']<- rowSums(datap[,OpenDumping]*7) # 7 dollars per ton of waste
datap[,'Landfilled_Cost']<- rowSums(datap[,Landfilled]*31) # 31 dollars per ton of waste
datap[,'AnaerobicBiogas_Cost']<- rowSums(datap[,AnaerobicBiogas]*94) # 94 dollars per ton of waste
datap[,'Composting_Cost']<- rowSums(datap[,Composting]*28) # 28 dollars per ton of waste
datap[,'Recycling_Cost']<- rowSums(datap[,Recycling]*115) # 115 dollars per ton of waste
datap[,'Collection_Cost']<- rowSums(datap[,c(Recycling,Landfilled)]*62) # 62 dollars per ton of waste
datap[,'Total_CostSolidWaste']<-datap[,'OpenBurning_Cost']+datap[,'OpenDumping_Cost']+datap[,'Landfilled_Cost']+datap[,'AnaerobicBiogas_Cost']+datap[,'Composting_Cost']+datap[,'Recycling_Cost']+datap[,'Collection_Cost']
Cost_SolidWaste<-append(Cost_SolidWaste,list(datap))
}

#Cost of reducing waste
tvars <-subset(colnames(data),grepl("qty_waso_total",colnames(data))==TRUE)
tvars_food <- subset(tvars,grepl("food",tvars)==TRUE)
tvars_other <- subset (tvars, grepl("food",tvars)==FALSE)
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
for (j in policies)
  {
#j<-1
  datapi<-subset(datap,policy==j)[,c(id_vars,tvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(tvars,'_Delta')]<-datapi[,tvars]-datapi[,paste0(tvars,"Ref")]
  datapi[,paste0(tvars_food,'_CostWasteReduction')]<-datapi[,paste0(tvars_food,'_Delta')]*40*-1 #row 1 table B.4
  datapi[,paste0(tvars_other,'_CostWasteReduction')]<-datapi[,paste0(tvars_other,'_Delta')]*400*-1 #row 2 table B.4
  datapi[,'Total_CostWasteReduction']<-datapi[,paste0(tvars_food,'_CostWasteReduction')]+rowSums(datapi[,paste0(tvars_other,'_CostWasteReduction')])
  datapi<-datapi[,c(id_vars,'Total_CostWasteReduction')]
  Cost_WasteReduction<-append(Cost_WasteReduction,list(datapi))
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
  datapi[,paste0(co2e_vars,'_Delta')]<-datapi[,co2e_vars]-datapi[,paste0(co2e_vars,"Ref")]
  datapi[,paste0(co2e_vars,'_BenefitSocialCostCarbon')]<-datapi[,paste0(co2e_vars,'_Delta')]*50*-1*1e6 # #social cost of carbon 50 dollars per ton, multiply by 1e6 becase ouput variables are in MT
  datapi<-datapi[,c(id_vars,paste0(co2e_vars,'_BenefitSocialCostCarbon'))]
  Benefit_CostCarbon<-append(Benefit_CostCarbon,list(datapi))
  }
}

#less cod and bod in effluent
#liquid waste

bod_vars<-
c(
#'qty_trww_tow_in_effluent_bod_treated_aerobic_tonne',
#'qty_trww_tow_in_effluent_bod_treated_anaerobic_tonne',
'qty_trww_tow_in_effluent_bod_treated_latrine_improved_tonne',
'qty_trww_tow_in_effluent_bod_treated_latrine_unimproved_tonne',
'qty_trww_tow_in_effluent_bod_treated_septic_tonne',
'qty_trww_tow_in_effluent_bod_untreated_no_sewerage_tonne',
'qty_trww_tow_in_effluent_bod_untreated_with_sewerage_tonne'
)

cod_vars<-
c(
#'qty_trww_tow_in_effluent_cod_treated_aerobic_tonne',
#'qty_trww_tow_in_effluent_cod_treated_anaerobic_tonne',
'qty_trww_tow_in_effluent_cod_treated_latrine_improved_tonne',
'qty_trww_tow_in_effluent_cod_treated_latrine_unimproved_tonne',
'qty_trww_tow_in_effluent_cod_treated_septic_tonne',
'qty_trww_tow_in_effluent_cod_untreated_no_sewerage_tonne',
'qty_trww_tow_in_effluent_cod_untreated_with_sewerage_tonne'
)

tvars<-c(bod_vars,cod_vars)

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
  datapi[,paste0(tvars,'_Delta')]<-datapi[,tvars]-datapi[,paste0(tvars,"Ref")]
  datapi[,paste0(bod_vars,'_BenefitImprovedTreatment')]<-datapi[,paste0(bod_vars,'_Delta')]*0.1*1000*-1 # #social cost of carbon 50 dollars per ton
  datapi[,paste0(cod_vars,'_BenefitImprovedTreatment')]<-datapi[,paste0(cod_vars,'_Delta')]*0.2*1000*-1 # #social cost of carbon 50 dollars per ton
  datapi<-datapi[,c(id_vars,paste0(tvars,'_BenefitImprovedTreatment'))]
  datapi[,'Total_Benefit_ImprovedTreatment']<-rowSums(datapi[,paste0(tvars,'_BenefitImprovedTreatment')])
  Benefit_ImprovedTreatment<-append(Benefit_ImprovedTreatment,list(datapi))
  }
}

#benefit waste avoided
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
  datapi[,paste0(tvars,'_Delta')]<-datapi[,tvars]-datapi[,paste0(tvars,"Ref")]
  datapi[,paste0(tvars_food,'_BenefitWasteReduction')]<-datapi[,paste0(tvars_food,'_Delta')]*3000*-1 #row 1 table B.4
  datapi[,paste0(tvars_other,'_BenefitWasteReduction')]<-datapi[,paste0(tvars_other,'_Delta')]*500*-1 #row 2 table B.4
  datapi[,'Total_BenefitWasteReduction']<-datapi[,paste0(tvars_food,'_BenefitWasteReduction')]+rowSums(datapi[,paste0(tvars_other,'_BenefitWasteReduction')])
  datapi<-datapi[,c(id_vars,'Total_BenefitWasteReduction')]
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
datap[,'Benefit_RecycledPaper']<-datap[,tvars_paper]*140 # table B.7
datap[,'Benefit_RecycledGlass']<-datap[,tvars_glass]*25 #table B.7
datap[,'Benefit_RecycledMetal']<-datap[,tvars_metal]*1307 # table B.7
datap[,'Benefit_other']<-rowSums(datap[,tvars_other]*mean(c(140,25)))
datap[,'Total_BenefitRecycledWaste']<-datap[,'Benefit_RecycledPaper']+datap[,'Benefit_RecycledGlass']+datap[,'Benefit_RecycledMetal']+datap[,'Benefit_other']
datap<-datap[,c(id_vars,'Total_BenefitRecycledWaste')]
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
datap[,'Total_BenefitCompostedWaste']<-rowSums(datap[,tvars]*2.9) # Table B.7
datap<-datap[,c(id_vars,'Total_BenefitCompostedWaste')]
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

data_dir<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Data\Population\)"
pop_all<-read.csv(paste0(data_dir,"pop_all_future.csv"))
pop_all$nation<-tolower(pop_all$nation)
pop_all$nation<-gsub(" ","_",pop_all$nation)

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
  datapi[,paste0(pvars,'_Delta')]<-datapi[,pvars]-datapi[,paste0(pvars,"Ref")]
  datapi[,'Total_BenefitImprovedSolidWasteManagement']<-ifelse(datapi[,'Population_ManagedWaste_Delta']>0,datapi[,'Population_ManagedWaste_Delta']*55,0) # table B.7
  datapi<-datapi[,c(id_vars,'Total_BenefitImprovedSolidWasteManagement')]
  Benefit_ImpWasteMgmnt<-append(Benefit_ImpWasteMgmnt,list(datapi))
  }
}

#pending
#Benefit of improved household sanitation
tvars<-subset(colnames(data),grepl("vol_trww_ww",colnames(data))==TRUE)
UnimprovedSanitation<-c('vol_trww_ww_untreated_no_sewerage_m3')
ImprovedSanitation<-c('vol_trww_ww_treated_latrine_unimproved_m3','vol_trww_ww_untreated_with_sewerage_m3','vol_trww_ww_treated_septic_m3','vol_trww_ww_untreated_no_sewerage_m3')
SafelyManaged<-c("vol_trww_ww_treated_aerobic_m3","vol_trww_ww_treated_anaerobic_m3","vol_trww_ww_treated_latrine_improved_m3")
data_dir<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Data\Population\)"
pop_all<-read.csv(paste0(data_dir,"pop_all_future.csv"))
pop_all$nation<-tolower(pop_all$nation)
pop_all$nation<-gsub(" ","_",pop_all$nation)

Benefit_ImpSanitation<-list()
for (i in nations)
{
#i<-'argentina'
datap<-subset(data,nation==i)
datap<-datap[,c(id_vars,tvars)]
datap[,'Total_UnimprovedSanitation']<- datap[,UnimprovedSanitation] #rowSums(datap[,UnimprovedSanitation])
datap[,'Total_ImprovedSanitation'] <-rowSums(datap[,ImprovedSanitation])
datap[,'Total_SafelyManaged'] <-rowSums(datap[,SafelyManaged])
datap[,'Percent_UnimprovedSanitation']<-datap[,'Total_UnimprovedSanitation']/(datap[,'Total_UnimprovedSanitation']+datap[,'Total_ImprovedSanitation']+datap[,'Total_SafelyManaged'])
datap[,'Percent_ImprovedSanitation'] <-datap[,'Total_ImprovedSanitation']/(datap[,'Total_UnimprovedSanitation']+datap[,'Total_ImprovedSanitation']+datap[,'Total_SafelyManaged'])
datap[,'Percent_SafelyManaged'] <-datap[,'Total_SafelyManaged']/(datap[,'Total_UnimprovedSanitation']+datap[,'Total_ImprovedSanitation']+datap[,'Total_SafelyManaged'])
datap<-datap[,c(id_vars,'Percent_UnimprovedSanitation','Percent_ImprovedSanitation','Percent_SafelyManaged')]
datap<-Reduce(function(...) merge(..., all=T), list(datap,subset(pop_all,nation==i)))

datap[,'rural_UnimprovedSanitation'] <- datap[,'population_rural']*datap[,'Percent_UnimprovedSanitation']
datap[,'rural_ImprovedSanitation']  <- datap[,'population_rural']*datap[,'Percent_ImprovedSanitation']
datap[,'rural_SafelyManaged'] <- datap[,'population_rural']*datap[,'Percent_ImprovedSanitation']

datap[,'urban_UnimprovedSanitation'] <- datap[,'population_urban']*datap[,'Percent_UnimprovedSanitation']
datap[,'urban_ImprovedSanitation']  <- datap[,'population_urban']*datap[,'Percent_ImprovedSanitation']
datap[,'urban_SafelyManaged'] <- datap[,'population_urban']*datap[,'Percent_ImprovedSanitation']
pvars<-c("rural_UnimprovedSanitation","rural_ImprovedSanitation","rural_SafelyManaged","urban_UnimprovedSanitation","urban_ImprovedSanitation","urban_SafelyManaged")
datap0<-subset(datap,policy==0)[,c(id_vars,pvars)]
datap0[,paste0(pvars,"Ref")] <- datap0 [,pvars]
datap0[,pvars]<-NULL
datap0[,'policy']<-NULL
#for all policies I want to estimate the delta in waste produced and then multiply that for the corresponding cost.
for (j in policies)
  {
#j<-2
  datapi<-subset(datap,policy==j)[,c(id_vars,pvars)]
  datapi<- Reduce(function(...) merge(..., all=T), list(datapi,datap0))
  datapi[,paste0(pvars,'_Delta')]<-datapi[,pvars]-datapi[,paste0(pvars,"Ref")]

datapi[order(datapi$time_period),]

  datapi[,'Total_BenefitImprovedSolidWasteManagement']<-ifelse(datapi[,'Population_ManagedWaste_Delta']>0,datapi[,'Population_ManagedWaste_Delta']*55,0) # table B.7
  datapi<-datapi[,c(id_vars,'Total_BenefitImprovedSolidWasteManagement')]
  Benefit_ImpWasteMgmnt<-append(Benefit_ImpWasteMgmnt,list(datapi))
  }
}




#Cost of household sanitation



#datap$Diff<-apply(datap,1,function(x){ max(c(0,as.numeric(x['Percent_ManagedWaste'])-as.numeric(x['Percent_UnmanagedWaste'])))})
#merge population
datap<-Reduce(function(...) merge(..., all=T), list(datap,subset(pop_all,nation==i)))
datap$pop<-datap$population_urban+datap$population_rural
#estimate benefit
data[,'Total_BenefitSolidWasteMng']<-data[,'pop']*data[,'Diff']
#no but we need to compare it against no action.


#merge all
Benefit_WasteReduction<-do.call('rbind',Benefit_WasteReduction)
Benefit_ImprovedTreatment<-do.call('rbind',Benefit_ImprovedTreatment)
Benefit_CostCarbon<-do.call('rbind',Benefit_CostCarbon)
Benefit_RecycledWaste<-do.call('rbind',Benefit_RecycledWaste)
Benefit_CompostedWaste<-do.call('rbind',Benefit_CompostedWaste)
Benefit_ImpWasteMgmnt<-do.call('rbind',Benefit_ImpWasteMgmnt)
Cost_SolidWaste<-do.call('rbind',Cost_SolidWaste)
Cost_Treatment<-do.call('rbind',Cost_Treatment)
Cost_WasteReduction<-do.call('rbind',Cost_WasteReduction)

dim(Benefit_ImprovedTreatment)
dim(Benefit_WasteReduction)
dim(Benefit_CostCarbon)
dim(Benefit_RecycledWaste)
dim(Benefit_CompostedWaste)
dim(Cost_Treatment)
dim(Cost_SolidWaste)
dim(Cost_WasteReduction)

#merge all
CostBenefits_all<- Reduce(function(...) merge(..., all=T), list(Benefit_ImprovedTreatment,Benefit_CostCarbon,Cost_SolidWaste,Cost_Treatment))
#write
dir_out<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Output\)"
write.csv(CostBenefits_all,paste0(dir_out,"CostBenefitAll.csv"),row.names=FALSE)
