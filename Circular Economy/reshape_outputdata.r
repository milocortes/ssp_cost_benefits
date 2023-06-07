library(data.table)
#Set root directory
#root<- r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\)"
#root<- r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\)"
root<- r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\)"
#ouputfile
output.file<-r"(outputs\output_waste_all_2023_03_05.csv)"
#output.file<-r"(outputs\output_waste_all_2022_12_14.csv)"
data<-read.csv(paste0(root,output.file))
data$total_co2e <-data$emission_co2e_subsector_total_trww+data$emission_co2e_subsector_total_wali+data$emission_co2e_subsector_total_waso
data$Future_ID <- 0
data$X<-NULL

#set unique identifiers
id_vars <-c('nation','time_period','policy','Future_ID')

#melt data output
data<-data.table::data.table(data)

DT.m1 = melt(data, id.vars = id_vars,
                   measure.vars = subset(colnames(data),!(colnames(data)%in%id_vars)),
             )

#dir_out<-r"(calibration\CircularEconomy\Output\)"
#write.csv(DT.m1,paste0(root,dir_out,output_waste_all_long.csv"),row.names=FALSE)

#dir_out<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Tableau\Nidhi_new\for Nidhi long\)"
#dir_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Tableau\Nidhi_new\for Nidhi long\)"
#dir_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\51. WB Decarbonization Project\InitialWorkbooks\Circular Economy Data\)"
dir_out<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\51. WB Decarbonization Project\InitialWorkbooks\Circular Economy Data\)"
write.csv(DT.m1,paste0(dir_out,"output_waste_all_long.csv"),row.names=FALSE)


#CosfBenefit file
library(data.table)
#cb.file<-r"(outputs\CostBenefitAll_2022_11_28.csv)"
#data<-read.csv(paste0(root,cb.file))

#adhoc
#root<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\outputs\)"
root<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\outputs\)"
#cb.file<-r"(CostBenefitAll_2022_11_28.csv)"
#cb.file<-r"(CostBenefitAll_2023_02_16.csv)"
cb.file<-r"(CostBenefitAll_2023_03_03.csv)"
data<-read.csv(paste0(root,cb.file))

#set unique identifiers
id_vars <-c('nation','time_period','policy','Future_ID')

#melt data output
data<-data.table::data.table(data)
#sapply(data,class)

library(data.table)

DT.m2 = melt(data, id.vars = id_vars,
                   measure.vars = subset(colnames(data),!(colnames(data)%in%id_vars)),
             )

#dir_out<-r"(calibration\CircularEconomy\Output\)"
#write.csv(DT.m2,paste0(root,dir_out,"CostBenefitAll_long.csv"),row.names=FALSE)


#adhoc
#dir_out<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Tableau\Nidhi_new\for Nidhi long\)"
#dir_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Tableau\Nidhi_new\for Nidhi long\)"
#dir_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\51. WB Decarbonization Project\InitialWorkbooks\Circular Economy Data\)"
dir_out<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\51. WB Decarbonization Project\InitialWorkbooks\Circular Economy Data\)"
write.csv(DT.m2,paste0(dir_out,"CostBenefitAll_long.csv"),row.names=FALSE)

