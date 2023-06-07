#
#data_in<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\)"
#data_in<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\)"

data_in<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\SocioEconomic\input_to_sisepuede\projected_data\)"
#data_in<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\SocioEconomic\input_to_sisepuede\projected_data\)"


#population
#pop<-read.csv(paste0(data_in,'pop_all_future.csv'))
pop_rural<-read.csv(paste0(data_in,'population_gnrl_rural.csv'))
pop_urban<-read.csv(paste0(data_in,'population_gnrl_urban.csv'))

#add gdp
#gdp<-read.csv(paste0(data_in,'gdp_all_future.csv'))

gdp<-read.csv(paste0(data_in,'gdp_mmm_usd.csv'))

#add ha
#areas<-read.csv(paste0(data_in,'areas_future.csv'))

areas<-read.csv(paste0(data_in,'area_gnrl_country_ha.csv'))

#merge the three socio-economic variables
dim(gdp)
dim(areas)
dim(pop_rural)
dim(pop_urban)
DataIn_A<-Reduce(function(...) merge(...), list(gdp,pop_rural,pop_urban,areas[,c("iso_code3","area_gnrl_country_ha")]))
dim(DataIn_A)

#add time_period 
DataIn_A$time_period<-DataIn_A$Year - 2015 
DataIn_A<-DataIn_A[order(DataIn_A$Nation,DataIn_A$time_period),]


#load fake data complete 
dir_fakedata<- r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\)"
#dir_fakedata<- r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\)"
fake_data<-read.csv(paste0(dir_fakedata,"fake_data_complete.csv"))
dim(fake_data)

#Delete already existing columns 
target_vars<-subset(colnames(DataIn_A),(colnames(DataIn_A)%in%colnames(fake_data)))
target_vars<-subset(target_vars,target_vars!="time_period")
fake_data[,target_vars] <- NULL 

#merge with DataIn_A
dim(fake_data)
dim(DataIn_A)
DataIn<-merge(DataIn_A,fake_data,by="time_period")
dim(DataIn)

DataIn<-DataIn[order(DataIn$Nation,DataIn$time_period),]


#read calib vector
data_calib<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\CalibrationVectors\)"
#data_calib<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\CalibrationVectors\)"
calib<-read.csv(paste0(data_calib,'df_vector_paises_all_period_2022_06_14_new.csv'))
target_nations<-unique(calib$iso_code3)

#subset to calibrated nations 
dim(DataIn)
DataIn<-subset(DataIn,iso_code3%in%target_nations)
dim(DataIn)
DataIn<-DataIn[order(DataIn$Nation,DataIn$time_period),]

# re-estimate values using calibration values 
ns<-unique(calib$iso_code3)
ts<-subset(colnames(calib),!(colnames(calib)%in%c('Nation','iso_code3')))
length(ts)

ts<-subset(ts,ts%in%colnames(DataIn))
ts%in%colnames(DataIn)
length(ts)
#i<-ns[23]
#j<-ts[1]
for (i in ns){
  for (j in ts){
  DataIn[DataIn$iso_code3==i,j]<-calib[calib$iso_code3==i,j]*mean(DataIn[DataIn$iso_code3==i,j])
  }
}
DataIn<-DataIn[order(DataIn$Nation,DataIn$time_period),]
unique(DataIn$N)

DataIn$nation <-tolower(DataIn$Nation)
DataIn$nation <- gsub(", the","",DataIn$nation)
DataIn$nation <- gsub(" ","_",DataIn$nation)

write.csv(DataIn,paste0(data_calib,'data_complete_future_2023_03_10.csv'),row.names=FALSE)
