#
data_in<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\)"

#population
pop<-read.csv(paste0(data_in,'pop_all_future.csv'))

#add gdp
gdp<-read.csv(paste0(data_in,'gdp_all_future.csv'))

#add ha
areas<-read.csv(paste0(data_in,'areas_future.csv'))

#read calib vector
data_calib<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\CalibrationVectors\)"
calib<-read.csv(paste0(data_calib,'df_vector_paises_all_period.csv'))
#read obs vector estimate calib real values
obs<-read.csv(paste0(data_in,'observed_fake_data_periodo.csv'))

ns<-unique(calib$nation)
ts<-subset(colnames(calib),colnames(calib)!='nation')
#i<-ns[1]
#j<-ts[1]
for (i in ns){
  for (j in ts){
  calib[calib$nation==i,j]<-calib[calib$nation==i,j]*mean(obs[obs$nation==i,j])
  }
}

#add observed data
 obs<-read.csv(paste0(data_in,'observed_fake_data_periodo.csv'))
 obs<-subset(obs,obs$time_period==max(obs$time_period))
 obs[,'time_period']<-NULL
 obs[,'area_country_ha']<-NULL
 obs[,'area_country_ha']<-NULL
 obs[,'population_urban']<-NULL
 obs[,'population_rural']<-NULL
 obs[,"va_commercial_mmm_usd"]<-NULL
 obs[,"va_industrial_mmm_usd"]<-NULL
 obs[,"va_manufacturing_mmm_usd"]<-NULL
 obs[,"va_mining_mmm_usd"]<-NULL
 obs[,subset(colnames(calib),colnames(calib)!='nation')]<-NULL
 obs<-subset(obs,obs$nation%in%unique(calib$nation))

#merge all
DataIn<-Reduce(function(...) merge(..., all.x=T), list(gdp,pop,areas,calib,obs))
DataIn<-DataIn[order(DataIn$nation,DataIn$time_period),]

write.csv(DataIn,paste0(data_calib,'data_complete_future.csv'),row.names=FALSE)

#verify
test<-read.csv(r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Transformations\fake_data_complete.csv)")
colnames(test)%in%colnames(DataIn)
notIn<-subset(colnames(DataIn),!(colnames(DataIn)%in%colnames(test)))

subset(colnames(test),!(colnames(test)%in%colnames(DataIn)))

#in this ocasion
final<-subset(colnames(DataIn),colnames(DataIn)%in%c(colnames(test),'nation'))
write.csv(DataIn[,final],paste0(data_calib,'data_complete_future_CE.csv'),row.names=FALSE)

DataIn


notIn%in%colnames(obs)
