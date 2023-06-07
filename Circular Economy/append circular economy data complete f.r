#append circular economy data complete file 

#read data  
dir.data<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\CalibrationVectors\)"
#data<-read.csv(paste0(dir.data,"data_complete_future_2022_12_08_test.csv"))
data<-read.csv(paste0(dir.data,"data_complete_future_2022_12_08.csv"))


#read transformation
dir.dataT<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Transformations\)"
sanitation<-read.csv(paste0(dir.dataT,"SanitationTransformation_2022_11_25.csv"))

length(colnames(sanitation))
target_vars<-subset(colnames(sanitation),!(colnames(sanitation)%in%c("time_period","TransformationName")))

dim(data)
data[,target_vars]<-NULL
dim(data)

dim(sanitation)
sanitation<-subset(sanitation,sanitation$TransformationName=="liquid_waste_none")
dim(sanitation)


#merge
 dim(data)
 new<-merge(data,sanitation[,c(target_vars,"time_period")],by="time_period") 
 dim(new)

#add calibrated turkey 
dir.turkey <- r"(C:\Users\L03054557\Downloads\)"
turkey <- read.csv(paste0(dir.turkey,"turquia_calibrado.csv"))
dim(turkey)
dim(new)

#remove columns extra in turkey 
new <- subset(new , nation != "Turkey")
test<-subset(colnames(turkey),colnames(turkey)%in%colnames(new))

#rbdin turkey 

dim(new)
new<-rbind(new,turkey[,test])
dim(new)

#add iso_code3  
iso_code3<-read.csv(r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\CountriesList.csv)")[,c("iso_code3","nation_SISEPUEDE")]
colnames(iso_code3)<-c("iso_code3","nation")

#merge 
dim(new)
new<-merge(new,iso_code3,by="nation")
dim(new)

#order 
new<-new[order(new$iso_code3,new$time_period),]
length(unique(new$iso_code3))

#write 
write.csv(new,paste0(dir.data,"data_complete_future_2022_12_14_test.csv"))


#ippu 
dir.data<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\IPPU\CalibrationVectors\)"
data<-read.csv(paste0(dir.data,"data_complete_future_2022_06_28_pivot_new_rescaled.csv"))

#merge 
dim(data)
new<-merge(data,iso_code3,by="nation")
dim(new)

#order 
new<-new[order(new$iso_code3,new$time_period),]
length(unique(new$iso_code3))
write.csv(new,paste0(dir.data,"data_complete_future_2022_06_28_pivot_new_rescaled_updated.csv.csv"))


#afolu 
dir.data<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\AFOLU\CalibrationVectors\)"
data<-read.csv(paste0(dir.data,"data_complete_future_2022_09_30_test.csv"))
data$nation<-data$Nation
data$Nation <- NULL

#merge 
dim(data)
new<-merge(data,iso_code3,by="nation")
dim(new)

#order 
new<-new[order(new$iso_code3,new$time_period),]
length(unique(new$iso_code3))
write.csv(new,paste0(dir.data,"data_complete_future_2022_09_30_test_updated.csv"))


