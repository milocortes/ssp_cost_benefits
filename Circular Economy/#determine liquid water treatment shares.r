#clean version 
#set root directory 
root <- r'(C:\Users\L03054557\OneDrive\)'

###############################################################################################
# INTEGRATE SOCIOECONOMIC 
###############################################################################################

dir_data <- paste0(root,r'(Edmundo-ITESM\sisepuede_data\SocioEconomic\\)')
files <- list.files(dir_data)

DataIn <- list()
for (i in 1:length(files))
{
#i<-4
target_dir_historial <- r"(\input_to_sisepuede\historical\)"
target_dir_projected <- r"(\input_to_sisepuede\projected\)"
historical<-read.csv(paste0(dir_data,files[i],target_dir_historial,files[i],".csv"))
projected<-read.csv(paste0(dir_data,files[i],target_dir_projected,files[i],".csv"))
projected<-subset(projected,Year>max(historical$Year))
all <- rbind(historical,projected)
#edit colnames 
all$X<-NULL
colnames(all) <- gsub("Country","Nation",colnames(all))
colnames(all) <- gsub("ISO3","iso_code3",colnames(all))
DataIn <- append(DataIn,list(all))
rm(all)
}
#elimate differences in country names 
DataIn[[3]][,"Nation"]<-NULL
DataIn[[4]][,"Nation"]<-NULL
#merge all data set  
DataIn <- Reduce(function(...) merge(...,), DataIn)
#check dimmensions make sense 

#================================================================================================
#Read FAO database 
#================================================================================================
dir_fao_data <- paste0(root,r'(Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\AQUASTAT Statistics Bulk Download\)')
fao_data <- read.csv(paste0(dir_fao_data,"AQUASTAT Statistics Bulk Download (EN).csv"))

#indicate variables of interest 
target_vars <- c(
                 "Collected municipal wastewater",
                 "Produced municipal wastewater" ,
                 "Treated municipal wastewater", 
                 "Not treated municipal wastewater",
                 "Total population with access to safe drinking-water (JMP)",
                 "Rural population with access to safe drinking-water (JMP)",
                 "Urban population with access to safe drinking-water (JMP)"
                 )
#
target_years <- c(2005,2010,2015)

#subset fao_data  
fao_data <- subset(fao_data,Variable%in%target_vars)
fao_data <-subset(fao_data,Year%in%target_years)

#transform fron long to wide
library(data.table)
fao_data<- data.frame(dcast(data.table(fao_data), Country + Year ~ Variable, value.var ="Value" ))
colnames(fao_data) <- gsub("Country","Nation", colnames(fao_data))

#homogenized country names
#note DataIn is missing at least 10 countries we could have  
fao_data$Nation <- gsub("Türkiye","Turkey",fao_data$Nation)
fao_data$Nation <-gsub("Viet Nam","Vietnam",fao_data$Nation)
fao_data$Nation <-gsub("United States of America","United States",fao_data$Nation)
fao_data$Nation <-gsub("United Kingdom of Great Britain and Northern Ireland","United Kingdom",fao_data$Nation)
fao_data$Nation <-gsub("Republic of Korea","Korea, Rep.",fao_data$Nation)  #review korea in Data In
fao_data$Nation <-gsub("Bolivia \\(Plurinational State of\\)","Bolivia",fao_data$Nation)
fao_data$Nation <-gsub("Venezuela \\(Bolivarian Republic of\\)","Venezuela",fao_data$Nation)


#Which countries in fao data are not in DataIn?
subset(unique(fao_data$Nation),!(unique(fao_data$Nation)%in%unique(DataIn$Nation)) )
#lets build the linear model 
dim(fao_data)
new_fao <- Reduce(function(...) merge(...), list(fao_data,DataIn[,c("Nation","Year","gdp_mmm_usd","population_gnrl_rural","population_gnrl_urban")]))
dim(new_fao)

#train model 
#now fill data gaps by imputation 

target_vars <- subset(colnames(fao_data),!(colnames(fao_data)%in%c("Nation","Year")))
imputation_models <- list()
for (i in 1:length(target_vars))
{
#i<-4
#model_vars <- subset(colnames(new_fao),!(colnames(new_fao)%in%c("Nation","Year",target_vars[i])))
#model_vars <-c("gdp_mmm_usd", "population_gnrl_rural","population_gnrl_urban","Urban.population.with.access.to.safe.drinking.water..JMP.","Total.population.with.access.to.safe.drinking.water..JMP.")
model_vars <-c("gdp_mmm_usd", "population_gnrl_rural","population_gnrl_urban")
model_vars <- subset(model_vars,model_vars!=target_vars[i])
formula_imputation <- as.formula(paste(target_vars[i],"~",paste(model_vars,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,new_fao)
#extract significant coefficients 
coefs_0<-data.frame(summary(model_imputation)$coefficients)
coefs<-row.names(subset(coefs_0,coefs_0[,4]<=0.05))
coefs<-subset(coefs,coefs!="(Intercept)")
if (length(coefs)>0) {
#run model only with significant coefficients 
formula_imputation <- as.formula(paste(target_vars[i],"~",paste(coefs,collapse="+"),sep=""))
} else {
coefs <- subset(rownames(coefs_0),rownames(coefs_0)!="(Intercept)") 
formula_imputation <- as.formula(paste(target_vars[i],"~",paste(coefs,collapse="+"),sep="")) 
}
#save model 
model_imputation <- lm(formula_imputation,new_fao)
imputation_models <- append(imputation_models,list(model_imputation))
names(imputation_models)[[i]] <- target_vars[i]
new_fao[,paste0(target_vars[i],"_imputation")] <- predict(model_imputation, new_fao[,c("Nation",coefs)] )
new_fao[,paste0(target_vars[i],"_imputation")] <- ifelse(new_fao[,paste0(target_vars[i],"_imputation")] <0,0,new_fao[,paste0(target_vars[i],"_imputation")] )
new_fao[,paste0(target_vars[i],"_imputation")] <- ifelse(is.na(new_fao[,paste0(target_vars[i],"_imputation")])==TRUE,mean(new_fao [, target_vars[i]],na.rm=TRUE),new_fao[,paste0(target_vars[i],"_imputation")] )
new_fao [, target_vars[i]] <- ifelse (is.na(new_fao [, target_vars[i]])==TRUE,new_fao[,paste0(target_vars[i],"_imputation")],new_fao[, target_vars[i]])
new_fao[,paste0(target_vars[i],"_imputation")] <- NULL
}
#eliminate columns not needed
new_fao$Year <- NULL
new_fao$gdp_mmm_usd <- NULL 
new_fao$population_gnrl_rural <- NULL 
new_fao$population_gnrl_urban <- NULL 

#now that we have a clean data set, we can merge this data set back to DataIn
dim(DataIn)
DataIn <- Reduce(function(...) merge(...), list(DataIn,new_fao)) #missing info here, check and review
dim(DataIn)

#now we can finnally estimate some sisepude variables 
#we want to estimate three initial variables per sector

#urban 
DataIn[,"Produced.municipal.wastewater"] <- ifelse(round(DataIn[,"Produced.municipal.wastewater"],4)<=as.numeric(quantile(DataIn[,"Produced.municipal.wastewater"],0.10)),as.numeric(quantile(DataIn[,"Produced.municipal.wastewater"],0.10)),DataIn[,"Produced.municipal.wastewater"]) #there has to be a minimium of produced municipal water
DataIn[,"Collected.municipal.wastewater"] <- ifelse(DataIn[,"Collected.municipal.wastewater"]>DataIn[,"Produced.municipal.wastewater"],DataIn[,"Produced.municipal.wastewater"] ,DataIn[,"Collected.municipal.wastewater"])
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage"] <- (DataIn[,"Collected.municipal.wastewater"]-DataIn[,"Treated.municipal.wastewater"])/DataIn[,"Produced.municipal.wastewater"]
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage"] <- ifelse(DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage"]<0,0,DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage"])
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"] <- DataIn[,"Treated.municipal.wastewater"]/DataIn[,"Produced.municipal.wastewater"]
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"] <- ifelse(DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"]>1,1,DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"])
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_no_sewerage"] <-1- DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage"]- DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"]

#industry, we assume the same rates as in municipal water  
DataIn[,"frac_wali_ww_industrial_treatment_path_untreated_no_sewerage"] <- DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_no_sewerage"]
DataIn[,"frac_wali_ww_industrial_treatment_path_untreated_with_sewerage"] <- DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage"]
DataIn[,"frac_wali_ww_industrial_treatment_path_treated"] <- DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"]

#rural 
DataIn$ScalingRural<-DataIn[,"Rural.population.with.access.to.safe.drinking.water..JMP."]/DataIn[,"Urban.population.with.access.to.safe.drinking.water..JMP."]
#then we assume the proportions of treated and collected municipal water are scaled using this ratio 
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_with_sewerage"] <- DataIn[,"frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage"]*DataIn$ScalingRural
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"] <- DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"]*DataIn$ScalingRural
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"] <- 1- DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_with_sewerage"]- DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"]
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"] <- ifelse(DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"]>1.0,1.0,DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"])
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"] <- ifelse(DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"]<0.0,0.0,DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"])
#Re-scale 
DataIn$test<-rowSums(DataIn[,c("frac_wali_ww_domestic_rural_treatment_path_untreated_with_sewerage",
                  "frac_wali_ww_domestic_rural_treatment_path_treated",
                  "frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage")])
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"] <- DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage"]/DataIn$test
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"] <- DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"]/DataIn$test
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_with_sewerage"] <- DataIn[,"frac_wali_ww_domestic_rural_treatment_path_untreated_with_sewerage"]/DataIn$test

#checks sums are correct 
mean(rowSums(DataIn[,c("frac_wali_ww_domestic_rural_treatment_path_untreated_with_sewerage","frac_wali_ww_domestic_rural_treatment_path_treated","frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage")]))
mean(rowSums(DataIn[,c("frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage","frac_wali_ww_domestic_urban_treatment_path_treated","frac_wali_ww_domestic_urban_treatment_path_untreated_no_sewerage")]))
mean(rowSums(DataIn[,c("frac_wali_ww_industrial_treatment_path_untreated_with_sewerage","frac_wali_ww_industrial_treatment_path_treated","frac_wali_ww_industrial_treatment_path_untreated_no_sewerage")]))

#now we need to distribute types of treatment  
dir_hw_data <- r'(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\HydroWASTE_v10\)'
hw_data <- read.csv(paste0(dir_hw_data,"HydroWASTE_v10.csv"))
hw_data$count <- 1
hw_data<-aggregate(list(total = hw_data$count), list(Type = hw_data$LEVEL,iso_code3 = hw_data$CNTRY_ISO),sum)
hw_data <- data.frame(dcast(data.table(hw_data), iso_code3 ~ Type, value.var ="total" ))

#get rid of NAs
hw_data[,"Advanced"] <- ifelse(is.na(hw_data[,"Advanced"])==TRUE,0,hw_data[,"Advanced"] )
hw_data[,"Primary"] <- ifelse(is.na(hw_data[,"Primary"])==TRUE,0,hw_data[,"Primary"] )
hw_data[,"Secondary"] <- ifelse(is.na(hw_data[,"Secondary"])==TRUE,0,hw_data[,"Secondary"] )
hw_data$Total <- rowSums(hw_data[,c("Advanced","Primary","Secondary")])

#estimate percentages 
hw_data[,"Advanced"] <- hw_data[,"Advanced"]/hw_data$Total
hw_data[,"Primary"] <- hw_data[,"Primary"]/hw_data$Total
hw_data[,"Secondary"] <- hw_data[,"Secondary"]/hw_data$Total
hw_data$Total <- NULL

#merge with DataIn 
dim(DataIn)
DataIn <- merge(DataIn,hw_data,by="iso_code3", all.x =TRUE)
dim(DataIn)

#Imputate missing values  
target_vars <- c("Advanced","Primary","Secondary")
for (i in 1:length(target_vars))
{
#i<- 1
model_vars <-c("gdp_mmm_usd", "population_gnrl_rural","population_gnrl_urban")
formula_imputation <- as.formula(paste(target_vars[i],"~",paste(model_vars,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,DataIn)
summary(model_imputation)
#extract significant coefficients 
coefs<-data.frame(summary(model_imputation)$coefficients)
coefs<-row.names(subset(coefs,coefs[,4]<=0.05))
coefs<-subset(coefs,coefs!="(Intercept)")
#run model only with significant coefficients 
formula_imputation <- as.formula(paste(target_vars[i],"~",paste(coefs,collapse="+"),sep=""))
#save model 
model_imputation <- lm(formula_imputation,DataIn)
DataIn[,paste0(target_vars[i],"_imputation")] <- predict(model_imputation, DataIn[,c("Nation",coefs)] )
DataIn[,paste0(target_vars[i],"_imputation")] <- ifelse(DataIn[,paste0(target_vars[i],"_imputation")] <0,0,DataIn[,paste0(target_vars[i],"_imputation")] )
DataIn[,paste0(target_vars[i],"_imputation")] <- ifelse(is.na(DataIn[,paste0(target_vars[i],"_imputation")])==TRUE,mean(DataIn [, target_vars[i]],na.rm=TRUE),DataIn[,paste0(target_vars[i],"_imputation")] )
DataIn [, target_vars[i]] <- ifelse (is.na(DataIn [, target_vars[i]])==TRUE,DataIn[,paste0(target_vars[i],"_imputation")],DataIn[, target_vars[i]])
DataIn[,paste0(target_vars[i],"_imputation")] <- NULL
}

#Re-scale imputated variables 
DataIn$Total <- rowSums(DataIn[,c("Advanced","Primary","Secondary")])
DataIn[,"Advanced"] <- DataIn[,"Advanced"]/DataIn$Total
DataIn[,"Primary"] <- DataIn[,"Primary"]/DataIn$Total
DataIn[,"Secondary"] <- DataIn[,"Secondary"]/DataIn$Total
DataIn$Total <- NULL

#check everything is ok
mean(rowSums(DataIn[,c("Advanced","Primary","Secondary")]))

#ok now let's build the sispuede variables 
#urban 
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_advanced_aerobic"] <- 0.5*DataIn[,"Advanced"]*DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"]
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_advanced_anaerobic"] <-0.5*DataIn[,"Advanced"]*DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"] 
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_primary"] <-0.9*DataIn[,"Primary"]*DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"]
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_secondary_aerobic"] <- 0.5*DataIn[,"Secondary"]*DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"] 
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_secondary_anaerobic"] <- 0.5*DataIn[,"Secondary"]*DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"] 
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_latrine_improved"] <- 0.05*DataIn[,"Primary"]*DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"] 
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_latrine_unimproved"] <-0.05*DataIn[,"Primary"]*DataIn[,"frac_wali_ww_domestic_urban_treatment_path_treated"] 
DataIn[,"frac_wali_ww_domestic_urban_treatment_path_septic"] <- 0.00

#rural
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_advanced_aerobic"] <-  0*DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"]
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_advanced_anaerobic"] <-0*DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"]
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_primary"] <- DataIn[,"Advanced"]*DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"]
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_secondary_aerobic"] <- 0
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_secondary_anaerobic"] <- 0
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_latrine_improved"] <- 0.5*DataIn[,"Primary"]*DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"] 
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_latrine_unimproved"] <-0.5*DataIn[,"Primary"]*DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"] 
DataIn[,"frac_wali_ww_domestic_rural_treatment_path_septic"] <- DataIn[,"Secondary"]*DataIn[,"frac_wali_ww_domestic_rural_treatment_path_treated"]  

#industry
DataIn[,"frac_wali_ww_industrial_treatment_path_advanced_aerobic"] <- 0.5*DataIn[,"Advanced"]*DataIn[,"frac_wali_ww_industrial_treatment_path_treated"]
DataIn[,"frac_wali_ww_industrial_treatment_path_advanced_anaerobic"] <-0.5*DataIn[,"Advanced"]*DataIn[,"frac_wali_ww_industrial_treatment_path_treated"] 
DataIn[,"frac_wali_ww_industrial_treatment_path_primary"] <-DataIn[,"Primary"]*DataIn[,"frac_wali_ww_industrial_treatment_path_treated"] 
DataIn[,"frac_wali_ww_industrial_treatment_path_secondary_aerobic"] <- 0.5*DataIn[,"Secondary"]*DataIn[,"frac_wali_ww_industrial_treatment_path_treated"] 
DataIn[,"frac_wali_ww_industrial_treatment_path_secondary_anaerobic"] <- 0.5*DataIn[,"Secondary"]*DataIn[,"frac_wali_ww_industrial_treatment_path_treated"] 
DataIn[,"frac_wali_ww_industrial_treatment_path_latrine_improved"] <- 0.00
DataIn[,"frac_wali_ww_industrial_treatment_path_latrine_unimproved"] <-0.00
DataIn[,"frac_wali_ww_industrial_treatment_path_septic"] <- 0.00

#check all sums make sense 
ruralvars<-c("frac_wali_ww_domestic_rural_treatment_path_advanced_aerobic",
                  "frac_wali_ww_domestic_rural_treatment_path_advanced_anaerobic",
                  "frac_wali_ww_domestic_rural_treatment_path_latrine_improved",
                  "frac_wali_ww_domestic_rural_treatment_path_latrine_unimproved",
                  "frac_wali_ww_domestic_rural_treatment_path_primary",
                  "frac_wali_ww_domestic_rural_treatment_path_secondary_aerobic",
                  "frac_wali_ww_domestic_rural_treatment_path_secondary_anaerobic",
                  "frac_wali_ww_domestic_rural_treatment_path_septic",
                  "frac_wali_ww_domestic_rural_treatment_path_untreated_no_sewerage",
                  "frac_wali_ww_domestic_rural_treatment_path_untreated_with_sewerage")
mean(rowSums(DataIn[,ruralvars]))

urbanvars<-c("frac_wali_ww_domestic_urban_treatment_path_advanced_aerobic",
                  "frac_wali_ww_domestic_urban_treatment_path_advanced_anaerobic",
                  "frac_wali_ww_domestic_urban_treatment_path_latrine_improved",
                  "frac_wali_ww_domestic_urban_treatment_path_latrine_unimproved",
                  "frac_wali_ww_domestic_urban_treatment_path_primary",
                  "frac_wali_ww_domestic_urban_treatment_path_secondary_aerobic",
                  "frac_wali_ww_domestic_urban_treatment_path_secondary_anaerobic",
                  "frac_wali_ww_domestic_urban_treatment_path_septic",
                  "frac_wali_ww_domestic_urban_treatment_path_untreated_no_sewerage",
                  "frac_wali_ww_domestic_urban_treatment_path_untreated_with_sewerage")
mean(rowSums(DataIn[,urbanvars]))

industrialvars<-c("frac_wali_ww_industrial_treatment_path_advanced_aerobic",
                  "frac_wali_ww_industrial_treatment_path_advanced_anaerobic",
                  "frac_wali_ww_industrial_treatment_path_latrine_improved",
                  "frac_wali_ww_industrial_treatment_path_latrine_unimproved",
                  "frac_wali_ww_industrial_treatment_path_primary",
                  "frac_wali_ww_industrial_treatment_path_secondary_aerobic",
                  "frac_wali_ww_industrial_treatment_path_secondary_anaerobic",
                  "frac_wali_ww_industrial_treatment_path_septic",
                  "frac_wali_ww_industrial_treatment_path_untreated_no_sewerage",
                  "frac_wali_ww_industrial_treatment_path_untreated_with_sewerage")
mean(rowSums(DataIn[,industrialvars]))

#write these files and we are done ,

#
dir_out<- r"(C:\Users\L03054557\OneDrive\test_ce\)"

target_vars<-c(urbanvars,industrialvars,ruralvars)


for (i in 1:length(target_vars))
{
#i<-1
var_sisepuede <- target_vars[i]
dir.create(paste0(dir_out,var_sisepuede,"\\"))
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\"))
#historical file 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"))
pivot_h<-subset(DataIn,Year<=2020)
pivot_h<-pivot_h[,c("iso_code3","Year",var_sisepuede)]
pivot_h<-pivot_h[order(pivot_h$iso_code3,pivot_h$Year),]
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection file 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<-subset(DataIn,Year>2020)
pivot_p<-pivot_p[,c("iso_code3","Year",var_sisepuede)]
pivot_p<-pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)
}


