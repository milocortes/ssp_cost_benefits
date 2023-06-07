#read fake data complete  
fake_data <- read.csv("https://raw.githubusercontent.com/egobiernoytp/lac_decarbonization/main/ref/fake_data/fake_data_complete.csv")
dim(fake_data)

#determine ouput directory 
#dir_out <- r'(C:\Users\Usuario\OneDrive\test_ce\)'
dir_out <- r'(C:\Users\L03054557\OneDrive\test_ce\)'

#
#dir_wb_data <- r'(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\World bank Data\)'
dir_wb_data <- r'(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\World bank Data\)'
wb_data <- read.csv(paste0(dir_wb_data,"country_level_data_0.csv"))
dim(wb_data)
colnames(wb_data)
head(wb_data)

########################
## frac_waso_recycled_glass
########################
#note the assumption is that general recycling rates apply to each product 
i <- 1
vars_sisepuede <- c("frac_waso_recycled_glass", 
                   "frac_waso_recycled_metal",
                   "frac_waso_recycled_paper",
                   "frac_waso_recycled_plastic",
                   "frac_waso_recycled_rubber_leather",
                   "frac_waso_recycled_textiles", 
                   "frac_waso_recycled_wood")
var_wb_data <- c("waste_treatment_recycling_percent")
var_sisepuede <- vars_sisepuede[i]
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,vars_sisepuede[i]] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now interpolate missing values  
#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(pivot[,paste0(var_sisepuede,"_imputation")] <0,0,pivot[,paste0(var_sisepuede,"_imputation")] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(is.na(pivot[,paste0(var_sisepuede,"_imputation")])==TRUE,mean(pivot [, var_sisepuede],na.rm=TRUE),pivot[,paste0(var_sisepuede,"_imputation")] )
pivot [, var_sisepuede] <- ifelse (is.na(pivot [, var_sisepuede])==TRUE,pivot[,paste0(var_sisepuede,"_imputation")],pivot [, var_sisepuede])

#edit and print 
colnames(pivot) <- gsub("iso3c","iso_code3",colnames(pivot))
pivot <- pivot[,c("iso_code3",var_sisepuede)]
head(pivot)
#create directory 
dir.create(paste0(dir_out,var_sisepuede,"\\"))
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede"))
#historical 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"))
pivot_h<- merge(pivot, data.frame(Year=c(2010:2020)))
pivot_h <- pivot_h[order(pivot_h$iso_code3,pivot_h$Year),]
summary(pivot_h[,var_sisepuede])
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<- merge(pivot, data.frame(Year=c(2021:2050)))
pivot_p<- pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
summary(pivot_p[,var_sisepuede])
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)


########################
## frac_waso_recycled_metal
########################
#note the assumption is that general recycling rates apply to each product 
i <- 2
vars_sisepuede <- c("frac_waso_recycled_glass", 
                   "frac_waso_recycled_metal",
                   "frac_waso_recycled_paper",
                   "frac_waso_recycled_plastic",
                   "frac_waso_recycled_rubber_leather",
                   "frac_waso_recycled_textiles", 
                   "frac_waso_recycled_wood")
var_wb_data <- c("waste_treatment_recycling_percent")
var_sisepuede <- vars_sisepuede[i]
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,vars_sisepuede[i]] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now interpolate missing values  
#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(pivot[,paste0(var_sisepuede,"_imputation")] <0,0,pivot[,paste0(var_sisepuede,"_imputation")] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(is.na(pivot[,paste0(var_sisepuede,"_imputation")])==TRUE,mean(pivot [, var_sisepuede],na.rm=TRUE),pivot[,paste0(var_sisepuede,"_imputation")] )
pivot [, var_sisepuede] <- ifelse (is.na(pivot [, var_sisepuede])==TRUE,pivot[,paste0(var_sisepuede,"_imputation")],pivot [, var_sisepuede])

#edit and print 
colnames(pivot) <- gsub("iso3c","iso_code3",colnames(pivot))
pivot <- pivot[,c("iso_code3",var_sisepuede)]
head(pivot)
#create directory 
dir.create(paste0(dir_out,var_sisepuede,"\\"))
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede"))
#historical 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"))
pivot_h<- merge(pivot, data.frame(Year=c(2010:2020)))
pivot_h <- pivot_h[order(pivot_h$iso_code3,pivot_h$Year),]
summary(pivot_h[,var_sisepuede])
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<- merge(pivot, data.frame(Year=c(2021:2050)))
pivot_p<- pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
summary(pivot_p[,var_sisepuede])
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)



########################
## frac_waso_recycled_paper
########################
#note the assumption is that general recycling rates apply to each product 
i <- 3
vars_sisepuede <- c("frac_waso_recycled_glass", 
                   "frac_waso_recycled_metal",
                   "frac_waso_recycled_paper",
                   "frac_waso_recycled_plastic",
                   "frac_waso_recycled_rubber_leather",
                   "frac_waso_recycled_textiles", 
                   "frac_waso_recycled_wood")
var_wb_data <- c("waste_treatment_recycling_percent")
var_sisepuede <- vars_sisepuede[i]
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,vars_sisepuede[i]] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now interpolate missing values  
#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(pivot[,paste0(var_sisepuede,"_imputation")] <0,0,pivot[,paste0(var_sisepuede,"_imputation")] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(is.na(pivot[,paste0(var_sisepuede,"_imputation")])==TRUE,mean(pivot [, var_sisepuede],na.rm=TRUE),pivot[,paste0(var_sisepuede,"_imputation")] )
pivot [, var_sisepuede] <- ifelse (is.na(pivot [, var_sisepuede])==TRUE,pivot[,paste0(var_sisepuede,"_imputation")],pivot [, var_sisepuede])

#edit and print 
colnames(pivot) <- gsub("iso3c","iso_code3",colnames(pivot))
pivot <- pivot[,c("iso_code3",var_sisepuede)]
head(pivot)
#create directory 
dir.create(paste0(dir_out,var_sisepuede,"\\"))
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede"))
#historical 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"))
pivot_h<- merge(pivot, data.frame(Year=c(2010:2020)))
pivot_h <- pivot_h[order(pivot_h$iso_code3,pivot_h$Year),]
summary(pivot_h[,var_sisepuede])
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<- merge(pivot, data.frame(Year=c(2021:2050)))
pivot_p<- pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
summary(pivot_p[,var_sisepuede])
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)


########################
## frac_waso_recycled_plastic
########################
#note the assumption is that general recycling rates apply to each product 
i <- 4
vars_sisepuede <- c("frac_waso_recycled_glass", 
                   "frac_waso_recycled_metal",
                   "frac_waso_recycled_paper",
                   "frac_waso_recycled_plastic",
                   "frac_waso_recycled_rubber_leather",
                   "frac_waso_recycled_textiles", 
                   "frac_waso_recycled_wood")
var_wb_data <- c("waste_treatment_recycling_percent")
var_sisepuede <- vars_sisepuede[i]
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,vars_sisepuede[i]] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now interpolate missing values  
#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(pivot[,paste0(var_sisepuede,"_imputation")] <0,0,pivot[,paste0(var_sisepuede,"_imputation")] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(is.na(pivot[,paste0(var_sisepuede,"_imputation")])==TRUE,mean(pivot [, var_sisepuede],na.rm=TRUE),pivot[,paste0(var_sisepuede,"_imputation")] )
pivot [, var_sisepuede] <- ifelse (is.na(pivot [, var_sisepuede])==TRUE,pivot[,paste0(var_sisepuede,"_imputation")],pivot [, var_sisepuede])

#edit and print 
colnames(pivot) <- gsub("iso3c","iso_code3",colnames(pivot))
pivot <- pivot[,c("iso_code3",var_sisepuede)]
head(pivot)
#create directory 
dir.create(paste0(dir_out,var_sisepuede,"\\"))
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede"))
#historical 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"))
pivot_h<- merge(pivot, data.frame(Year=c(2010:2020)))
pivot_h <- pivot_h[order(pivot_h$iso_code3,pivot_h$Year),]
summary(pivot_h[,var_sisepuede])
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<- merge(pivot, data.frame(Year=c(2021:2050)))
pivot_p<- pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
summary(pivot_p[,var_sisepuede])
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)


########################
## frac_waso_recycled_rubber_leather
########################
#note the assumption is that general recycling rates apply to each product 
i <- 5
vars_sisepuede <- c("frac_waso_recycled_glass", 
                   "frac_waso_recycled_metal",
                   "frac_waso_recycled_paper",
                   "frac_waso_recycled_plastic",
                   "frac_waso_recycled_rubber_leather",
                   "frac_waso_recycled_textiles", 
                   "frac_waso_recycled_wood")
var_wb_data <- c("waste_treatment_recycling_percent")
var_sisepuede <- vars_sisepuede[i]
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,vars_sisepuede[i]] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now interpolate missing values  
#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(pivot[,paste0(var_sisepuede,"_imputation")] <0,0,pivot[,paste0(var_sisepuede,"_imputation")] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(is.na(pivot[,paste0(var_sisepuede,"_imputation")])==TRUE,mean(pivot [, var_sisepuede],na.rm=TRUE),pivot[,paste0(var_sisepuede,"_imputation")] )
pivot [, var_sisepuede] <- ifelse (is.na(pivot [, var_sisepuede])==TRUE,pivot[,paste0(var_sisepuede,"_imputation")],pivot [, var_sisepuede])

#edit and print 
colnames(pivot) <- gsub("iso3c","iso_code3",colnames(pivot))
pivot <- pivot[,c("iso_code3",var_sisepuede)]
head(pivot)
#create directory 
dir.create(paste0(dir_out,var_sisepuede,"\\"))
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede"))
#historical 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"))
pivot_h<- merge(pivot, data.frame(Year=c(2010:2020)))
pivot_h <- pivot_h[order(pivot_h$iso_code3,pivot_h$Year),]
summary(pivot_h[,var_sisepuede])
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<- merge(pivot, data.frame(Year=c(2021:2050)))
pivot_p<- pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
summary(pivot_p[,var_sisepuede])
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)


########################
## frac_waso_recycled_textiles
########################
#note the assumption is that general recycling rates apply to each product 
i <- 6
vars_sisepuede <- c("frac_waso_recycled_glass", 
                   "frac_waso_recycled_metal",
                   "frac_waso_recycled_paper",
                   "frac_waso_recycled_plastic",
                   "frac_waso_recycled_rubber_leather",
                   "frac_waso_recycled_textiles", 
                   "frac_waso_recycled_wood")
var_wb_data <- c("waste_treatment_recycling_percent")
var_sisepuede <- vars_sisepuede[i]
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,vars_sisepuede[i]] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now interpolate missing values  
#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(pivot[,paste0(var_sisepuede,"_imputation")] <0,0,pivot[,paste0(var_sisepuede,"_imputation")] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(is.na(pivot[,paste0(var_sisepuede,"_imputation")])==TRUE,mean(pivot [, var_sisepuede],na.rm=TRUE),pivot[,paste0(var_sisepuede,"_imputation")] )
pivot [, var_sisepuede] <- ifelse (is.na(pivot [, var_sisepuede])==TRUE,pivot[,paste0(var_sisepuede,"_imputation")],pivot [, var_sisepuede])

#edit and print 
colnames(pivot) <- gsub("iso3c","iso_code3",colnames(pivot))
pivot <- pivot[,c("iso_code3",var_sisepuede)]
head(pivot)
#create directory 
dir.create(paste0(dir_out,var_sisepuede,"\\"))
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede"))
#historical 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"))
pivot_h<- merge(pivot, data.frame(Year=c(2010:2020)))
pivot_h <- pivot_h[order(pivot_h$iso_code3,pivot_h$Year),]
summary(pivot_h[,var_sisepuede])
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<- merge(pivot, data.frame(Year=c(2021:2050)))
pivot_p<- pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
summary(pivot_p[,var_sisepuede])
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)

########################
## frac_waso_recycled_wood
########################
#note the assumption is that general recycling rates apply to each product 
i <- 7
vars_sisepuede <- c("frac_waso_recycled_glass", 
                   "frac_waso_recycled_metal",
                   "frac_waso_recycled_paper",
                   "frac_waso_recycled_plastic",
                   "frac_waso_recycled_rubber_leather",
                   "frac_waso_recycled_textiles", 
                   "frac_waso_recycled_wood")
var_wb_data <- c("waste_treatment_recycling_percent")
var_sisepuede <- vars_sisepuede[i]
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,vars_sisepuede[i]] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now interpolate missing values  
#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(pivot[,paste0(var_sisepuede,"_imputation")] <0,0,pivot[,paste0(var_sisepuede,"_imputation")] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(is.na(pivot[,paste0(var_sisepuede,"_imputation")])==TRUE,mean(pivot [, var_sisepuede],na.rm=TRUE),pivot[,paste0(var_sisepuede,"_imputation")] )
pivot [, var_sisepuede] <- ifelse (is.na(pivot [, var_sisepuede])==TRUE,pivot[,paste0(var_sisepuede,"_imputation")],pivot [, var_sisepuede])

#edit and print 
colnames(pivot) <- gsub("iso3c","iso_code3",colnames(pivot))
pivot <- pivot[,c("iso_code3",var_sisepuede)]
head(pivot)
#create directory 
dir.create(paste0(dir_out,var_sisepuede,"\\"))
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede"))
#historical 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"))
pivot_h<- merge(pivot, data.frame(Year=c(2010:2020)))
pivot_h <- pivot_h[order(pivot_h$iso_code3,pivot_h$Year),]
summary(pivot_h[,var_sisepuede])
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<- merge(pivot, data.frame(Year=c(2021:2050)))
pivot_p<- pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
summary(pivot_p[,var_sisepuede])
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)









