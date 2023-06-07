
#lets process all circular economy parameters  

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

###
# [1] frac_waso_initial_composition_mun_paper
###
var_sisepuede <- "frac_waso_initial_composition_mun_paper"
var_wb_data <- subset(colnames(wb_data),grepl("paper",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
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
summary(pivot_h)
write.csv(pivot_h,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\historical\\"),var_sisepuede,".csv"),row.names=FALSE)
#projection 
dir.create(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"))
pivot_p<- merge(pivot, data.frame(Year=c(2021:2050)))
pivot_p<- pivot_p[order(pivot_p$iso_code3,pivot_p$Year),]
summary(pivot_p)
write.csv(pivot_p,paste0(paste0(dir_out,var_sisepuede,"\\","input_to_sisepuede\\projected\\"),var_sisepuede,".csv"),row.names=FALSE)

###
# [2] frac_waso_initial_composition_mun_food
###
var_sisepuede <- "frac_waso_initial_composition_mun_food"
var_wb_data <- subset(colnames(wb_data),grepl("food",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
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

###
# [3] frac_waso_initial_composition_mun_glass
###
var_sisepuede <- "frac_waso_initial_composition_mun_glass"
var_wb_data <- subset(colnames(wb_data),grepl("glass",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
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

##########################
# [4] frac_waso_initial_composition_mun_plastic
##########################
var_sisepuede <- "frac_waso_initial_composition_mun_plastic"
var_wb_data <- subset(colnames(wb_data),grepl("plastic",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
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

###################################
# [5] frac_waso_initial_composition_mun_metal
###################################
var_sisepuede <- "frac_waso_initial_composition_mun_metal"
var_wb_data <- subset(colnames(wb_data),grepl("metal",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now fill data gaps by imputation 
formula_imputation <- as.formula(paste(var_sisepuede,"~",paste(socio_economic,collapse="+"),sep=""))
model_imputation <- lm(formula_imputation,pivot)
pivot[,paste0(var_sisepuede,"_imputation")] <- predict(model_imputation, pivot[,socio_economic] )
pivot[,paste0(var_sisepuede,"_imputation")] <- ifelse(pivot[,paste0(var_sisepuede,"_imputation")] <0,0,pivot[,paste0(var_sisepuede,"_imputation")] )
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

###################################
# [6] frac_waso_initial_composition_mun_wood
###################################
var_sisepuede <- "frac_waso_initial_composition_mun_wood"
var_wb_data <- subset(colnames(wb_data),grepl("wood",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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

##########################################
# [7] frac_waso_initial_composition_mun_rubber_leather
#########################################
var_sisepuede <- "frac_waso_initial_composition_mun_rubber_leather"
var_wb_data <- subset(colnames(wb_data),grepl("rubber",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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


##########################################
# [8] frac_waso_initial_composition_mun_yard
#########################################
var_sisepuede <- "frac_waso_initial_composition_mun_yard"
var_wb_data <- subset(colnames(wb_data),grepl("yard",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- pivot[,var_wb_data]/100
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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


##########################################
# [9] frac_waso_initial_composition_mun_textiles
#########################################
other_group<- c("frac_waso_initial_composition_mun_chemical_industrial",
                "frac_waso_initial_composition_mun_nappies",
                "frac_waso_initial_composition_mun_other",
                "frac_waso_initial_composition_mun_sludge",
                "frac_waso_initial_composition_mun_textiles"
                 )
# the remainder of categories are proportionally divided by the WB's other category 
var_sisepuede <- "frac_waso_initial_composition_mun_textiles"
var_wb_data <- subset(colnames(wb_data),grepl("composition_other",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- ( pivot[,var_wb_data]/100 ) * (1/length(other_group))
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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

##########################################
# [10] frac_waso_initial_composition_mun_sludge
#########################################
other_group<- c("frac_waso_initial_composition_mun_chemical_industrial",
                "frac_waso_initial_composition_mun_nappies",
                "frac_waso_initial_composition_mun_other",
                "frac_waso_initial_composition_mun_sludge",
                "frac_waso_initial_composition_mun_textiles"
                 )
# the remainder of categories are proportionally divided by the WB's other category 
var_sisepuede <- "frac_waso_initial_composition_mun_sludge"
var_wb_data <- subset(colnames(wb_data),grepl("composition_other",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- ( pivot[,var_wb_data]/100 ) * (1/length(other_group))
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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

##########################################
# [11] frac_waso_initial_composition_mun_other
#########################################
other_group<- c("frac_waso_initial_composition_mun_chemical_industrial",
                "frac_waso_initial_composition_mun_nappies",
                "frac_waso_initial_composition_mun_other",
                "frac_waso_initial_composition_mun_sludge",
                "frac_waso_initial_composition_mun_textiles"
                 )
# the remainder of categories are proportionally divided by the WB's other category 
var_sisepuede <- "frac_waso_initial_composition_mun_other"
var_wb_data <- subset(colnames(wb_data),grepl("composition_other",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- ( pivot[,var_wb_data]/100 ) * (1/length(other_group))
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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


##########################################
# [12] frac_waso_initial_composition_mun_nappies
#########################################
other_group<- c("frac_waso_initial_composition_mun_chemical_industrial",
                "frac_waso_initial_composition_mun_nappies",
                "frac_waso_initial_composition_mun_other",
                "frac_waso_initial_composition_mun_sludge",
                "frac_waso_initial_composition_mun_textiles"
                 )
# the remainder of categories are proportionally divided by the WB's other category 
var_sisepuede <- "frac_waso_initial_composition_mun_nappies"
var_wb_data <- subset(colnames(wb_data),grepl("composition_other",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- ( pivot[,var_wb_data]/100 ) * (1/length(other_group))
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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

##########################################
# [13] frac_waso_initial_composition_mun_chemical_industrial
#########################################
other_group<- c("frac_waso_initial_composition_mun_chemical_industrial",
                "frac_waso_initial_composition_mun_nappies",
                "frac_waso_initial_composition_mun_other",
                "frac_waso_initial_composition_mun_sludge",
                "frac_waso_initial_composition_mun_textiles"
                 )
# the remainder of categories are proportionally divided by the WB's other category 
var_sisepuede <- "frac_waso_initial_composition_mun_chemical_industrial"
var_wb_data <- subset(colnames(wb_data),grepl("composition_other",colnames(wb_data))==TRUE)
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- ( pivot[,var_wb_data]/100 ) * (1/length(other_group))
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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

####################
#Estimate industry variables 
###############################
ind_vars <- subset( colnames(fake_data), grepl("waso_initial_composition_ind",colnames(fake_data))==TRUE)
summary(ind_vars)

special_waste_vars <- subset(colnames(wb_data),grepl("special_waste",colnames(wb_data))==TRUE)
wb_data$special_waste_total <- rowSums(wb_data[,special_waste_vars],na.rm=TRUE)
wb_data[,paste0(special_waste_vars,"_new")] <- wb_data [,special_waste_vars]/wb_data$special_waste_total

#####
#"frac_waso_initial_composition_ind_chemical_industrial"
######
var_sisepuede <- "frac_waso_initial_composition_ind_chemical_industrial"
var_wb_data <- c("special_waste_construction_and_demolition_waste_tons_year_new",
                  "special_waste_e_waste_tons_year_new", 
                  "special_waste_hazardous_waste_tons_year_new", 
                  "special_waste_industrial_waste_tons_year_new", 
                  "special_waste_medical_waste_tons_year_new"
                )
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

##########
#"frac_waso_initial_composition_ind_food"
##############
var_sisepuede <- "frac_waso_initial_composition_ind_food"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

#############
#  [3] "frac_waso_initial_composition_ind_glass"
#################
var_sisepuede <- "frac_waso_initial_composition_ind_glass"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

##########################################
#  [4] "frac_waso_initial_composition_ind_metal"              
###########################################
var_sisepuede <- "frac_waso_initial_composition_ind_metal"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

#################
# [5] "frac_waso_initial_composition_ind_nappies"
#################
var_sisepuede <- "frac_waso_initial_composition_ind_nappies"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

#####################
# [6] "frac_waso_initial_composition_ind_other"
#####################
var_sisepuede <- "frac_waso_initial_composition_ind_other"
var_wb_data <- c("special_waste_construction_and_demolition_waste_tons_year_new",
                  "special_waste_e_waste_tons_year_new", 
                  "special_waste_hazardous_waste_tons_year_new", 
                  "special_waste_industrial_waste_tons_year_new", 
                  "special_waste_medical_waste_tons_year_new"
                )
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,"dummy"] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
pivot [, "dummy"] <- ifelse (pivot [, "dummy"]<=0.05,mean(pivot [, "dummy"]),pivot [, "dummy"])
pivot[,var_sisepuede] <- 1- pivot [, "dummy"]
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])

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

###################################
#  [7] "frac_waso_initial_composition_ind_paper"
##################################
var_sisepuede <- "frac_waso_initial_composition_ind_paper"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

##############################################
##  [8] "frac_waso_initial_composition_ind_plastic"
##############################################
var_sisepuede <- "frac_waso_initial_composition_ind_plastic"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

#########################
# [9] "frac_waso_initial_composition_ind_rubber_leather"     
############################
var_sisepuede <- "frac_waso_initial_composition_ind_rubber_leather"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

#################
#[10] "frac_waso_initial_composition_ind_sludge"
#################
var_sisepuede <- "frac_waso_initial_composition_ind_sludge"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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
#[11] "frac_waso_initial_composition_ind_textiles"
########################
var_sisepuede <- "frac_waso_initial_composition_ind_textiles"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

####################
#[12] "frac_waso_initial_composition_ind_wood"
####################
var_sisepuede <- "frac_waso_initial_composition_ind_wood"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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

####################
#[13] "frac_waso_initial_composition_ind_yard"
###################
var_sisepuede <- "frac_waso_initial_composition_ind_yard"
var_wb_data <- c()
ids<- c("iso3c","region_id","income_id")
socio_economic <- c("gdp","population_population_number_of_people")
pivot <- wb_data[,c(ids,socio_economic,var_wb_data)]
head(pivot)
pivot[,var_sisepuede] <- rowSums(wb_data[,var_wb_data],na.rm=TRUE)
#check dimmensionality makes sense
summary(pivot[,var_sisepuede]) 
summary(fake_data[,var_sisepuede])
#now fill data gaps by imputation, zero values are associated with mostly missing values 
pivot [, var_sisepuede] <- ifelse (pivot [, var_sisepuede]<=0.05,mean(pivot [, var_sisepuede]),pivot [, var_sisepuede])

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
## reciclying rates 
########################



#####
# treatment pathways  
#####

summary(rowSums( wb_data[,subset(colnames(wb_data),grepl("treatment",colnames(wb_data))==TRUE)], na.rm=TRUE ))


#which apply to solid waste 
 [1] "waste_treatment_anaerobic_digestion_percent"
 [2] "waste_treatment_compost_percent"
 [3] "waste_treatment_controlled_landfill_percent"
 [4] "waste_treatment_incineration_percent"
 [5] "waste_treatment_landfill_unspecified_percent"
 [6] "waste_treatment_open_dump_percent"
 [7] "waste_treatment_other_percent"
 [9] "waste_treatment_sanitary_landfill_landfill_gas_system_percent"
[10] "waste_treatment_unaccounted_for_percent"
[11] "waste_treatment_waterways_marine_percent"

#this applies to these factors
frac_waso_isw_incinerated_recovered_for_energy
frac_waso_landfill_gas_recovered
frac_waso_lgc_recovered_for_energy
frac_waso_msw_incinerated_recovered_for_energy
frac_waso_non_recycled_incinerated
frac_waso_non_recycled_landfilled
frac_waso_non_recycled_open_dump



# to determine recicyling rates, we can use:
 [8] "waste_treatment_recycling_percent"

frac_waso_recycled_glass
frac_waso_recycled_metal
frac_waso_recycled_paper
frac_waso_recycled_plastic
frac_waso_recycled_rubber_leather
frac_waso_recycled_textiles
frac_waso_recycled_wood



# 
subset(colnames(wb_data), grepl("treatment",colnames(wb_data)))

#to determine
qty_waso_initial_municipal_waste_tonne_per_capita

"total_msw_total_msw_generated_tons_year"    
divided by 
"population_population_number_of_people"


summary(wb_data[,paste0(special_waste_vars,"_new")])
summary(wb_data[,special_waste_vars])

wb_data[]

#so chemical industrial is 


#other is 
[21] "special_waste_agricultural_waste_tons_year"

all other are zero 

summary(fake_data[,ind_vars])

colnames(wb_data)
length(colnames(fake_data))


waso_initial_composition

# [1] frac_waso_initial_composition_mun_paper
 