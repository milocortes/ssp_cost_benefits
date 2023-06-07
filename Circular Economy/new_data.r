
#lets process all circular economy parameters  

#read fake data complete  
fake_data <- read.csv("https://raw.githubusercontent.com/egobiernoytp/lac_decarbonization/main/ref/fake_data/fake_data_complete.csv")
dim(fake_data)

#determine ouput directory 
dir_out <- r'(C:\Users\Usuario\OneDrive\test_ce\)'

#
dir_wb_data <- r'(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\World bank Data\)'
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


