#design transformations for waste 

dir_input_data <- r"(D:\1. Projects\42. LAC Decarbonization\lac_decarbonization-servidor-22092022\lac_decarbonization-main\ref\fake_data\)"
name_file <- "fake_data_complete.csv"
df_input_data <- read.csv(paste0(dir_input_data,name_file))

head(df_input_data)


sanitation_tr<-df_input_data[,c("time_period",subset(colnames(df_input_data),grepl("frac_wali_ww",colnames(df_input_data))==TRUE),
                                              subset(colnames(df_input_data),grepl("gasrf_trww",colnames(df_input_data))==TRUE) )]

dir_out<-r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Transformations\)"
write.csv(sanitation_tr,paste0(dir_out,"SanitationTransformation_2022_11_25.csv"))

#read fake data and calibration 
dir_input_data = r"(D:\1. Projects\42. LAC Decarbonization\lac_decarbonization-servidor-22092022\lac_decarbonization-main\ref\fake_data\)"
name_file = "fake_data_complete.csv"
df_input_data = read.csv(paste0(dir_input_data,name_file)) 


name_file = "data_complete_future_2022_06_13.csv"
dir_calib_data = r"(C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\CalibrationVectors\)"
df_data_calib = read.csv(paste0(dir_calib_data,name_file))


subset(colnames(df_data_calib),!(colnames(df_data_calib)%in%colnames(df_input_data)))





subset(colnames(df_input_data),grepl("frac_wali_ww",colnames(df_input_data))==TRUE)

subset(colnames(df_input_data),grepl("gasrf_trww",colnames(df_input_data))==TRUE)