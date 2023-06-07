
import os, os.path

#set up working directory
#os.chdir(r"D:\1. Projects\42. LAC Decarbonization\lac_decarbonization-servidor-22052022\lac_decarbonization\python")
os.chdir(r"D:\1. Projects\42. LAC Decarbonization\lac_decarbonization-servidor-24052022\lac_decarbonization\python")
#os.chdir(r"D:\1. Projects\42. LAC Decarbonization\lac_decarbonization-servidor-13062022\lac_decarbonization\python")

import numpy as np
import pandas as pd
import data_structures as ds
import setup_analysis as sa
import support_functions as sf
import argparse


from model_socioeconomic import Socioeconomic
from model_afolu import AFOLU
from model_circular_economy import CircularEconomy
from model_ippu import IPPU

import random



#load raw input file
#dir_input_data = r"D:\1. Projects\42. LAC Decarbonization\lac_decarbonization-main\lac_decarbonization-main\ref\fake_data"
dir_input_data = r"D:\1. Projects\42. LAC Decarbonization\lac_decarbonization-servidor-24052022\lac_decarbonization\ref\fake_data"
#dir_input_data = r"D:\1. Projects\42. LAC Decarbonization\lac_decarbonization-servidor-13062022\lac_decarbonization\ref\fake_data"
name_file = r"\fake_data_complete.csv"
df_input_data =  pd.read_csv(dir_input_data+name_file)


#load transformations files

#load combinations of transformations to be run
dir_input_transformations = r"C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Transformations"
exp_file =  r"\transformations_experiment_new.csv"
df_exp =  pd.read_csv(dir_input_transformations+exp_file)

#load transformation tables
#load waste reduction
dir_input_transformations = r"C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\Transformations"
#waste_reduction =  r"\WasteReductionTransformation.csv"
waste_reduction =  r"\WasteReductionTransformation_2022_06_13.csv"
df_wr =  pd.read_csv(dir_input_transformations+waste_reduction)

#load liquid waste-sanitation and treatment
sanitation =  r"\SanitationTransformation_test3.csv"
df_st =  pd.read_csv(dir_input_transformations+sanitation)

#load solid waste transformation
solid =  r"\SolidWasteTransformation.csv"
df_sot =  pd.read_csv(dir_input_transformations+solid)


#list all policies
policies = np.unique(df_exp['Policy_ID'])

#load calibration parameters
#name_file = r"\data_complete_future.csv"
name_file = r"\data_complete_future_2022_06_13.csv"
dir_calib_data = r"C:\Users\AP03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\CalibrationVectors"
df_data_calib = pd.read_csv(dir_calib_data+name_file)

#aggregate results to obtain mean values of calibration
#df_data_calib = df_data_calib.groupby(["nation"]).mean()
#df_data_calib['nation'] = df_data_calib.index

#check column names, always check if calib and policy parameters are interrelated
#df_sot.columns.isin(df_data_calib.columns)
#df_sot.columns[df_sot.columns.isin(df_data_calib.columns)==True]


#list all nations in  data calib
nations = np.unique(df_data_calib['nation'])

#Initialize loop
df_all = []
for policy in policies:
    #policy = policies[2]
    #load transformation tables
    tt = list(df_exp[df_exp['Policy_ID']==policy]['waste_reduction'])[0]
    st = list(df_exp[df_exp['Policy_ID']==policy]['sanitation'])[0]
    sot = list(df_exp[df_exp['Policy_ID']==policy]['solid'])[0]
    df_wrp = df_wr[df_wr['TransformationName']==tt]
    df_stp = df_st[df_st['TransformationName']==st]
    df_sotp = df_sot[df_sot['TransformationName']==sot]
    for i in nations:
        #i= 'argentina'
        df_input_new = df_input_data.copy(deep=True)
        df_dcp= df_data_calib[df_data_calib['nation']==i]
        #now substitue values in df_dcp en df_input_data
        #do all columns names are in in the input file
        #subset columns names that we need to replace
        target_vars = df_dcp.columns[df_dcp.columns.isin(df_input_new.columns)==True]
        #print(df_input_data[target_vars[0]])
        #print(df_input_new[target_vars[0]])
        for j in target_vars:
            df_input_new[j] = list(df_dcp[j].astype(float)) #I'm passing a whole vector to a column
        del(j)
        #print(df_input_data[target_vars[0]])
        #print(df_input_new[target_vars[0]])
        #now modify input table to reflect transformations impact

        #waste reduction
        target_Tvars = df_wrp.columns[df_wrp.columns.isin(df_input_new.columns)==True]
        for j in target_Tvars:
            #j= 'factor_waso_waste_per_capita_scalar_metal'
            df_input_new[j] = list(df_wrp[j].astype(float)) #I'm passing a whole vector to a column
        del(j)
        #sanitation
        target_Svars = df_stp.columns[df_stp.columns.isin(df_input_new.columns)==True]
        for j in target_Svars:
            #j= 'factor_waso_waste_per_capita_scalar_metal'
            df_input_new[j] = list(df_stp[j].astype(float)) #I'm passing a whole vector to a column
        del(j)

        #solid waste
        target_SOvars = df_sotp.columns[df_sotp.columns.isin(df_input_new.columns)==True]
        for j in target_SOvars:
            #j= 'factor_waso_waste_per_capita_scalar_metal'
            df_input_new[j] = list(df_sotp[j].astype(float)) #I'm passing a whole vector to a column
        del(j)

        #now we used df_input_new to run the model
        #set initial attributes of model circular economy
        model_circecon = CircularEconomy(sa.model_attributes)
        #run the model
        df_output = model_circecon.project(df_input_new)
        #add ids
        df_output['nation'] = i
        df_output['policy'] = policy
        df_all.append(df_output)
        del(df_output)

#save final table with results
df_all = pd.concat(df_all)

#save results of runs
#df_all.to_csv("D:\\1. Projects\\42. LAC Decarbonization\\outputs\\output_waste_all.csv")
#df_all.to_csv("C:\\Users\AP03054557\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\42. LAC Decarbonization\\Git-LAC-Calib\\lac_decarbonization\\outputs\\output_waste_all_2022_05_24.csv")
df_all.to_csv("C:\\Users\AP03054557\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\42. LAC Decarbonization\\Git-LAC-Calib\\lac_decarbonization\\outputs\\output_waste_all_2022_06_13.csv")

#df_output.to_csv("C:\\Users\AP03054557\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\42. LAC Decarbonization\\Git-LAC-Calib\\lac_decarbonization\\outputs\\output_waste_all_2022_06_13_test.csv")


#step 1. Load fake_data_complete
#step 2. Load calibrated parameters
#step 3. load policy tables
#step 4. simulate policy
