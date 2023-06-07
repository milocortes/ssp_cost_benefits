#Cost Benefit Utility Functions

library(reshape)

#-------create a transformation-------
#start_val is the initial value for this transformatin (t=0)
#end_val is the final value for this transformation (t=len)
#len is the length of the series from 0 to len inclusive
#var_name is the nmae of the variable being created (i.e., the input scalar)
#policy_name is the name of hte policy to assign to this
create_tx<-function(start_val, end_val, final_yr, var_name, policy_name){
  output<-data.frame(matrix(ncol=3, nrow=final_yr+1))
  colnames(output)<-c("time_period", var_name, "TransformationName")
  output[,"time_period"]<-seq(0,final_yr)
  output[,var_name]<-seq(start_val, end_val, length.out=(final_yr+1))
  output[,"TransformationName"]<-policy_name
  return(output)
}
  
#-------create a per-nation transformation-----------
#data is a data frame with 3 columns: Nation, start_val, end_val
#
create_tx_by_nation<-function(data, len, var_name, policy_name){

  nations <- unique(data$Nation)
  numnations <-length(nations)
  output<-data.frame(matrix(ncol=4, nrow=(len+1)*numnations))
  colnames(output)<-c("time_period", var_name, "TransformationName", "Nation")
  time_period_seq<-seq(0,len)
  output[,"TransformationName"]<-policy_name
  
  rowstart<-1
  rowend<-numtimesteps
  for (i in nations){
    nation_data<-data[data$Nation==i,]
    
    output[rowstart:rowend,c("time_period")]<-time_period_seq
    output[rowstart:rowend,c("Nation")]<-i
    output[rowstart:rowend,c(var_name)]<-seq(as.numeric(nation_data[1,2]), as.numeric(nation_data[1,3]), length.out=(len+1))
    rowstart<-rowend+1
    rowend<-rowend+numtimesteps
  }
  return(output)
}


#-------calculate cost or benefit from difference between 2 policies-----------

#Data is a dataframe with id_vars nation, time_period, and policy
#policy_tx: string identifier of transformation we are calculating CB for
#policy_base: string identifier of transformation we are comparing to
#the difference is base-tx
#diff_var: string name of variable in data to difference
#mults: mutpliers to apply to the difference
#output_var: vector of names of the output variables
#subtractbase: flag that, if set, subtracts the baseline run from the transformed run

cb<-function(data, policy_tx, policy_base, diff_var, output_vars, output_mults){

  #basic setup info  
  id_vars <-c('Nation','time_period','policy', 'policy_name')
  nations <- unique(data$Nation)
  numnations <-length(nations)
  numtimesteps <-length(unique(data$time_period))
  
  #create output table
  diff_var_name<-paste0("diff_", diff_var)
  output_rownames<-c(id_vars, diff_var_name, output_vars)
  output<-data.frame(matrix(ncol=length(output_rownames), nrow=numtimesteps*numnations))
  colnames(output)<- output_rownames
  
  #loop through calculations
  rowstart<-1
  rowend<-numtimesteps
  for (i in nations){
    datap<-subset(data,Nation==i)

    datap_base<-subset(datap, policy==policy_base & variable==diff_var)
    datap_tx<-subset(datap,policy==policy_tx & variable==diff_var)
    difference<-datap_tx$value-datap_base$value
    values<-t(t(difference)*output_mults)
    output[rowstart:rowend,c(id_vars)]<-datap_tx[,c(id_vars)]
    output[rowstart:rowend,diff_var_name]<-difference
    output[rowstart:rowend,c(output_vars)]<-values
    rowstart<-rowend+1
    rowend<-rowend+numtimesteps
  }
  
  return(output)
}

#TEST function cb
#output.file<-read.csv("output_ippu_all_2022_06_24.csv")
#data<-output.file
#n2o_policy <-5
#n2o_base <-0
#n2o_diff_var <-c('emission_co2e_n2o_ippu_chemicals')
#n2o_output_var <-c('cost_USD_M_n2o_emission_co2e_n2o_ippu_chemicals')
#n2o_mult<-c(2)
#n2o_mult<-2
#n2o_out <- cb(data, n2o_policy, n2o_base, n2o_diff_var, n2o_output_var, n2o_mult)


#-------calculate difference in livestock or crop output and values -----

#livestock_crop_var_list is the list of variables that represent crop or livestock produced
#output_var_previx is the prefix of the variable to use
cb_crop_livestock<-function(livestock_crop_var_list, 
                            output_var_prefix, data, policy_tx, policy_base, cb_output,
                            value_multiplier){
  livestock_crop_unit_values<-read.csv('livestock_and_crop_values.csv')
  for (l in livestock_crop_var_list){
    diff_var <-l
    diff_var_name<-paste('diff_', diff_var, sep='')
    output_var_name<-paste(output_var_prefix, l)
    multiplier<-livestock_crop_unit_values[livestock_crop_unit_values$category==l, 2]*value_multiplier
    cb_tmp<-cb(data, policy_tx, policy_base, diff_var, output_var_name, multiplier)
    cb_output[diff_var_name]<-cb_tmp[,5]
    cb_output[output_var_name]<-cb_tmp[,6]
  }
  return(cb_output)
}
