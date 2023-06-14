setwd("~/Desktop/LAC Energy Tableaus")
library(reshape)
library("readxl")
library(dplyr)

GJ_per_PJ <-10^6
MJ_per_l_oil <-30

#=============-----TRANSPORT------------------============

#------------Fuel Switch Maritime
global_cost<-10^12 #trillion $
global_mtkm<-500000*10^9 #500,000 billion
frac_non_h2<-0.55 #fraction that is for ammonia production
US_maritime_cost_per_mtkm<-global_cost*frac_non_h2/global_mtkm
lac_maritime_cost_per_mktm<-ssp_cost_converter(US_maritime_cost_per_mtkm, 'USA', 2020, 'LAC_AVERAGE', 2019)

#------------Electrify LDVs-------------
marginal_upfront_vehicle_cost<-12000 #USD in USA (Baik et al., 2019)
marginal_annual_maintenance<-330 #USD in USA (AAA, 2019)
marginal_upfront_charging_infrastructure<-1000 #USD in USA (Purnazeri, 2022) costs/charger and charging stations/car (evadoption, 2021)
vehicle_lifetime<-12 #years (BTS, undated)
annual_vmt<-15000 #km (Ecola et al., 2008)
lifetime_vkm<-vehicle_lifetime*annual_vmt
  
levelized_capital_cost_per_km <- (marginal_upfront_vehicle_cost + marginal_upfront_charging_infrastructure)/(vehicle_lifetime*annual_vmt) #USE THIS


kwh_per_km <- 0.3/1.6 # https://theicct.org/sites/default/files/publications/EV_cost_2020_2030_20190401.pdf
km_per_kwh <- 1/kwh_per_km

kwh_per_pj<-277777777.7777778
pj_per_kwh<-1/kwh_per_pj

km_per_pj<-km_per_kwh*kwh_per_pj

levelized_cost_per_pj <-levelized_capital_cost_per_km/kwh_per_km/pj_per_kwh


savings_per_km <- marginal_annual_maintenance/annual_vmt # USE THIS
savings_per_pj<-savings_per_km/kwh_per_km/pj_per_kwh

#levelized_capital_cost_per_km_in_LAC <-ssp_cost_converter(levelized_capital_cost_per_km, "USA", 2019, "LAC_AVERAGE", 2019)
#levelized_savings_per_km_in_lac<-ssp_cost_converter(savings_per_km, "USA", 2019, "LAC_AVERAGE", 2019)

#------------Electrify HDVs----------------
#Source: https://itspubs.ucdavis.edu/publication_detail.php?id=3768
hdv_costs <- read_excel("Transportation Calculations.xlsx", sheet = "Burke HDV Data")
hdv_costs$capital_cost_per_km<-hdv_costs$`Capital ($)`/(hdv_costs$`Mileage/ year`*hdv_costs$LifetimeYears)/1.6
hdv_costs$maintenance_per_km<-hdv_costs$`Maintenance ($/mile)`/1.6
levelized_capital_HDV_per_km<-mean(hdv_costs$capital_cost_per_km) #USE THIS
levelized_maintenance_HDV_per_km<-mean(hdv_costs$maintenance_per_km) #USE THIS

hdv_charger_cost_per_kwh<-.022 #$/kWh
hdv_charger_cost_per_pj<-hdv_charger_cost_per_kwh * kwh_per_pj

#------------Electrify Rail - Freight------------
#From: https://www.nature.com/articles/s41560-021-00915-5#MOESM11
#Up front capital cost over 20 years ($M)
rail_ev_cap_cost<-2.177631605+2.471772097 #battery charging marginal cost
rail_ev_maintenance_savings<-1.086055665-0.543027832 #marginal maintenance (EV-diesel)
rail_revenue_tons<-1090 #revenue_tons
rail_distance<-241 #km
rail_operation_days <- 350
rail_lifetime_years<-20
rail_lifetime_mt_km<-rail_revenue_tons*rail_distance*rail_operation_days*rail_lifetime_years

rail_ev_levelized_cap_cost_per_mt_km<-(rail_ev_cap_cost*10^6)/rail_lifetime_mt_km
rail_ev_levelized_maintenance_savings_per_mt_km<-(rail_ev_maintenance_savings*10^6)/rail_lifetime_mt_km

rail_kwh_per_mt_km<-0.0345 #kwh/mt-km
rail_ev_levelized_cap_cost_per_kwh<-rail_ev_levelized_cap_cost_per_mt_km/rail_kwh_per_mt_km
rail_ev_levelized_maintenance_savings_per_kwh<-rail_ev_levelized_maintenance_savings_per_mt_km/rail_kwh_per_mt_km

rail_ev_levelized_cap_cost_per_PJ<-rail_ev_levelized_cap_cost_per_kwh*kwh_per_pj #USE THIS
rail_ev_levelized_maintenance_savings_per_PJ<-rail_ev_levelized_maintenance_savings_per_kwh*kwh_per_pj #USE THIS

#------------Increase ICE Energy Efficiency for LDV #1 --- 
cost_per_percent_LDV<-43 #average cost is $43/percent fuel economy improvement
pct_improvement<-20	#percent improvement
cost_20_pct<-cost_per_percent_LDV*pct_improvement
cost_per_km<-cost_20_pct/lifetime_vkm
ldv_km_per_l<-12 #km per litre
MJ_per_l_gasoline<-34 #MJ/l
MJ_per_PJ<-10^9
cost_per_PJ<-cost_per_km*ldv_km_per_l/MJ_per_l_gasoline*MJ_per_PJ

#-----------------Fuel Switch Ships
#calculate demand from "Aggregate investment for the decarbonisation of the shipping industry", slide 12
#Bulk carrier, oil, and container billion of tonne-nautical-miles
x1<-2015
x2<-2020
y1_bulk<-25000
y2_bulk<-100000
total_tnm_bulk_billions<-(x2-x1)*(y2_bulk - y1_bulk)*0.5 + (x2-x1)*y1_bulk

y_oil = 13000
total_tnm_oil_billions<-(x2-x1)*y_oil

y1_container<-10000
y2_container<-40000
total_tnm_container_billions<-(x2-x1)*(y2_container - y1_container)*0.5 + (x2-x1)*y1_container

km_per_nm<-1.852
total_tnm_billions<-total_tnm_bulk_billions+total_tnm_oil_billions+total_tnm_container_billions
total_nautical_mtkm <- total_tnm_billions * km_per_nm * 10^9

total_cost<-10^12 #trillion dolalrs of investments
frac_cost_ammonia<-0.43
frac_cost_engines_storage<-0.12

total_cost_non_hydrogen<-total_cost*(frac_cost_ammonia+frac_cost_engines_storage)

cost_per_mtkm <- total_cost_non_hydrogen/total_nautical_mtkm

#------------Cost of Crashes and Congestion ----------
imf_data<-read_xlsx('IMF fuel-subsidies-template-2022.xlsx', sheet='data')
imf_data_lac<-imf_data[imf_data$regionname=='Latin America & Caribbean',]
transport_externality_colnames<-c('countryname', 'cor_con_die_all', 'cor_con_gso_all', 'cor_acc_die_all', 'cor_acc_gso_all')
external_fraction_total<-c(0.75, 1, 0.75, 1) # assume the external costs for accidents are 75% of total.
lac_transport_externalities_2019<-imf_data_lac[imf_data_lac$year==2019, colnames(imf_data_lac) %in% transport_externality_colnames]
avg_lac_transport_costs_2019<-colMeans(lac_transport_externalities_2019[sapply(lac_transport_externalities_2019, is.numeric)])/external_fraction_total

#convert to cost/PJ
MJ_per_l_diesel<-36.9 #MJ per litre diesel
multipliers<-c(MJ_per_l_gasoline, MJ_per_l_gasoline, MJ_per_l_diesel, MJ_per_l_diesel)
efficiency_factor_electric_vs_fossiel_fuel<-0.25 #electric vehicles are 4 times as efficient
MJ_per_kwh<-3.6

#EV costs per MWH and PJ
#(25 cents / litre gas) * (litre gas / MJ) * (MJ / kWH) * 1/2 = $/kwh electirc vehicles
# Use only gas
avg_lac_transport_costs_2019_EV_per_kwh <- avg_lac_transport_costs_2019 / multipliers * MJ_per_kwh * efficiency_factor_electric_vs_fossiel_fuel

#turn these costs to PJ and use in our table
avg_lac_transport_costs_2019_per_PJ <- avg_lac_transport_costs_2019 / multipliers * MJ_per_PJ
avg_lac_transport_costs_2019_EV_per_PJ <- avg_lac_transport_costs_2019_EV_per_kwh / MJ_per_kwh * MJ_per_PJ


#------------Cost of Transport Air Pollution ----------------
imf_data<-read_xlsx('IMF fuel-subsidies-template-2022.xlsx', sheet='data')
imf_data_lac<-imf_data[imf_data$regionname=='Latin America & Caribbean',]
transport_air_quality_colnames<-c('countryname', 
                                  'cor_lap_gso_all',
                                  'cor_lap_die_all',
                                  'cor_lap_lpg_all',
                                  'cor_lap_ker_all')
lac_transport_air_quality_2019<-imf_data_lac[imf_data_lac$year==2019, colnames(imf_data_lac) %in% transport_air_quality_colnames]
avg_lac_air_quality_costs_2019<-colMeans(lac_transport_air_quality_2019[sapply(lac_transport_air_quality_2019, is.numeric)])
MJ_per_l_kerosene<-35
PJ_multipliers<-c(1/MJ_per_l_gasoline*MJ_per_PJ, 1/MJ_per_l_diesel*MJ_per_PJ, 0, 1/MJ_per_l_kerosene*MJ_per_PJ)
avg_lac_air_quality_costs_2019_PJ <- avg_lac_air_quality_costs_2019 * PJ_multipliers

#------------Cost of Other (INEN and ENTC) Air Pollution ----------------
imf_data<-read_xlsx('IMF fuel-subsidies-template-2022.xlsx', sheet='data')
imf_data_lac<-imf_data[imf_data$regionname=='Latin America & Caribbean',]
other_air_quality_colnames<-c('countryname', 
                             'cor_lap_coa_ind',
                             'cor_lap_nga_ind',
                             'cor_lap_coa_pow',
                             'cor_lap_nga_pow')

lac_other_air_quality_2019<-imf_data_lac[imf_data_lac$year==2019, colnames(imf_data_lac) %in% other_air_quality_colnames]
avg_lac_other_air_quality_costs_2019<-colMeans(lac_other_air_quality_2019[sapply(lac_other_air_quality_2019, is.numeric)])


conversion_factors_to_PJ<-c(GJ_per_PJ, GJ_per_PJ, GJ_per_PJ, GJ_per_PJ)
other_air_quality_cost_per_PJ<-avg_lac_other_air_quality_costs_2019* conversion_factors_to_PJ
