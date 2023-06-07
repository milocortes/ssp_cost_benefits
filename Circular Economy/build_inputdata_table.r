#so I need to put together the next table based on ngfs data
#va_commercial_mmm_usd
#va_industrial_mmm_usd
#va_manufacturing_mmm_usd
#va_mining_mmm_usd

#
#population_rural
#population_urban


#plus all of the observed data


#load country list names
data_nt<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\)"
nations_table<-read.csv(paste0(data_nt,"CountriesList.csv"))

#load population tables
data_dir<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Data\Population\)"
#data_dir<-r"(C:\Users\L03054557\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Data\Population\)"
#pop_all<-read.csv(paste0(data_dir,"pop_all_future.csv"))
pop_all<-read.csv(paste0(data_dir,"pop_all_future_with_urban_interpolation.csv"))
pop_all$nation<-tolower(pop_all$nation)
pop_all$nation<-gsub(" ","_",pop_all$nation)
data_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\)"
write.csv(pop_all,paste0(data_out,'pop_all_future_with_urban_interpolation.csv'),row.names=FALSE)


#load gdp tables
data.dir<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Data\NGFS Data\)"
ngfs.data<-read.csv(paste0(data.dir,"LAC_test.csv"))
ngfs.data<-subset( ngfs.data,Variable%in%c("Population","GDP|PPP")  )
ngfs.data<-subset(ngfs.data,Scenario=="Nationally Determined Contributions (NDCs) " & Model=="MESSAGEix-GLOBIOM 1.1_downscaled")
ngfs.data<-subset(ngfs.data,Year>2010)
gdp<-ngfs.data[ngfs.data$Variable=="GDP|PPP",c("value","Year","Region")]
gdp$iso_code3<-gdp$Region
gdp$Region<-NULL

#add list of countries
dim(gdp)
gdp<-merge(gdp,nations_table[,c("iso_code3","nation")],by="iso_code3")
dim(gdp)

nations<-unique(gdp$nation)
gdp_future<-list()
for (i in nations)
{
#i<-'Argentina'
 pivot<-data.frame(Year=c(2015:2050),nation=i)
 pivot$gdp<-apply(pivot,1,function(x) { approx(subset(gdp,nation==i)$Year,subset(gdp,nation==i)$value,x['Year'])$y  } )
 gdp_future<-append(gdp_future,list(pivot))
}
gdp_future<-do.call('rbind',gdp_future)

gdp_proportions<-read.csv(paste0(data_out,'gdp_sector_shares.csv'))
gdp_proportions<-subset(gdp_proportions,Year==2020)
gdp_proportions$nation<-gdp_proportions$Nation
gdp_proportions$gdp_nominal_2020<-gdp_proportions$gdp

gdp_future<-merge(gdp_future,gdp_proportions[,c('nation','gdp_nominal_2020','manu_share','service_share','mineria_share','industria_share','comercio_share')],by='nation')

#now estimate value added proportions based on 2020 data

gdp_future$gdp_mmm_usd<-gdp_future$gdp
gdp_future$va_commercial_mmm_usd<-gdp_future$gdp*gdp_future[,'comercio_share']
gdp_future$va_industrial_mmm_usd<-gdp_future$gdp*gdp_future[,'industria_share']
gdp_future$va_manufacturing_mmm_usd<-gdp_future$gdp*gdp_future[,'manu_share']
gdp_future$va_mining_mmm_usd<-gdp_future$gdp*gdp_future[,'mineria_share']

gdp_future$time_period<-gdp_future$Year-2015

gdp_future<-gdp_future[order(gdp_future$nation,gdp_future$time_period),]

write.csv(gdp_future[,c('time_period','nation','gdp_mmm_usd','va_commercial_mmm_usd','va_industrial_mmm_usd','va_manufacturing_mmm_usd','va_mining_mmm_usd')],paste0(data_out,'gdp_all_future.csv'),row.names=FALSE)

#country areas
data_out<-r"(C:\Users\Usuario\OneDrive\Edmundo-ITESM\3.Proyectos\42. LAC Decarbonization\Git-LAC-Calib\lac_decarbonization\calibration\CircularEconomy\SupportData\)"
areas<-read.csv(paste0(data_dir,"area_country_ha.csv"))
areas$nation<-areas[,'ï..Nation']
areas$nation<-tolower(areas$nation)
areas$nation<-gsub(" ","_",areas$nation)

areas<-subset(areas,Year==2019)


write.csv(areas[,c('nation','area_country_ha')],paste0(data_out,'areas_future.csv'),row.names=FALSE)












subset(gdp_future,nation=='costa_rica')
subset(gdp,nation=='costa_rica')



gdp<-unlist(lapply(input_file$time_period+2015,function(x){approx(gdp$Year,gdp$value,xout=x)$y}))
