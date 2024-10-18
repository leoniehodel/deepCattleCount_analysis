library(sf)
library(tidyverse)
library(arm)
library(units)
library(texreg)
library(vtable)

################################
#'## HYPERPARAMETERS
################################
min_area_car = 5
min_pasture = 2
max_area_car  = 800
min_ncattle = 4 
sigma_cut = 0.90
#CV_cut = 0.8
min_prop_mun = 10
sr_max = 10
scale_factor = 1.3
################################
#'## Read in data
################################

output<- 'regression_vars.csv'

#new regession pars added like population number
ds_r<-st_read('intermediate_files/regression_pars_final_raw.geojson')
# filter out the newest images (the rest wil be filtered out in the regression analysis script)
ds_r_filtered <- ds_r %>% filter( imagedate < '2021-01-01')
agg_df <- aggregate(ds_r_filtered$COD_IMOVEL, by=list(ds_r_filtered$SIGLA_UF), FUN=length)

ds_r$agricultural_credits
ds_r_clean<- ds_r_filtered  %>% dplyr::select(SIGLA_UF,
                                     n_cattle,
                                     n_cattle_sd,
                                     pasture_201819_tot,
                                     pasture_201819_perc,
                                     defo_abs_13_17,
                                     defo_perc_wo_refo_13_17,
                                     defo_buffer_abs_13_17,
                                     defo_buffer_perc_13_17,
                                     mod_deg_sum_201819,
                                     mod_deg_perc_201819,
                                     sev_deg_sum_201819,
                                     sev_deg_perc_201819,
                                     crop_201819_tot,
                                     crop_201819_perc,
                                     agricultural_credits,
                                     mean_mun_msg4_1317,
                                     area,
                                     buffer_forest_tot_2013,
                                     precipitation_2018_19,
                                     mean_temp_18_19,
                                     population,
                                     nearest_fedSh,
                                     IBGE_CODE,
                                     outline_id, imagedate)

ds_r_clean$area<- st_area(ds_r_clean) %>% drop_units()
ds_r_clean<- ds_r_clean%>% as.data.frame()  %>% distinct()
ds_r_clean<- ds_r_clean[ds_r_clean$n_cattle_sd < quantile(ds_r_clean$n_cattle_sd, sigma_cut),]


ds<-ds_r_clean
ds$nearest_fedSh <- ds$nearest_fedSh/1000
ds$population <- ds$population/1000
ds$buffer_forest_tot_2013 <- ds$buffer_forest_tot_2013*0.01
ds$agricultural_credits <- ds$agricultural_credits*0.001
################################
#'## Preprocess
################################

dsfilter<-ds%>%
  mutate(area = round(area*0.0001,3), stocking_rate = (n_cattle*scale_factor)/pasture_201819_tot) %>%
  filter(area > min_area_car, area< max_area_car, n_cattle > min_ncattle, pasture_201819_tot>min_pasture) %>%
  group_by(IBGE_CODE) %>% filter(n() >min_prop_mun) %>% ungroup

# filter out sr higher than 10, these areas have been checked and are artefacts  
# confinement systems are therefore most likely not included

dsfilter<- dsfilter  %>% replace(is.na(.), 0) %>% filter(stocking_rate<sr_max)

################################
#'##  Drop variables not used in the regression 
###############################
drop <- c("COD_IMOVEL","date",
          'geometry','cv' )
dsdrop = dsfilter[,!(names(dsfilter) %in% drop)] 
dsdrop$stocking_rate
# save dsdrop
write.csv(dsdrop, output, row.names = FALSE)


