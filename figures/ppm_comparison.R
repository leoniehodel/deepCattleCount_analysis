################################
#'## Code by Leonie Hodel
# ## Date:
################################
library(sf)
library(tidyverse)
library(vtable)
################################
#'## Read in  data
################################

# image outlines
image_outlines<- st_read('data/image_outlines/image_outlines24.geojson')
catte <- st_read('data/S3_cattle_maps.geojson')
source("code/src/rcode_EPL_clean_data.R")
##############################################################################
#  Supplementary plot 1: comparison with PPA
##############################################################################

# pasture area/img outline
lu_2019 <- terra::rast('data/Mapbiomas/mapbiomas-brazil-collection-70-amazonia-2019.tif')
MPB_meta <-read_csv('data/Mapbiomas/mapbiomas-brazil-collection-70-amazonia-area.csv')


## prep MPB meta
MPB_meta_clean <- MPB_meta %>% 
  dplyr::select(band, class , class_name) %>% 
  mutate(year = str_extract(band, "[0-9]+") %>% as.integer())

MPB_meta_clean<-MPB_meta_clean%>% dplyr::select(class, class_name)
# Check differences by year
MPB_meta_clean %>% 
  distinct(class, class_name) %>% 
  add_count(class, class_name) %>% 
  arrange(n)

epl_ras_extract_table(lu_2019,image_outlines[1:3,], cols_keep = c('id','SIGLA_UF','CD_MUN','image_date'))
img_outlines_2019 <- epl_ras_extract_table(lu_2019,image_outlines, cols_keep = c('id','SIGLA_UF','CD_MUN','image_date'))

out_LU <-  img_outlines_2019 %>% 
  dplyr::rename(class = value) %>% 
  left_join(MPB_meta_clean, by = "class") %>% 
  mutate(as.integer(class)) %>% 
  #group_by(id,class,CD_MUN,SIGLA_UF,class_name) %>% 
  #summarise(mean_area =mean(sum_area), )%>%
  ungroup() %>%
  group_by(id)%>%
  mutate(area_tot = sum(sum_area)) %>% 
  mutate(area_perc = round(100 * sum_area/area_tot,2)) %>% 
  ungroup() %>% 
  #rename(deg_class_id = value) %>% 
  mutate(year = '2018-2019') %>%
  relocate(year, .after = SIGLA_UF) 

out_LU

pasture <- out_LU %>% filter(class == 15)

cattle<- catte %>% group_by(id_bbox, ) %>% summarise(sum_n_cattle_img_outline = sum(n_cattle))

Pasture <- pasture %>%  inner_join(cattle, by = c('id'='id_bbox'))

Pasture$sr <- (Pasture$sum_n_cattle_img_outline/(Pasture$sum_area/10000))
Pasture$sr <- 1.3 * Pasture$sr
Pasture_mun <- Pasture %>% group_by(CD_MUN) %>% summarise( vhr_stocking_rate = round(mean(sr),3), vhr_pasture_area_km2 = round(sum(sum_area/1000000,3)),vhr_heads = sum(sum_n_cattle_img_outline))
sum(Pasture_mun$vhr_pasture_area_km2)

# compare with PPA
ppm_2019 <- read_csv('input/ppm/SR_mun_PPM_2019.csv') %>% select(heads, pasture_area,CD_MUN,state, stocking_rate) %>% 
  mutate(CD_MUN = as.character(CD_MUN), ppm_pasture_area_km2 = round(pasture_area *0.01), ppm_stocking_rate = round(stocking_rate,3), ppm_heads = heads) %>% 
  select(-stocking_rate, -pasture_area, -heads)

Pasture_mun<-Pasture_mun %>% inner_join(ppm_2019, by = 'CD_MUN')
Pasture_mun$vhr_perc_pasture <-round(Pasture_mun$vhr_pasture_area_km2 *100/ Pasture_mun$ppm_pasture_area_km2,3)
r <- cor(Pasture_mun$vhr_stocking_rate, Pasture_mun$ppm_stocking_rate)
ggplot(Pasture_mun, aes(x=vhr_stocking_rate, y=ppm_stocking_rate, size = vhr_perc_pasture, color=state))+
  geom_point()+
  scale_size_continuous(name = '% of pasture area \n with VHR img coverage') +
  scale_color_discrete(name = 'States') +
  theme_minimal()+
  ylab('Stocking rates from \n Agricultural Census 2019 (PPM)')+
  xlab('Stocking rates from cattle density maps')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation (r):", round(r, 2)), 
           hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

  
ggsave('figures/ppm_comparison.png')

Pasture_mun$difference_sr_ppm_vhr <- round(Pasture_mun$ppm_stocking_rate - Pasture_mun$vhr_stocking_rate,2)

Pasture_mun<-Pasture_mun %>% select(state,CD_MUN,ppm_pasture_area_km2,ppm_heads, ppm_stocking_rate,
                                    vhr_pasture_area_km2,vhr_heads, vhr_stocking_rate,vhr_perc_pasture,difference_sr_ppm_vhr )
st(Pasture_mun, file='results/ppm_comparison_municipalities.csv')






