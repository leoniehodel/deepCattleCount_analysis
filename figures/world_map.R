library(tidyverse)
library(sf)
library(maps)
library(magrittr)
library(ggpubr)
library(ggspatial)
library(patchwork)
library(openxlsx)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "forestgreen", 'lightgoldenrod', "#0072B2", "#D55E00", "#CC79A7")


################################
#'##  Read in Data
################################

S1 <- read.xlsx('input/S1_groundtruthing.xlsx',startRow =2 ) 
ds <- read_csv('regression_vars.csv')
biome <- st_read('figures/figure_inputs/AtlasMar_BiomasBrasil.shp') 
biom<-biome[biome$objectid==1,]

states <- st_read('figures/figure_inputs/states/BR_UF_2021.shp') %>% filter(CD_UF %in% c('11','12','13','15') )
st_crs(states$geometry) <- 4674
muns <- st_read('figures/figure_inputs/municipalities/BR_Municipios_2020.shp')%>% filter (CD_MUN %in% unique(ds$IBGE_CODE))
outlines <- st_read('input/image_outlines/image_outlines24.geojson')
# how much total area?
dir.create("figures/figure_output", showWarnings = FALSE)

# figure 1a can be found in the jupyter notebook on github

################################
#'##  Plot 1c
################################

idaf <- na.omit(S1$groundTruth)
imgs<- na.omit(S1$cattleImgs)

got_imgs<- data.frame(idaf, imgs)
c<-ggplot(got_imgs, aes(y=idaf,x=imgs))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  #geom_abline(intercept = 0, slope = 1.3, color="blue")+
  geom_abline(intercept = 0.3*mean(idaf), slope = 1, color="red")+
  theme_light()+
  scale_linetype_manual(name = "Legend", values = c("Underestimation" = "solid", "Scale Factor" = "solid")) +
  ylab('True cattle stock/property')+
  xlab('Cattle stock \n on images')
c
ggsave('figures/figure_output/figure1c.png',width = 2.0, height = 3.9 )

################################
#'##  Plot 1d: Dataset, World map
################################

amazon_bb_big <- c( -74.2, -17, -42.0, 6.7)

world <- map_data("world")

littlemap<-ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), 
               fill = "grey", color = "black", size = 0.2) +
  geom_sf(data = biom, fill = cbPalette[4],  size = 1.5) +
  coord_sf(xlim = c(-90, -26), ylim = c(-40, 20), expand = FALSE) +
  theme_void()+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

littlemap

# land use map saved 
# am_biome_lu_2019 <- terra::rast('data/06_spatial/01_mapbiomas_coverage/Amazon/mapbiomas-brazil-collection-70-amazonia-2019.tif')
# am_biome_lu_2019_agg <- aggregate(am_biome_lu_2019, fact = 33, fun='modal')
# am_biome_lu_2019_agg_poly <-terra::as.polygons(am_biome_lu_2019_agg)
# # convert in sf object
# am_biome_lu_2019_agg_poly_sf <-st_as_sf(am_biome_lu_2019_agg_poly)
## read in the names of
# name_table <- read_csv('data/06_spatial/01_mapbiomas_coverage/Amazon/mapbiomas-brazil-collection-70-amazonia-area.csv')
# name_table<-name_table %>% select(class, class_name)
# am_biome_lu_2019_agg_poly_sf$land_use_2019 <- name_table$class_name[match(am_biome_lu_2019_agg_poly_sf$classification_2019, name_table$class)]
# st_write(am_biome_lu_2019_agg_poly_sf, 'analysis_2018-19/temp_figures/lu_2019_polygon.geojson',)
am_biome_lu_2019_agg_poly_sf<- st_read('figures/figure_inputs/lu_2019_polygon.geojson')


forest_2019 <- am_biome_lu_2019_agg_poly_sf%>% filter(land_use_2019=='Forest Formation')
pasture_2019 <- am_biome_lu_2019_agg_poly_sf%>% filter(land_use_2019=='Pasture')
water <- am_biome_lu_2019_agg_poly_sf%>% filter(land_use_2019=='River, Lake and Ocean')
crop_2019 <- am_biome_lu_2019_agg_poly_sf%>% filter(land_use_2019 %in% c('Mosaic of Crops',"Other Perennial Crops","Soy Beans"  ))

#ggplot()+
#  geom_sf(data=am_biome_lu_2019_agg_poly_sf, aes(fill=land_use_2019))

# Plot the map with unique fill color for biom
biomeplot <-ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black", size = 0.2) +
  theme_light() +
  #coord_sf(xlim = c(-75, -40), ylim = c(-15, 7), expand = FALSE) +
  geom_sf(data = biom, aes(fill = "Amazon Biome"), alpha=0.6,color = "#9FAE9E") +
  geom_sf(data=forest_2019, aes(fill="Forest"),color=NA)+
  #degradation
  #geom_sf(data=water, aes(fill="Water"),color=NA)+
  geom_sf(data=crop_2019, aes(fill="Crops"),color=NA)+
  geom_sf(data=pasture_2019, aes(fill="Pasture"),color=NA)+
  geom_sf(data = muns, aes(fill = 'Municipalities'),alpha =0.3,color = NA) +
  geom_sf(data = states, aes(fill = NA), alpha=0,color = "black") +
  geom_sf(data = outlines, aes(color = 'Image Outlines'), lwd = 1.2) +
  scale_fill_manual(name = ' ',values = c("Amazon Biome" = cbPalette[1],
                                          'States'= NA,
                                          'Municipalities'= 'grey', 
                                          "Forest"= cbPalette[4],
                                          "Pasture"= cbPalette[5],
                                          "Water"= cbPalette[3], 'Crops'=cbPalette[8]))+
  scale_color_manual(values = c("Image Outlines" =cbPalette[7] ), name = '')+
  annotate(geom = "text", x = -65, y = -4, label = "AM", 
           fontface = "italic", color = "grey22", size = 5) +
  annotate(geom = "text", x = -70, y = -10, label = "AC", 
           fontface = "italic", color = "grey22", size = 5) +
  annotate(geom = "text", x = -52, y = -7, label = "PA", 
           fontface = "italic", color = "grey22", size = 5) +
  annotate(geom = "text", x = -64.5, y = -11, label = "RO", 
           fontface = "italic", color = "grey22", size = 5) +
  annotation_scale()+
  coord_sf(xlim = c(amazon_bb_big[1], amazon_bb_big[3]), ylim = c(amazon_bb_big[2],amazon_bb_big[4]), expand = FALSE) 
#coord_fixed()


onea <-biomeplot + inset_element(littlemap, left = 0.75, bottom = 0.7, right = 1, top = 1)
onea
ggsave('figures/figure_output/map.png', width = 6, height = 5.5)











