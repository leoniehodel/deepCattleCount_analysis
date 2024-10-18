library(tidyverse)
library(sf)
library(terra)
library(patchwork)
library(ggmap)
library(raster)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
car2<-'AC-1200054-CC52E27E6FF646FD865EBF6D22B83A8A'
car_shape2 <- st_read('intermediate_files/all_car_contained_cattle.geojson') %>% filter(cod_imovel == car2)
bbox2 <-st_bbox(car_shape2)
ac <- terra::rast('data/Mapbiomas_v8_amazon/mapbiomas-brazil-collection-80-amaznia-2018_2019-0000031744-0000000000.tif')

# crop raster AC 
r2 <- crop(ac, extent(car_shape2))
r2 <- crop(ac, car_shape2)
# and plot it 
r3 <- mask(r2, car_shape2,touches=FALSE)
plot(r3)
r3_poly <-terra::as.polygons(r3)
# convert in sf object
r3_poly <-st_as_sf(r3_poly)


# change th r3-poly
r3_poly$transition_2018_2019 <- c('Forest','Forest to Pasture','Pasture')
plot(r3_poly)
c('Forest','Forest to Pasture','Pasture to Forest','Pasture','Pasture','Pasture')

# add the cattle numbers
outline_id <- car_shape2$outline_id
outline <- st_read('input/S3_cattle_maps.geojson') %>% filter(id_bbox==outline_id)
ncattle<-outline %>% st_set_crs(4674)  #%>% filter(n_cattle >0.1) #%>% select(n_cattle)
car_shape2<- car_shape2 %>% st_set_crs(4674)
r4<- st_intersection(outline, car_shape2)%>%filter(n_cattle>1)


# probably i should define two bounding boxes and 
# 
center <- st_centroid(car_shape2)
center$geometry[[1]][1]
apikey <-  readLines("../google_api_key.txt")
# Read the key into R
apikey_text <- readLines(key)
register_google(key=apikey_text)
g <- get_googlemap(center = c(lon =center$geometry[[1]][1] , lat=center$geometry[[1]][2] ), maptype = "satellite", zoom=15)

zoom_on_car<-ggmap(g) + 
  theme_light() +
  geom_sf(data=r3_poly, aes(fill=transition_2018_2019),alpha=0.40,inherit.aes = FALSE)+
  scale_fill_manual(name='Land use change',values=c(cbPalette[4], cbPalette[2],cbPalette[5],cbPalette[6]))+
  #inset_raster(as.raster(r3),-62.60494, -62.58931, -10.48011, -10.46501 )+
  geom_sf(data = car_shape2, fill = NA, inherit.aes = FALSE,color = cbPalette[4],lwd=1)+
  geom_point(data = r4, aes(x = Longitude, y = Latitude, color = n_cattle,lwd =4 ), alpha = .8,inherit.aes = FALSE)+
  #scale_size_manual(values=c())+
  geom_text(data = r4, aes(x = Longitude, y = Latitude, label = n_cattle), color = "black", size = 3, vjust = -0.5, inherit.aes = FALSE) +
  scale_colour_gradient(name='Cattle number',low = "white", high = "darkred")

#annotation_scale(location = "tl")

zoom_on_car
ggsave(filename = 'figures/figure_output/zoom_on_car.png', width = 6, height = 6)


