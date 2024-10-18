library(sf)
library(tidyverse)
library(openxlsx)

################################
#'## Read in data
################################
#df <- read.xlsx('input/S2_image_outlines.xlsx')
#outlines <- st_as_sf(df, wkt = "geometry_wkt", crs = 4674)
#excel_base_date <- as.Date("1900-01-01")
outlines <- st_read('input/image_outlines/image_outlines24.geojson')
#dates <- excel_base_date + outlines$image_date
outlines$image_date<-outlines$dates

# in case cattle maps are newly generated
inf_dir <- 'input/raw_inference_files_clean/'

car_AC <- st_read('input/CAR/AC_CAR_2022.geojson')
car_PA <- st_read('input/CAR/PA_CAR_2022.geojson')
car_RO <- st_read('input/CAR/RO_CAR_2022.geojson')
car_AM <- st_read('input/CAR/AM_CAR_2022.geojson')

################################
#'## Combine all CAR properties contained in the image outlines
################################
binary_AC <-st_covered_by(car_AC, outlines)
a = as.vector.data.frame(binary_AC)
car_AC_select <- car_AC %>%  mutate(covered = ifelse(a>0,1,0)) %>% filter(covered!=0) %>% dplyr::select(seq,cod_imovel,IBGE_CODE) %>% mutate(SIGLA_UF = 'AC')

binary_PA <-st_covered_by(car_PA, outlines)
a = as.vector.data.frame(binary_PA)
car_PA_select <- car_PA %>%  mutate(covered = ifelse(a>0,1,0)) %>% filter(covered!=0) %>% dplyr::select(seq,'cod_imovel'=COD_IMOVEL,IBGE_CODE)%>% mutate(SIGLA_UF = 'PA')
#write_rds(car_PA_select,'data/08_CAR-outlinesmerged/car_PA_select.rds')

binary_RO <-st_covered_by(st_make_valid(car_RO), outlines)
a = as.vector.data.frame(binary_RO)
car_RO_select <- car_RO %>%  mutate(covered = ifelse(a>0,1,0)) %>% filter(covered!=0) %>% dplyr::select(seq,'cod_imovel'=COD_IMOVEL,IBGE_CODE)%>% mutate(SIGLA_UF = 'RO')
#write_rds(car_RO_select,'data/08_CAR-outlinesmerged/car_RO_select.rds')

binary_AM <-st_covered_by(st_make_valid(car_AM), outlines)
a = as.vector.data.frame(binary_AM)
car_AM_select <- car_AM %>%  mutate(covered = ifelse(a>0,1,0)) %>% filter(covered!=0) %>% dplyr::select(seq,'cod_imovel'=COD_IMOVEL,IBGE_CODE)%>% mutate(SIGLA_UF = 'AM')

# combine all car
all_selected_car <- rbind(car_AC_select,car_PA_select,car_RO_select,car_AM_select)
st_write(all_selected_car,'intermediate_files/all_car_contained.geojson', append=TRUE )

##################################
#  the same with intersection - 
#################################
binary_AC <- st_intersects(car_AC, outlines)
# Convert list of intersections to a vector indicating presence
has_intersection <- sapply(binary_AC, function(x) length(x) > 0)
# Filter polygons and select desired columns
car_AC_select <- car_AC %>%
  mutate(covered = ifelse(has_intersection, 1, 0)) %>%
  filter(covered != 0) %>%
  dplyr::select(seq, cod_imovel, IBGE_CODE) %>%
  mutate(SIGLA_UF = 'AC')

binary_PA <- st_intersects(car_PA, outlines)

# Convert list of intersections to a vector indicating presence
has_intersection <- sapply(binary_PA, function(x) length(x) > 0)
# Filter polygons and select desired columns
car_PA_select <- car_PA %>%
  mutate(covered = ifelse(has_intersection, 1, 0)) %>%
  filter(covered != 0) %>%
  dplyr::select(seq, 'cod_imovel'=COD_IMOVEL, IBGE_CODE) %>%
  mutate(SIGLA_UF = 'PA')

binary_AM <- st_intersects(st_make_valid(car_AM), outlines)

# Convert list of intersections to a vector indicating presence
has_intersection <- sapply(binary_AM, function(x) length(x) > 0)
# Filter polygons and select desired columns
car_AM_select <- car_AM %>%
  mutate(covered = ifelse(has_intersection, 1, 0)) %>%
  filter(covered != 0) %>%
  dplyr::select(seq, 'cod_imovel'=COD_IMOVEL, IBGE_CODE) %>%
  mutate(SIGLA_UF = 'AM')

binary_RO <- st_intersects(st_make_valid(car_RO), outlines)

# Convert list of intersections to a vector indicating presence
has_intersection <- sapply(binary_RO, function(x) length(x) > 0)
# Filter polygons and select desired columns
car_RO_select <- car_RO %>%
  mutate(covered = ifelse(has_intersection, 1, 0)) %>%
  filter(covered != 0) %>%
  dplyr::select(seq, 'cod_imovel'=COD_IMOVEL, IBGE_CODE) %>%
  mutate(SIGLA_UF = 'RO')

# combine all car
all_selected_car_contains <- rbind(car_AC_select,car_PA_select,car_RO_select,car_AM_select)
st_write(all_selected_car_contains,'intermediate_files/all_car_intersected.geojson', append=TRUE )

##################################
#  stats outlines and save S2
#################################

area <- st_area(outlines) 
outlines$area <- as.numeric(area) / 1e6
cat('Total area of image patches (km2):' , round(sum(outlines$area),1))
cat('Mean area of image patches (km2):' , round(mean(outlines$area),1))


################################
#'##  merge all outputs and filter cattle
#'  generate table S3 - this is an input 
################################



all_inf_cattle_19 <- data.frame()
inference_list <- list.files(inf_dir)

for(inf in inference_list){
  id = strsplit(inf, "[.]")[[1]][1]
  date <- outlines$image_date[outlines$id == id]

  # the geopoints are stored in EPGS:4674 already, just have to be assigned
  geopoints <- st_read(file.path(inf_dir,inf)) %>% st_set_crs(4674) %>% filter(round(n_cattle)>=1)%>%
    mutate(n_cattle = round(floor(n_cattle)))

  geopoints$date <- date

  all_inf_cattle_19 <- rbind(all_inf_cattle_19,geopoints)
}

sum(all_inf_cattle_19$n_cattle)
st_write(all_inf_cattle_19,'input/S3_cattle_maps.geojson', append=FALSE)


all_inf_cattle_19$geometry_wkt <- st_as_text(all_inf_cattle_19$geometry)
all_inf_cattle_19_excel<- all_inf_cattle_19 %>% as.data.frame() %>% dplyr::select(-geometry)

write.xlsx(all_inf_cattle_19_excel, 'input/S3_cattle_maps.xlsx')

