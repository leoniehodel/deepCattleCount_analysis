# run all scripts of this repository 

# Create a directory for intermediate files if it doesn't exist
dir.create("intermediate_files", showWarnings = FALSE)

cat("Step 1: clean the raw output from the deep learning cattle counter \n")
# raw data files can be requested from author
# the clean outputs are in S3_cattle_maps.geojson
# source("code/preprocessing/01_inference_clean.R")

cat("Step 2: process CAR files \n")

# CAR data files can be requested from author, or downloaded here:
#data for RO from https://www.car.gov.br/publico/municipios/downloads?sigla=RO
#data for AC from https://www.car.gov.br/publico/municipios/downloads?sigla=AC
#data for PA from https://www.car.gov.br/publico/municipios/downloads?sigla=PA
#data for AM from https://www.car.gov.br/publico/municipios/downloads?sigla=AM
# --> in input/CAR/

source("code/preprocessing/02_merge_car_outlines.R")

cat("Step 3: merge CAR and inference \n")
source("code/preprocessing/03_merge_car-inference.R")

# 
cat("Step 4: Land use data for CAR  \n")
# data from https://brasil.mapbiomas.org/en/
# and https://www.bcb.gov.br/estabilidadefinanceira/micrrural
source("code/preprocessing/04_landuse_2019_selected_car_biome.R")
source("code/preprocessing/05_degradation_selected_car_biome.R")
source("code/preprocessing/06_buffer_deforestation_2013_2017.R")
source("code/preprocessing/07_agriculturalCredits.R")


cat("Step 5: Generate independent variables  \n")
source("code/01_generate_ind_vars.R")

cat("Step 6: Add hyperparameters  \n")
source("code/02_add_hyperpars.R")

cat("Step 7:  Regression  \n")
source("code/03_reg_analysis.R")


cat("Step 8: Generate figures  \n")
source('figures/world_map.R')
source('figures/zoom_on_car.R')
source("figures/boxplots.R")
source("figures/coef_plot.R")
source("figures/ppm_comparison.R")


cat(" R scripts have been executed.\n")

