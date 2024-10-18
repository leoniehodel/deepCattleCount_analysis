library(tidyverse)
library(sf)
library(terra)
library(patchwork)
library(ggmap)

library(ggpubr)
library(rstatix)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols <- c("AC" = cbPalette[2], "RO" = cbPalette[3],"AM" = cbPalette[4],"PA" = cbPalette[6] )


ds <- read_csv('intermediate_files/regression_vars.csv') %>% as.data.frame() 
ds<- ds %>% filter(imagedate > '2018-01-01' )
# width and height for boxplots
w_b<-811*1.2
h_b<-  476*1.2

# mean stocking rates STATE (scale factor added)

sr_sum<-ds %>%                               
  group_by(SIGLA_UF) %>% 
  summarize(mean = mean(stocking_rate),median = median(stocking_rate),
            sd = sd(stocking_rate), n())

sr <- ggplot(ds, aes(x = stocking_rate, y = SIGLA_UF, color = SIGLA_UF)) +
  geom_boxplot(outlier.size = 0.7) +
  geom_point(data = sr_sum, aes(x = mean, y = SIGLA_UF), color = "black", size = 1) +  # Add points for means
  ylab('') +
  xlab('Stocking Rate') +
  theme_light() +
  scale_colour_manual(values = cols)

sr<-sr+scale_colour_manual(values=cols)
sr
ggsave('figures/box_sr.png', height = h_b, width = w_b, units = 'px')





##############################################################################
# Land use for Supplementary plot 1
##############################################################################

#
area<-ggplot(ds) + # Boxplot of total area
  geom_boxplot(aes(y = SIGLA_UF, x = pasture_201819_tot, color=SIGLA_UF),size=0.7,outlier.size=0.7) +
  xlab('Property pasture area (ha)')+
  ylab('')+
  theme_light()

area<-area+scale_color_manual(values=cols)
area<-area+theme(legend.position = "none")  
area
ggsave('figures/figure_output/Fig_S2/area.png', height = h_b, width = w_b, units = 'px')
n_cattle_car<-ggplot(ds) + # Boxplot of total area
  geom_boxplot(aes(y = SIGLA_UF, x = n_cattle, color=SIGLA_UF),size=0.7,outlier.size=0.7) +
  xlab('Number of cattle/CAR ')+
  ylab('')+
  theme_light()

n_cattle_car<-n_cattle_car+theme(legend.position = "none")  
n_cattle_car<-n_cattle_car+scale_color_manual(values=cols)
ggsave('figures/figure_output/Fig_S2/n_cattle.png', height = h_b, width = w_b, units = 'px')


ds$withDeforestation<-as.factor(ifelse(ds$defo_abs_13_17 >0.1,'with Deforestation','No Deforestation'))
ds$withDegradation <-as.factor(ifelse(ds$sev_deg_sum_201819 >0.05,'Severe Degradation','No severe Degradation'))
ds$withModDegradation <-as.factor(ifelse(ds$mod_deg_sum_201819 >0.05,'Mod Degradation','No mod Degradation'))
ds$withCrops <-as.factor(ifelse(ds$crop_201819_tot>0.05,'Crops','No crops'))

tot<- nrow(ds)

ds_defo<- ds[ds$withDeforestation=='with Deforestation',]
ds_moddeg<- ds[ds$withModDegradation=='Mod Degradation',]
ds_sevdeg<- ds[ds$withDegradation=='Severe Degradation',]
ds_crop<- ds[ds$withCrops=='Crops',]

# width and height for pie charts
w_p<-300
h_p <-200

# width and height for boxplots
w_b<-811*1.2
h_b<-  476*1.2
# PIE CHART 1
slices <- c(nrow(ds_defo)/tot*100,100-nrow(ds_defo)/tot*100)
lbls <- c(paste0(round(nrow(ds_defo)/tot*100,1),' %') ,' ')
dat<-as.data.frame(slices,lbls)
dat$lbls<-lbls
png("figures/figure_output/Fig_S2/pie1.png",         # File name
    width = w_p, height = h_p, units='px') # Width and height in inches
pie_def<-pie(slices,lbls, col=c(cbPalette[8],'white'))
pie_def
# Closing the graphical device
dev.off() 


# BOXPLOT 1
crop<-ggplot(ds_defo, aes(x=defo_abs_13_17, y=SIGLA_UF, color=SIGLA_UF,outlier.size=0.7)) + 
  geom_boxplot() +
  #xlim(c(0,60))+
  #ylim(c(0,40))+
  ylab('')+
  theme_light()+
  xlab('Deforested area 2013-2017 \n (%)')
#geom_text(x=-1, y=100, label= paste0('n = ',nrow(ds_crop$SIGLA_UF)))

crop<-crop+scale_colour_manual(values=cols)
crop<- crop+theme(legend.position = "none")
crop
ggsave('figures/figure_output/Fig_S2/box_1.png', height = h_b, width = w_b, units = 'px')

# PIE CHART 2
slices <- c(nrow(ds_moddeg)/tot*100,100-nrow(ds_moddeg)/tot*100)
lbls <- c(paste0(round(nrow(ds_moddeg)/tot*100,1),' %') ,' ')
dat<-as.data.frame(slices,lbls)
dat$lbls<-lbls
png("figures/figure_output/Fig_S2/pie2.png",         # File name
    width = w_p, height = h_p, units='px') # Width and height in inches
pie_deg<-pie(slices,lbls, col=c(cbPalette[1],'white'))
pie_deg
# Closing the graphical device
dev.off() 


# BOXPLOT 2
crop<-ggplot(ds_moddeg, aes(x=mod_deg_perc_201819, y=SIGLA_UF, color=SIGLA_UF,outlier.size=0.7)) + 
  geom_boxplot() +
  #xlim(c(0,60))+
  #ylim(c(0,40))+
  ylab('')+
  theme_light()+
  xlab('Moderatly degraded \n pasture area 2018-2019 (%)')
#geom_text(x=-1, y=100, label= paste0('n = ',nrow(ds_crop$SIGLA_UF)))

crop<-crop+scale_colour_manual(values=cols)
crop<- crop+theme(legend.position = "none")
crop
ggsave('figures/figure_output/Fig_S2/box_2.png', height = h_b, width = w_b, units = 'px')


# PIE CHART 3
slices <- c(nrow(ds_sevdeg)/tot*200,100-nrow(ds_sevdeg)/tot*100)
lbls <- c(paste0(round(nrow(ds_sevdeg)/tot*100,1),' %') ,' ')
dat<-as.data.frame(slices,lbls)
dat$lbls<-lbls
png("figures/figure_output/Fig_S2/pie3.png",         # File name
    width = w_p, height = h_p, units='px') # Width and height in inches
pie_deg<-pie(slices,lbls, col=c(cbPalette[7],'white'))
pie_deg
# Closing the graphical device
dev.off() 


# BOXPLOT 3
crop<-ggplot(ds_sevdeg, aes(x=sev_deg_perc_201819, y=SIGLA_UF, color=SIGLA_UF,outlier.size=0.7)) + 
  geom_boxplot() +
  #xlim(c(0,60))+
  #ylim(c(0,40))+
  ylab('')+
  theme_light()+
  xlab('Severely degraded \n pasture area 2018-2019 (%)')
#geom_text(x=-1, y=100, label= paste0('n = ',nrow(ds_crop$SIGLA_UF)))
crop
crop<-crop+scale_colour_manual(values=cols)
crop<- crop+theme(legend.position = "none")
crop
ggsave('figures/figure_output/Fig_S2/box_3.png', height = h_b, width = w_b, units = 'px')


# PIE CHART 4
slices <- c(nrow(ds_crop)/tot*200,100-nrow(ds_crop)/tot*100)
lbls <- c(paste0(round(nrow(ds_crop)/tot*100,1),' %') ,' ')
dat<-as.data.frame(slices,lbls)
dat$lbls<-lbls
png("figures/figure_output/Fig_S2/pie4.png",         # File name
    width = w_p, height = h_p, units='px') # Width and height in inches
pie_deg<-pie(slices,lbls, col=c(cbPalette[5],'white'))
pie_deg
# Closing the graphical device
dev.off() 


# BOXPLOT 4
crop<-ggplot(ds_crop, aes(x=crop_201819_perc, y=SIGLA_UF, color=SIGLA_UF,outlier.size=0.7)) + 
  geom_boxplot() +
  #xlim(c(0,40))+
  #ylim(c(0,40))+
  ylab('')+
  theme_light()+
  xlab('Crop area (%)')
#geom_text(x=-1, y=100, label= paste0('n = ',nrow(ds_crop$SIGLA_UF)))

crop<-crop+scale_colour_manual(values=cols)
crop<- crop+theme(legend.position = "none")
crop
ggsave('figures/figure_output/Fig_S2/box_4.png', height = h_b, width = w_b, units = 'px')



###############################################################################
## Summary stats
###############################################################################

mean(ds$area)
sd(ds$area)
ds %>%                               
  group_by(SIGLA_UF) %>% 
  summarize(mean = mean(area),
            sd = sd(area), n())

mean(ds$stocking_rate)
sd(ds$stocking_rate)
nrow(ds)
ds %>%                              
  group_by(SIGLA_UF) %>% 
  summarize(mean = mean(stocking_rate),
            sd = sd(stocking_rate), n())

mean(ds$pasture_201819_tot)
sd(ds$pasture_201819_tot)
nrow(ds)
ds %>%                               
  group_by(SIGLA_UF) %>% 
  summarize(mean = mean(pasture_201819_tot),
            sd = sd(pasture_201819_tot), n())

mean(ds_defo$defo_perc_wo_refo_13_17)
sd(ds_defo$defo_perc_wo_refo_13_17)

nrow(ds_defo)
ds_defo %>%   select(SIGLA_UF,defo_perc_wo_refo_13_17)%>%  
  group_by(SIGLA_UF) %>% 
  summarize(mean = mean(defo_perc_wo_refo_13_17),
            sd = sd(defo_perc_wo_refo_13_17), n())


mean(ds_sevdeg$sev_deg_perc_201819)
sd(ds_sevdeg$sev_deg_perc_201819)
nrow(ds_sevdeg)
ds_sevdeg %>%   dplyr::select(SIGLA_UF,sev_deg_perc_201819)%>%     
  group_by(SIGLA_UF) %>% 
  summarize(median = median(sev_deg_perc_201819),
            sd = sd(sev_deg_perc_201819), n())

mean(ds_moddeg$mod_deg_perc_201819)
sd(ds_moddeg$mod_deg_perc_201819)
nrow(ds_moddeg)
ds_moddeg %>%   dplyr::select(SIGLA_UF,mod_deg_perc_201819)%>%                          
  group_by(SIGLA_UF) %>% 
  summarize(median = median(mod_deg_perc_201819),
            sd = sd(mod_deg_perc_201819), n())

median(ds_crop$crop_201819_perc)
sd(ds_crop$crop_201819_perc)
nrow(ds_crop)
ds_crop %>%   dplyr::select(SIGLA_UF,crop_201819_perc)%>%                            
  group_by(SIGLA_UF) %>% 
  summarize(median = median(crop_201819_perc),
            sd = sd(crop_201819_perc), n())

