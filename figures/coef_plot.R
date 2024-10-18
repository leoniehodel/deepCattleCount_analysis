library(ggplot2)
library(ggpubr)
library(broom)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols <- c("AC" = cbPalette[2], "RO" = cbPalette[3],"AM" = cbPalette[4],"PA" = cbPalette[6] )

ds <- read_csv('data/regression_vars.csv') %>% as.data.frame() 

# read in the models
models<- readRDS("results/regression_models.RData")

# Function to extract coefficients, standard errors, and confidence intervals
# coefficient plots from the standardized model with confidence intervals as error bars
m1<-models[1][[1]] %>% tidy(conf.int = TRUE)
m2<-models[2][[1]] %>% tidy(conf.int = TRUE)


coeff_model1 <- data.frame(
  Variable = rep(c("defo", "defo_buffer",'deg','degsev','crop'),each=2),
  Model = rep(c("Model I ", 'Model II'),5),
  Value = c(models[1][[1]]$coefficients['defo_perc_wo_refo_13_17'],NA,
            NA,models[2][[1]]$coefficients['defo_buffer_perc_13_17'],
            models[1][[1]]$coefficients['mod_deg_sum_201819'],models[2][[1]]$coefficients['mod_deg_sum_201819'],
            models[1][[1]]$coefficients['sev_deg_sum_201819'],models[2][[1]]$coefficients['sev_deg_sum_201819'],
            models[1][[1]]$coefficients['crop_201819_tot'],models[2][[1]]$coefficients['crop_201819_tot']),
  
  conf_low = c(m1$conf.low[m1$term=='defo_perc_wo_refo_13_17'],NA,
               NA, m2$conf.low[m2$term=='defo_buffer_perc_13_17'],
               m1$conf.low[m1$term=='mod_deg_sum_201819'],m2$conf.low[m1$term=='mod_deg_sum_201819'],
               m1$conf.low[m1$term=='sev_deg_sum_201819'],m2$conf.low[m1$term=='sev_deg_sum_201819'],
               m1$conf.low[m1$term=='crop_201819_tot'],m2$conf.low[m1$term=='crop_201819_tot']
  ),
  conf_high = c(m1$conf.high[m1$term=='defo_perc_wo_refo_13_17'],NA,
                NA, m2$conf.high[m2$term=='defo_buffer_perc_13_17'],
                m1$conf.high[m1$term=='mod_deg_sum_201819'],m2$conf.high[m1$term=='mod_deg_sum_201819'],
                m1$conf.high[m1$term=='sev_deg_sum_201819'],m2$conf.high[m1$term=='sev_deg_sum_201819'],
                m1$conf.high[m1$term=='crop_201819_tot'],m2$conf.high[m1$term=='crop_201819_tot'])
)

# Create dot-and-whisker plot using ggplot2

# levels
coeff_model1$Variable <- factor(coeff_model1$Variable, levels = c('crop','degsev','deg', "defo_buffer","defo"))

a<-ggplot(coeff_model1, aes(x = Value, y = Variable, color = Model)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(position = position_dodge(width = 0.3),aes(xmin = conf_low, xmax = conf_high,width=0.5)) +
  #xlim(c(-0.0001,0.001))+
  theme_minimal()+
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  labs(x = '', y = "", title= '') +
  theme(axis.text.y = element_text(hjust = 1,size = 6),
        axis.text.x = element_text(size = 6),
        legend.position="bottom",legend.text=element_text(size=6)) +
  scale_y_discrete(labels = c("defo" = expression(bold("Deforestation"["[t-2,t-6]"])),
                              "defo_buffer"=expression(bold("Deforestation(buffer) "["[t-2,t-6]"])),
                              "deg"=expression("Degradation(moderate) "["[t0]"]), 
                              "degsev"=expression("Degradation(severe) "["[t0]"]),
                              "crop"=expression("Crop area "["[t0]"])))+
  labs(color = "")+
  scale_color_manual(labels = c(expression("Model I"[delta]), 
                                expression("Model II"[delta])), 
                     values = c(cbbPalette[2],cbbPalette[3])) 
a

m3<-models[3][[1]] %>% tidy(conf.int = TRUE)
m4<-models[4][[1]] %>% tidy(conf.int = TRUE)
m5<-models[5][[1]] %>% tidy(conf.int = TRUE)
m5 
coeff_model2 <- data.frame(
  Variable = rep(c("abc", "zdc",'abczdc'),3),
  Model = rep(c("Model III", 'Model IV','Model V'),each=3),
  Value = c(models[3][[1]]$coefficients['agricultural_credits'],NA,NA,
            NA,models[4][[1]]$coefficients['mean_mun_msg4_1317'],NA,
            models[5][[1]]$coefficients['agricultural_credits'],models[5][[1]]$coefficients['zdc_bin'],
            models[5][[1]]$coefficients['agricultural_credits:zdc_bin']
  ),
  sd =c(m3$std.error[m3$term=='agricultural_credits'],NA,NA,
        NA, m4$std.error[m4$term=='mean_mun_msg4_1317'],NA,
        m5$std.error[m5$term=='agricultural_credits'],m5$std.error[m5$term=='zdc_bin'],
        m5$std.error[m5$term=='agricultural_credits:zdc_bin']),
  conf_low = c(m3$conf.low[m3$term=='agricultural_credits'],NA,NA,
               NA, m4$conf.low[m4$term=='mean_mun_msg4_1317'],NA,
               m5$conf.low[m5$term=='agricultural_credits'],m5$conf.low[m5$term=='zdc_bin'],
               m5$conf.low[m5$term=='agricultural_credits:zdc_bin']),
  conf_high = c(m3$conf.high[m3$term=='agricultural_credits'],NA,NA,
                NA, m4$conf.high[m4$term=='mean_mun_msg4_1317'],NA,
                #NA,NA,
                m5$conf.high[m5$term=='agricultural_credits'],m5$conf.high[m5$term=='zdc_bin'],
                m5$conf.high[m5$term=='agricultural_credits:zdc_bin']
  )
)

coeff_model2
coeff_model2$Variable <- factor(coeff_model2$Variable, levels = c('abczdc', "zdc","abc"))

b<-ggplot(coeff_model2, aes(x = Value, y = Variable, color = Model)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(position = position_dodge(width = 0.3),aes(xmin = conf_low, xmax = conf_high,width=0.5)) +
  #xlim(c(-0.3,0.35))+
  theme_minimal()+
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  labs(x = '', y = "", title= '') +
  theme(axis.text.y = element_text(hjust = 1,size = 6),legend.position="bottom",
        axis.text.x = element_text(size = 6),
        legend.text=element_text(size=6)) +
  scale_y_discrete(labels = c("abc" = expression("ABC credits"["[t-2,t-6]"]),
                              "zdc"=expression("ZDC "["[t-2,t-6]"]),
                              "abczdc"=expression(bold("ABC*ZDC"["binary"]))
  ))+
  labs(color = "")+
  scale_color_manual(labels = c(expression("Model III"[delta]), 
                                expression("Model IV"[delta]),
                                expression("Model V"[delta])) ,
                     values = c(cbbPalette[4],cbbPalette[5],cbbPalette[6])) 
b
ggarrange(a,b)
ggsave('results/Figure2_coefficients.png',units ="cm",width = 16,height=5)


# the calculation like in Figueras, 1999 results in the same CI
mod_summ <- summary(models[5][[1]])
coefs <- mod_summ$coefficients[,1]; coefs
X <- model.matrix(models[5][[1]])
dof <- nrow(X) - ncol(X)
coefs_var <- vcov(models[5][[1]])
halfCI <- qt(0.975, dof) * sqrt(diag(coefs_var))
matrix(c(coefs - halfCI, coefs + halfCI), nrow=4)
confint(models[5][[1]])
