################################
#'##  interaction plot
################################

dsdrop2<- readRDS("results/dsdrop2_notstandardized.RData")

regression1 <- lm(stocking_rate ~ agricultural_credits, data = subset(dsdrop2, zdc_bin == 0))
regression2 <- lm(stocking_rate ~ agricultural_credits, data = subset(dsdrop2, zdc_bin == 1))
# Plot the data points and linear regressions
ggplot(dsdrop2, aes(x = agricultural_credits, y = stocking_rate, color = factor(zdc_bin))) +
  geom_point() +
  geom_abline(intercept = coef(regression1)[1], slope = coef(regression1)[2], color = "blue") +
  geom_abline(intercept = coef(regression2)[1], slope = coef(regression2)[2], color = "red") +
  labs(x = "Low carbon credits (M R$)", y = "Stocking rates") +
  scale_color_manual(values = c("blue", "red"), labels = c("<50%", ">50%"), name = "ZDC Market Share")+
  theme_classic()
ggsave('results/FigureS2_interaction.png', height = 800 ,width =1400 ,units = 'px')
