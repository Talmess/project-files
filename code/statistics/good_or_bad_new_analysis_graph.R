library(glmmTMB)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotrix)
library(gplots)
library(rasterVis)
library(maps)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(stats)
library(glmulti)
library(MASS)
library(lme4)
library(humidity)
library(visreg)
library(car)
library(tidyr)
library(MuMIn)
library(sjPlot)

org_df <- read.csv("good_or_bad_behaviors.csv")

df <- org_df %>%
  filter(dog != "amigo") %>%
  filter(behavior == "undesired") %>%
  filter(speed < 10)

rel_df <- df[,c(4,5,7,9,10,12:16)]

dir.create("plots", showWarnings = F)

# prop ~ climate plots

dir.create("plots/good_or_bad/prop_vs_climate", showWarnings = F)

ggplot(rel_df, aes(x = temperature, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/prop_vs_climate/temperature.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = humidity, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/prop_vs_climate/humidity.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = ground, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/prop_vs_climate/ground.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = radiation, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/prop_vs_climate/radiation.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = speed, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/prop_vs_climate/speed.jpg", width = 10, height = 5, limitsize = F)


# histograms
dir.create("plots/good_or_bad/histograms", showWarnings = F)

ggplot(rel_df, aes(x = prop)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/histograms/prop.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = temperature)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/histograms/temperature.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = speed)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/histograms/speed.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = humidity)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/histograms/humidity.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = ground)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/histograms/ground.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = radiation)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/good_or_bad/histograms/radiation.jpg", width = 10, height = 5, limitsize = F)


mean_temperature <- mean(rel_df$temperature)
sd_temperature <- sd(rel_df$temperature)

mean_humidity <- mean(rel_df$humidity)
sd_humidity <- sd(rel_df$humidity)

mean_speed <- mean(rel_df$speed)
sd_speed <- sd(rel_df$speed)

mean_radiation <- mean(rel_df$radiation)
sd_radiation <- sd(rel_df$radiation)

mean_ground <- mean(rel_df$ground)
sd_ground <- sd(rel_df$ground)



std_df <- rel_df %>% 
  mutate_at(c("temperature", "speed", "humidity", "radiation", "ground"), ~(scale(.) %>% as.vector)) %>%
  drop_na()

std_df$sess <- as.factor(std_df$sess)

pairs(std_df[,c(6:10)])

basic_model <- lm(prop ~ temperature + speed + humidity + radiation + ground, data = std_df)
vif(basic_model)

# temperature       speed    humidity   radiation      ground 
# 10.275107    1.743217    1.331082    4.030411   14.642182  

basic_model_2 <- lm(prop ~ temperature + speed + humidity + radiation, data = std_df)
vif(basic_model_2)

# temperature       speed    humidity   radiation 
# 2.605936    1.619016    1.229316    2.296124 


##### choose family #####

full_model_binomial <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                                 temperature:humidity + temperature:speed + temperature:radiation +
                                 humidity:speed + humidity:radiation + speed:radiation + 
                                 (1 | dog) + (1 | site) + (1 | sess), data = std_df, family = binomial(), weights = total, na.action = na.fail)

full_model_beta <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                             temperature:humidity + temperature:speed + temperature:radiation +
                             humidity:speed + humidity:radiation + speed:radiation + 
                             (1 | dog) + (1 | site) + (1 | sess), data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)

full_model_betabinomial <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                                     temperature:humidity + temperature:speed + temperature:radiation +
                                     humidity:speed + humidity:radiation + speed:radiation + 
                                     (1 | dog) + (1 | site) + (1 | sess), data = std_df, ziformula = ~1, family = betabinomial(), weights = total, na.action = na.fail)


AIC(full_model_binomial, full_model_beta, full_model_betabinomial)

# df       AIC
# full_model_binomial     14 2889.1631
# full_model_beta         16 -151.7446
# full_model_betabinomial 16 1366.8643

# we will use beta!


# create full model

model_0 <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                     temperature:humidity + temperature:speed + temperature:radiation +
                     humidity:speed + humidity:radiation + speed:radiation + 
                     (1 | dog), data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)
best_so_far <- model_0

##### choose random effects #####

# 1st random effect
model_0_site <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                          temperature:humidity + temperature:speed + temperature:radiation +
                          humidity:speed + humidity:radiation + speed:radiation + 
                          (1 | dog) + (1 | site), data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)

model_0_sess <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                          temperature:humidity + temperature:speed + temperature:radiation +
                          humidity:speed + humidity:radiation + speed:radiation + 
                          (1 | dog) + (1 | sess), data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)

model_0_site_sess <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                               temperature:humidity + temperature:speed + temperature:radiation +
                               humidity:speed + humidity:radiation + speed:radiation + 
                               (1 | dog) + (1 | sess) + (1 | site), data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)


AIC(best_so_far, model_0_site, model_0_sess, model_0_site_sess)

# df       AIC
# best_so_far       14 -155.1857
# model_0_site      15 -153.7446
# model_0_sess      15 -153.1857
# model_0_site_sess 16 -151.7446

# no need for random effects (only dog)!
best_so_far <- model_0


##### model selection #####
final_model <- stepAIC(best_so_far, direction = "backward")


# Start:  AIC=-155.19
# prop ~ temperature + humidity + speed + radiation + temperature:humidity + 
#   temperature:speed + temperature:radiation + humidity:speed + 
#   humidity:radiation + speed:radiation
# 
# Df     AIC
# - humidity:radiation     2 -155.65
# - speed:radiation        2 -155.43
# - temperature:speed      2 -155.19
# <none>                     -155.19
# - temperature:humidity   2 -154.82
# - humidity:speed         2 -153.38
# - temperature:radiation  2 -149.70
# 
# Step:  AIC=-155.65
# prop ~ temperature + humidity + speed + radiation + temperature:humidity + 
#   temperature:speed + temperature:radiation + humidity:speed + 
#   speed:radiation
# 
# Df     AIC
# - speed:radiation        1 -157.42
# - temperature:speed      1 -156.95
# - temperature:humidity   1 -156.81
# <none>                     -155.65
# - humidity:speed         1 -154.67
# - temperature:radiation  1 -150.61
# 
# Step:  AIC=-157.42
# prop ~ temperature + humidity + speed + radiation + temperature:humidity + 
#   temperature:speed + temperature:radiation + humidity:speed
# 
# Df     AIC
# - temperature:humidity   1 -158.81
# <none>                     -157.42
# - temperature:speed      1 -156.53
# - humidity:speed         1 -156.37
# - temperature:radiation  1 -152.19
# 
# Step:  AIC=-158.81
# prop ~ temperature + humidity + speed + radiation + temperature:speed + 
#   temperature:radiation + humidity:speed
# 
# Df     AIC
# <none>                     -158.81
# - humidity:speed         1 -157.88
# - temperature:speed      1 -153.90
# - temperature:radiation  1 -152.72


final_model_random <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                                temperature:speed + temperature:radiation + humidity:speed +
                                (1 | dog), data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)

summary(final_model_random)

# Family: beta  ( logit )
# Formula:          prop ~ temperature + humidity + speed + radiation + temperature:speed +  
#   temperature:radiation + humidity:speed + (1 | dog)
# Zero inflation:        ~1
# Data: std_df
# 
# AIC      BIC   logLik deviance df.resid 
# -159.8   -124.4     90.9   -181.8      173 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# dog    (Intercept) 0.03226  0.1796  
# Number of obs: 184, groups:  dog, 7
# 
# Dispersion parameter for beta family (): 11.5 
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -1.83489    0.10305 -17.806  < 2e-16 ***
#   temperature            0.39798    0.10682   3.726 0.000195 ***
#   humidity               0.17208    0.07339   2.345 0.019031 *  
#   speed                 -0.18425    0.08835  -2.085 0.037028 *  
#   radiation             -0.17487    0.09646  -1.813 0.069847 .  
# temperature:speed      0.19006    0.08648   2.198 0.027963 *  
#   temperature:radiation  0.15974    0.06675   2.393 0.016703 *  
#   humidity:speed         0.14606    0.07826   1.866 0.062002 .  
# ---
#   Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
# 
# Zero-inflation model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.8500     0.2151  -8.599   <2e-16 ***
#   ---
#   Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1

library(rcompanion)
res <- residuals(final_model_random, type = "pearson")
pred <- predict(final_model_random, type = "response")

efronRSquared(residual = res, 
              predicted = pred, 
              statistic = "EfronRSquared")

# EfronRSquared 
# 0.742 


plot(res ~ std_df$temperature)
plot(res ~ std_df$radiation)
plot(res ~ std_df$speed)

plot(pred ~ std_df$prop)


mean(res)

# [1] -0.0005158676

min_temperature <- ceiling(min(rel_df$temperature))
max_temperature <- floor(max(rel_df$temperature))

temperature <- seq(min_temperature, max_temperature, 1)

min_radiation <- ceiling(min(rel_df$radiation))
max_radiation <- floor(max(rel_df$radiation))

radiation <- seq(min_radiation, max_radiation, 50)

radiation_temperature_df <- expand.grid(radiation = radiation, temperature = temperature)

b_intercept <- -1.83
b_radiation <- -0.17487
b_temperature <- 0.4
b_interaction <- -0.16

radiation_temperature_df <- radiation_temperature_df %>%
  mutate(normal_radiation = (radiation - mean_radiation) / sd_radiation,
         normal_temperature = (temperature - mean_temperature) / sd_temperature) %>%
  mutate(right_side = exp(b_intercept) * exp(b_radiation* normal_radiation) *
           exp(b_temperature * normal_temperature) * exp(b_interaction * normal_radiation * normal_temperature)) %>%
  mutate(bad_probability = right_side / (1 + right_side))

ggplot(radiation_temperature_df, aes(x = radiation, y = temperature, fill = bad_probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize color scale
  labs(x = "radiation", y = "temperature", fill = "bad_probability") +
  theme_minimal()  # Customize the plot theme
