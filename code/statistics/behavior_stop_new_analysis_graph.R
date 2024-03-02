
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

rel_behavior <- "stop"

org_df <- read.csv("merged_spec_behaviors.csv")

df <- org_df %>%
  filter(dog != "amigo") %>%
  filter(behavior == rel_behavior) %>%
  filter(speed < 10)

rel_df <- df[,c(4,5,7,9,10,12:16)]

dir.create("plots", showWarnings = F)

# prop ~ climate plots

dir.create(paste0("plots/behavior_",rel_behavior, "/prop_vs_climate"), showWarnings = F)

ggplot(rel_df, aes(x = temperature, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/prop_vs_climate/temperature.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = humidity, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/prop_vs_climate/humidity.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = ground, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/prop_vs_climate/ground.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = radiation, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/prop_vs_climate/radiation.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = speed, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/prop_vs_climate/speed.jpg"), width = 10, height = 5, limitsize = F)


# histograms
dir.create(paste0("plots/behavior_",rel_behavior, "/histograms"), showWarnings = F)

ggplot(rel_df, aes(x = prop)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/histograms/prop.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = temperature)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/histograms/temperature.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = speed)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/histograms/speed.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = humidity)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/histograms/humidity.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = ground)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/histograms/ground.jpg"), width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = radiation)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave(paste0("plots/behavior_",rel_behavior, "/histograms/radiation.jpg"), width = 10, height = 5, limitsize = F)


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
# full_model_binomial     14 1882.6479
# full_model_beta         16 -239.2168
# full_model_betabinomial 16 1198.3194

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
# best_so_far       14 -240.7074
# model_0_site      15 -239.0373
# model_0_sess      15 -240.8053
# model_0_site_sess 16 -239.2168

# we also need sess as random effect!
best_so_far <- model_0_sess


##### model selection #####
final_model <- stepAIC(best_so_far, direction = "backward")


# Start:  AIC=-240.81
# prop ~ temperature + humidity + speed + radiation + temperature:humidity + 
#   temperature:speed + temperature:radiation + humidity:speed + 
#   humidity:radiation + speed:radiation
# 
# Df     AIC
# <none>                     -240.81
# - humidity:radiation     3 -226.41
# - humidity:speed         3 -225.47
# - temperature:speed      3 -225.35
# - speed:radiation        3 -225.09
# - temperature:humidity   3 -224.32
# - temperature:radiation  3 -223.10

final_model_random <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                                temperature:humidity + temperature:speed + temperature:radiation +
                                humidity:speed + humidity:radiation + speed:radiation +
                                (1 | dog) + (1 | sess), data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)

summary(final_model_random)

# Family: beta  ( logit )
# Formula:          prop ~ temperature + humidity + speed + radiation + temperature:humidity +  
#   temperature:speed + temperature:radiation + humidity:speed +  
#   humidity:radiation + speed:radiation + (1 | dog) + (1 | sess)
# Zero inflation:        ~1
# Data: std_df
# 
# AIC      BIC   logLik deviance df.resid 
# -240.8   -192.6    135.4   -270.8      169 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# dog    (Intercept) 0.11424  0.3380  
# sess   (Intercept) 0.01479  0.1216  
# Number of obs: 184, groups:  dog, 7; sess, 2
# 
# Dispersion parameter for beta family (): 22.9 
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -2.346273   0.177968 -13.184  < 2e-16 ***
#   temperature            0.008327   0.144557   0.058 0.954066    
# humidity               0.031524   0.072181   0.437 0.662305    
# speed                 -0.312374   0.088136  -3.544 0.000394 ***
#   radiation              0.063746   0.114927   0.555 0.579124    
# temperature:humidity   0.233945   0.125966   1.857 0.063281 .  
# temperature:speed     -0.216413   0.120623  -1.794 0.072794 .  
# temperature:radiation  0.065144   0.071018   0.917 0.358991    
# humidity:speed         0.117858   0.091680   1.286 0.198604    
# humidity:radiation    -0.039025   0.123074  -0.317 0.751180    
# speed:radiation        0.144068   0.098175   1.467 0.142250    
# ---
#   Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
# 
# Zero-inflation model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.5208     0.1922  -7.914 2.49e-15 ***
#   ---
#   Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1

library(rcompanion)
res <- residuals(final_model_random, type = "pearson")
pred <- predict(final_model_random, type = "response")

efronRSquared(residual = res, 
              predicted = pred, 
              statistic = "EfronRSquared")

# EfronRSquared 
# 0.943 


plot(res ~ std_df$temperature)
plot(res ~ std_df$radiation)
plot(res ~ std_df$speed)

plot(pred ~ std_df$prop)


mean(res)

# [1] 2.386622e-05
min_speed <- ceiling(min(rel_df$speed))
max_speed <- floor(max(rel_df$speed))

speed <- seq(min_speed, max_speed, 0.25)

speed_df <- as.data.frame(speed)

b_intercept <- -2.35
b_speed <- 0.31

speed_df <- speed_df %>%
  mutate(normal = (speed - mean_speed) / sd_speed) %>%
  mutate(right_side = exp(b_intercept) * exp(b_speed * normal)) %>%
  mutate(stop_probability = right_side / (1 + right_side))

ggplot(speed_df, aes(x = speed, y = stop_probability)) +
  geom_line()



min_temperature <- ceiling(min(rel_df$temperature))
max_temperature <- floor(max(rel_df$temperature))

temperature <- seq(min_temperature, max_temperature, 1)

min_humidity <- ceiling(min(rel_df$humidity))
max_humidity <- floor(max(rel_df$humidity))

humidity <- seq(min_humidity, max_humidity, 2)

temperature_humidity_df <- expand.grid(temperature = temperature, humidity = humidity)

b_intercept <- -2.35
b_temperature <- 0.01
b_humidity <- -0.03
b_interaction <- 0.23

temperature_humidity_df <- temperature_humidity_df %>%
  mutate(normal_temperature = (temperature - mean_temperature) / sd_temperature,
         normal_humidity = (humidity - mean_humidity) / sd_humidity) %>%
  mutate(right_side = exp(b_intercept) * exp(b_temperature* normal_temperature) *
           exp(b_humidity * normal_humidity) * exp(b_interaction * normal_temperature * normal_humidity)) %>%
  mutate(stop_probability = right_side / (1 + right_side))

ggplot(temperature_humidity_df, aes(x = temperature, y = humidity, fill = stop_probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize color scale
  labs(x = "temperature", y = "humidity", fill = "stop_probability") +
  theme_minimal()  # Customize the plot theme