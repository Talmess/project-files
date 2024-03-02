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

org_df <- read.csv("merged_where.csv")

df <- org_df %>%
  filter(dog != "amigo") %>%
  filter(where == "inside") %>%
  filter(site != "hadasa2") %>%
  filter(speed < 10)

rel_df <- df[,c(4,5,7,9,10,12:16)]

dir.create("plots", showWarnings = F)

# prop ~ climate plots

dir.create("plots/in_or_out/prop_vs_climate", showWarnings = F)

ggplot(rel_df, aes(x = temperature, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/prop_vs_climate/temperature.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = humidity, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/prop_vs_climate/humidity.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = ground, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/prop_vs_climate/ground.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = radiation, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/prop_vs_climate/radiation.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = speed, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/prop_vs_climate/speed.jpg", width = 10, height = 5, limitsize = F)


# histograms
dir.create("plots/in_or_out/histograms", showWarnings = F)

ggplot(rel_df, aes(x = prop)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/histograms/prop.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = temperature)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/histograms/temperature.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = speed)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/histograms/speed.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = humidity)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/histograms/humidity.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = ground)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/histograms/ground.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = radiation)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/in_or_out/histograms/radiation.jpg", width = 10, height = 5, limitsize = F)


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
# 8.102623    1.899647    1.469156    3.909349   12.705819  

basic_model_2 <- lm(prop ~ temperature + speed + humidity + radiation, data = std_df)
vif(basic_model_2)

# temperature       speed    humidity   radiation 
# 2.091904    1.667799    1.201851    1.911415 


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
# full_model_binomial     14 6117.2256
# full_model_beta         16  179.1950
# full_model_betabinomial 16  797.8642


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

# df     AIC
# best_so_far       14 175.195
# model_0_site      15 177.195
# model_0_sess      15 177.195
# model_0_site_sess 16 179.195

# no need for random effects (only dog)!
best_so_far <- model_0


##### model selection #####
final_model <- stepAIC(best_so_far, direction = "backward")


# Start:  AIC=175.2
# prop ~ temperature + humidity + speed + radiation + temperature:humidity + 
#   temperature:speed + temperature:radiation + humidity:speed + 
#   humidity:radiation + speed:radiation
# 
# Df    AIC
# - temperature:radiation  2 171.22
# - humidity:radiation     2 171.25
# - temperature:humidity   2 171.33
# - humidity:speed         2 171.74
# - temperature:speed      2 173.00
# - speed:radiation        2 173.58
# <none>                     175.19
# 
# Step:  AIC=171.22
# prop ~ temperature + humidity + speed + radiation + temperature:humidity + 
#   temperature:speed + humidity:speed + humidity:radiation + 
#   speed:radiation
# 
# Df    AIC
# - humidity:radiation    1 169.26
# - temperature:humidity  1 169.33
# - humidity:speed        1 169.74
# <none>                    171.22
# - temperature:speed     1 171.41
# - speed:radiation       1 171.60
# 
# Step:  AIC=169.27
# prop ~ temperature + humidity + speed + radiation + temperature:humidity + 
#   temperature:speed + humidity:speed + speed:radiation
# 
# Df    AIC
# - temperature:humidity  1 167.35
# - humidity:speed        1 168.32
# <none>                    169.26
# - temperature:speed     1 169.42
# - speed:radiation       1 170.11
# 
# Step:  AIC=167.35
# prop ~ temperature + humidity + speed + radiation + temperature:speed + 
#   humidity:speed + speed:radiation
# 
# Df    AIC
# - humidity:speed     1 166.32
# <none>                 167.35
# - temperature:speed  1 167.60
# - speed:radiation    1 168.39
# 
# Step:  AIC=166.32
# prop ~ temperature + humidity + speed + radiation + temperature:speed + 
#   speed:radiation
# 
# Df    AIC
# - humidity           1 164.51
# <none>                 166.32
# - temperature:speed  1 167.47
# - speed:radiation    1 169.16
# 
# Step:  AIC=164.51
# prop ~ temperature + speed + radiation + temperature:speed + 
#   speed:radiation
# 
# Df    AIC
# <none>                 164.51
# - temperature:speed  1 167.46
# - speed:radiation    1 171.11


final_model_random <- glmmTMB(prop ~ temperature + speed + radiation +
                                temperature:speed + speed:radiation + 
                                (1 | dog), data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)

summary(final_model_random)

# Family: beta  ( logit )
# Formula:          prop ~ temperature + speed + radiation + temperature:speed +  
#   speed:radiation + (1 | dog)
# Zero inflation:        ~1
# Data: std_df
# 
# AIC      BIC   logLik deviance df.resid 
# 166.5    191.8    -74.3    148.5      114 
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# dog    (Intercept) 7.13e-10 2.67e-05
# Number of obs: 123, groups:  dog, 7
# 
# Dispersion parameter for beta family (): 2.77 
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -0.3221     0.1564  -2.059 0.039473 *  
#   temperature         1.0449     0.3009   3.473 0.000515 ***
#   speed               0.1293     0.1828   0.707 0.479612    
# radiation          -0.4443     0.2347  -1.893 0.058312 .  
# temperature:speed   0.5866     0.2739   2.142 0.032208 *  
#   speed:radiation    -0.5828     0.2032  -2.869 0.004122 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Zero-inflation model:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.01626    0.18034   -0.09    0.928

library(rcompanion)
res <- residuals(final_model_random, type = "pearson")
pred <- predict(final_model_random, type = "response")

efronRSquared(residual = res, 
              predicted = pred, 
              statistic = "EfronRSquared")

# EfronRSquared 
# 0.21 


plot(res ~ std_df$temperature)
plot(res ~ std_df$radiation)
plot(res ~ std_df$speed)

plot(pred ~ std_df$prop)


mean(res)

# [1] 0.02006916


min_temperature <- ceiling(min(rel_df$temperature))
max_temperature <- floor(max(rel_df$temperature))

temperature <- seq(min_temperature, max_temperature, 1)

temperature_df <- as.data.frame(temperature)

b_intercept <- -0.32
b_temperature <- 1.05

temperature_df <- temperature_df %>%
  mutate(normal = (temperature - mean_temperature) / sd_temperature) %>%
  mutate(right_side = exp(b_intercept) * exp(1.05 * normal)) %>%
  mutate(in_probability = right_side / (1 + right_side))

ggplot(temperature_df, aes(x = temperature, y = in_probability)) +
  geom_line()


min_radiation <- ceiling(min(rel_df$radiation))
max_radiation <- floor(max(rel_df$radiation))

radiation <- seq(min_radiation, max_radiation, 50)

radiation_df <- as.data.frame(radiation)

b_intercept <- -0.32
b_radiation <- -0.44

radiation_df <- radiation_df %>%
  mutate(normal = (radiation - mean_radiation) / sd_radiation) %>%
  mutate(right_side = exp(b_intercept) * exp(b_radiation * normal)) %>%
  mutate(in_probability = right_side / (1 + right_side))

ggplot(radiation_df, aes(x = radiation, y = in_probability)) +
  geom_line()



min_speed <- ceiling(min(rel_df$speed))
max_speed <- floor(max(rel_df$speed))

speed <- seq(min_speed, max_speed, 0.25)

min_radiation <- ceiling(min(rel_df$radiation))
max_radiation <- floor(max(rel_df$radiation))

radiation <- seq(min_radiation, max_radiation, 50)

speed_radiation_df <- expand.grid(speed = speed, radiation = radiation)

b_intercept <- -0.32
b_speed <- 0.13
b_radiation <- -0.44
b_interaction <- -0.58

speed_radiation_df <- speed_radiation_df %>%
  mutate(normal_speed = (speed - mean_speed) / sd_speed,
         normal_radiation = (radiation - mean_radiation) / sd_radiation) %>%
  mutate(right_side = exp(b_intercept) * exp(b_speed* normal_speed) *
           exp(b_radiation * normal_radiation) * exp(b_interaction * normal_speed * normal_radiation)) %>%
  mutate(in_probability = right_side / (1 + right_side))

ggplot(speed_radiation_df, aes(x = speed, y = radiation, fill = in_probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize color scale
  labs(x = "speed", y = "radiation", fill = "in_probability") +
  theme_minimal()  # Customize the plot theme


min_temperature <- ceiling(min(rel_df$temperature))
max_temperature <- floor(max(rel_df$temperature))

temperature <- seq(min_temperature, max_temperature, 1)

min_speed <- ceiling(min(rel_df$speed))
max_speed <- floor(max(rel_df$speed))

speed <- seq(min_speed, max_speed, 0.25)

temperature_speed_df <- expand.grid(temperature = temperature, speed = speed)

b_intercept <- -0.32
b_temperature <- 1.05
b_speed <- 0.13
b_interaction <- 0.59
temperature_speed_df <- temperature_speed_df %>%
  mutate(normal_temperature = (temperature - mean_temperature) / sd_temperature,
         normal_speed = (speed - mean_speed) / sd_speed) %>%
  mutate(right_side = exp(b_intercept) * exp(b_temperature* normal_temperature) *
           exp(b_speed * normal_speed) * exp(b_interaction * normal_temperature * normal_speed)) %>%
  mutate(in_probability = right_side / (1 + right_side))

ggplot(temperature_speed_df, aes(x = temperature, y = speed, fill = in_probability)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize color scale
  labs(x = "temperature", y = "Speed", fill = "in_probability") +
  theme_minimal()  # Customize the plot theme