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
library(car)
library(tidyr)
library(MuMIn)

org_df <- read.csv("merged_where.csv")

df <- org_df %>%
  filter(dog != "amigo") %>%
  filter(where == "inside") %>%
  filter(speed < 10)

rel_df <- df[,c(4,5,7,9,10,12:16)]

dir.create("plots", showWarnings = F)

# prop ~ climate plots

dir.create("plots/prop_vs_climate", showWarnings = F)

ggplot(rel_df, aes(x = temperature, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/prop_vs_climate/temperature.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = humidity, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/prop_vs_climate/humidity.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = ground, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/prop_vs_climate/ground.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = radiation, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/prop_vs_climate/radiation.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = speed, y = prop)) +
  geom_point() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/prop_vs_climate/speed.jpg", width = 10, height = 5, limitsize = F)


# histograms
dir.create("plots/histograms", showWarnings = F)

ggplot(rel_df, aes(x = prop)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/histograms/prop.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = temperature)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/histograms/temperature.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = speed)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/histograms/speed.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = humidity)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/histograms/humidity.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = ground)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/histograms/ground.jpg", width = 10, height = 5, limitsize = F)

ggplot(rel_df, aes(x = radiation)) +
  geom_histogram() +
  theme_bw() +
  facet_grid(site ~ .)
ggsave("plots/histograms/radiation.jpg", width = 10, height = 5, limitsize = F)


std_df <- rel_df %>% 
  mutate_at(c("temperature", "speed", "humidity", "radiation", "ground"), ~(scale(.) %>% as.vector))

pairs(std_df[,c(6:10)])

# basic_model <- lm(prop ~ temperature + speed + humidity + radiation + ground, data = std_df)
# vif(basic_model)
# 
# # temperature       speed    humidity   radiation      ground 
# # 9.505208    2.125046    1.519936    5.975884   16.559208 
# 
# basic_model_2 <- lm(prop ~ temperature + speed + humidity + radiation, data = std_df)
# vif(basic_model_2)
# 
# # temperature       speed    humidity   radiation 
# # 3.117152    1.699019    1.181982    3.063373 


full_model_binomial <- glmmTMB(prop ~ temperature + humidity + speed + radiation +
                                 temperature:humidity + temperature:speed + temperature:radiation +
                                 humidity:speed + humidity:radiation + speed:radiation
                               + (1 | site) + (1 | dog), data = std_df, family = binomial, weights = total, na.action = na.fail)

summary(full_model_binomial)

AIC(full_model)
# model selection forward AIC for random effects

full_model_beta <- glmmTMB(prop ~ (1|site) + (1|dog) +
                             radiation + speed + temperature + humidity +
                             radiation:humidity + speed:temperature + radiation:speed +
                             temperature:humidity + radiation:temperature, 
                           data = std_df, ziformula = ~1, family = beta_family(), na.action = na.fail)
summary(full_model_beta)

final = stepAIC(full_model_beta)

summary(final)
