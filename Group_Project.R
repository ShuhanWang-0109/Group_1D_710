#### Load the packages####

library(readxl)
urbanization <- read_excel("~/Public/Duke-2025/applied statistcal/Group_Projrct_Data/710_1D_Data/All_States_Urbanization_Trend.xlsx")
View(urbanization)
library(dplyr)
library(lme4)
library(ggplot2)


# as.factor
urbanization <- urbanization %>%
  mutate(state = as.factor(state))

urbanization <- urbanization %>%
  mutate(year = factor(year, ordered = TRUE))
# linear model
base_model <- lm(Bird_Count ~ urban_area_km2, data = urbanization)
summary(base_model)

# visulization
ggplot(urbanization, aes(x = urban_area_km2, y = Bird_Count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Urbanization", y = "Bird Count")

# mix_effect_model
mixed_model <- lmer(Bird_Count ~ urban_area_km2 + Temperature + (1 | state) + (1 | year), data = urbanization)
summary(mixed_model)


ggplot(urbanization, aes(x = urban_area_km2, y = Bird_Count)) +
  geom_point() +
  geom_smooth(method = "lmer", se = FALSE) +
  labs(x = "Urbanization", y = "Bird Count")

# non_linear model
quadratic_model <- lmer(Bird_Count ~ urban_area_km2 + I(urban_area_km2^2) + Temperature  + (1 | state) + (1 | year), data = urbanization)
summary(quadratic_model)

#GLM
model_GLM <- glm(Bird_Count ~ year + urban_area_km2, data = urbanization, family = gaussian)
summary(model_GLM)


#  AIC
AIC(base_model, mixed_model, quadratic_model,model_GLM)


