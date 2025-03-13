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

# linear model visulization

library(ggplot2)
library(dplyr)  

linear_plot_1 <- ggplot(urbanization_clean, aes(x = urban_area_km2, y = Bird_Count)) +
  geom_point(color = "lightblue3", size = 0.8, alpha = 0.7) +  
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linewidth = 1.2) +  
  labs(
    x = "Urbanization (Urban Area km²)", 
    y = "Bird Count", 
    title = "Relationship Between Urbanization and Bird Count"  
  ) +
  theme_minimal() +  
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),  
    panel.grid.minor = element_blank(), 
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),  
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
    axis.text = element_text(size = 10)  
  )

linear_plot_1




# visualization
UrbanVsBirdScatter<-ggplot(urbanization, aes(x = urban_area_km2, y = Bird_Count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Urbanization", y = "Bird Count")

print(UrbanVsBirdScatter)

# mix_effect_model
mixed_model <- lmer(Bird_Count ~ urban_area_km2 + Temperature + (1 | state) + (1 | year), data = urbanization)
summary(mixed_model)


MixedScatter<-ggplot(urbanization, aes(x = urban_area_km2, y = Bird_Count)) +
  geom_point() +
  geom_smooth(method = "lmer", se = FALSE) +
  labs(x = "Urbanization", y = "Bird Count")

print(MixedScatter)

# non_linear model
quadratic_model <- lmer(Bird_Count ~ urban_area_km2 + I(urban_area_km2^2) + Temperature  + (1 | state) + (1 | year), data = urbanization)
summary(quadratic_model)

#GLM
model_GLM <- glm(Bird_Count ~ year + urban_area_km2, data = urbanization, family = gaussian)
summary(model_GLM)


#  AIC
AIC(base_model, mixed_model, quadratic_model,model_GLM)


