#### Shuhan's part Load the packages####

library(readxl)
X0329_Data <- read_excel("0329_Data.xlsx")
View(X0329_Data)
library(dplyr)
library(lme4)
library(ggplot2)


#### Regression####

# as.factor
urbanization <- X0329_Data %>%
  mutate(state = as.factor(state),
         year = factor(year, ordered = TRUE),
         rate = urban_area_km2/Population)
# Remove the NAs
urbanization_clean <- urbanization %>%
  filter(!is.na(Bird_Count) & 
           !is.nan(Bird_Count) & 
           is.finite(Bird_Count) & 
           !is.na(rate))  

# linear model( bird data ~ urban/population  && bird data ~ time series )

linear_model <- lm(Bird_Count ~ rate, data = urbanization_clean)

summary(linear_model)


linear_model_time<-lm(Bird_Count ~ year, data = urbanization_clean)
linear_model_time

# linear model visulization

library(ggplot2)
library(dplyr)  

linear_plot_1 <- ggplot(urbanization_clean, aes(x = rate, y = Bird_Count)) +
  geom_point(size = 0.8, alpha = 0.7) +  
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linewidth = 1.2) +  
  labs(
    x = "Urbanization (Urban Area km²)/Population", 
    y = "Bird Count", 
    title = "Relationship Between Urbanization and Bird Count" ,
    color = "state"
  ) +
  scale_color_viridis_d()+
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

# Liner plot---time series
linear_plot_time <- ggplot(urbanization_clean, aes(x = year, y = Bird_Count)) +
  geom_boxplot(
    width = 0.3,        
    alpha = 0.5,        
    outlier.size = 0.8,  
    color = "gray40",   
    fill = "gray90"     
  ) +
  geom_point(
    aes(color= state),
    size = 0.8, 
    alpha = 0.7,
    position = position_jitter(width = 0.1)  
  ) +  
  geom_smooth(
    method = "lm", 
    se = FALSE, 
    color = "darkgreen", 
    linewidth = 1.2,
    aes(group = 1)  
  ) +  
  labs(
    x = "Year", 
    y = "Bird Count", 
    title = "Relationship Between Year and Bird Count",
    ) +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +  
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),  
    panel.grid.minor = element_blank(), 
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),  
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
    axis.text = element_text(size = 10)  
  )

linear_plot_time


#### multiple regression- multi-linear regression####

multiple_model<-lm(Bird_Count ~ rate + Temperature + Precipitation,data = urbanization_clean)
summary(multiple_model)

# mix_effect_model
mixed_model <- lmer(Bird_Count ~ rate + Temperature + Precipitation + (1 | year) +(1 | state), data = urbanization_clean)
summary(mixed_model)


####0330Visulization####
library(ggplot2)

# Random effect(State)
random_effects <- ranef(mixed_model)$state
random_effects$state <- rownames(random_effects)  

ggplot(random_effects, aes(x = state, y = `(Intercept)`)) +
  geom_point(size = 3, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Random Intercepts by State",
    x = "State",
    y = "Deviation from Fixed Intercept"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


fixed_effects <- fixef(mixed_model)
fixed_effects_df <- data.frame(
  Predictor = names(fixed_effects),
  Estimate = fixed_effects,
  stringsAsFactors = FALSE
)

# Fix effect(others, temp, precipitation)
ggplot(fixed_effects_df[-1, ], aes(x = Predictor, y = Estimate)) +  
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Fixed Effects (Slopes) of Predictors",
    x = "Predictor",
    y = "Estimate"
  ) +
  theme_minimal()

urbanization_clean$fitted <- predict(mixed_model)

ggplot(urbanization_clean, aes(x = fitted, y = Bird_Count, color = state)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Observed vs. Fitted Bird Count",
    x = "Fitted Values",
    y = "Observed Bird Count",
    color = "State"
  ) +
  theme_minimal()




# Hanbin
## Poisson regression
urbanization_p_t <- urbanization_clean %>%
  filter(!is.na(Precipitation), !is.na(Temperature))

birdcount_glm_r_p_t <- glm(
  Bird_Count ~ rate + Precipitation + Temperature,
  data = urbanization_p_t,
  family = "poisson")

summary(birdcount_glm_r_p_t)
plot(birdcount_glm_r_p_t)

library(gtsummary)
tbl_regression(birdcount_glm_r_p_t)



## Non-linear effects
birdcount_glm_quad <- glm(
  Bird_Count ~ rate + Precipitation + I(Precipitation^2) + Temperature + I(Temperature^2),
  data = urbanization_p_t,
  family = "poisson")

summary(birdcount_glm_quad)
plot(birdcount_glm_quad)
tbl_regression(birdcount_glm_quad)



## Interaction between temperature and precipitation
birdcount_interaction <- glm(
  Bird_Count ~ rate + Temperature * Precipitation,
  data = urbanization_p_t,
  family = poisson()
)
summary(birdcount_interaction)
plot(birdcount_interaction)
tbl_regression(birdcount_interaction)



## Visualize individual effects
mean_temp <- mean(urbanization_p_t$Temperature, na.rm = TRUE)
mean_precip <- mean(urbanization_p_t$Precipitation, na.rm = TRUE)
mean_rate <- mean(urbanization_p_t$rate, na.rm = TRUE)

newdata_rate <- data.frame(
  rate = seq(min(urbanization_p_t$rate), max(urbanization_p_t$rate), length.out = 100),
  Precipitation = mean_precip,
  Temperature = mean_temp
)
View(newdata_rate)

newdata_precip <- data.frame(
  rate = mean_rate,
  Precipitation = seq(min(urbanization_p_t$Precipitation), max(urbanization_p_t$Precipitation), length.out = 100),
  Temperature = mean_temp
)
View(newdata_precip)

newdata_temp <- data.frame(
  rate = mean_rate,
  Precipitation = mean_precip,
  Temperature = seq(min(urbanization_p_t$Temperature), max(urbanization_p_t$Temperature), length.out = 100)
)
View(newdata_temp)

newdata_rate$Predicted <- predict(birdcount_glm_r_p_t, newdata = newdata_rate, type = "response")
View(newdata_rate)
newdata_precip$Predicted <- predict(birdcount_glm_r_p_t, newdata = newdata_precip, type = "response")
View(newdata_precip)
newdata_temp$Predicted <- predict(birdcount_glm_r_p_t, newdata = newdata_temp, type = "response")
View(newdata_temp)

effect_urbanization <- ggplot(newdata_rate, aes(x = rate, y = Predicted)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Effect of Urbanization Rate on Bird Count",
    x = "Urban Area per Capita",
    y = "Predicted Bird Count"
  ) +
  theme_minimal()
effect_urbanization

effect_preciptation <- ggplot(newdata_precip, aes(x = Precipitation, y = Predicted)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  labs(
    title = "Effect of Precipitation on Bird Count",
    x = "Precipitation",
    y = "Predicted Bird Count"
  ) +
  theme_minimal()
effect_preciptation

effect_temperature <- ggplot(newdata_temp, aes(x = Temperature, y = Predicted)) +
  geom_line(color = "firebrick", linewidth = 1.2) +
  labs(
    title = "Effect of Temperature on Bird Count",
    x = "Temperature",
    y = "Predicted Bird Count"
  ) +
  theme_minimal()
effect_temperature



## Descriptive Statistics for Data Set
summary(urbanization_clean)
sd(urbanization_clean$Bird_Count, na.rm = TRUE)
sd(urbanization_clean$rate, na.rm = TRUE)
sd(urbanization_clean$Precipitation, na.rm = TRUE)
sd(urbanization_clean$Temperature, na.rm = TRUE)


