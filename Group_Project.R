#### Load the packages####

library(readxl)
X0329_Data <- read_excel("~/Public/Duke-2025/applied statistcal/Group_Projrct_Data/710_1D_Data/0329_Data.xlsx")
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





# Sam's Plot
 
#initial look at US urbanization trend by state/territory
 
 urbanization_plot_absolutevaluescale <- urbanization%>%
   filter(year %in% c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
   ) %>%  # Years
   ggplot(aes(x = factor(year), y = urban_area_km2, fill = factor(year))) +  
   geom_bar(stat = "identity") +  #bar equals urban area
   facet_wrap(~state) +  #faceted for each state
   labs(
     title = "Urbanization Trends by State/Territory by Year",
     x = "Year",
     y = "Urban Area (km²)",
     fill = "Year") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 75, hjust = 1, size=6))
 
 print( urbanization_plot_absolutevaluescale)
 
 
 #free y scale so that trends can be observed for smaller states
 
 urbanization_plot_adjustedscale <- urbanization%>%
   filter(year %in% c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)
   ) %>%  # Years
   ggplot(aes(x = factor(year), y = urban_area_km2, fill = factor(year))) +  
   geom_bar(stat = "identity") +  #bar equals urban area
   facet_wrap(~state, scales = "free_y") +  #faceted for each state
   labs(
     title = "Urbanization Trends by State/Territory by Year Adjusted Scale",
     x = "Year",
     y = "Urban Area (km²)",
     fill = "Year") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 75, hjust = 1, size=6))
 
 print(urbanization_plot_adjustedscale)
 

