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
 
 # Sam's Species Charts
 
 ---
   title: "Final Project Sam Drafting"
 output: pdf_document
 date: "2025-03-30"
 ---
   
   
   Setup
 ```{r setup, include=FALSE}
 knitr::opts_chunk$set(echo = TRUE)
 
 library(here)
 library(readxl)
 here()
 library(ggplot2)
 library(lme4)
 library(dplyr)
 library(tibble)
 ```
 
 Data Wrangling-- Using only states with population data + creating column Urban Km2/Population to normalize by population 
 ```{r}
 
 SpeciesData<-read_excel(here("0329_Data.xlsx"))
 
 SpeciesDataWrangled<-SpeciesData %>% 
   mutate(Urban_Area_Per_Capita = urban_area_km2/Population) %>% 
   filter(!is.na(Urban_Area_Per_Capita))
 ```
 
 
 
 Exploratory Graph: Birdcount~PopDensity
 ```{r}
 
 ggplot(SpeciesDataWrangled, aes(x = Population, y = Bird_Count)) +
   geom_point(alpha = 0.6, color = "darkblue") +
   labs(
     x = "Population",
     y = "Bird Count",
     title = "Bird Count vs. Population"
   ) +
   theme_bw()
 
 ggplot(SpeciesDataWrangled, aes(x = urban_area_km2, y = Bird_Count)) +
   geom_point(alpha = 0.6, color = "darkblue") +
   labs(
     x = "Urban Area km2",
     y = "Bird Count",
     title = "Bird Count vs. Urban Area km2"
   ) +
   theme_bw()
 
 ggplot(SpeciesDataWrangled, aes(x = Urban_Area_Per_Capita, y = Bird_Count)) +
   geom_point(alpha = 0.6, color = "darkblue") +
   labs(
     x = "Urban_Area_Per_Capita",
     y = "Bird Count",
     title = "Bird Count vs. Urban_Area_Per_Capita"
   ) +
   theme_bw()
 
 
 ggplot(SpeciesDataWrangled, aes(x = Urban_Area_Per_Capita, y = `Swainson's Hawk`)) +
   geom_point(alpha = 0.6, color = "darkblue") +
   labs(
     x = "Urban_Area_Per_Capita",
     y = "Swainson's Hawk Poplation",
     title = "Swainson's Hawk Population vs. Urban_Area_Per_Capita"
   ) +
   theme_bw()
 
 ggplot(SpeciesDataWrangled, aes(x = Urban_Area_Per_Capita, y = `Northern Pintail`)) +
   geom_point(alpha = 0.6, color = "darkblue") +
   labs(
     x = "Urban_Area_Per_Capita",
     y = "Northern Pintal Popilation",
     title = "Northern Pintail Population vs. Urban_Area_Per_Capita"
   ) +
   theme_bw()
 
 
 
 #Run Poisson Distribution
 ```
 
 
 These trends are inerestinf but I want to focus on two specific species that could be sensitive to urbanization-- the Northern Pintail and Swainson's Hawk. The Northern Pintail is a long-distance migratory duck that breeds across much of the northern U.S. and Canada and winters as far south as Central America, making it a strong indicator of shifting migratory patterns due to climate change. Its reliance on shallow wetlands and grasslands also makes it highly vulnerable to urban sprawl, agricultural expansion, and habitat fragmentation. Swainson’s Hawk undertakes one of the longest migrations of any North American raptor, traveling from the western and central U.S. to Argentina, highlighting its sensitivity to disruptions across international flyways. It depends on wide-open prairies and agricultural lands, making it particularly affected by land-use change and increasingly rare in areas of suburban expansion.

```{r Wrangling Species Data}

library(ggplot2)

SpeciesDataPintail <- SpeciesDataWrangled %>%
  filter(!is.na(`Northern Pintail`)) %>%                    # Drop NAs
  group_by(state) %>%
  filter(sum(`Northern Pintail`, na.rm = TRUE) > 0) %>%     # Keep states with >0 total count
  ungroup()


ggplot(SpeciesDataPintail, aes(x = Urban_Area_Per_Capita, y = `Northern Pintail`)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  facet_wrap(~ state, scales = "free_y") +
  labs(
    title = "Northern Pintail Count vs Urban Area Per Capita (Only States with Observations)",
    x = "Urban Area Per Capita (km²/person)",
    y = "Northern Pintail Population"
  ) +
  theme_minimal(base_size = 11)


SpeciesDataSwainson <- SpeciesDataWrangled %>%
  filter(!is.na(`Swainson's Hawk`)) %>%                 #drop NAs
  group_by(state) %>%
  filter(sum(`Swainson's Hawk`, na.rm = TRUE) > 0) %>% #Keep only states with positive totals
  ungroup()


ggplot(SpeciesDataSwainson, aes(x = Urban_Area_Per_Capita, y = `Swainson's Hawk`)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  facet_wrap(~ state, scales = "free_y") +
  labs(
    title = "Swainson's Hawk Count vs Urban Area Per Capita (States with Observations)",
    x = "Urban Area Per Capita (km²/person)",
    y = "Swainson's Hawk Population"
  ) +
  theme_minimal(base_size = 11)

```

These relationships still are too varied and furthmore, the urbanization variables (effects on farmland, water use) map more cleanly onto regions than state. I will create regions to organize further.

```{r}

state_regions <-tibble(
  state = c("California", "Oregon", "Washington", "Nevada", "Arizona", "New Mexico", "Colorado", "Utah", "Idaho", "Montana", "Wyoming", "Alaska", "Hawaii",
            "North Dakota", "South Dakota", "Nebraska", "Kansas", "Minnesota", "Iowa", "Missouri", "Wisconsin", "Illinois", "Indiana", "Michigan", "Ohio",
            "Texas", "Oklahoma", "Arkansas", "Louisiana", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Georgia", "Florida", "South Carolina", "North Carolina", "Virginia", "West Virginia",
            "Delaware", "Maryland", "Pennsylvania", "New Jersey", "New York", "Connecticut", "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine", "District of Columbia"),
  region = c(rep("West", 13),
             rep("Midwest", 12),
             rep("South", 17),
             rep("Northeast", 9))
)


SpeciesDataPintail <- SpeciesDataPintail %>%
  left_join(state_regions, by = "state")

SpeciesDataSwainson <- SpeciesDataSwainson %>%
  left_join(state_regions, by = "state")
```

Now I'll create plot to see regional trends.
```{r}
PintailByRegionPlot<-ggplot(SpeciesDataPintail, aes(x = Urban_Area_Per_Capita, y = `Northern Pintail`)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  facet_wrap(region, scales = "free_y") +
  labs(
    title = "Northern Pintail Count vs Urban Area Per Capita by Region",
    x = "Urban Area Per Capita (km²/person)",
    y = "Northern Pintail Population"
  ) +
  theme_minimal(base_size = 11)

SwainsonByRegionPlot<-ggplot(SpeciesDataSwainson, aes(x = Urban_Area_Per_Capita, y = `Swainson's Hawk`)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  facet_wrap(region, scales = "free_y") +
  labs(
    title = "Swainson's Hawk Count vs Urban Area Per Capita by Region",
    x = "Urban Area Per Capita (km²/person)",
    y = "Swainson's Hawk Population"
  ) +
  theme_minimal(base_size = 11)

ggsave("Pintail_Urban_Area_by_Region.png", 
       plot = PintailByRegionPlot,   # or your specific plot object, like `pintail_plot`
       width = 12, height = 8, dpi = 300)

ggsave("Swainson_Urban_Area_by_Region.png", 
       plot = SwainsonByRegionPlot,   # or your specific plot object, like `pintail_plot`
       width = 12, height = 8, dpi = 300)




```


To begin analyzing trends in species occurrence in relation to urbanization, I first used Poisson generalized linear models (GLMs) with interaction terms between urban area per capita and region. This allowed for a straightforward examination of how the relationship between urbanization and bird counts might vary across geographic areas, assuming independent observations and fixed effects.

```{r}

# Poisson GLM with interaction for Northern Pintail
pintail_glm <- glm(
  `Northern Pintail` ~ Urban_Area_Per_Capita * region,
  data = SpeciesDataPintail,
  family = poisson()
)
summary(pintail_glm)

# Poisson GLM with interaction for Swainson's Hawk
swainson_glm <- glm(
  `Swainson's Hawk` ~ Urban_Area_Per_Capita * region,
  data = SpeciesDataSwainson,
  family = poisson()
)
summary(swainson_glm)



```
Northern Pintail
A Poisson generalized linear model (GLM) was used to examine the relationship between Northern Pintail counts and urbanization, as measured by urban area per capita, across four U.S. regions (Midwest [reference], Northeast, South, and West). The model revealed a significant negative association between urban area per capita and Northern Pintail abundance in the Midwest (β = -0.0231, p < 0.001), indicating decreasing counts with increasing urbanization in this reference region.

However, significant positive interaction terms for the Northeast (β = 0.0181, p < 0.001), South (β = 0.0282, p < 0.001), and West (β = 0.0202, p < 0.001) suggest that the negative effect of urbanization is attenuated or even reversed in these regions. Region main effects were all strongly negative relative to the Midwest, reflecting lower baseline Pintail counts in those regions. The model fit was acceptable (AIC = 6323.8; residual deviance = 5672.5 on 149 df), though overdispersion remains a potential concern to evaluate in future modeling.

Swainson’s Hawk
The Poisson GLM for Swainson’s Hawk also identified a significant negative main effect of urban area per capita in the Midwest (β = -0.0036, p < 0.001), indicating lower Hawk counts in more urbanized areas. Unlike the Pintail model, only the interaction with the South (β = 0.0076, p < 0.001) and West (β = 0.0016, p = 0.002) were statistically significant, suggesting regional variation in the strength of the urbanization effect.

The interaction term for the Northeast was not significant (β = 0.0070, p = 0.25), and the main effect for the West was also non-significant, suggesting Swainson’s Hawk abundance there may be relatively stable regardless of urbanization levels. The model showed moderate fit (AIC = 8009.2; residual deviance = 7133.3 on 185 df), and the results point to regionally heterogeneous responses to urban expansion.


However, GLMs are limited in that they assume independence across regions and do not account for the hierarchical structure of the data (i.e., repeated measures or nested variation within regions). To better capture regional heterogeneity and account for potential non-independence, I then implemented multilevel (mixed-effects) models. These models allowed both the intercept and slope of the urbanization effect to vary by region, offering a more flexible and realistic framework for understanding spatial variability in species sensitivity to urbanization.
Poisson Distribution of Region
```{r}

multilevelpintail <- lmer(`Northern Pintail` ~ Urban_Area_Per_Capita + (1 + Urban_Area_Per_Capita | region), data = SpeciesDataPintail)
summary(multilevelpintail)

multilevelswainsons <- lmer(`Swainson's Hawk` ~ Urban_Area_Per_Capita + (1 + Urban_Area_Per_Capita | region), data = SpeciesDataSwainson)
summary(multilevelswainsons)


```

```{r}
library(lme4)

# Random intercepts and slopes for each region
pintail_multilevel <- glmer(
  `Northern Pintail` ~ Urban_Area_Per_Capita + 
    (1 + Urban_Area_Per_Capita | region),
  data = SpeciesDataPintail,
  family = poisson()
)
summary(pintail_multilevel)

swainson_multilevel <- glmer(
  `Swainson's Hawk` ~ Urban_Area_Per_Capita + 
    (1 + Urban_Area_Per_Capita | region),
  data = SpeciesDataSwainson,
  family = poisson()
)
summary(swainson_multilevel)

```

Mixed Effects Modeling Results and Comparison
Northern Pintail
To further account for hierarchical structure in the data, a linear mixed-effects model (LMM) was fit using region as a grouping factor with both random intercepts and random slopes for urban area per capita. The model identified substantial variation across regions in both baseline Pintail abundance (σ² = 8,401) and the slope of urbanization effects (σ² = 0.0256), with a perfect negative correlation between the intercept and slope terms (r = –1.00). However, the model issued a singular fit warning, suggesting potential overparameterization given the small number of regions (n = 4) and high correlation of random effects.

Fixed effects revealed a negative association between urban area per capita and Pintail counts (β = –0.1683, t = –1.87), consistent in direction with the Poisson GLM but smaller in magnitude. This effect was not statistically significant at the conventional alpha level. Compared to the GLM, the multilevel model allows for regional heterogeneity in urbanization effects but does so at the cost of statistical power and interpretability given the small group sample size.

Swainson’s Hawk
For Swainson’s Hawk, the LMM encountered convergence warnings and signs of a near-unidentifiable model, including large eigenvalue ratios and gradient issues. Despite this, the model estimated moderate variation in intercepts (σ² = 3,559) and slopes (σ² = 0.0086), again with a high negative correlation (r = –0.97). The fixed effect of urban area per capita was positive but not statistically significant (β = 0.0284, t = 0.50), differing in direction from the negative main effect observed in the GLM.

These instability issues and the reversal of direction highlight the difficulty of fitting random slope models with limited higher-level units (n = 4 regions), especially when multicollinearity and small sample sizes are present.

Comparison with GLMs
Both species’ GLMs provided statistically significant estimates for the interaction between urbanization and region, capturing regionally variable responses to urban development. However, they assume fixed differences and do not estimate variance components, potentially overlooking deeper structure in the data.

The mixed models attempted to model this structure directly through random effects, offering more flexibility and accounting for potential shrinkage toward the group mean. Yet, the small number of regions caused singular fits and convergence problems, limiting the reliability of these models. These results highlight a trade-off between model flexibility and stability: while GLMs may be more robust with limited group sizes, multilevel models can offer richer interpretations when properly supported by the data.

```{r}

pintail_plot <- ggplot(SpeciesDataPintail, aes(x = Urban_Area_Per_Capita, y = `Northern Pintail`, color = region)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Northern Pintail Count vs Urban Area Per Capita (Grouped by Region)",
    x = "Urban Area Per Capita (km²/person)",
    y = "Northern Pintail Population"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    strip.text = element_text(color = "black")
  )

ggsave("Pintail_Plot_Clean_Region.png", plot = pintail_plot, width = 12, height = 8, dpi = 300)


Swainsons_plot <- ggplot(SpeciesDataSwainson, aes(x = Urban_Area_Per_Capita, y = `Swainson's Hawk`, color = region)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Swainson's Hawk Count vs Urban Area Per Capita (Grouped by Region)",
    x = "Urban Area Per Capita (km²/person)",
    y = "Swainson's Hawk Population"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    strip.text = element_text(color = "black")
  )

ggsave("Swainsons_Plot_Clean_Region.png", plot = Swainsons_plot, width = 12, height = 8, dpi = 300)




```




 

