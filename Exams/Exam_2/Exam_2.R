# libraries
library(tidyverse)
library(modelr)
library(easystats)

# 1. Read in unicef data
df <- read_csv("unicef-u5mr.csv")

# 2. Tidy unicef data
df_long <- df %>%
  pivot_longer(starts_with("U5MR."),
               names_to = "Year",
               values_to = "U5MR", names_prefix ="U5MR.") %>% 
  mutate(Year=as.numeric(Year))
  

# 3. Plot each country’s U5MR over time
# create a line plot for each country (NOT SMOOTH TRENDLINE)
# facet by continent
df_long %>% 
  ggplot(aes(x=Year,y=U5MR)) +
  geom_path() +
  facet_wrap(~Continent)

# 4. save as OHRAN_Plot_1.png
#ggsave("OHRAN_Plot_1.png", width = 10, height = 7, dpi = 300)

# 5. Create another line plot (NOT SMOOTH TRENDLINE)
# shows the mean U5MR for all the countries within a given continent at each year
# colored by continent
df_long %>% 
  group_by(Continent, Year) %>% 
  summarise(Mean_U5MR = mean(U5MR, na.rm=TRUE)) %>% 
  ggplot(aes(x=Year,y=Mean_U5MR, color=Continent)) +
  geom_line(size=2)

# 6. save as OHRAN_Plot_2.png
#ggsave("OHRAN_Plot_2.png", width = 10, height = 7, dpi = 300)

# 7. Create 3 models of U5MR
# mod1 only accounts for Year
mod1 <- df_long %>% 
  glm(data = ., formula = U5MR ~ Year)
summary(mod1)
# mod2 accounts for Year and Continent
mod2 <- df_long %>% 
  glm(data = ., formula = U5MR ~ Year + Continent)
summary(mod2)
# mod3 accounts for Year, Continent, and interaction
mod3 <- df_long %>% 
  glm(data = ., formula = U5MR ~ Year * Continent)
summary(mod3)

# 8. Compare the three models
# code should do comparing
# Include comment explaining which model I think is best
compare_models(mod1,mod2,mod3)
compare_performance(mod1,mod2,mod3)
# I think mod3 is the best. It has the lowest AIC, and the highest R2 
# also looking at the summaries, Year, Continent, and their interactions are significant

# 9. Plot 3 models' predictions
# facet by model
# color by Continent
# x = Year, y = pred
df_long %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=Year,y=pred, color = Continent)) +
  geom_line(size=1) +
  facet_wrap(~model) +
  labs(y="Predicted U5MR")

# 10. Use preferred model to predict U5MR for Ecuador in 2020
# Real life value = 13 under 5 deaths per 1000 live births
ecuador_pred <- data.frame(Continent="Americas",Year = 2020,CountryName="Ecuador") %>% 
  gather_predictions(mod3)
# How far off was my model prediction?
diff <- ecuador_pred$pred - 13
# Create a model that improves on this
mod4 <- df_long %>% 
  filter(Year == (c(2005:2015))) %>%  
  glm(data = ., formula = U5MR ~ Year * Continent)
summary(mod4)

new_pred <- data.frame(Continent="Americas",Year = 2020,CountryName="Ecuador", Region = "South America") %>% 
  gather_predictions(mod4)
new_diff <- new_pred$pred - 13
