# libraries
library(tidyverse)
library(janitor)
library(gganimate)

# read csv file
dat <- read_csv("../../Data/BioLog_Plate_Data.csv") 

# get clean names
dat <- janitor::clean_names(dat)

# tidy the data
dat_long <- dat %>% 
  pivot_longer(starts_with("hr_"),
               names_to = "time",
               values_to = "absorbance",names_prefix = "hr_") %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(sample_type = case_when(sample_id == "Clear_Creek" ~ "Water",
                                sample_id == "Waste_Water" ~ "Water",
                                TRUE ~ "Soil"))
# plot absorbance over time, filtered by 0.1 dilution and faceted by substrate
dat_long %>% 
  filter(dilution == 0.1) %>% 
  ggplot(aes(x=time,y=absorbance,color=sample_type)) +
  geom_smooth(se=FALSE) +
  facet_wrap(~substrate) +
  labs(x="Time",y="Absorbance",color="Sample Type", title = "Just plotting dilution 1.0") +
  theme_minimal()

#ggsave("Assignment_6_Plot_1.pdf",width = 15,height = 10,dpi = 300)

# filter by Itaconic Acid, calculate the mean absorbance of each replicate for each sample,
# plot mean absorbance over time, faceted by dilution
dat_long%>% 
  filter(substrate == "Itaconic Acid") %>%
  group_by(sample_id,rep,time,dilution) %>% 
  summarise(mean_absorbance = mean(absorbance)) %>% 
  ggplot(aes(x=time,y=mean_absorbance,color=sample_id)) +
  geom_smooth(se=FALSE, span=0.45) +
  facet_wrap(~dilution) +
  theme_minimal() +
  labs(x="Time",y="Mean absorbance",color="Sample ID") #+
  #gganimate::transition_reveal(rev(seq_along(time)))

