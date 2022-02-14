# I. Read the cleaned_covid_data.csv file into an R data frame.
covid_data <- read.csv("cleaned_covid_data.csv")

# II. Subset the data set to just show states that begin with "A" 
# Save this as an object called A_states.
  # load tidyverse
library(tidyverse) 
  # use grepl to find states that begin with "A"
A_states <- covid_data[grepl("^A", covid_data$Province_State),]

# III. Create a plot of A_states showing Deaths over time.
A_states %>% 
  ggplot(aes(x=Last_Update,
             y=Deaths)) +
  # create scatterplot
  geom_point(alpha=.3,
             size=.5,
             colour="blue") +
  # Add loess curves WITHOUT standard error shading
  # Why does it look like geom_smooth isn't doing anything?
  # Is it related to the enormous amount of points on this thing?
  geom_smooth(color="black",
              se=FALSE,
              size=3) +
  # Separate facet for each state
  # Keep scales "free"
  facet_wrap(~Province_State,
             scales = "free") +
  # Adjust labels
  labs(x = "Report date",
       y = "Number of covid deaths",
       title = "Covid deaths over time") +
  theme(axis.text.x = element_text(size=5, 
                                   angle = 90,
                                   hjust = 1))

# IV. Find the "peak" of Case_Fatality_Ratio for each state (full dataset) 
# Save this as a new data frame object called state_max_fatality_rate.
  # 2 columns: Province_State and Maximum_Fatality_Ratio
  # arrange data frame in descending order by Maximum_Fatality_Ratio
  # might take a few steps
  # BEWARE OF MISSING VALUES
state_max_fatality_rate <- covid_data %>% 
  group_by(Province_State) %>% 
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio,na.rm=TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))

# V. Use state_max_fatality_rate to create another plot
  # X = Province_State
  # Y = Maximum_Fatality_Ratio
  # bar plot (geom_col())
  # x-axis arranged in descending order (make it a factor to accomplish this)
  # x-axis labels turned 90 degrees
  # should be able to see that different states had different fatality ratios
state_max_fatality_rate %>% 
  mutate(State = factor(Province_State,levels = state_max_fatality_rate$Province_State)) %>% 
  ggplot(aes(x=State,
             y=Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(size=8, 
                                   angle = 90,
                                   hjust = 1)) +
  labs(x="State/Province",
       y="Maximum Fatality Ratio")

# VI. Using the FULL data set, plot cumulative deaths for the entire US over time
  # group_by() and summarize()
covid_data %>% 
  group_by(Last_Update) %>% 
  summarise(Total = sum(Deaths)) %>% 
  ggplot(aes(x=Last_Update,y=Total)) +
  geom_point(alpha=.15,
             size=2) +
  geom_smooth(color="black",
              se=FALSE,
              size=3) +
  labs(x = "Report date",
       y = "Number of covid deaths",
       title = "Covid deaths in US over time") +
  theme(axis.text.x = element_text(size=5, 
                                   angle = 90,
                                   hjust = 1))
