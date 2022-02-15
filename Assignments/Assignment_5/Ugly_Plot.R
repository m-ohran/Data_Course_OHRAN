library(tidyverse)
library(ggcats)
data("storms")

storms %>% 
  ggplot(aes(x = name,y=pressure,fill="#2b1818")) +
  #geom_point() +
  geom_cat(cat="grumpy",size=4) +
  coord_polar() +
  geom_boxplot() +
  labs(x="peso", y="nombre", fill="inundacion", title = "CAOS") +
  theme(axis.text.x = element_text(size = 40, angle = 180, color = "#172113"), 
        axis.text.y = element_text(size = 35, angle = 90, color = "#2b1818"),
        panel.background = element_rect(fill = "#2b1818",color = "#2b1818"), 
        rect = element_rect(fill = "#172113"),
        axis.title.x = element_text(size = 30, angle = 180, color = "#2b1818"),
        axis.title.y = element_text(size = 35, angle = 180,color="#2b1818"),
        plot.title = element_text(size = 30, angle = 247))
ggsave("mohran_ugly_plot.png",width = 15,height = 16,dpi = 40)  
