library(readxl) 
library(dplyr) 
library(tidyverse)
library(ggthemes)
library(mapproj)
library(maps)

df <- read_xlsx("popvssoda.xlsx")
county_choice <- cbind(df[2:3])
county_choice$Choice <- factor(county_choice$Choice,levels=c("Pop","Soda","Coke","Other"),labels=c(1,2,3,4))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
state_choice <- county_choice %>% group_by(State) %>% summarise(Choice = getmode(Choice))
state_choice$Choice <- factor(state_choice$Choice,levels=c(1,2,3,4),labels=c("Pop","Soda","Coke","Other"))


us_states <- map_data("state")
colnames(us_states)[5] <-  c("State")
state_choice$State <- tolower(state_choice$State)
state_choice_map <- left_join(us_states,state_choice,by="State")

p0 <- ggplot(data = state_choice_map,
             mapping = aes(x = long, y = lat,
                           group = group, fill = Choice))
p1 <- p0 + geom_polygon(color = "black", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
p2 <- p1 +
  labs(title = "US Term Used: Saying Coke, Pop or Soda by State", fill = NULL) + 
  scale_fill_manual(values=c("#3c528b", "#22908b", "#5bc962"))
p2 + theme_map()










