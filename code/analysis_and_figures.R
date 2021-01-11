## ---------------------- Data analysis and figures ----------------------------####


library(tidyverse)
library(here)
#devtools::install_github("jcbain/cuttlefish")

## get colour pallette for plotting with
colours_vector <- cuttlefish::create_palette("MEP_Logo_transparent.png", n = 32)

## read in data:
mepwell <- read_csv(here("outputs", "MEP_Wellbeing.csv"))

## explore and tidy the data

mepwell[mepwell== "-" ] <- NA

mepwell <- mepwell %>%
  select(1:10) %>% # only keep the days we've got data til - 8th Jan
  rename("Jan_04" = "04-Jan") %>%
  rename("Jan_05" = "05-Jan") %>%
  rename("Jan_06" = "06-Jan") %>%
  rename("Jan_07" = "07-Jan") %>%
  rename("Jan_08" = "08-Jan") %>%
  rename("Jan_09" = "09-Jan") %>% 
  rename("Jan_10" = "10-Jan") %>%
  mutate_at(vars(starts_with("Jan")), as.numeric) ## change all vars that start with Jan to numeric

table(mepwell$Activity)

# there is almost certainly a neater way to do this but I don't know it and this is how I change variables!

## i can never do this sort of thing in a neater way either!
mepwell$Activity[mepwell$Activity == "Cycle"] <- "Cycling"
mepwell$Activity[mepwell$Activity == "Ballet"] <- "Dancing"
mepwell$Activity[mepwell$Activity == "Dance class"] <- "Dancing"
mepwell$Activity[mepwell$Activity == "Tap"] <- "Dancing"
mepwell$Activity[mepwell$Activity == "Oddities: you tube dance (sigh) - also discovered the joys of wireless headphones and trying out some hiphop moves with no one watching in the dark :)"] <- "Dancing"
mepwell$Activity[mepwell$Activity == "Entertaining Mischief (cat)"] <- "Pet time"
mepwell$Activity[mepwell$Activity == "Playing with ferrets"] <- "Pet time"
mepwell$Activity[mepwell$Activity == "Walking my dog"] <- "Pet time"
mepwell$Activity[mepwell$Activity == "Playing fetch with the Kipster"] <- "Pet time"
mepwell$Activity[mepwell$Activity == "Hanging out with my mice"] <- "Pet time"
mepwell$Activity[mepwell$Activity == "reading (Hornblower)"] <- "Reading"
mepwell$Activity[mepwell$Activity == "Reading for fun"] <- "Reading"
mepwell$Activity[mepwell$Activity == "Walk"] <- "Walking"
mepwell$Activity[mepwell$Activity == "walking"] <- "Walking"
mepwell$Activity[mepwell$Activity == "Home workout"] <- "Workout"

mepwell$tot_time <- rowSums(mepwell[4:10], na.rm = TRUE)

## Figure ideas - could just do a couple so that there are still some things to plot as a group in thursday meetings!
## Most active team, most popular activity (lollipop plot?), peak/slump activity days (get mean/median and do deviation from plot), tracking folks activity levels throughout the week

ggplot(mepwell) + geom_col(aes(x = Activity, y = tot_time, fill = Group)) + theme(axis.text.x = element_text(angle = 90))

median(mepwell$tot_time)



###----------------- Plot of time spent per person per group -----------------####


### Calculate people per group:
mepwell %>% 
  select(1:3, 11) %>% 
  filter(tot_time != 0) %>% 
  select(Group, Name) %>% 
  unique() %>% 
  count(Group) ->
  ppgroup

## Calculate how much time each group spent, divided by group members:
mepwell %>% 
  select(1:3, 11) %>% 
  group_by(Group) %>% 
  summarise(sum = sum(tot_time)) %>% 
  left_join(ppgroup) %>% 
  mutate(sumpp = sum / n) ->
  progresssummary

## Add line breaks to very long group name:
progresssummary[4,1] <- "TEAM: - If you're\nhappy and you know\nit, wash your hands"

ggplot(progresssummary, aes(x = sumpp, y = fct_reorder(Group, sumpp), fill = Group)) +
  geom_col() +
  scale_fill_manual(values = colours_vector[c(1, 6, 8, 10, 14, 28)]) +
  labs(x = "Time per person in minutes", y = "") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("outputs/minutespppgroup.jpg")


