## ---------------------- Data analysis and figures ----------------------------####


library(tidyverse)
library(here)
#library(googlesheets4)
#library(devtools)
#devtools::install_github("jcbain/cuttlefish")

## get colour pallette for plotting with
#colours_vector <- cuttlefish::create_palette("MEP_Logo_transparent.png", n = 20)
#team_colours <- cuttlefish::find_segmented(colours_vector, 6)
#team_colours <- cuttlefish::find_prominent(colours_vector, 6)
library(scales)
#show_col(colours_vector)
show_col(team_colours)
team_colours <- c("3" = "#768D3B", "Royal purple" = "#886F50", "Goal Diggers" = "#6AB0DB", "TEAM: - If you're\nhappy and you know\nit, wash your hands" = "#B9E39D", "Well in Hell" = "#E5F6F9", Wellabies = "#E9E1D6")

## read in data:
mepwell <- read_csv(here("outputs", "MEP_Wellbeing.csv"))

###----------------- Tidy the data -----------------####

mepwell[mepwell== "-" ] <- NA

mepwell <- mepwell %>%
  select(1:17) %>% # only keep the days we've got data til - 8th Jan
  mutate_at(vars(starts_with("Jan")), funs(as.numeric)) %>%
  filter_at(vars(starts_with("Jan")), any_vars(!is.na(.)))

mepwell$Group[mepwell$Group == "TEAM: - If you're happy and you know it, wash your hands"] <- "TEAM: - If you're\nhappy and you know\nit, wash your hands"

# create activity categories
mepwell$Category[mepwell$Activity == "Ab workout"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Arts and Crafts"] <- "Arts, crafts and cooking"
mepwell$Category[mepwell$Activity == "Arts/Crafts"] <- "Arts, crafts and cooking"
mepwell$Category[mepwell$Activity == "Baking"] <- "Arts, crafts and cooking"
mepwell$Category[mepwell$Activity == "Ballet"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Boardgames"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Body pump"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Capoeira"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Card Games/Board Games"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Choir"] <- "Being musical"
mepwell$Category[mepwell$Activity == "Cross stitch"] <- "Arts, crafts and cooking"
mepwell$Category[mepwell$Activity == "Cross trainer"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Cycle"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Cycling"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Dance class"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Duolingo"] <- "Learning/practicing a skill"
mepwell$Category[mepwell$Activity == "Entertaining Mischief (cat)"] <- "Time with pets"
mepwell$Category[mepwell$Activity == "Gaming"] <- "Playing video games"
mepwell$Category[mepwell$Activity == "Hanging out with my mice"] <- "Time with pets"
mepwell$Category[mepwell$Activity == "HIIT"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Home workout"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Jigsaw"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Juggling"] <- "Learning/practicing a skill"
mepwell$Category[mepwell$Activity == "Kickboxing"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Knitting"] <- "Arts, crafts and cooking"
mepwell$Category[mepwell$Activity == "Language Puzzles"] <- "Learning/practicing a skill"
mepwell$Category[mepwell$Activity == "Listen to audiobook"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "Long bubble bath"] <- "Self care"
mepwell$Category[mepwell$Activity == "Meditating"] <- "Meditation"
mepwell$Category[mepwell$Activity == "Oddities: you tube dance (sigh) - also discovered the joys of wireless headphones and trying out some hiphop moves with no one watching in the dark :)"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Painting"] <- "Arts, crafts and cooking"
mepwell$Category[mepwell$Activity == "Physiotherapy/pilates"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Playing fetch with the Kipster"] <- "Time with pets"
mepwell$Category[mepwell$Activity == "Playing in the snow"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Playing with ferrets"] <- "Time with pets"
mepwell$Category[mepwell$Activity == "reading (Hornblower)"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "Reading for fun"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "Reading"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "Reading (Earthsea)"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "Running"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Rollerskating"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Sea swim"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Singing and Dancing like a lunatic"] <- "Being musical"
mepwell$Category[mepwell$Activity == "Snap card game"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Stretch Class/ Body Conditioning"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Stretching/yoga"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Stuff with Izzy: reading, activity book"] <- "Time with kids"
mepwell$Category[mepwell$Activity == "Surfing"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Table tennis"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Tabletop Games"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Tap"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Video games"] <- "Playing video games"
mepwell$Category[mepwell$Activity == "Violin"] <- "Being musical"
mepwell$Category[mepwell$Activity == "Walk"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "walking"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Walking"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Walking my dog"] <- "Time with pets"
mepwell$Category[mepwell$Activity == "Walking/running w/podcast"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Weaving"] <- "Arts, crafts and cooking"
mepwell$Category[mepwell$Activity == "Workout"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Yoga"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Yoga/ workout"] <- "Active indoors"
mepwell$Category[mepwell$Activity == "Built Snowman"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Games"] <- "Playing games"

unique(mepwell$Activity[is.na(mepwell$Category)]) # check out the new activities - 20 new ones!

mepwell$Category[mepwell$Activity == "Zoom quiz"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Sudoko"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Football"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Walking / Hiking"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Jogging"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Stuff with kids: reading books, playing, homework activities"]  <- "Time with kids"
mepwell$Category[mepwell$Activity == "Meditation"] <- "Meditation"
mepwell$Category[mepwell$Activity == "Walking - always with Ninjapie"] <- "Active outdoors"
mepwell$Category[mepwell$Activity == "Reading/ audiobook"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "Board games & card games"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Podcast"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "Reading to my daughter"]  <- "Time with kids"
mepwell$Category[mepwell$Activity == "Art with my children"]  <- "Time with kids"
mepwell$Category[mepwell$Activity == "Playing cards with children"]  <- "Time with kids"
mepwell$Category[mepwell$Activity == "audiobook"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "podcast"] <- "Reading/Listening to books"
mepwell$Category[mepwell$Activity == "Jigsaw/other puzzles"] <- "Playing games"
mepwell$Category[mepwell$Activity == "Guitar/Berimbau"] <- "Being musical"
mepwell$Category[mepwell$Activity == "Kneading bread"] <- "Arts, crafts and cooking"


###----------------- Setting up plot theme  -----------------####

plottheme <- theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "#4D4D4D", colour = NA),
  panel.background = element_rect(fill = "#878787", colour = NA),
  legend.background = element_rect(fill = "#4D4D4D", colour = NA),
  axis.text = element_text(colour = "linen"),
  axis.title = element_text(colour = "linen")
)


###----------------- Plot of time spent per activity  -----------------####

mepwell <- mepwell %>%
  mutate(tot_time = rowSums(.[4:17], na.rm = TRUE)) %>%
  group_by(Category) %>%
  mutate(Cat_time = sum(tot_time))

ggplot(mepwell, aes(x = tot_time, y = reorder(Category, Cat_time), fill = Group)) + 
  geom_col() + 
  scale_fill_manual(values = team_colours, labels = function(x) str_wrap(x, width = 7)) + 
  labs(x = "Time per activity in minutes", y = "") +
  plottheme +
  theme(legend.position = "none")
ggsave("outputs/minutespactivity_week2.jpg")

###----------------- Plot of time spent per person per group -----------------####


### Calculate people per group:
mepwell %>% 
  ungroup() %>%
  select(1:3, 19) %>% 
  filter(tot_time != 0) %>% 
  select(Group, Name) %>% 
  unique() %>% 
  count(Group) ->
  ppgroup

## Calculate how much time each group spent, divided by group members:
mepwell %>% 
  ungroup() %>%
  select(1:3, 19) %>% 
  group_by(Group) %>% 
  summarise(sum = sum(tot_time)) %>% 
  left_join(ppgroup) %>% 
  mutate(sumpp = sum / n) ->
  progresssummary

## Add line breaks to very long group name:

ggplot(progresssummary, aes(x = sumpp, y = fct_reorder(Group, sumpp), fill = Group)) +
  geom_col() +
  scale_fill_manual(values = team_colours) +
  labs(x = "Time per person in minutes", y = "") +
  plottheme +
  theme(legend.position = "none")
ggsave("outputs/minutespppgroup_week2.jpg")
