## ---------------------- Data analysis and figures ----------------------------####


library(tidyverse)
library(here)
#library(googlesheets4)
library(devtools)
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
  mutate(Jan_04 = as.numeric(Jan_04)) %>%
  mutate(Jan_05 = as.numeric(Jan_05)) %>%
  mutate(Jan_06 = as.numeric(Jan_06)) %>%
  mutate(Jan_07 = as.numeric(Jan_07)) %>%
  mutate(Jan_08 = as.numeric(Jan_08)) %>%
  mutate(Jan_09 = as.numeric(Jan_09)) %>%
  mutate(Jan_10 = as.numeric(Jan_10))

table(mepwell$Activity)

# there is almost certainly a neater way to do this but I don't know it and this is how I change variables!
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
