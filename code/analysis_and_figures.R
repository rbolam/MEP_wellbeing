## ---------------------- Data analysis and figures ----------------------------####


library(tidyverse)
library(googlesheets4)


## Read in data:
mepwell <- read_sheet("https://docs.google.com/spreadsheets/d/18TyJ9tJnrrwx9aEAyB2bmtnfWnFVXgjlRTCmZ1qoA-w/edit?usp=sharing")

## Figure ideas - could just do a couple so that there are still some things to plot as a group in thursday meetings!
## Most active team, most popular activity (lollipop plot?), peak/slump activity days (get mean/median and do deviation from plot), tracking folks activity levels throughout the week