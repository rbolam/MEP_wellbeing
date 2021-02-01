## for thursday meeting

library(tidyverse)
library(here)

## read in data:
mepwell <- read_csv(here("outputs", "MEP_Wellbeing.csv"))

names <- unique(mepwell$Name) # extract names of contestants

# create dataframe for variables
df <- data.frame(Name = names, Postcode = NA, N_household = NA, N_children = NA, Comp_increase = NA)

write.csv(df, here("outputs", "Variables.csv"), row.names = FALSE)
