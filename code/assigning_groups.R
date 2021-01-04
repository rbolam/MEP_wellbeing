## ----------------------------------Creating groups ----------------------------####
library(tidyverse)


## MEP members that have joined since lockdown:
sinceld <- c("Abdulrasheed", "Bekah", "Eleanor", "Hazel", "Hollie", "Natasha", "Nicholas",
             "Will O")


## MEP members that have joined before lockdown:
beforeld <- c("Andy", "Becca", "Ben", "Chess", "Ellen", "Emily", "Jess", "Khunsa", 
              "Laura", "Louise", "Marion", "Mark", "Nicola", "Rike", "Roy", "Simone", 
              "Steve", "Thea", "Will R", "Zarah")


## Creating groups:
set.seed(2021)

group <- rep(1:5, 2)
group2 <- rep(5:1, 4)


## Df of new group members, so they don't all end up in one:
groupss <- data.frame(cbind(group, name = sample(sinceld)))
groupss <- groupss[1:8,]


## Df of other group members:
groupsb <- data.frame(cbind(group = group2, name = sample(beforeld)))


## Bind dfs and show groups:
groups <- groupss %>% 
  bind_rows(groupsb) %>% 
  arrange(group)


