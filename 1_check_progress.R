# Title: 1_check_progress.R
# Author: Nick Manning
# Date Created: 9/26/2024
# Last Edited: Jan 2025
# Purpose: Bring in extracted papers to see which ones are left
# Notes:
## Requires csv from Covidence. Go to Export > Extraction > Data Extraction + Consensus > Prepare File

# # # # # # # # # 

library(dplyr)

# read in progress CSV
csv <- read.csv("../Progress/progress_20240926.csv")
str(csv)
# remove all duplicated titles - that means these have been completed by two reviewers
# I HAVE NO IDEA WHY THIS ISN'T WORKING # 
# DID THIS FILTERING BY HAND INSTEAD #
# incomplete <-  csv %>% 
#   select(Covidence..) %>%
#   filter(!duplicated(Covidence..))
# 
# incomplete2 <- distinct(csv, Title, .keep_all = TRUE)


# read in assignments 
csv <- read.csv("../Assignments/_consensus_20241002.csv")
names(csv)
length(unique(csv$Covidence..))

# Assign Nick or Andres as consensus reviewer
## Progress as of 10/20/2024 ##
cons <-csv %>% 
  select(c(Covidence.., Study.ID, Title)) %>% 
  distinct()

set.seed(121)
cons$Cons_Reviewer <- sample(rep(c("Nick", "Andres"), nrow(cons) / 2))

# export as CSV
write.csv(cons, "../Assignments/_consensus_20241002_NMorAV_121.csv")