# Title: 0_extraction_assignment.R
# Author: Nick Manning
# Date Created: 6/19/2024
# Last Edited: June 2024
# Purpose: Randomly assign people to papers
# Notes:
## Requires csv from Covidence
## Requires re-assigned CSV
## Structure should be a Code folder which houses this code then the 

# # # # # # # # # 

# Load Libraries 
rm(list=ls())

library(dplyr)
library(tidyr) #pivot_longer

# read in CSV from Covidence
csv <- read.csv("../extraction_preassignment_20240619.csv")
df <- csv

# volunteer list as of 6/19/2024
vols <- c('Nick', 'Andres', 'Shen', 'Yunfan', 'Bo', 'Alex', 'Rebecca', 'Wei',
         'Peter', 'Jimeng', 'Adam', 'Nan', 'Jincheng', 'Hodo', 'Sydney', 'Michele', 'Hubert')


# create df from names
df_vols <- as.data.frame(vols)
names(df_vols) <- "Volunteer1"


# get the minimum number of papers per volunteer
n_rep <- floor(nrow(csv)/nrow(df_vols))

# repeat the names enough times for all the papers we need 
df_vols <- df_vols %>% slice(rep(1:n(), each = n_rep)) 

# assign the extra to Andres
extra <- as.data.frame(
  # repeat Andres the extra number of times 
  rep("Andres", times = nrow(csv) - nrow(df_vols))
  )
names(extra) <- "Volunteer1"

# bind this to the previous df
df_vols <- rbind(df_vols, extra)

# bind this column to the original df (from the CSV)
df_withvols <- cbind(df, df_vols)

# create other volunteer columns 
df_withvols$Volunteer2 <- df_withvols$Volunteer1
#df_withvols$Volunteer3 <- df_withvols$Volunteer1

# shuffle volunteer columns 
set.seed(125)
df_withvols$Volunteer1 <- sample(df_withvols$Volunteer1,length(df_withvols$Volunteer1))

df_withvols$Volunteer2 <- sample(df_withvols$Volunteer2,length(df_withvols$Volunteer2))

#df_withvols$Volunteer3 <- sample(df_withvols$Volunteer3,length(df_withvols$Volunteer3))

# make sure there are no duplicates
# NOTE: Lots of duplicates so if people find a duplicate they should just pick another paper
sum(df_withvols$Volunteer1 == df_withvols$Volunteer2)
#sum(df_withvols$Volunteer2 == df_withvols$Volunteer3)
#sum(df_withvols$Volunteer1 == df_withvols$Volunteer3)

# make df with duplicates 
duplicates <- df_withvols %>% 
  filter(df_withvols$Volunteer1 == df_withvols$Volunteer2) #|
      #df_withvols$Volunteer1 == df_withvols$Volunteer3 |      
      #df_withvols$Volunteer2 == df_withvols$Volunteer3

# save duplicates for viewing and cross-referencing
write.csv(duplicates, file = "../Assignments/_duplicates.csv", row.names = F)


# save wide 
df_withvols_wide <- df_withvols %>% 
  select(c(Title, Authors, Published.Year, Volunteer1, Volunteer2
           #, Volunteer3
           )) %>% 
  mutate(Notes = "-") 

write.csv(df_withvols_wide, file = "../Assignments/_assignments_full_wide.csv", row.names = F)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # MANUAL RE-ASSIGNING # # # # # # # # # # # # # # # # # # #  

# Check Notes for description
# Notes link: https://docs.google.com/document/d/1KxIxqAIIFSItmjyIK-qSUDYG0NfQFBcpmsCdlBjfNJY/edit

# Essentially, we manually examined all duplicates and replaced them sequentially
# from the list of reviewers (in chronological order of sign-up, as it is 
# here). Andres had a disproportionate amount of papers (9 papers * 2 review 
# slots) so we redistributed papers from the same order (papers sorted alphabetically
# this time). Andres ended up with 38 papers and everyone else has 32 now. 

# Re-assignment was done in an Excel sheet (.xlsx) format, then saved as a CSV to import to R below. 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# re-import wide with corrected assignments 
df_withvols_wide_reassign <- read.csv("../Assignments/_assignments_full_wide_reassign.csv")

# check if there are still duplicates 
sum(df_withvols_wide_reassign$Volunteer1 == df_withvols_wide_reassign$Volunteer2)
#sum(df_withvols_wide_reassign$Volunteer2 == df_withvols_wide_reassign$Volunteer3)
#sum(df_withvols_wide_reassign$Volunteer1 == df_withvols_wide_reassign$Volunteer3)

# pivot and remove all
df_withvols_long <- df_withvols_wide_reassign %>% 
  select(c(Title, Authors, Published.Year, Volunteer1, Volunteer2
           #, Volunteer3
           )) %>%
  pivot_longer(cols = c(Volunteer1, Volunteer2), names_to = "rm", values_to = "Volunteer") %>% 
  mutate(Notes = "-") %>% 
  select(-rm)

# Create full CSV of assignments
write.csv(df_withvols_long, file = "../Assignments/_assignments_full_long.csv", row.names = F)

# create function to filter to each reviewer and create an individual CSV per person
F_assign <- function(indiv){
  # filter our list to one reviewer
  df_indiv <- df_withvols_long %>% 
    filter(Volunteer == indiv) %>% 
    arrange(Title)
  
  # create CSV in Assignments folder
  write.csv(df_indiv, paste0("../Assignments/", indiv, ".csv"), row.names = F)
}

# run this function over the list of volunteers 
lapply(vols, FUN = F_assign)