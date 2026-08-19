# Title: 2_reassign_unfinished.R
# Author: Nick Manning
# Date Created: 1/14/2025
# Last Edited: Jan 2024
# Purpose: Import unfinished reviews and reassign them to the revised list of reviewers  
# Notes:

# # # # # # # # # # # # # # # # # # # # 

library(dplyr)
library(readr)
library(tidyr)

# 1) Import and Merge Unfinished CSV's -------
getwd()

# Set folder path
path <- "../Assignments/Uncompleted_20241002"

# Get list of files in folder
file_list <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# Import and merge all CSV files into one dataframe
merged_df <- file_list %>%
  lapply(function(file) read_csv(file, show_col_types = FALSE)) %>%
  bind_rows()

csv <- merged_df 

# 2) Repeat process from script 0_extraction_assignment.R ------

# volunteer list as of 1/14/2025
vols <- c('Nick', 'Andres', 'Shen', 'Yunfan', 'Alex', 'Rebecca', 'Wei',
          'Peter', 'Jimeng', 'Nan', 'Jincheng', 'Hodo', 'Sydney', 'Michele')


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
df_withvols <- cbind(csv, df_vols)

# create other volunteer columns 
df_withvols$Volunteer2 <- df_withvols$Volunteer1
#df_withvols$Volunteer3 <- df_withvols$Volunteer1

# shuffle volunteer columns 
set.seed(121)
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
write.csv(duplicates, file = "../Assignments/Round2_Assignments/_reassign_duplicates.csv", row.names = F)


# save wide 
df_withvols_wide <- df_withvols %>% 
  select(c(Title, Authors, Published.Year, Volunteer1, Volunteer2
           #, Volunteer3
  )) %>% 
  mutate(Notes = "-") 

write.csv(df_withvols_wide, file = "../Assignments/Round2_Assignments/_reassignments_full_wide.csv", row.names = F)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # # MANUAL RE-ASSIGNING # # # # # # # # # # # # # # # # # # #  

# Check Notes for description
# Notes link: https://docs.google.com/document/d/1KxIxqAIIFSItmjyIK-qSUDYG0NfQFBcpmsCdlBjfNJY/edit

# For this second round of manual re-assigning, we removed two instances of papers that were
# erroneously assigned to 4 reviewers, as the original reviewers had *both* left the project, so 
# the papers were re-assigned twice. These were 
# "Validating human decision making in an agent-based land-use model" and 
# "Integrating Problem Structuring Methods And Concept-Knowledge Theory For An Advanced Policy Design: Lessons From A Case Study In Cyprus"

# Re-assignment was done and saved as a CSV (manually) then imported to R below. 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# re-import wide with corrected assignments 
df_withvols_wide_reassign <- read.csv("../Assignments/Round2_Assignments/_reassignments_full_wide_reassign.csv")

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
write.csv(df_withvols_long, file = "../Assignments/Round2_Assignments/_reassignments_full_long.csv", row.names = F)

# create function to filter to each reviewer and create an individual CSV per person
F_assign <- function(indiv){
  # filter our list to one reviewer
  df_indiv <- df_withvols_long %>% 
    filter(Volunteer == indiv) %>% 
    arrange(Title)
  
  # create CSV in Assignments folder
  write.csv(df_indiv, paste0("../Assignments/Round2_Assignments/", indiv, ".csv"), row.names = F)
}

# run this function over the list of volunteers 
lapply(vols, FUN = F_assign)


# 3) Create master list by merging old list with re-assigned list

# load old & reassigned
csv_main_old <- read.csv("../Assignments/_assignments_full_wide_reassign.csv")
csv_main_reassign <- read.csv("../Assignments/Round2_Assignments/_reassignments_full_wide_reassign.csv")
csv_main_new <- csv_main_old %>% 
  rows_update(csv_main_reassign)  

# save new master copy
write.csv(csv_main_new, "../Assignments/Round2_Assignments/_reassignments_all_main.csv", row.names = F)
