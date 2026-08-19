# title: 3a_ReviewerConsensus.R
# purpose: import the blind reviewer + consensus CSV to count the number of disagreements
# created June 2026
# last edited: July 2026

# author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())

# 0) Load Libraries --------
library(dplyr)
library(tidyr)
library(janitor)

# 1) (Dis)Agreement on if the study includes a model --------
# Read data
df <- read.csv("MU_ReviewerConsensus_Extraction_Blind.csv")
names(df)
# Clean column names for convenience
df <- df %>%
  select("Covidence..", "Study.ID", "Reviewer.Name" , "Does.the.study.include.a.socio.environmental.model.") %>% 
  rename(
    cov_num = Covidence..,
    study_id = Study.ID,
    reviewer = Reviewer.Name,
    decision = Does.the.study.include.a.socio.environmental.model.
  )

# Pivot to wide format: one row per study
df_wide <- df %>%
  filter(reviewer %in% c("Reviewer1", "Reviewer2")) %>%
  select(cov_num, reviewer, decision) %>%
  pivot_wider(names_from = reviewer, values_from = decision)

# Count disagreements in if the study includes a model
n_disagree <- df_wide %>%
  filter(
    (Reviewer1 == "Yes" & Reviewer2 == "No") |
      (Reviewer1 == "No" & Reviewer2 == "Yes")
  )

# get number only   
n_disagree %>% nrow()

# print number of agreements
nrow(df_wide) - nrow(n_disagree)

# print percent agreement on if the study includes a model
1 - nrow(n_disagree)/nrow(df_wide)

# 2)  (Dis)Agreement on if the study quantifies uncertainty -------
# Re-read data
df <- read.csv("MU_ReviewerConsensus_Extraction_Blind.csv")
names(df)

# Clean column names for convenience
df <- df %>%
  select("Covidence..", "Study.ID", "Reviewer.Name" , "Does.the.study.include.a.socio.environmental.model.", "Does.the.study.quantify.model.uncertainty.") %>% 
  rename(
    cov_num = Covidence..,
    study_id = Study.ID,
    reviewer = Reviewer.Name,
    model_decision = Does.the.study.include.a.socio.environmental.model.,
    unc_decision = Does.the.study.quantify.model.uncertainty.
  )

# Only keep agrees 
df_cons_yes <- df %>%
  filter(reviewer == "Consensus" & model_decision == "Yes") #%>%

# Pivot to wide format: one row per study
df_wide <- df %>%
  #filter(reviewer %in% c("Reviewer1", "Reviewer2")) %>%
  select(cov_num, reviewer, model_decision, unc_decision)%>%
  pivot_wider(names_from = reviewer, values_from = c(model_decision, unc_decision)) %>% 
  filter(model_decision_Consensus == "Yes")

# Count disagreements
n_disagree <- df_wide %>%
  filter(
    (unc_decision_Reviewer1 == "Yes" & unc_decision_Reviewer2 == "No") |
      (unc_decision_Reviewer1 == "No" & unc_decision_Reviewer2 == "Yes")
  )

# get number only   
n_disagree %>% nrow()

# print number of agreements
nrow(df_wide) - nrow(n_disagree)

# print agreement percent on if the study quantifies uncertainty
1 - nrow(n_disagree)/nrow(df_wide)


# 3) Triple-Check Agreements -----

# NOTE: There were two studies, covidence ID #7 & # 194 which were marked as Other: yes by the consensus reviewer in the blind CSV. I changed these to Yes and the numbers agreed at 148. 

test_df_148 <- read.csv("MU_Consensus_All175_ManualEdit.csv", stringsAsFactors = FALSE) %>% clean_names()

# Filter for studies that include both socio-environmental models and quantify uncertainty
test_df148_filtered_data <- test_df_148 %>%
  dplyr::filter(
    does_the_study_include_a_socio_environmental_model == "Yes" &
    does_the_study_quantify_model_uncertainty == "Yes"
  )
numcov_df_148 <- test_df148_filtered_data$covidence
numcov_df_146 <- df_wide$cov_num

#str(numcov_df_146)

setdiff(numcov_df_148, numcov_df_146)
# SHOULD BE integer(0)! If so, then the correct CSVs were used here. 

