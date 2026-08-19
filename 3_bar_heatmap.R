# title: 3_bar_heatmap.R
# purpose: import the results file (post-consensus) and create a barplot and heatmap
# created on: March 2025
# last edited: April 2025

# author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())

# 0) Load Libraries --------
#library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(janitor)
library(RColorBrewer)


# 1) Load data ----
#data <- read.csv("MU_Consensus_140_woNick_ManualEdit.csv", stringsAsFactors = FALSE) %>% clean_names()
data <- read.csv("MU_Consensus_All175_ManualEdit.csv", stringsAsFactors = FALSE) %>% clean_names()
#data <- read.csv("MU_Consensus_All175_ManualEdit_NoneOther.csv", stringsAsFactors = FALSE) %>% clean_names()


data_filtered <- data %>%
  filter(
    `does_the_study_include_a_socio_environmental_model` == "Yes" &
      `does_the_study_quantify_model_uncertainty` == "Yes"
  )

## 1.1) Get counts of model types---------

model_counts <- data_filtered %>% # include only studies that include a model and quantify their uncertainty
  separate_rows(type_of_model, sep = ";") %>%          # split multiple models
  mutate(type_of_model = str_trim(type_of_model)) %>%  # clean whitespace
  mutate(type_of_model = if_else(
    str_detect(type_of_model, "^Other:"),
    "Other",
    type_of_model
  )) %>%
  count(type_of_model, sort = TRUE)

model_counts



## 1.2) Get counts of uncertainty quantification approaches

uncertainty_counts <- data_filtered %>%
  
  # Split rows where multiple uncertainty methods are listed in one cell
  # (e.g., "Sensitivity analysis; Validation ...") into separate rows
  separate_rows(how_does_this_model_quantify_uncertainty, sep = ";") %>%
  
  # Remove leading/trailing whitespace from each entry
  mutate(how_does_this_model_quantify_uncertainty = 
           str_trim(how_does_this_model_quantify_uncertainty)) %>%
  
  # Recode anything that starts with "Other:" to just "Other"
  # (this standardizes all "Other: ..." variations into one category)
  mutate(how_does_this_model_quantify_uncertainty = if_else(
    str_detect(how_does_this_model_quantify_uncertainty, "^Other:"),
    "Other",
    how_does_this_model_quantify_uncertainty
  )) %>%
  
  # Remove empty strings that result from missing or blank entries
  filter(how_does_this_model_quantify_uncertainty != "") %>%
  
  # Count how many times each uncertainty method appears
  count(how_does_this_model_quantify_uncertainty, sort = TRUE)

uncertainty_counts



## 1.3) Get counts of metauncertainty principles ---------- 
# Split rows by the different types of metauncertainty principles
mu_princ <- data_filtered %>%
  separate_rows(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories`, sep = "; ") 

# Create a table of counts for the 'id' column
princ_count <- table(mu_princ$covidence)

# Filter to get only duplicated values (count > 1)
princ_duplicated_counts <- princ_count[princ_count > 1]

# how many studies used more than 1 metauncertainty principle?
length(princ_duplicated_counts)

# Count the number of studies per principle
mu_counts <- mu_princ %>% 
  count(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories`, name = "count") %>%
  drop_na() %>% 
  rename(type = does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories) #%>%
# mutate(type = sub("^$", "None", type)) #%>% 
# filter(type != c("None", "Other"))
#filter(type != "")

## 1.4) Get how many studies addressed more than 1 category of meta-uncertainty ----

multi_category_studies <- data_filtered %>%
  
  # Split multiple categories into separate rows
  separate_rows(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories`, sep = "; ") %>%
  
  # Remove missing or blank entries
  filter(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories` != "",
         !is.na(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories`)) %>%
  
  # Count how many categories each study has
  group_by(covidence) %>%
  summarise(n_categories = n_distinct(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories`)) %>%
  
  # Keep only studies with more than 1 category
  filter(n_categories > 1) %>%
  
  # Count how many such studies exist
  summarise(n_studies = n())

multi_category_studies

## 1.5) Test to see if any assesssed all 6 categories

target_categories <- c(
  "Knowledge of the system", 
  "Model specification", 
  "Empirical data limitations",
  "Computing limitations",
  "Spatial and temporal scale issues",
  "Spatial dependence & heterogeneity issues"
)

all_six <- data_filtered %>%
  separate_rows(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories`, sep = "; ") %>%
  filter(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories` %in% target_categories) %>%
  group_by(covidence) %>%
  summarise(n_categories = n_distinct(
    `does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories`
  )) %>%
  filter(n_categories == length(target_categories))

nrow(all_six)
all_six %>% pull(covidence)

# 2) Plot Barplot ------

# manually set order
mu_counts$type = factor(mu_counts$type, 
                        levels = c(
                          "Computing limitations",
                          "Empirical data limitations",
                          "Knowledge of the system", 
                          "Model specification", 
                          "Spatial dependence & heterogeneity issues",
                          "Spatial and temporal scale issues",
                          "Other",
                          "None"
                        ), 
                        ordered = TRUE)

# reverse order for plotting 
mu_counts$type <- factor(mu_counts$type, levels=rev(levels(mu_counts$type)))

# plot

# category = gsub("Model specification", "MS", category),
# category = gsub("Knowledge of the system", "KS", category),
# category = gsub("Spatial and temporal scale issues", "STS", category),
# category = gsub("Empirical data limitations", "ED", category),
# category = gsub("Spatial dependence & heterogeneity issues", "SDH", category),
# category = gsub("Computing limitations", "CL", category)) %>% 
#   mutate(category = sub("^$", "None", category))

ggplot(data = mu_counts, aes(x = type, y = count, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "Model specification" = "#00BFC4",
    "Knowledge of the system" = "#00B035",
    "Empirical data limitations" = "#B79F00",
    "Computing limitations" = "#F8766D",
    "Spatial dependence & heterogeneity issues" = "#619CFF",
    "Spatial and temporal scale issues" = "#F564E3"
  )) +
  scale_x_discrete(labels = c(
    "Model specification" = "Model specification \n(MS)",
    "Knowledge of the system" = "Knowledge of the system \n(KS)",
    "Empirical data limitations" = "Empirical data limitations \n(EDL)",
    "Computing limitations" = "Computing limitations \n(CL)",
    "Spatial dependence & heterogeneity issues" = "Spatial dependence &\nheterogeneity issues \n(SDH)",
    "Spatial and temporal scale issues" = "Spatial &\ntemporal scale issues \n(STS)"
  )) +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "")+
  theme(
    axis.text = element_text(size = 17),
    legend.position = "none"
  )
# save 
ggsave(
  filename = "../Figures/bar_color.png",
  dpi = 300,
  width = 14, height = 7
)

# 3) Heatmap ------------

df <- mu_princ

# Select relevant columns
df_selected <- df %>%
  select(covidence, study_id, does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories) %>%
  rename(category = does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories) %>% 
  mutate(
    category_full = category,
    category = gsub("Model specification", "MS", category),
    category = gsub("Knowledge of the system", "KS", category),
    category = gsub("Spatial and temporal scale issues", "STS", category),
    category = gsub("Empirical data limitations", "EDL", category),
    category = gsub("Spatial dependence & heterogeneity issues", "SDH", category),
    category = gsub("Computing limitations", "CL", category)) %>% 
  mutate(category = sub("^$", "None", category))


# Create all category pairs within each 'covidence' group
df_pairs <- df_selected %>%
  group_by(covidence) %>%
  summarise(pairs = list(expand.grid(category, category))) %>%
  unnest(pairs)

# Count occurrences of each category pair
df_counts <- df_pairs %>%
  count(Var1, Var2)

# Get the max value for each unique group
df_max <- df_counts %>%
  group_by(Var1) %>%
  summarise(max_value = max(n, na.rm = TRUE))

# add max back into the df
df_counts <- df_counts %>% 
  right_join(df_max) %>% 
  # calculate percentages
  mutate(perc = (n/max_value)*100)

# Plot heatmap with desired order 

order <- c("CL", "EDL", "KS", "MS", "SDH", "STS")

df_counts$Var1 <- factor(df_counts$Var1, levels = rev(order))
df_counts$Var2 <- factor(df_counts$Var2, levels = rev(order))

ggplot(df_counts, aes(x = Var1, y = Var2, fill = perc)) +
  geom_tile() +
  geom_text(aes(label = n), color = "black") +
  #geom_text(aes(label = paste0(round(perc), "%")), color = "black") +
  #scale_fill_gradient(low = "white", high = "red", name = "Percent") +
  #scale_fill_gradient(name = "Percent")+
  #scale_fill_distiller(name = "Percent", palette ="RdBu", direction = -1)+ # or direction=1
  scale_fill_distiller(name = "Percent", palette ="Blues", direction = 1)+ # or direction=
  theme_minimal() +
  labs(x = "", y = "", 
       #title = "heatmap filled with percent but labeled with 'n'"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 14))

# save 
ggsave(
  filename = "../Figures/heat.png",
  dpi = 300,
  width = 10, height = 5
)

# # now with percents - e.g. what percent of the time were these assessed together?
# ggplot(df_counts, aes(x = Var1, y = Var2, fill = perc)) +
#   geom_tile() +
#   #geom_text(aes(label = n), color = "black") +
#   geom_text(aes(label = paste0(round(perc), "%")), color = "black") +
#   #scale_fill_gradient(low = "white", high = "red", name = "Percent") +
#   #scale_fill_gradient(name = "Percent")+
#   #scale_fill_distiller(name = "Percent", palette ="RdBu", direction = -1)+ # or direction=1
#   scale_fill_distiller(name = "Percent", palette ="Blues", direction = 1)+ # or direction=
#   theme_minimal() +
#   labs(x = "", y = "") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # save 
# ggsave(
#   filename = "../Figures/heat_perc.png",
#   dpi = 300,
#   width = 10, height = 6
# )
# 
# 
# 
# # Heatmap over time ------------
data_year <- df_selected %>%
  separate(study_id, into = c("late_name", "year"), sep = "\\s") %>%
  mutate(year = as.numeric(year))
# 
# 
# # get by year (PICK UP HERE) ---
# Create all category pairs within each 'covidence' group
df_y_pairs <- data_year %>%
  group_by(year) %>%
  count(category)

# Count occurrences of each category pair
# df_counts <- df_y_pairs %>%
#   count(Var1, Var2)

# Get the max value for each unique group
df_y_max <- df_y_pairs %>%
  group_by(category) %>%
  summarise(total_cat = sum(n, na.rm = TRUE))

# add max back into the df
df_y_counts <- df_y_pairs %>%
  right_join(df_y_max) %>%
  # calculate percentages
  mutate(perc = (n/total_cat)*100)
# 
# # Plot heatmap
# 
# # with % of each typology on the inside 
# ggplot(df_y_counts, aes(x = factor(year), y = category, fill = perc)) +
#   geom_tile() +
#   geom_text(aes(label = paste0(round(perc),"%")), color = "black") +
#   #scale_fill_gradient(low = "white", high = "red", name = "Percent") +
#   #scale_fill_gradient(name = "Percent")+
#   #scale_fill_distiller(name = "Percent", palette ="RdBu", direction = -1)+ # or direction=1
#   scale_fill_distiller(name = "% of Typology", 
#                        #breaks = c(2,6,10), labels = c(2, 6, 10),
#                        palette ="Blues", direction = 1)+ # or direction=
#   theme_minimal() +
#   labs(x = "", y = "") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.text = element_text(size = 20))
# 
# # save 
# ggsave(
#   filename = "../Figures/heat_y_perc.png",
#   dpi = 300,
#   width = 11, height = 4
# )

# with n on the inside ---

# re-order
df_y_counts$category <- factor(
  df_y_counts$category,
  levels = rev(order)
)
# 
# # plot
# ggplot(df_y_counts, aes(x = factor(year), y = category, fill = n)) +
#   geom_tile() +
#   geom_text(aes(label = n), color = "black") +
#   #scale_fill_gradient(low = "white", high = "red", name = "Percent") +
#   #scale_fill_gradient(name = "Percent")+
#   #scale_fill_distiller(name = "Percent", palette ="RdBu", direction = -1)+ # or direction=1
#   scale_fill_distiller(name = "n Studies", 
#                        breaks = c(2,6,10), labels = c(2, 6, 10),
#                        palette ="Blues", direction = 1)+ # or direction=
#   theme_minimal() +
#   labs(x = "", y = "") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),        
#         axis.text = element_text(size = 14),
#         legend.position = "bottom")
# 
# 
# # save 
# ggsave(
#   filename = "../Figures/heat_y_nstudies.png",
#   dpi = 300,
#   width = 10, height = 5
# )

# with % of each typology as the fill, but with labels of n studies inside  
ggplot(df_y_counts, aes(x = factor(year), y = category, fill = perc)) +
  geom_tile() +
  geom_text(aes(label = n), color = "black") +
  scale_fill_distiller(name = "Percent", 
                       #breaks = c(2,6,10), labels = c(2, 6, 10),
                       palette ="Blues", direction = 1)+ # or direction=
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 20),
        legend.position = "bottom")

# save 
ggsave(
  filename = "../Figures/heat_y_n_percfill.png",
  dpi = 300,
  width = 11, height = 4
)

