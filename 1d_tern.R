# title: 3_tern.R
# purpose: Plot ternary diagrams to show the distributions of the sources of uncertainty within our study
# created on: June 2025
# last edited: August 2026

# author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
rm(list = ls())

# 0) Load Libraries --------
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(RColorBrewer)
library(ggtern)


# 1) Load data ----
data <- read.csv("MU_Consensus_All175_ManualEdit.csv", stringsAsFactors = FALSE) %>% clean_names()

# filter to include studies with models that quantified uncertainty 
data_filtered <- data %>%
  filter(
    `does_the_study_include_a_socio_environmental_model` == "Yes" &
      `does_the_study_quantify_model_uncertainty` == "Yes"
  )

# Split rows by the different types of metauncertainty principles
mu_princ <- data_filtered %>%
  separate_rows(`does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories`, sep = "; ") %>% 
  mutate(princ = does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories)

# Set the ternary principles
mu_princ <- mu_princ %>% 
  mutate( tern = case_when(
    princ == "Model specification" ~ "System",
    princ == "Knowledge of the system" ~ "System",
    princ == "Spatial and temporal scale issues" ~ "Unit",
    princ == "Spatial dependence & heterogeneity issues" ~ "Unit",
    princ == "Computing limitations" ~ "Data",
    princ == "Empirical data limitations" ~ "Data",
    .default = "NA"
  )) %>%
  filter(tern != "NA")

# 2) Plotting --------
# Set mu_princ to df for simplicity
df <- mu_princ

# Step 2.1: Count Tern values per ID
df_counts <- df %>%
  count(covidence, tern) %>%
  pivot_wider(names_from = tern, values_from = n, values_fill = 0)

# Step 2.2: Normalize to proportions
df_props <- df_counts %>%
  mutate(total = rowSums(across(c(System, Unit, Data), ~ .x, .names = NULL)),
         System = System / total,
         Unit = Unit / total,
         Data = Data / total) %>%
  select(-total)

# Step 2.3: Count how many IDs share the same ternary composition
df_plot <- df_props %>%
  count(System, Unit, Data, name = "freq")

# Step 2.4: bin the data to make it obvious what the different sizes are
df_plot <- df_plot %>%
  mutate(freq_bin = cut(freq,
                        breaks = c(0, 1, 5, 10, Inf),
                        labels = c("1", "2-5", "6-10", "10+"),
                        #labels = c("Low", "Medium", "High", "Very High"),
                        right = TRUE))

# Step 2.5: Create Percents and bin
n_freq <- as.numeric(sum(df_plot$freq))

df_plot <- df_plot %>% 
  mutate(freq_perc = (freq/n_freq)*100) %>% 
  mutate(freq_perc_bin = cut(freq_perc,
                              breaks = c(0, 5, 10, 20, Inf),
                              labels = c("<5%", "5-10%", "10-20%", ">20%"),
                              #labels = c("Low", "Medium", "High", "Very High"),
                              right = F))


# Step 2.5: Plot

## set color scheme 
c_bin_perc<- c(
  "<5%" = "grey90", 
  "5-10%" = "skyblue", 
  "10-20%" = "dodgerblue", 
  ">20%" = "darkblue")

## With bin - Color; labs + basic tern
ggtern(data = df_plot, aes(x = System, y = Unit, z = Data)) +
  geom_point(aes(color = freq_perc_bin), size = 6, alpha = 0.8) +
  scale_color_manual(values = c_bin_perc) +
  
  geom_text(aes(label = round(freq_perc, 1)), 
            vjust = -0.5, hjust = -0.8, 
            size = 6) +
  theme_bw() +
  labs(title = "Ternary Plot of Model Uncertainty Evaluation Approaches",
       subtitle = "Percent Frequency for Each Point",
       y = "Unit of Analysis",
       color = "Frequency Bin")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        #axis.text = element_text(size = 12),
        axis.title = element_text(size = 18))


## Save 
ggsave("../Figures/tern_binperc_labels.png",
       dpi = 300,
       height = 8, width = 8, #nits = "in"
)