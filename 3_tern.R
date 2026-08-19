# title: 3_tern.R
# purpose: Plot ternary diagrams to show the distributions of the sources of uncertainty within our study
# created on: June 2025
# last edited: September 2025

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

# Part 1) Load data ----
#data <- read.csv("MU_Consensus_140_woNick_ManualEdit.csv", stringsAsFactors = FALSE) %>% clean_names()
data <- read.csv("MU_Consensus_All175_ManualEdit.csv", stringsAsFactors = FALSE) %>% clean_names()


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

# Part 2) Plotting --------
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

## create Colors Schemes
c_bin_num <- c(
  "1" = "grey90",
  "2-5" = "skyblue",
  "6-10" = "dodgerblue",
  "10+" = "darkblue"
)

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
            vjust = -0.5, hjust = -0.8, size = 6) +
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


ggsave("../Figures/tern_binperc_labels.png", 
       dpi = 300,
       height = 8, width = 8, #nits = "in"
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Part 3) Other Plots ------

## Blank Plots for PowerPoints-----

## Blank clean --
ggtern() +
  theme_bw() +
  labs(
    x = "", 
    y = "",
    z = ""
  )+
  theme(
    axis.text = element_blank())
getwd()
ggsave("../Figures/transparent_plot.png", bg = "transparent")

## Red Blue Green Blank Plot --
ggtern() +
  theme_rgbw() +
  labs(
    x = "", 
    y = "",
    z = ""
  )+
  theme(
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 24),
    
  )

## Plots with Colored Tern -----
## With bin - Color; no labs
ggtern(data = df_plot, aes(x = System, y = Unit, z = Data)) +
  geom_point(aes(color = freq_bin), size = 6, alpha = 0.8) +
  
  scale_color_manual(values = c_bin_num) +
  
  theme_rgbw() +
  
  labs(title = "Ternary Plot of Model Uncertainty Evaluation Approaches",
       subtitle = "",
       y = "Unit of Analysis",
       color = "Frequency Bin")+
  
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        #plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
  )

ggsave("../Figures/tern_binfreq_color.png", 
       dpi = 300,
       height = 8, width = 8, #nits = "in"
)

## With percent bin - Color; no labs
ggtern(data = df_plot, aes(x = System, y = Unit, z = Data)) +
  geom_point(aes(color = freq_perc_bin), size = 6, alpha = 0.8) +
  scale_color_manual(values = c_bin_perc) +
  
  #geom_text(aes(label = freq), vjust = -0.5, hjust = -0.8, size = 4) +
  theme_rgbw() +
  labs(title = "Ternary Plot of Model Uncertainty Evaluation Approaches",
       subtitle = "",
       y = "Unit of Analysis",
       color = "Frequency Bin")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    #plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18)
  )

ggsave("../Figures/tern_binperc_color.png", 
       dpi = 300,
       height = 8, width = 8, #nits = "in"
)


## Plots with Clean Tern ------
## With bin - Color; labs + basic tern
ggtern(data = df_plot, aes(x = System, y = Unit, z = Data)) +
  geom_point(aes(color = freq_bin), size = 6, alpha = 0.8) +
  scale_color_manual(values = c_bin_num) +
  
  geom_text(aes(label = freq), vjust = -0.5, hjust = -0.8, size = 4) +
  theme_bw() +
  labs(title = "Ternary Plot of Model Uncertainty Evaluation Approaches",
       subtitle = "Count for Each Point",
       y = "Unit of Analysis",
       color = "frequency bin")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


ggsave("../Figures/tern_binfreq_labels.png", 
       dpi = 300,
       height = 8, width = 8, #nits = "in"
)

# GRAVEYARD ----
## G1) Plots I didn't end up using but do look okay ----

## G2) Extra Code ------

# # Find values in df1$covidence that are not in df2$covidence
# missing_values <- setdiff(data_filtered$covidence, df_counts$covidence)
# 
# # Show the missing values
# print(missing_values)



# Plotting 
# ## With bin - Size
# 
# # Plot with size mapped to frequency bin
# ggtern(data = df_plot, aes(x = System, y = Unit, z = Data)) +
#   geom_point(aes(size = freq_bin), color = "steelblue", alpha = 0.8) +
#   scale_size_manual(values = c("0-1" = 2, "2-5" = 4, "6-10" = 6, "10+" = 8)) +
#   theme_bw() +
#   labs(title = "Ternary Plot with Size by frequency bin",
#        size = "frequency bin")
