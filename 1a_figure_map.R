# title: 3_figure_map.R
# purpose: import the results file (post-consensus) and create a map based on studies that include a model and quantify uncertainty

# created on: March 2025
# last edited: August 2026

# author: Nick Manning

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())

# 0) Load Libraries --------
library(ggplot2)
library(tidyr)
library(maps)
library(ggrepel)
library(sf)
library(janitor)
library(stringi)
library(stringr)
library(scatterpie)
library(dplyr)

### Create "Figures" Folder if one does not already exist ###

# set relative path
folder_fig <- "../Figures"

# check for folder 
if (!dir.exists(folder_fig)) {
  dir.create(folder_fig, recursive = TRUE)
  
  cat("Figures folder created.\n")
} else {
  cat("Figures folder already exists.\n")
}

# 1) Load & Clean Data -----
# Load the data
data <- read.csv("MU_Consensus_All175_ManualEdit.csv", stringsAsFactors = FALSE) %>% clean_names()

# Filter for studies that include both socio-environmental models and quantify uncertainty
filtered_data <- data %>%
  dplyr::filter(
    does_the_study_include_a_socio_environmental_model == "Yes" &
    does_the_study_quantify_model_uncertainty == "Yes"
  )

# Count the number of studies per model
# separate_rows is used when the study is listed on the same line (e,g, France, Germany). Also useful for models. 
counts_model <- filtered_data %>%
  separate_rows(`type_of_model`, sep = "; ") %>% # Split multiple countries
  mutate(type_of_model = gsub("^Other.*", "Other", type_of_model)) %>% 
  count(`type_of_model`, name = "count")

# count number of countries
counts_country <- filtered_data %>%
  separate_rows(`country_region_in_which_the_study_is_focused`, sep = ", ") %>% # Split multiple countries
  count(`country_region_in_which_the_study_is_focused`, name = "count") %>%
  drop_na()

# count types of uncertainty quantifcation
counts_unc_quant <- filtered_data %>%
  separate_rows(`how_does_this_model_quantify_uncertainty`, sep = "; ") %>% # Split multiple countries
  mutate(how_does_this_model_quantify_uncertainty = gsub("^Other.*", "Other", how_does_this_model_quantify_uncertainty)) %>% 
  count(`how_does_this_model_quantify_uncertainty`, name = "count")


# 2) Get Spatial Data for Plotting ----------

# Merge with country counts for plotting
counts_country <- counts_country %>%
  rename(region = `country_region_in_which_the_study_is_focused`)

# Get world map data and centroids
world <- map_data("world")

# Load country geometries and fix invalid ones
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Use `st_point_on_surface()` instead of `st_centroid()` to ensure valid points
centroids <- world_sf %>%
  st_make_valid() %>%  # Ensure geometries are valid
  mutate(geometry = st_point_on_surface(geometry)) %>%  # Get a valid centroid
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(world_sf %>% select(name)) %>%
  rename(long = X, lat = Y, region = name)

# replace accents to work with UTF-8 CSV
centroids <- centroids %>% 
  mutate(region = stri_trans_general(str = region, id = "Latin-ASCII")) %>% 
  mutate(region = str_replace_all(region, "United States of America", "USA"))

# Merge study counts with centroids
map_data <- left_join(counts_country, centroids, by = "region") %>%
  drop_na()

names(map_data)



# 3) Plot ScatterPie -------------

# Get example pie plot with USA
# Filter for studies that include both socio-environmental models and quantify uncertainty
filtered_data <- data %>%
  filter(
    `does_the_study_include_a_socio_environmental_model` == "Yes" &
      `does_the_study_quantify_model_uncertainty` == "Yes"
  )

filtered_data <- filtered_data %>%
  #select(covidence, does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories) %>%
  rename(
    category = does_the_model_uncertainty_approach_fall_into_any_of_the_following_categories,
    region = country_region_in_which_the_study_is_focused
    ) %>% 
  mutate(
    category_full = category,
    category = gsub("Model specification", "MS", category),
    category = gsub("Knowledge of the system", "KS", category),
    category = gsub("Spatial and temporal scale issues", "STS", category),
    category = gsub("Empirical data limitations", "EDL", category),
    category = gsub("Spatial dependence & heterogeneity issues", "SDH", category),
    category = gsub("Computing limitations", "CL", category)) %>% 
  mutate(category = sub("^$", "None", category)) %>%
  mutate(region = str_replace(region, "None_Unspecified", "Unspecified")) %>% 
  mutate(region = str_replace(region, "Other", "Cross-Border"))
  

# Count the number of studies per country
# separate_rows is used when the study is listed on the same line (e,g, France, Germany). Also useful for models. 
usa <- filtered_data %>%
  separate_rows(region, sep = ", ") %>%
  #filter(region == "USA") %>% 
  separate_rows(category, sep = "; ") %>% 
  mutate(tern = case_when(
    category == "MS" ~ "System",
    category == "KS" ~ "System",
    category == "STS" ~ "Unit",
    category == "SDH" ~ "Unit",
    category == "CL" ~ "Data",
    category == "EDL" ~ "Data",
    .default = "NA"
  )) 
  
  
# Split multiple countries
usa_counts <- usa %>% 
  group_by(region) %>% 
  count(`category`, name = "count") %>%
  #drop_na() %>% 
  rename(type = category) %>%
  mutate(type = sub("^$", "None", type),
         #region = "USA"
         )

# get usa_counts wide to merge with maps_data
usa_counts_wide <- usa_counts %>% 
  spread(key = "type", value = "count") %>% 
  # replace NA values with 0
  mutate_at(., vars(-group_cols()), ~if_else(is.na(.), 0, .))

# get count of other categories that aren't present in the 'world' dataset
other_count <- filtered_data %>%
  filter(region %in% c("Unspecified", "Cross-Border", "Global")) %>% 
  separate_rows(region, sep = ", ") %>% # Split multiple countries
  count(region, name = "count") %>% 
  mutate(
    long = c(-150, -150, -150),
    lat = c(10, -5, 35),
    geometry = NA
  )

# add these 
manual_world <- map_data %>% rbind(other_count)
map_data_world <- manual_world %>% left_join(usa_counts_wide) 

## 3.1) Plot ScatterPie of Meta-Uncertainty Principles -------
# plot
ggplot() + 
 # plot background map
  geom_polygon(data = world, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "white") +
  # add data 
  geom_scatterpie(aes(x=long, y=lat, group=region, r = count), 
                  data=map_data_world,
                  cols=c("CL","EDL","KS","MS","SDH","STS")) +
  scale_fill_manual(
  #  breaks = c("MS", "KS", "EDL", "CL", "STS", "SDH"),
    values = c(
      "CL"  = "#F8766D",
      "EDL" = "#B79F00",
      "KS"  = "#00B035",
      "MS"  = "#00BFC4",
      "SDH" = "#619CFF",
      "STS" = "#F564E3"
    )
  )+
  coord_equal()+
  theme_minimal() +
  labs(title = "Countries/Regions Studied using CHANS Models that Quantify Uncertainty",
       r = "Number of Studies",
       x = "", y = "",
       #caption = "CL = Computing Limitation   EDL = Empirical Data Limitations   KS = Knowledge of the System   MS = Model Specification   SDH = Spatial Dependence & Heterogeneity   STS = Spatial & Temporal Scale" 
       )+
  theme(
    axis.text = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))+
  geom_scatterpie_legend(map_data_world$count, x=-155, y=-45)

# save 
ggsave(
  filename = "../Figures/map_scatter.png",
  dpi = 300,
  width = 10, height = 6
)

## 3.2: Plot ScatterPie with 3 Meta-Uncertainty Categories (Omitted from Manuscript) -------

# Set the ternary principles
usa_tern_counts <- usa %>% 
  #filter(tern != "NA") %>% 
  group_by(region) %>% 
  count(`tern`, name = "count") %>%
  #drop_na() %>% 
  rename(type = tern) %>%  #%>%
  mutate(type = sub("NA", "Other", type),
         #region = "USA"
  )

# get usa_counts wide to merge with maps_data
usa_tern_counts_wide <- usa_tern_counts %>% 
  spread(key = "type", value = "count") %>% 
  # replace NA values with 0
  mutate_at(., vars(-group_cols()), ~if_else(is.na(.), 0, .))

# get count of other categories that aren't present in the 'world' dataset
other_tern_count <- filtered_data %>%
  filter(region %in% c("Unspecified", "Cross-Border", "Global")) %>% 
  separate_rows(region, sep = ", ") %>% # Split multiple countries
  count(region, name = "count") %>% 
  mutate(
    long = c(-150, -150, -150),
    lat = c(10, -5, 35),
    # long = c(-150, -150),
    # lat = c(-5, 35),
    geometry = NA
  )

# add these 
manual_world <- map_data %>% rbind(other_tern_count)
map_tern_data_world <- manual_world %>% left_join(usa_tern_counts_wide) 

# Define custom colors
custom_colors <- c(
  #"Unit" = "deeppink4",
  # "Unit" = "mediumvioletred",
  # "System" = "dodgerblue4",
  # "Data" = "forestgreen", 
  # "Other" = "gray70" 
  # 
  # colorblind friendly
  "Unit" = "#AA4499", # dark red
  #"System" = "#332288", # dark blue
  "Data" = "#117733", # dark green
  # "Data" = "#44AA99", # light green
  "System" = "#88CCEE"# light blue

  #"Other" = "gray60" #mideum-light gray
)

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_scatterpie(aes(x=long, y=lat, group=region, r = count),
                  data=map_tern_data_world,
                  cols=c("Data", "System", "Unit")) +
  coord_equal()+
  theme_minimal() +
  scale_fill_manual(values = custom_colors)+
  labs(title = "Studies with Socio-Environmental Models and Uncertainty Quantification",
       size = "Number of Studies",
       x = "", y = ""
  )+
  theme(
    axis.text = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )+
  geom_scatterpie_legend(map_data_world$count, x=-155, y=-45)
 
# # save
# ggsave(
#   filename = "../Figures/map_tern_scatter.png",
#   dpi = 300,
#   width = 4, height = 3
# )

# 4) Pie plots ------

# plot each country as it's own pie plot (since they're not all visible on the map)
usa_counts <- usa_counts %>%
  group_by(region) %>%
  mutate(percentage = count / sum(count) * 100,
         total_value = sum(count),
         #label = paste(type, ":", count)
         )

# use this to get the full text in the legend 
usa_counts_noabv <- usa_counts %>% 
  mutate(type = recode(
    type,
    "MS" = "Model specification",
    "KS" = "Knowledge of the system",
    "STS" = "Spatial & temporal scale issues",
    "EDL" = "Empirical data limitations",
    "SDH" = "Spatial dependence & heterogeneity issues",
    "CL" = "Computing limitations"
  ))
  
  
  

# Create pie charts with facet wrap and labels
ggplot(usa_counts_noabv, aes(x = "", y = percentage, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~ region) +
  theme_void() +
  scale_fill_manual(
    values = c(
    "Computing limitations"  = "#F8766D",
    "Empirical data limitations" = "#B79F00",
    "Knowledge of the system"  = "#00B035",
    "Model specification"  = "#00BFC4",
    "Spatial dependence & heterogeneity issues" = "#619CFF",
    "Spatial & temporal scale issues" = "#F564E3"
  ))+
  #theme(legend.position = "bottom") +
  labs(title = "Meta-uncertainty Typologies per Country/Region", fill = "Typology") +
  # NOTE: uncomment to get the total number of studies per region instead of per type per region
  # geom_text(data = usa_counts %>% distinct(region, total_value), 
  #           aes(x = 1, y = 1, label = paste("n= ", total_value)),
  #           size = 4, inherit.aes = FALSE)+
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 4)+
  theme(plot.title = element_text(h = 0.5),
        legend.position = "bottom")

# save
ggsave(
  filename = "../Figures/pie_typ_region.png",
  dpi = 300,
  width = 8, height = 8
)


# plot each country as it's own pie plot (since they're not all visible on the map)
usa_tern_counts <- usa_tern_counts %>%
  group_by(region) %>%
  mutate(percentage = count / sum(count) * 100,
         total_value = sum(count),
         #label = paste(type, ":", count)
  )

# Create pie charts with facet wrap and labels
ggplot(usa_tern_counts, aes(x = "", y = percentage, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~ region) +
  theme_void() +
  scale_fill_manual(values = custom_colors)+
  #theme(legend.position = "bottom") +
  labs(title = "Meta-uncertainty Ternary Categories per Country/Region", fill = "Category") +
  # NOTE: uncomment to get the total number of studies per region instead of per type per region
  # geom_text(data = usa_counts %>% distinct(region, total_value), 
  #           aes(x = 1, y = 1, label = paste("n= ", total_value)),
  #           size = 4, inherit.aes = FALSE)+
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 4)+
  theme(plot.title = element_text(h = 0.5),
        legend.position = "bottom")

# save
ggsave(
  filename = "../Figures/pie_tern_region.png",
  dpi = 300,
  width = 8, height = 8
)