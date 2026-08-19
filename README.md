# Meta-Uncertainty
Code and data repository for the manuscript "Meta-uncertainty in coupled human and natural systems models" co-led by Manning and 

This repository contains:
- *MU_Consensus_All175_ManualEdit.csv*
--This CSV contains the consensus responses to each of the studies included in our review. 

- *MU_ReviewerConsensus_Extraction_Blind.csv*
--This CSV contains both of the initial reviewer responses as well as the consensus responses to each of the studies included in our review. 

- *1a_figure_map.R*
--NOTE: run this script first to create the “Figures” folder where all figures will be saved to! 
--This script imports the results file (post-consensus) and creates a map of how many studies focus on each region/country with pie charts of the meta-uncertainty principles and categories quantified in these studies 
--Inputs include “MU_Consensus_All175_ManualEdit.csv" and outputs include Figure 2 (“map_scatter.png”) and Figures SF3 (“pie_typ_region.png”) and SF4 (“pie_tern_region.png”)

- *1b_bar_heatmap.R*
--This script imports the results file (post-consensus) and creates a barplot showing the percentage of studies that quantified each principle of meta-uncertainty and two heatmaps of the studies included in our review
--Inputs include “MU_Consensus_All175_ManualEdit.csv" and Outputs include the Figure 3 (“bar_color.png”) and Figure 4 (4A = “heat.png” and 4B = “heat_y_n_percfill.png”)

- *1c_ReviewerConsensus.R*
--This script imports the blind reviewer + consensus CSV (“MU_ReviewerConsensus_Extraction_Blind.csv”) to count the number of disagreements between reviewers for whether or not the study included a model and whether or not the study quantified uncertainty
--Inputs include the CSV file showing each reviewer (anonymized to Reviewer 1 for the first reviewer and Reviewer 2 for the second) and the final consensus for each study and outputs include the overall agreement percentages.  

- *1d_tern.R*
--This script imports the results file (post-consensus), recodes the principles into three main categories of meta-uncertainty, and then plots a ternary diagram to show the distributions of the sources of uncertainty addressed within the studies included in our review
--Inputs include “MU_Consensus_All175_ManualEdit.csv" and outputs include Figure 5 (“tern_binperc_labels.png”)

- *2_shiny_tern_weighted_size_5Model.R*
--This script contains the code used to generate and run the [R Shiny application](https://nbm-msu.shinyapps.io/metaunc_app/). This application takes user inputs for how confident they are that their model addresses each of the six meta-uncertainty principles, re-codes these inputs to the three main categories of meta-uncertainty, and plots their inputs on a ternary diagram for up to 5 independent models. 

