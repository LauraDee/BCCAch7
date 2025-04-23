## 008_PrepDataForSynthesisFigures
# Laura Dee March 24 2025
# Run this before the synthesis figures which cleans up names # 
# and aggregates subflows further # 

#Prep data for the data.table.figures.R code# 

#load libraries
library(ggplot2)
library(tidyverse)
library(data.table) #load last

"%notin%" <- Negate("%in%")

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- fread("data/007_output_interventions.csv")
impact.group <- fread("data/data_cleaning/impact_group.csv")

#clean up Flow and Subflow types
reshaped_data = reshaped_data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]

#group:
  # groundwater &  groundwater flow
  # sea ice change & sea ice retreat
  # stream flow and river flow to  surface water quantity
reshaped_data = reshaped_data[X2.2.Subtype =="groundwater flow", X2.2.Subtype := "groundwater"]
reshaped_data = reshaped_data[X2.2.Subtype =="sea ice change", X2.2.Subtype := "sea ice retreat"]
reshaped_data = reshaped_data[X2.2.Subtype =="stream flow", X2.2.Subtype := "surface water quantity"]
reshaped_data = reshaped_data[X2.2.Subtype =="river flow", X2.2.Subtype := "surface water quantity"]
# make a combined nutrient and sediment flow
reshaped_data = reshaped_data[X2.2.Subtype =="sediment flow", X2.2.Subtype := "nutrient and sediment flow"]
reshaped_data = reshaped_data[X2.2.Subtype =="nutrient flow", X2.2.Subtype := "nutrient and sediment flow"]
  #rename wind flow to just wind
  # rename glacial melt and discharge to just glacial melt
reshaped_data = reshaped_data[X2.2.Subtype =="wind flow", X2.2.Subtype := "wind"]
reshaped_data = reshaped_data[X2.2.Subtype =="glacial melt and discharge", X2.2.Subtype := "glacial melt"]

#check how well this condensed
phys = reshaped_data[X2.1.Flow.Type =="Physical",]
table(phys$X2.2.Subtype)

#COULD group as the nutrient, sediment and polluation all as water quality??
phys = reshaped_data[X2.1.Flow.Type =="Physical",]
table(phys$X2.2.Subtype)

## probably add other things too that are in Colleen and my data.table.figures.R files!

write.csv(reshaped_data, "data/008_preppedata_forsynthesis.csv")
