#Figures of drivers by flow and impacts and NCP

#load libraries
library(ggplot2)
library(tidyverse)
library(data.table)
library(forcats)

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- fread("data/007_output_interventions.csv")

driver_data = melt(reshaped_data, 
                 id.vars=c("ID_DOI_by_Flow", "X2.1.Flow.Type", "X2.2.Subtype","DOI"),
                 measure.vars = patterns(driver="^driver."),
                 variable.name = "driver")[value==TRUE,][,value:=NULL][,driver:=gsub(pattern="^driver.(*)", replacement="\\1",driver)]

impact_data = melt(reshaped_data, 
                   id.vars=c("ID_DOI_by_Flow"),
                   measure.vars = patterns(driver="^X2.12.Impact"),
                   variable.name = "impact",
                   value.name="direction")[direction!="",][,impact:=gsub(pattern="^X2.12.Impact..(*)", replacement="\\1",impact)]


driver_impact_data = merge(driver_data, 
                           impact_data, 
                           by="ID_DOI_by_Flow",
                           allow.cartesian=TRUE)
