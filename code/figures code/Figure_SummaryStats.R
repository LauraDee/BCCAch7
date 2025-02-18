#BCCA summary stat figures

#Figures of drivers, breakdown by flow, impacts, etc.
graphics.off()
rm(list=ls())

#load libraries
library(ggplot2)
library(data.table)

# Filter relevant columns for Altered Flow and Impact
"%notin%" <- Negate("%in%")

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- read.csv("data/006_output_recoded.csv")
glimpse(reshaped_data)
table(reshaped_data$X2.1.Flow.Type)
data <- reshaped_data %>% filter(`X2.1.Flow.Type` %notin% c("Remove","Recode"))
driver_cols <- names(data)[grepl("driver.", names(data))]

## Count of paper by Flow
table(data$X2.1.Flow.Type)
data = as.data.table(data)
data[X2.1.Flow.Type =="Trade (transport of goods and services)", X2.1.Flow.Type:="Human movement"]

flowcount <- ggplot(as.data.frame(data), aes(X2.1.Flow.Type,  fill = X2.1.Flow.Type)) +
  geom_bar(position = 'dodge') +
  labs(
    title = "Count of Papers by Flow Type",
    x = "Flow Type",
    y = "Count") +
  theme_minimal() + theme(legend.position="none")
flowcount

## Count of paper by Subflow
table(data$X2.2.Subtype)

subflowcount <- ggplot(as.data.frame(data), aes(X2.2.Subtype,  fill = X2.1.Flow.Type)) +
  geom_bar(position = 'dodge') +
  labs(
    title = "Count of Papers by Subflow Type",
    x = "Subflow Type",
    y = "Count") +
  theme_minimal() + coord_flip() + theme(legend.title = "Flow Type")
subflowcount

biotic = data[X2.1.Flow.Type == "Biotic",]
biotic[X2.2.Subtype == "range-shift", X2.2.Subtype := "Range shift"]
biotic[X2.2.Subtype == "species range shifts", X2.2.Subtype := "Range shift"]

biotic.subflowcount <- ggplot(as.data.frame(biotic), aes(X2.2.Subtype)) +
  geom_bar(position = 'dodge') +
  labs(
    title = "Count of Biotic Papers by Subflow Type",
    x = "Subflow Type",
    y = "Count") +
  theme_minimal() + coord_flip()
biotic.subflowcount

## Count of papers by Driver

table(reshaped_data$driver.heat.waves)
sum(data$driver.heat.waves) #1
sum(data$driver.Drought) #62


driverdat = data[, .(total = sum(na.omit(unlist(.SD)))), .SD, .SDcols = patterns("^driver.")]

library(dplyr)
data %>%
  group_by(driver_cols) %>%
  summarise(total = sum(unlist(select(cur_data(), starts_with('task'))), na.rm = TRUE))


# https://stackoverflow.com/questions/57702227/count-number-of-rows-where-value-in-two-columns-are-both-true/57702283


count_true_cols <- function(data, cols) {
  data %>%
    select(cols) %>%
    summarise_all(.funs = ~sum(.x), starts_with('driver.'))
}

results <- count_true_cols(data, driver_cols)
print(results)
