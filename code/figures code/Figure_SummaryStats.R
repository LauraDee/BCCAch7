#BCCA summary stat figures

#Figures of drivers, breakdown by flow, impacts, etc.
graphics.off()
rm(list=ls())

#load libraries
library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)

# Filter relevant columns for Altered Flow and Impact
"%notin%" <- Negate("%in%")

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- read.csv("data/007_output_interventions.csv")
glimpse(reshaped_data)
table(reshaped_data$X2.1.Flow.Type)

data <- reshaped_data %>% filter(`X2.1.Flow.Type` %notin% c("Remove","Recode"))
driver_cols <- names(data)[grepl("driver.", names(data))]

## Count of paper by Flow
table(data$X2.1.Flow.Type)
data = as.data.table(data)
data = data[X2.1.Flow.Type =="'Trade (transport of goods and services)'", X2.1.Flow.Type := "Human movement"]
data = data[X2.1.Flow.Type =="Trade", X2.1.Flow.Type := "Human movement"]

flowcount <- ggplot(as.data.frame(data), aes(X2.1.Flow.Type,  fill = X2.1.Flow.Type)) +
  geom_bar(position = 'dodge') +
  labs(
    title = "Count of Papers by Flow Type",
    x = "Flow Type",
    y = "Count") +
  theme_minimal() + theme(legend.position="none") +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", "NA" = "white"))
flowcount

#do as % :
flow_percent <- data %>%
  group_by(X2.1.Flow.Type) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(flow_percent, aes(as.factor(X2.1.Flow.Type), prop)) +
    geom_col(aes(fill = as.factor(X2.1.Flow.Type))) +
 labs( title = "Proportion of Flow Type",
      x = "Flow Type",
      y = "Proportion of Entries") +
    theme_minimal() + theme(legend.position="none") +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", "NA" = "white"))

## Count of paper by Subflow
table(data$X2.2.Subtype)
data = data[X2.2.Subtype =="knowledge transfer", X2.2.Subtype := "Knowledge transfer"]
data = data[X2.2.Subtype =="knowledge transfer", X2.2.Subtype := "Knowledge transfer"]

#group:
# groundwater &  groundwater flow
# sea ice change & sea ice retreat
# stream flow and river flow to  surface water quantity
data = data[X2.2.Subtype =="groundwater flow", X2.2.Subtype := "groundwater"]
data = data[X2.2.Subtype =="sea ice change", X2.2.Subtype := "sea ice retreat"]
data = data[X2.2.Subtype =="stream flow", X2.2.Subtype := "surface water quantity"]
data = data[X2.2.Subtype =="river flow", X2.2.Subtype := "surface water quantity"]
# make a combined nutrient and sediment flow
data = data[X2.2.Subtype =="sediment flow", X2.2.Subtype := "nutrient and sediment flow"]
data = data[X2.2.Subtype =="nutrient flow", X2.2.Subtype := "nutrient and sediment flow"]
#rename wind flow to just wind
# rename glacial melt and discharge to just glacial melt
data = data[X2.2.Subtype =="wind flow", X2.2.Subtype := "wind"]
data = data[X2.2.Subtype =="glacial melt and discharge", X2.2.Subtype := "glacial melt"]

subflowcount <- ggplot(as.data.frame(data), aes(X2.2.Subtype,  fill = X2.1.Flow.Type)) +
  geom_bar(position = 'dodge') +
  labs(
    title = "Count of Papers by Subflow Type",
    x = "Subflow Type",
    y = "Count") + coord_flip() +
  scale_fill_manual(values = c("Biotic" = "burlywood", "Physical" = "darkturquoise", "Human movement" = "firebrick1", "Sociocultural" = "darkmagenta", "NA" = "white"))
subflowcount

#subflowcount +  theme_minimal() + coord_flip() + theme(legend.title = "Flow Type")

subflow_percent <- data %>%
  group_by(X2.2.Subtype) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n))
subflow_percent
print(subflow_percent)

ggplot(subflow_percent, aes(as.factor(X2.2.Subtype), prop, fill = X2.1.Flow.Type)) +
  geom_col(aes(fill = as.factor(X2.2.Subtype))) +
  labs( title = "Proportion of Subflow Types",
        x = "Subflow Type",
        y = "Proportion of Entries") + coord_flip() +
  theme_minimal() + theme(legend.position="none")

#for just biotic
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

## Count of papers by Driver - this could be improved upon
# to automate:
table(reshaped_data$driver.heat.waves)
sum(data$driver.heat.waves) #1
sum(data$driver.Drought) #62

count_true_cols <- function(data, cols) {
  data %>%
    select(all_of(cols),starts_with('driver.')) %>%
    summarise_all(.funs = ~sum(.x))
}

results <- count_true_cols(data, driver_cols)
results <- as.data.frame(results)
print(results)

# df2 <- results |>
#   pivot_longer(cols = everything(),
#                names_to = 'date',
#                values_to = 'x')

#write.csv(results, "driver_counts.csv")
df <- fread("driver_counts.csv")

df <- as.matrix(df)
df2_new<- t(df)
write.csv(df2_new, "driver_counts.csv")

df <- fread("driver_counts.csv")
head(df)

ggplot(df, aes(Driver, count)) +
  geom_col() + labs(
    title = "Count of Drivers",
    x = "Driver",
    y = "Count") +
  theme_minimal() + coord_flip()

## filter to drivers with more than 5 entries
df5 <- df %>%  filter(count>5)
nrow(df5)
nrow(df)

drivers <- ggplot(df5, aes(Driver, count)) +
  geom_col() + labs(
    title = "Count of Drivers",
    x = "Driver",
    y = "Count") +
  theme_minimal() + coord_flip()
drivers

## filter to drivers with more than  10 entries
df10 <- df %>%  filter(count>10)
nrow(df5)
nrow(df10)

ggplot(df10, aes(Driver, count)) +
  geom_col() + labs(
    title = "Count of Drivers",
    x = "Driver",
    y = "Count") +
  theme_minimal() + coord_flip()

## Impact summaries
glimpse(data)
impact_cols <- names(reshaped_data)[grepl("2.12.Impact.", names(reshaped_data))]

table(data$X2.12.Impact..Abundance)
table(data$X2.12.Impact..Abundance)
unique(list(data$X2.21.Well.being.List))

#for just physical
phys = data[X2.1.Flow.Type == "Physical",]
phys.subflowcount <- ggplot(as.data.frame(phys), aes(X2.2.Subtype)) +
  geom_bar(position = 'dodge') +
  labs(title = "Count of Physical Papers by Subflow Type",
    x = "Subflow Type",
    y = "Count") + theme_minimal() + coord_flip()
phys.subflowcount

