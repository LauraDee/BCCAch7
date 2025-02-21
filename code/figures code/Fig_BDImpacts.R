#Figures of drivers by flow and impacts and NCP
graphics.off()
rm(list=ls())

#load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggforce)

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/")
reshaped_data <- read.csv("data/007_output_interventions.csv")
glimpse(reshaped_data)

# Select and reshape relevant columns
altered_flow_cols <- names(reshaped_data)[grepl("2.7.Altered.Flow.", names(reshaped_data))]
impact_cols <- names(reshaped_data)[grepl("2.12.Impact.", names(reshaped_data))]

# Filter relevant columns for Altered Flow and Impact
"%notin%" <- Negate("%in%")

interaction_data <- reshaped_data %>% filter(`Citation` %notin% c("TEST","test","Test")) %>% 
  select(all_of(c(altered_flow_cols, impact_cols))) %>%
  mutate(row_id = row_number())  %>%
  filter(!if_all(-row_id, ~ .x == ""))
glimpse(interaction_data)

#update column names
flow_columns <- names(interaction_data)[grepl("2.7.Altered.Flow.", names(interaction_data))]
impact_columns <- names(interaction_data)[grepl("Impact.", names(interaction_data))]

# Generate all possible combinations of flows and impacts
combinations <- expand.grid(
  Flow = flow_columns,
  Impact = impact_columns,
  stringsAsFactors = FALSE
)

# Count occurrences of each combination in the data
combination_counts <- combinations %>%
  rowwise() %>% 
  mutate(
    count = sum(
      interaction_data[[Flow]] != "" & interaction_data[[Impact]] != "",
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Convert to a dataframe for easy viewing
combination_counts_df <- as.data.frame(combination_counts)
# Preview the result
glimpse(combination_counts_df)

# Create the plot of paper counts by combination
ggplot(combination_counts_df, aes(x = Flow, y = Impact, size = count)) +
  geom_point(color = "blue", alpha = 0.7) +  # Use points to represent combinations
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range for better visibility
  labs(
    title = "Interaction Between Altered Flows and Impacts",
    x = "Altered Flow",
    y = "Impact",
    size = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )

# Count occurrences for each Impact direction
combination_counts_by_impact <- combinations %>%
  rowwise() %>%
   mutate(
    Increase = sum(
      interaction_data[[Flow]] != "" & interaction_data[[Impact]] == "Increase",
      na.rm = TRUE
    ),
    Decrease = sum(
      interaction_data[[Flow]] != "" & interaction_data[[Impact]] == "Decrease",
      na.rm = TRUE
    ),
    Complex = sum(
      interaction_data[[Flow]] != "" & interaction_data[[Impact]] == "Complex change",
      na.rm = TRUE
    )
  ) %>%
  pivot_longer(cols = c(Increase, Decrease, Complex), names_to = "ImpactDirection", values_to = "count") %>%
  ungroup()

# Filter out rows with count == 0
combination_counts_by_impact_filtered <- combination_counts_by_impact %>%
  filter(count > 0)

# Plot with facets by Impact direction, excluding zero counts
ggplot(combination_counts_by_impact_filtered, aes(x = Flow, y = Impact, size = count, color = ImpactDirection)) +
  geom_point(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~ImpactDirection, scales = "free") +  # Create facets for each impact direction
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex" = "purple")) +
  labs(
    title = "Interaction Between Altered Flows and Impacts by Impact Direction",
    x = "Altered Flow",
    y = "Impact",
    size = "Count",
    color = "Impact Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )

#do counts of impacts and by direction for bar plots
head(combination_counts_by_impact_filtered)

#remove impact of none
combination_counts_by_impact_filtered = as.data.table(combination_counts_by_impact_filtered)
# need to do: 
combination_counts_by_impact_filtered <- combination_counts_by_impact_filtered  %>%
  filter(count > 0)  %>%
  filter(Impact != "X2.12.Impact..None")

write.csv(combination_counts_by_impact_filtered, "view_impacts_feb2025.csv")

Impacts <- ggplot(combination_counts_by_impact_filtered, aes(x = Impact, fill = ImpactDirection)) +
 geom_bar() +  theme_minimal() +
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex" = "purple")) +
  labs(
    title = "Biodiversity Impact",
    x = "Impact to Biodiversity",
    y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip() 
Impacts

# Impacts +
#   # The vertical axis only extends upwards 
#   scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
#   theme(
#     # Set background color to white
#     panel.background = element_rect(fill = "white"),
#     # Set the color and the width of the grid lines for the horizontal axis
#     panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
#     # Remove tick marks by setting their length to 0
#     axis.ticks.length = unit(0, "mm"),
#     # Remove the title for both axes
#     # axis.title = element_blank(),
#     # Only left line of the vertical axis is painted in black
#     axis.line.y.left = element_line(color = "black"),
#     # Remove labels from the vertical axis
#     #  axis.text.y = element_blank(),
#     # But customize labels for the horizontal axis
#     axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)
#   )

#facet wrap impacts by flow type
Impacts_by_Flow <- ggplot(combination_counts_by_impact_filtered, aes(x = Impact, y=count, fill = ImpactDirection)) +
  geom_col() +  theme_minimal() +
  facet_wrap(~Flow, scales = "fixed", shrink = TRUE, labeller = "label_value") +
  scale_fill_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex" = "purple")) +
  labs(
    title = "Biodiversity Impact by Flow",
    x = "Impact to Biodiversity",
   # y = "count",
    size = "Count",
    color = "Impact Direction") + coord_flip()
Impacts_by_Flow



# unique_flows <- interaction_data %>%
#   select(starts_with("2.7")) %>%
#   unlist() %>%
#   unique() %>%
#   na.omit()
# 
# unique_impacts <- interaction_data %>%
#   select(starts_with("2.12")) %>%
#   unlist() %>%
#   unique() %>%
#   na.omit()
# 
# # Print unique values to ensure we understand the data structure
# print(unique_flows)
# print(unique_impacts)
# # Count occurrences for each combination
# combination_counts_by_impact <- combinations %>%
#   rowwise() %>%
#   mutate(
#     count = sum(
#       apply(interaction_data, 1, function(row) {
#         row[[Flow]] != "" && row[[Impact]] != "" && 
#           row[[Flow]] == Flow && row[[Impact]] == Impact
#       })
#     ),
#     FlowDirection = case_when(
#       grepl("Increase", Flow) ~ "Increase",
#       grepl("Decrease", Flow) ~ "Decrease",
#       grepl("Complex change", Flow) ~ "Complex change",
#       TRUE ~ NA_character_
#     ),
#     ImpactDirection = case_when(
#       grepl("Increase", Impact) ~ "Increase",
#       grepl("Decrease", Impact) ~ "Decrease",
#       grepl("Complex change", Impact) ~ "Complex change"
#     )
#   ) %>%
#   filter(!is.na(FlowDirection) & !is.na(ImpactDirection)) %>%  # Exclude undefined combinations
#   ungroup()
# 
# # Plot with corrected counts
# ggplot(combination_counts_by_impact_filtered, aes(
#   x = Flow, y = Impact, size = count, color = ImpactDirection, shape = FlowDirection
# )) +
#   geom_point(alpha = 0.7) +
#   facet_wrap(~ImpactDirection, scales = "free") +
#   scale_size_continuous(range = c(3, 10)) +
#   scale_color_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex" = "purple")) +
#   scale_shape_manual(values = c("Increase" = 16, "Decrease" = 17, "Complex" = 15)) +
#   labs(
#     title = "Interaction Between Altered Flows and Impacts by Impact Direction",
#     x = "Altered Flow",
#     y = "Impact",
#     size = "Count",
#     color = "Impact Direction",
#     shape = "Flow Direction"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     panel.grid.major = element_line(color = "grey80", linetype = "dotted")
#   )
# 
# 
# 
