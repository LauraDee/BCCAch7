#Figures of drivers by flow and impacts and NCP
graphics.off()
rm(list=ls())

#load libraries
library(ggplot2)

setwd("/Users/lade8828/Library/CloudStorage/OneDrive-UCB-O365/Documents/GitHub/BCCAch7/data")
reshaped_data <- read.csv("data/003_output_byFlow.csv")

glimpse(reshaped_data)

# Select and reshape relevant columns
#altered_flow_cols <- names(reshaped_data)[grepl("Altered Flow", names(reshaped_data))]
#impact_cols <- names(reshaped_data)[grepl("Impact", names(reshaped_data))]

#updated col names
altered_flow_cols <- names(reshaped_data)[grepl("2.7.Altered.Flow.", names(reshaped_data))]
impact_cols <- names(reshaped_data)[grepl("2.12.Impact.", names(reshaped_data))]

# Filter relevant columns for Altered Flow and Impact
"%notin%" <- Negate("%in%")

interaction_data <- reshaped_data %>% filter(`Citation` %notin% c("TEST","test","Test")) %>% 
  select(all_of(c(altered_flow_cols, impact_cols))) %>%
  mutate(row_id = row_number())  %>%
  filter(!if_all(-row_id, ~ .x == ""))
glimpse(interaction_data)

#flow_columns <- names(interaction_data)[grepl("Altered Flow", names(interaction_data))]
#impact_columns <- names(interaction_data)[grepl("Impact", names(interaction_data))]

#update column names
flow_columns <- names(interaction_data)[grepl("2.7.Altered.Flow.", names(interaction_data))]
impact_columns <- names(interaction_data)[grepl("2.12.Impact.", names(interaction_data))]

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

# Plot with facets by Impact direction

ggplot(combination_counts_by_impact, aes(x = Flow, y = Impact, size = count, color = ImpactDirection)) +
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

























unique_flows <- interaction_data %>%
  select(starts_with("2.7")) %>%
  unlist() %>%
  unique() %>%
  na.omit()

unique_impacts <- interaction_data %>%
  select(starts_with("2.12")) %>%
  unlist() %>%
  unique() %>%
  na.omit()

# Print unique values to ensure we understand the data structure
print(unique_flows)
print(unique_impacts)
# Count occurrences for each combination
combination_counts_by_impact <- combinations %>%
  rowwise() %>%
  mutate(
    count = sum(
      apply(interaction_data, 1, function(row) {
        row[[Flow]] != "" && row[[Impact]] != "" && 
          row[[Flow]] == Flow && row[[Impact]] == Impact
      })
    ),
    FlowDirection = case_when(
      grepl("Increase", Flow) ~ "Increase",
      grepl("Decrease", Flow) ~ "Decrease",
      grepl("Complex change", Flow) ~ "Complex change",
      TRUE ~ NA_character_
    ),
    ImpactDirection = case_when(
      grepl("Increase", Impact) ~ "Increase",
      grepl("Decrease", Impact) ~ "Decrease",
      grepl("Complex change", Impact) ~ "Complex change"
    )
  ) %>%
  filter(!is.na(FlowDirection) & !is.na(ImpactDirection)) %>%  # Exclude undefined combinations
  ungroup()

# Plot with corrected counts
ggplot(combination_counts_by_impact_filtered, aes(
  x = Flow, y = Impact, size = count, color = ImpactDirection, shape = FlowDirection
)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ImpactDirection, scales = "free") +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex" = "purple")) +
  scale_shape_manual(values = c("Increase" = 16, "Decrease" = 17, "Complex" = 15)) +
  labs(
    title = "Interaction Between Altered Flows and Impacts by Impact Direction",
    x = "Altered Flow",
    y = "Impact",
    size = "Count",
    color = "Impact Direction",
    shape = "Flow Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )



