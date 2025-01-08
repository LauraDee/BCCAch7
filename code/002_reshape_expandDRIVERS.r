
# Select and reshape relevant columns
levels(factor(reshaped_data$"2.4 Climate Driver"))
glimpse(reshaped_data)



library(dplyr)
library(tidyr)
library(stringr)

relabel_drivers <- read_excel("C:\\Users\\basti\\Documents\\GitHub\\BCCAch7\\data\\refined_drivers\\otherclimatedrivers_lookup.xlsx") %>% as.data.frame()
glimpse(relabel_drivers)

relabel_drivers <- relabel_drivers %>%
  mutate(raw2 = gsub("[^a-zA-Z ]", "", raw)) %>% # Keep only letters and spaces
  mutate(raw2 = gsub("\\s+", " ", raw2)) %>%     # Replace multiple spaces with a single space
  mutate(raw2 = trimws(raw2))                   # Trim leading and trailing spaces

# Check the result
print(relabel_drivers$raw2)

# Combine cleaned terms into a search pattern with "|" (OR operator for regex)
search_pattern <- paste(relabel_drivers$raw2, collapse = "|")

# Filter rows where any string from the combined `raw` strings is found
# Create a named vector for replacements from relabel_drivers
replacement_map <- setNames(relabel_drivers$driver, relabel_drivers$raw2)

# Replace matching strings in reshaped_data$driver
updated_data <- reshaped_data %>%
  rename(driver = `2.4 Climate Driver`) %>%
  mutate(driver = str_replace_all(driver, replacement_map))






# Rename the column for easier handling
reshaped_data_d <- updated_data  %>% select(ID,driver)%>%
  group_by(ID) %>%
  slice_head(n = 1) %>% # Keep only the first row for each DOI
  ungroup() # Ungroup the data

# Split the 'driver' column into individual components
all_drivers <- reshaped_data_d$driver %>%
  strsplit(",") %>%         # Split by ", " to separate categories
  unlist() %>%               # Unlist to make it a vector
  unique() %>%               # Get unique values
  trimws() %>%               # Remove leading and trailing whitespaces
  sort()                     # Sort for consistency
glimpse(all_drivers)
# Create new columns for each unique driver
reshaped_data_expanded <- reshaped_data_d %>%
  mutate(driver_list = strsplit(driver, ",")) %>% # Split the drivers
  unnest_longer(driver_list) %>%                  # Expand rows for each driver
  mutate(driver_list = trimws(driver_list)) %>%   # Remove extra spaces
  pivot_wider(
    names_from = driver_list,
    values_from = driver_list,
    values_fn = length, # Set to 1 if present, otherwise NA
    values_fill = list(driver_list = 0) # Replace NA with 0
  ) %>%
  mutate(across(everything(), ~. == 1, .names = "driver:{.col}")) # Convert to TRUE/FALSE

# Drop intermediate columns (optional)
reshaped_data_expanded <- reshaped_data_expanded #%>%
  #select(-driver)

  reshaped_data_expanded %>% filter(Floods==TRUE)

glimpse(reshaped_data_expanded)
dim(reshaped_data_expanded)
levels(factor(reshaped_data_expanded$ID))

reshaped_data_expanded <- reshaped_data_expanded %>%
  select(contains(c("driver","ID")))

glimpse(reshaped_data_expanded)

reshaped_data_drivers <- reshaped_data %>% left_join(reshaped_data_expanded,by="ID") %>% select(-c("driver:ID","driver","driver:driver",`driver:la la la`,`driver:lala`))

glimpse(reshaped_data_drivers)

write.csv(reshaped_data_drivers, "data/reshaped_v3_driversNOOTHERS_jan8.csv")



################ END


















































































































driver_cols <- names(reshaped_data_drivers)[grepl("driver:", names(reshaped_data_drivers))]
# Filter relevant columns for Altered Flow and driver

"%notin%" <- Negate("%in%")

interaction_data <- reshaped_data_drivers %>% filter(Citation %notin% c("TEST","test","Test")) %>% 
  select(all_of(c(altered_flow_cols, driver_cols))) %>%
  mutate(row_id = row_number())  %>%
  filter(!if_all(-row_id, ~ .x == ""))
glimpse(interaction_data)

flow_columns <- names(interaction_data)[grepl("Altered Flow", names(interaction_data))]
driver_columns <- names(interaction_data)[grepl("driver", names(interaction_data))]

# Generate all possible combinations of flows and drivers
combinations <- expand.grid(
  Flow = flow_columns,
  driver = driver_columns,
  stringsAsFactors = FALSE
)

# Count occurrences of each combination in the data
combination_counts <- combinations %>%
  rowwise() %>% 
  mutate(
    count = sum(
      interaction_data[[Flow]] != "" & interaction_data[[driver]] != "",
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Convert to a dataframe for easy viewing
combination_counts_df <- as.data.frame(combination_counts)

# Preview the result
glimpse(combination_counts_df)

library(ggplot2)

# Create the plot
ggplot(combination_counts_df, aes(x = Flow, y = driver, size = count)) +
  geom_point(color = "blue", alpha = 0.7) +  # Use points to represent combinations
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range for better visibility
  labs(
    title = "Interaction Between Altered Flows and drivers",
    x = "Altered Flow",
    y = "driver",
    size = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )















# Count occurrences for each driver direction
combination_counts_by_driver <- combinations %>%
  rowwise() %>%
  mutate(
    Increase = sum(
      interaction_data[[Flow]] != "" & interaction_data[[driver]] == "Increase",
      na.rm = TRUE
    ),
    Decrease = sum(
      interaction_data[[Flow]] != "" & interaction_data[[driver]] == "Decrease",
      na.rm = TRUE
    ),
    Complex = sum(
      interaction_data[[Flow]] != "" & interaction_data[[driver]] == "Complex change",
      na.rm = TRUE
    )
  ) %>%
  pivot_longer(cols = c(Increase, Decrease, Complex), names_to = "driverDirection", values_to = "count") %>%
  ungroup()

# Plot with facets by driver direction
library(ggplot2)

ggplot(combination_counts_by_driver, aes(x = Flow, y = driver, size = count, color = driverDirection)) +
  geom_point(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~driverDirection, scales = "free") +  # Create facets for each driver direction
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex" = "purple")) +
  labs(
    title = "Interaction Between Altered Flows and drivers by driver Direction",
    x = "Altered Flow",
    y = "driver",
    size = "Count",
    color = "driver Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )



# Filter out rows with count == 0
combination_counts_by_driver_filtered <- combination_counts_by_driver %>%
  filter(count > 0)

# Plot with facets by driver direction, excluding zero counts
ggplot(combination_counts_by_driver_filtered, aes(x = Flow, y = driver, size = count, color = driverDirection)) +
  geom_point(alpha = 0.7) +  # Add points with alpha transparency
  facet_wrap(~driverDirection, scales = "free") +  # Create facets for each driver direction
  scale_size_continuous(range = c(3, 10)) +  # Adjust size range
  scale_color_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex" = "purple")) +
  labs(
    title = "Interaction Between Altered Flows and drivers by driver Direction",
    x = "Altered Flow",
    y = "driver",
    size = "Count",
    color = "driver Direction"
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

unique_drivers <- interaction_data %>%
  select(starts_with("2.12")) %>%
  unlist() %>%
  unique() %>%
  na.omit()

# Print unique values to ensure we understand the data structure
print(unique_flows)
print(unique_drivers)
# Count occurrences for each combination
combination_counts_by_driver <- combinations %>%
  rowwise() %>%
  mutate(
    count = sum(
      apply(interaction_data, 1, function(row) {
        row[[Flow]] != "" && row[[driver]] != "" && 
        row[[Flow]] == Flow && row[[driver]] == driver
      })
    ),
    FlowDirection = case_when(
      grepl("Increase", Flow) ~ "Increase",
      grepl("Decrease", Flow) ~ "Decrease",
      grepl("Complex change", Flow) ~ "Complex change",
      TRUE ~ NA_character_
    ),
    driverDirection = case_when(
      grepl("Increase", driver) ~ "Increase",
      grepl("Decrease", driver) ~ "Decrease",
      grepl("Complex change", driver) ~ "Complex change"
    )
  ) %>%
  filter(!is.na(FlowDirection) & !is.na(driverDirection)) %>%  # Exclude undefined combinations
  ungroup()

# Plot with corrected counts
ggplot(combination_counts_by_driver_filtered, aes(
  x = Flow, y = driver, size = count, color = driverDirection, shape = FlowDirection
)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~driverDirection, scales = "free") +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(values = c("Increase" = "green", "Decrease" = "red", "Complex" = "purple")) +
  scale_shape_manual(values = c("Increase" = 16, "Decrease" = 17, "Complex" = 15)) +
  labs(
    title = "Interaction Between Altered Flows and drivers by driver Direction",
    x = "Altered Flow",
    y = "driver",
    size = "Count",
    color = "driver Direction",
    shape = "Flow Direction"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted")
  )


graphics.off()
rm(list=ls())
