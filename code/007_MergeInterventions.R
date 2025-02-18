data <- read_excel("data/data_cleaning/reshaped_3_drivers_Jan10_interventions_feb13.xlsx", sheet = "combined_coding")
df1 <- read.csv('data/006_output_recoded.csv')
levels(factor(df1$X2.1.Flow.Type))
data_selected <- data %>% select(ID_DOI_by_Flow, Response.Text, final.codes, Response.Mentioned.) %>% rename(intervention.text = Response.Text,intervention.codes=final.codes,Response.Mentioned.verified = Response.Mentioned.)
merged_df <- df1 %>% left_join(data_selected, by = "ID_DOI_by_Flow") %>% 
mutate(Response.Mentioned.=ifelse(!is.na(Response.Mentioned.verified),Response.Mentioned.verified,Response.Mentioned.)) %>% 
dplyr::select(-Response.Mentioned.verified)
glimpse(merged_df)
## Add new column 

unique_interventions <- merged_df %>%
  tidyr::separate_rows(intervention.codes, sep = ";\\s*") %>%
  distinct(intervention.codes) %>%
  filter(intervention.codes != "" & intervention.codes != "NA") %>%
  arrange(intervention.codes) # Sort the interventions alphabetically

# Print the cleaned list of interventions
print(unique_interventions)
interventions_vector <- unique_interventions$intervention.codes

fleshed_df <- merged_df

# Iterate over each intervention and create a new column
for (intervention in interventions_vector) {
  fleshed_df <- fleshed_df %>%
    mutate(!!sym(intervention) := as.integer(str_detect(intervention.codes, fixed(intervention))))
}


fleshed_df <- fleshed_df %>%
  rename_with(~paste("intervention:", .x),
              .cols = interventions_vector)

glimpse(fleshed_df)
write.csv(fleshed_df,'data/007_output_interventions.csv')

fleshed_df <- fleshed_df %>% filter(X2.1.Flow.Type %notin% c("Remove","Recode"))

# We'll gather only the intervention columns
intervention_data <- fleshed_df %>%
  pivot_longer(
    cols = starts_with("intervention:"),
    names_to = "intervention",
    values_to = "value"
  ) %>%
  filter(value == 1) %>%  # Keep only rows where intervention is applied
  group_by(X2.1.Flow.Type, intervention) %>%
  summarise(count = n(), .groups = 'drop')  # Count each intervention per flow type

#### FUNCTIONING PLOTS - DO NOT ERASE

# ggplot(intervention_data, aes(x = X2.1.Flow.Type, y = count, fill = intervention)) +
#   geom_bar(stat = "identity", position = position_dodge()) +  # Use dodge to group bars
#   theme_minimal() +
#   labs(
#     title = "Intervention Frequency by Flow Type",
#     x = "Flow Type",
#     y = "Count of Interventions",
#     fill = "Intervention"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
# library(tidyverse)

# # Gather the intervention columns and convert to long format
# intervention_data <- fleshed_df %>%
#   pivot_longer(
#     cols = starts_with("intervention:"),
#     names_to = "intervention",
#     values_to = "value"
#   ) %>%
#   filter(value == 1) %>%  # Consider only cases where the intervention is applied
#   group_by(X2.1.Flow.Type, intervention) %>%
#   summarise(count = n(), .groups = 'drop')  # Count each intervention per flow type

# Create a complete grid to fill in missing combinations with zeros
intervention_matrix <- expand_grid(
  X2.1.Flow.Type = unique(fleshed_df$X2.1.Flow.Type),
  intervention = unique(intervention_data$intervention)
) %>%
  left_join(intervention_data, by = c("X2.1.Flow.Type", "intervention")) %>%
  replace_na(list(count = 0))  # Replace NA values with zero

  glimpse(intervention_matrix)
  intervention_matrix$intervention <- sub("^intervention: ", "", intervention_matrix$intervention)

ggplot(intervention_matrix, aes(x = intervention, y = X2.1.Flow.Type, fill = count)) +
  geom_tile() +  # Use geom_tile() for heatmap squares
  scale_fill_gradient(low = "white", high = "steelblue", name = "Count") +
  theme_minimal() +
  labs(
    title = "Heatmap of Interventions by Flow Type",
    x = "Intervention",
    y = "Flow Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  # Rotate x-axis labels for better readability





# Create a complete grid to fill in missing combinations with zeros



glimpse(fleshed_df)
