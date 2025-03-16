library(tidyverse)
library(readxl)
library(ggalluvial)

# colormap with subdivisions
library(scico)

# Define the colormap
palette_name <- "managua"

# Define main color positions along the colormap (scaled to [0,1])
main_positions <- c(6, 23, 41, 59, 77, 94) / 100

# Define number of subdivisions for each main section
subdivisions <- c(3, 7, 1, 4, 6, 5)

color_list = c()

for (i in 1:length(main_positions)) {
  interval_start <- main_positions[i] - 0.06  # -4 in scale [0,100]
  interval_end <- main_positions[i] + 0.06
  colors <- scico(subdivisions[i], palette = palette_name, begin = interval_start, end = interval_end)
  color_list <- c(color_list, colors)
}

dataframe <- read_excel("LLM_Reasons.xlsx")
# Create a new column combining 'Major Category assigned' and 'Minor Category assigned'
dataframe <- dataframe %>%
    mutate(Major_Minor_Category = paste0(`Major_cat`, " - ", `Minor_cat`))

dataframe$Freq = 1

library(dplyr)
library(ggsankey)

# Prepare the data for the Sankey diagram
sankey_data <- dataframe %>%
    make_long(`LLM_reason`, Major_cat, Major_Minor_Category)

# Add a new column 'label' based on the conditions
sankey_data <- sankey_data %>%
    mutate(label = ifelse(x == "LLM_reason", "", node))



# Generate a discrete color palette using viridis
library(viridis)

unique_reasons <- sort(unique(dataframe$LLM_reason), decreasing = TRUE)
color_palette <- viridis(length(unique_reasons))

# Create a named vector for the color palette
names(color_palette) <- unique_reasons
# for the unique Major_Minor_Category
# Load necessary library

# Assign colors to unique Major_Minor_Category values in descending alphabetical order
unique_categories <- sort(unique(dataframe$Major_Minor_Category), decreasing = TRUE)
color_map <- setNames(color_list[1:length(unique_categories)], unique_categories)

# Assign colors to unique Major_cat values in descending alphabetical order
unique_major_cats <- sort(unique(dataframe$Major_cat), decreasing = TRUE)
major_cat_colors <- scico(length(unique_major_cats), palette = palette_name)
major_cat_color_map <- setNames(major_cat_colors, unique_major_cats)

# Combine all color maps into one large named character vector
combined_color_map <- c(color_palette, color_map, major_cat_color_map)

# Create the Sankey diagram with adjusted vertical space for the first x Category
sankey_plot <- ggplot(sankey_data, 
                      aes(x = x, next_x = next_x, node = node, 
                          next_node = next_node, fill = factor(node))) +
  geom_sankey(flow.alpha = 0.5, node.color = "black", space = 0.4) +
  #geom_sankey_label(aes(label = label), size = 1, space = 0.4, fill = "white") +
  theme_sankey(base_size = 4) +
  ggtitle("Sankey Diagram of LLM Reasons") +
  scale_fill_manual(values = combined_color_map, guide = "none")

# Save the Sankey diagram
ggsave("sankey_diagram.svg", sankey_plot, width = 20, height = 20, units = "in")

# Create the Sankey diagram with adjusted vertical space for the first x Category
sankey_plot <- ggplot(sankey_data, 
                      aes(x = x, next_x = next_x, node = node, 
                          next_node = next_node, fill = factor(node))) +
  geom_sankey(flow.alpha = 0.5, node.color = "black", space = 0.4) +
  geom_sankey_label(aes(label = label), size = 2, space = 0.4, fill = "white") +
  theme_sankey(base_size = 4) +
  ggtitle("Sankey Diagram of LLM Reasons") +
  scale_fill_manual(values = combined_color_map, guide = "none")

ggsave("sankey_diagram_w_labels.svg", sankey_plot, width = 20, height = 20, units = "in")

# Create a legend for the color_map colormap
color_map_df <- data.frame(name = names(color_map), color = color_map)
color_map_df$name <- factor(color_map_df$name, levels = sort(unique(color_map_df$name), decreasing = TRUE))
color_map_df$short <- sub(".*- ", "", color_map_df$name)

legend_plot <- ggplot(color_map_df, 
                      aes(x = short, y = 1, fill = name)) +
  geom_tile() +
  scale_fill_manual(values = color_map, labels = color_map_df$short) +
  ggtitle("Legend for Major_Minor_Category")

# Save the legend plot
ggsave("legend_colormap.pdf", legend_plot, width = 20, height = 20, units = "in")

# Count the occurrences of unique elements in "Major_cat"
major_cat_counts <- dataframe %>%
    group_by(Major_cat) %>%
    summarise(count = n())

# Print the counts
print(major_cat_counts)