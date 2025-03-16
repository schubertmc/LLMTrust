# trust in oncology
library(ggplot2)
library(tidyverse)
library(strex)
library(patchwork)
library(ggpubr)
library(viridis)
library(ggpubr)
library(ComplexHeatmap)
blues <- c(
  "#173678", 
  "#2e6b9e", 
  "#5b709f",
  "#a1adc8", 
  "#1e3c7c", 
  "#fefefe", 
  "#1d3b7b")


objects_colors <- c("#AC80A0", "#89AAE6", "#3685B5", "#0471A6","grey")
reason_colors_alt <- c(
  "#4A9FC1", # Muted Cyan Blue
  "#A3C4F3", # Soft Sky Blue
  "#2894D3", # Rich Azure
  
  "#B592B8", # Muted Lavender
  "#8C7E75", # Darker Grey
  "grey") # Subtle Steel Blue
# Categories of mistrust
reason_detailed_colors <- c(
  "#4A9FC1",  # Muted Cyan Blue
  "#A3C4F3",  # Soft Sky Blue
  "#2894D3",  # Rich Azure
  "#B592B8",  # Muted Lavender
  "#8C7E75",  # Warm Taupe Grey
  "grey",     # Neutral Grey
  "#F2C1D1",  # Soft Pink
  "#E5E29F"   # Pale Yellow
)

# Define categories for each evaluation type

# Communication Evaluation
communication_factor <- c(
  "Unclear communication", 
  "Conflicting information", 
  "Perceived lack of empathy", 
  "Lack of Integrity", 
  "Other"
)
# Perceived Incompetence of Medical Management
perceived_incompetence_factor <- c(
  "Lack of knowledge/competency", 
  "Inadequate actions - perceived lack of thoroughness", 
  "Inadequate actions - Inadequate pain management", 
  "Inadequate actions - Delayed care", 
  "Inadequate actions - Potential overtreatment", 
  "Inadequate actions - Misdiagnoses", 
  "Inadequate actions - Other", 
  "Other"
)

# Disregard for Patient Concerns
disregard_factor <- c(
  "Symptoms", 
  "Treatment wishes/expectations", 
  "Mental health problems", 
  "Other medical conditions", 
  "Other"
)

# Profit-Driven Medical Management
profit_driven_factor <- c(
  "By Insurance", 
  "By Health care providers", 
  "Other"
)

# Lack of Trust in Medical Procedures
lack_of_trust_factor <- c(
  "Treatment", 
  "Diagnosis", 
  "Lack of prioritization of cancer research", 
  "General diffuse mistrust of medical procedures", 
  "Other"
)

# Other
other_factor <- c(
  "Unspecified issue",
  "Multiple overlapping concerns"
)

# Combine all into a named list for easy access
FACTORS <- list(
  "Communication" = communication_factor,
  "Perceived Incompetence of Medical Management" = perceived_incompetence_factor,
  "Disregard for Patient Concerns" = disregard_factor,
  "Profit-Driven Medical Management" = profit_driven_factor,
  "Lack of Trust in Medical Procedures" = lack_of_trust_factor,
  "Other" = other_factor
)


## Paths 

setwd("../plots/")
saving_dir = "../plots/"
data_dir = "../validation/"

# Human Validation Post
# path <- "../B_eval_all_vBatch_1.csv"
# data <- read.csv(path)
# data$post_id   <- str_after_last(str_before_last(data$custom_id, "_"),"_")
# 
# set(42)
# validation_sample <- data %>% 
#   sample_n(200)
# dim(validation_sample)
# write.csv(validation_sample, paste0(data_dir, "validation_sample.csv"))

# Read in data, preprocess
path <- "../B_eval_all_vBatch_1.csv"
data <- read.csv(path)
data$subreddit <- str_before_first(data$custom_id, "_filtered")
data$mistrust_trust_NA[data$mistrust_trust_NA == ""] <- "N/A"
data$mistrust_trust_NA <- factor(data$mistrust_trust_NA, levels =rev(c("mistrust", "N/A", "trust")))
data$post_id   <- str_after_last(str_before_last(data$custom_id, "_"),"_")
data$object_of_mistrust_category <- factor(data$object_of_mistrust_category, levels = (c("Healthcare Professionals",  "Healthcare Institutions", "Insurance Providers", "Medical Science",  "Other")))
data$mistrust_reason_category <- factor(data$mistrust_reason_category, levels = c("Communication", 
                                                                                  "Disregard for Patient Concerns", 
                                                                                  "Perceived Incompetence of Medical Management", 
                                                                                  "Profit-Driven Medical Management",
                                                                                  "Lack of Trust in Medical Procedures", 
                                                                                  "Other"
                                                                                  ))

# Analyse

# Counts
nrow(data)
table(data$mistrust_trust_NA)
100*table(data$mistrust_trust_NA) / nrow(data)


# Mistrust counts
data <- data %>% 
  group_by(subreddit) %>%
  mutate(perc_mistrust = sum(mistrust_trust_NA == "mistrust") / n())
mistrust <- data %>% 
  filter(mistrust_trust_NA == "mistrust")
nrow(mistrust)

# Objects
table(mistrust$object_of_mistrust_category)
100*table(mistrust$object_of_mistrust_category) / nrow(mistrust)

# Reasons
table(mistrust$mistrust_reason_category)
100*table(mistrust$mistrust_reason_category) / nrow(mistrust)


# Figure 1a Graphical Abstract

# Figure 2
data$subreddit <- str_replace(data$subreddit, "_", "/")
pa <- ggplot(data, aes(y = reorder(subreddit, perc_mistrust) , fill = mistrust_trust_NA)) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values = rev(c("darkred","grey","lightblue" )), name = "Evaluation")+
  ylab("Subreddit") + 
  xlab("Proportion")+
  scale_x_continuous(labels = scales::percent)  + xlab("")+
  theme_bw()+
  theme(axis.text = element_text(size = 6), 
        text = element_text(color = "black"),
        legend.text = element_text(size = 6), 
        legend.title =element_text(size = 6), 
        legend.key.size = unit(4, "mm"),
        axis.title = element_text(size = 6)
        )



pb <- ggplot(mistrust, aes(y = reorder(subreddit, perc_mistrust), fill = object_of_mistrust_category)) + 
  geom_bar(position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = (objects_colors), name = "Mistrusted entities")+
  ylab("Subreddit") + 
  xlab("Proportion")+
  scale_x_continuous(labels = scales::percent )  + 
  theme_bw()+
  theme(axis.text = element_text(size = 6), 
        text = element_text(color = "black"),
        legend.text = element_text(size = 6), 
        legend.key.size = unit(4, "mm"),
        legend.title =element_text(size = 6), 
        axis.title = element_text(size = 6)
  )


pa + pb
ggarrange(pa, pb, labels = c("A","B"), font.label = list(size=7))
ggsave(paste0(saving_dir, "Fig2_Mistrust vs Trust_v0.png"), width = 6, height = 2, dpi=1000)

conting <- t(table(data$mistrust_trust_NA=="mistrust", data$subreddit))

chi_result <- chisq.test(conting)
print(chi_result)
print(chi_result$residuals)

# Pairwise comparison of proportions with Holm adjustment (default)
pairwise_result <- pairwise.prop.test(conting, p.adjust.method = "bonferroni")
print(pairwise_result)

# Print the results
print(pairwise_result$p.value<0.05)
pairwise_result$p.value <0.05

# Mistrust percentages
data %>% 
  group_by(subreddit) %>%
  summarize(mistrust_perc = 100*sum(mistrust_trust_NA=="mistrust") / n() ) %>% 
  arrange(-mistrust_perc)

# Figure3:
mistrust$CAT <- as.character(mistrust$mistrust_reason_category)
mistrust$CAT[mistrust$CAT == "Perceived Incompetence of Medical Management"] <- "Perceived Incompetence of\nMedical Management"
mistrust$CAT[mistrust$CAT == "Lack of Trust in Medical Procedures"] <- "Lack of Trust in\nMedical Procedures"
mistrust$CAT <- factor(mistrust$CAT, levels = c(
  "Disregard for Patient Concerns", 
  "Perceived Incompetence of\nMedical Management", 
  "Communication", 
                                                "Profit-Driven Medical Management",
                                                "Lack of Trust in\nMedical Procedures", 
                                                "Other"
))

mistrust <- mistrust %>% 
  group_by(subreddit) %>% 
  mutate(perc_disreg = sum(CAT==  "Disregard for Patient Concerns") / n()) 

px <- ggplot(mistrust, aes(y = reorder(subreddit, perc_disreg), fill = CAT)) + 
  geom_bar(position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = reason_colors_alt, name = "Reason\nfor Mistrust")+
  ylab("Subreddit") + 
  xlab("Proportion")+
  scale_x_continuous(labels = scales::percent )  + 
  theme_bw()+
  theme(axis.text = element_text(size = 6), 
        text = element_text(color = "black"),
        legend.text = element_text(size = 6), 
        legend.key.size = unit(3, "mm"),
        legend.title =element_text(size = 6), 
        axis.title = element_text(size = 6), 
        legend.position ="bottom",
        legend.justification = c(0.7,0), #"bottom", 
        legend.box = "vertical", 
        legend.box.margin = margin(#r = 20, 
                                   t=0, b=0),
        plot.margin = margin(c(0,0,0,0))
  ) + 

  guides(fill=guide_legend(ncol=1))



px2 <- ggplot(mistrust %>% 
         group_by(CAT) %>%
         summarize(freq = n()), aes(y  = reorder(CAT, freq), x = freq, fill = CAT)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = reason_colors_alt, name = "Reason\nfor Mistrust")+
  ylab("Subreddit") + 
  xlab("Proportion")+
  #scale_y_reverse()+
  #scale_x_continuous(labels = scales::percent )  + 
  theme_bw()+
  theme(axis.text = element_text(size = 6), 
        text = element_text(color = "black"),
        legend.text = element_text(size = 6), 
        legend.key.size = unit(3, "mm"),
        legend.title =element_text(size = 6), 
        axis.title = element_text(size = 6), 
        #legend.position ="bottom",
        legend.justification = c(0.7,0), #"bottom", 
        legend.box = "vertical", 
        legend.position = "none",
        legend.box.margin = margin(r = 20) ,
        #plot.margin = margin(c(0,0,0,0))

  ) + 
  guides(fill=guide_legend(ncol=1))


conting <- table(data$subreddit, data$mistrust_reason_category)
chisq.test(conting)

#ggsave(plot=px, paste0(saving_dir, "Fig3a_Reasons.png"), width = 3, height = 3.5, dpi = 1000)

fine_base <- read.csv("../FINE_B_eval_all_vBatch_Fine.csv")
fine_base$post_id   <- str_after_last(str_before_last(fine_base$custom_id, "_"),"_")

fine <- merge(mistrust, fine_base, by= "post_id")

categories <- unique(fine$mistrust_reason_category)
categories <- rev(categories)
# Define data subsets for each category
cur_dat_communication <- fine %>% filter(mistrust_reason_category == "Communication")
cur_dat_perceived_incompetence <- fine %>% filter(mistrust_reason_category == "Perceived Incompetence of Medical Management")
cur_dat_disregard <- fine %>% filter(mistrust_reason_category == "Disregard for Patient Concerns")
cur_dat_profit_driven <- fine %>% filter(mistrust_reason_category == "Profit-Driven Medical Management")
cur_dat_lack_of_trust <- fine %>% filter(mistrust_reason_category == "Lack of Trust in Medical Procedures")
cur_dat_other <- fine %>% filter(mistrust_reason_category == "Other")

# List of values for factor levels
list_of_values_communication <- FACTORS[["Communication"]]
list_of_values_incompetence <- FACTORS[["Perceived Incompetence of Medical Management"]]
list_of_values_disregard <- FACTORS[["Disregard for Patient Concerns"]]
list_of_values_profit_driven <- FACTORS[["Profit-Driven Medical Management"]]
list_of_values_lack_of_trust <- FACTORS[["Lack of Trust in Medical Procedures"]]
list_of_values_other <- FACTORS[["Other"]]

# Reason colors (assuming defined earlier)
reason_colors <- reason_detailed_colors

# Individual ggplots
plot_communication <- ggplot(cur_dat_communication, 
                             aes(y = "", fill = factor(category, levels = list_of_values_communication))) +
  geom_bar(position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = reason_colors, name = "") +
  ylab("") + scale_x_continuous(labels = scales::percent)  + xlab("")+
    theme_bw() +
  ggtitle("Communication") +
  theme(
    axis.text = element_text(size = 6),
    text = element_text(color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(2, "mm"),
    legend.title = element_text(size = 6),
    legend.position = "right",
    legend.margin = margin(c(0, 0, 0, 0)),
    plot.title = element_text(size = 6), 
    plot.margin = margin(c(1,1,0,0))
  )
nrow(cur_dat_communication)
table(cur_dat_communication$category)
100*table(cur_dat_communication$category) / nrow(cur_dat_communication)


plot_perceived_incompetence <- ggplot(cur_dat_perceived_incompetence, 
                                      aes(y = "", fill = factor(category, levels = list_of_values_incompetence))) +
  geom_bar(position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = reason_colors, name = "") +
  ylab("") + scale_x_continuous(labels = scales::percent)  + xlab("")+
  theme_bw() +
  ggtitle("Perceived Incompetence of Medical Management") +
  theme(
    axis.text = element_text(size = 6),
    text = element_text(color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(2, "mm"),
    legend.title = element_text(size = 6),
    legend.position = "right",
    legend.margin = margin(c(0, 0, 0, 0)),
    plot.title = element_text(size = 6), 
    plot.margin = margin(c(1,1,0,0))
  )

nrow(cur_dat_perceived_incompetence)
table(cur_dat_perceived_incompetence$category)
100*table(cur_dat_perceived_incompetence$category) / nrow(cur_dat_perceived_incompetence)


plot_disregard <- ggplot(cur_dat_disregard, 
                         aes(y = "", fill = factor(category, levels = list_of_values_disregard))) +
  geom_bar(position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = reason_colors, name = "") +
  ylab("") + scale_x_continuous(labels = scales::percent)  + xlab("")+
  theme_bw() +
  ggtitle("Disregard for Patient Concerns") +
  theme(
    axis.text = element_text(size = 6),
    text = element_text(color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(2, "mm"),
    legend.title = element_text(size = 6),
    legend.position = "right",
    legend.margin = margin(c(0, 0, 0, 0)),
    plot.title = element_text(size = 6), 
    plot.margin = margin(c(1,1,0,0))
  )

nrow(cur_dat_disregard)
table(cur_dat_disregard$category)
100*table(cur_dat_disregard$category) / nrow(cur_dat_disregard)


plot_profit_driven <- ggplot(cur_dat_profit_driven, 
                             aes(y = "", fill = factor(category, levels = list_of_values_profit_driven))) +
  geom_bar(position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = reason_colors, name = "") +
  ylab("") + scale_x_continuous(labels = scales::percent)  + xlab("")+
  theme_bw() +
  ggtitle("Profit-Driven Medical Management") +
  theme(
    axis.text = element_text(size = 6),
    text = element_text(color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(2, "mm"),
    legend.title = element_text(size = 6),
    legend.position = "right",
    legend.margin = margin(c(0, 0, 0, 0)),
    plot.title = element_text(size = 6), 
    plot.margin = margin(c(1,1,0,0))
  )

plot_lack_of_trust <- ggplot(cur_dat_lack_of_trust, 
                             aes(y = "", fill = factor(category, levels = list_of_values_lack_of_trust))) +
  geom_bar(position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = reason_colors, name = "") +
  ylab("") + scale_x_continuous(labels = scales::percent)  + xlab("")+
  theme_bw() +
  ggtitle("Lack of Trust in Medical Procedures") +
  theme(
    axis.text = element_text(size = 6),
    text = element_text(color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(2, "mm"),
    legend.title = element_text(size = 6),
    legend.position = "right",
    legend.margin = margin(c(0, 0, 0, 0)),
    plot.title = element_text(size = 6), 
    plot.margin = margin(c(1,1,0,0))
  )

combined_plot <- plot_disregard + plot_perceived_incompetence + 
  plot_communication + plot_profit_driven + 
  plot_lack_of_trust +
  plot_layout(ncol = 1, guides = "collect")

# Display the combined plot
print(combined_plot)


ggarrange(px, combined_plot, widths = c(1,1.5))
ggsave( paste0(saving_dir, "Fig3both_Reasons_v5.png"), width = 7.5, height = 3.5, dpi = 1000)
px_comb <- px2 / px + plot_layout(heights = c(1,1.5))

#px_comb <- px2 / px + plot_layout(heights = c(1,2))
px_comb <- ggarrange(px2, px, ncol = 1, 
                     heights = c(1,1.5),
                     labels = c("A", "B"), font.label = list(size=7))
ggarrange(px_comb, combined_plot, widths = c(1,1.5), labels = c("A", "C"), font.label = list(size=7))

ggsave( paste0(saving_dir, "Fig3both_Reasons_comb_v0.png"), width = 7.5, height = 4, dpi = 1000)



### TONES
counts <- table(data$tone, data$mistrust_trust_NA) %>% data.frame()
mat <- pivot_wider(counts, names_from = Var2, values_from = Freq) %>% data.frame()
rownames(mat) <- mat$Var1
mat$Var1 <- NULL
print(mat)

chi_result <- chisq.test(mat)
print(chi_result)

mat <- mat * 100
pdf(file = "Fig4_Tone_MistrustTrust.pdf", width = 2.5, height= 2.5)
ht1 <- Heatmap(mat, 
        name = "Distribution\n[%]",
        col = viridis(10, option="inferno", direction = -1), 
        row_names_gp = gpar(fontsize =6), 
        column_names_gp = gpar(fontsize=6),
        heatmap_legend_param = list(
          title_gp = gpar(fontsize = 6),                # Font size for the legend title
          labels_gp = gpar(fontsize = 6)                # Font size for legend labels
        ),
        column_names_rot =45
        )
draw(ht1)
dev.off()

counts <- table(mistrust$tone, mistrust$object_of_mistrust_category) %>% data.frame()
mat <- pivot_wider(counts, names_from = Var2, values_from = Freq) %>% data.frame()
mat$Other <- NULL
rownames(mat) <- mat$Var1
mat$Var1 <- NULL
print(mat)
print(colSums(mat))
mat <- apply(mat, 2, function(x) x / sum(x))
print(mat)


colnames(mat) <- c("Healthcare Professionals", 
                   "Healthcare Institutions"  ,
                   "Insurance Providers"  , 
                   "Medical Science")



mat <- mat * 100
pdf(file = "Fig4b_Tone_MistrustTrust.pdf", width = 2.5, height= 2.5)
ht2 <- Heatmap(mat, 
               name = "Distribution\n[%]",
               col = viridis(10, option="inferno", direction = -1), 
               row_names_gp = gpar(fontsize =6), 
               column_names_gp = gpar(fontsize=6),
               heatmap_legend_param = list(
                 title_gp = gpar(fontsize = 6),                # Font size for the legend title
                 labels_gp = gpar(fontsize = 6)                # Font size for legend labels
               ),
               column_names_rot =45
)
draw(ht2)
dev.off()


counts <- table(mistrust$tone, mistrust$mistrust_reason_category) %>% data.frame()
mat <- pivot_wider(counts, names_from = Var2, values_from = Freq) %>% data.frame()
mat$Other <- NULL
rownames(mat) <- mat$Var1
mat$Var1 <- NULL
print(mat)
print(colSums(mat))
mat <- apply(mat, 2, function(x) x / sum(x))
print(mat)

colnames(mat) <- c(
  "Communication",
  "Disregard for Patient Concerns",
  "Perceived Incompetence of Medical Management",
  "Profit Driven Medical Management",
  "Lack of Trust in Medical Procedures"
)

colnames(mat) <- c(
  "Communication",
  "Disregard Pt. Concerns",
  "Perceived Incompetence",
  "Profit Driven",
  "Lack of Trust in Med. Proc."
)

mat <- mat * 100
pdf(file = "Fig4c_Tone_MistrustTrust.pdf", width = 2.5, height= 2.5)
ht3 <- Heatmap(mat, 
               name = "Distribution\n[%]",
               col = viridis(10, option="inferno", direction = -1), 
               row_names_gp = gpar(fontsize =6), 
               column_names_gp = gpar(fontsize=6),
               heatmap_legend_param = list(
                 title_gp = gpar(fontsize = 6),                # Font size for the legend title
                 labels_gp = gpar(fontsize = 6)                # Font size for legend labels
               ),
               column_names_rot =45
)
draw(ht3)
dev.off()



# initial tone count 
t <- read.csv("../tone_counts.csv")

t <- t %>% 
  group_by(count) %>%
  summarize(terms = paste(X, collapse = ",")) %>% 
  arrange(desc(count)) 
write.csv(t, "../plots/tone_table.csv")

# exploratory dataset

eval <- read.csv("../evaluation_all_bound_v50.csv")
x <- data.frame(row = 1:length(unique(eval$mistrust_reason)), reason = unique(eval$mistrust_reason))
View(x)
write_csv(x, "../plots/reasons.csv")

all <- read.csv("../B_eval_all_vBatch_1.csv")
all$subreddit <- str_before_first(all$custom_id, "_filtered")

head(all)
colnames(all)
all <- all %>%
  select(c(subreddit,mistrust_trust_NA,object_of_mistrust,mistrust_reason, object_of_mistrust_category,mistrust_reason_category, tone  ))
write.csv(all, "../MedicalMistrustInOncologyDataset.csv")

# Supplementary Data
ggplot(data, aes(x = reorder(subreddit, -table(subreddit)[subreddit]))) + 
  geom_bar(color="darkblue", fill = blues[2]) + 
  theme_bw() + 
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
  ggtitle("Number of Posts per Subreddit")
ggsave( paste0(saving_dir, "SupplementaryFig1.png"), width = 6, height = 4, dpi = 1000)

time <- read.csv("../data/RD/time_details.csv")
dim(data)
mer <- merge(data, time, by = "post_id")


#rm(data); rm(time); gc()
# Convert created_utc to a Date format if not already
mer$year <- year(as_datetime(mer$created_utc))  # Use as_datetime for better handling
mer$q <- quarter(as_datetime(mer$created_utc))
# filter out all data after 2024 9 30

mer$quarter
# Group data by quarter and subreddit, then count posts
mer_aggregated <- mer %>%
  mutate(quarter = paste0(year, " Q", q)) %>%
  group_by(quarter, subreddit) %>%
  summarise(count = n(), .groups = "drop", 
            n_mistrust = sum(mistrust_trust_NA == "mistrust"), 
            perc_mistrust = n_mistrust / count
            ) %>% 
  filter(quarter != "2024 Q4")
# filter out all data after 2024 9 30


# Plot line chart
ggplot(mer_aggregated, aes(x = quarter, y = count, color = subreddit, group = subreddit)) + 
  geom_line() + 
  geom_point(size = 0.5)+
  theme_bw() + 
  labs(
    title = "Post Counts over Time per Subreddit",
    x = "Quarter",
    y = "Number of Posts"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom"
        )
ggsave( paste0(saving_dir, "SupplementaryFig2.png"), width = 9, height = 4.5, dpi = 1000)




# Plot line chart
mer_aggregated %>%
  group_by(quarter, subreddit) %>%
  mutate(n_pqs = n())
ggplot(mer_aggregated %>%
         filter(count>=100)
         , aes(x = quarter, y = 100*perc_mistrust, color = subreddit, group = subreddit)) + 
  geom_line() + 
  geom_point(size = 0.5)+
  theme_bw() + 
  ylim(0,100)+
  labs(
    title = "Mistrust Percentage Over Time per Subreddit",
    x = "Quarter",
    y = "Percentage of Posts [%]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom"
  )
ggsave( paste0(saving_dir, "SupplementaryFig3.png"), width = 8, height = 4.5, dpi = 1000)




agg <- mer %>%
  mutate(quarter = paste0(year, " Q", q)) %>%
  group_by(quarter) %>%
  summarise(count = n(), .groups = "drop", 
            n_mistrust = sum(mistrust_trust_NA == "mistrust"), 
            perc_mistrust = n_mistrust / count
  ) %>% 
  filter(quarter != "2024 Q4")



ggplot(agg %>% 
         filter(count>=100)
         , aes(x = quarter, y = 100*perc_mistrust, group="")) + 
  geom_line(color = blues[1]) + 
  geom_point(size = 0.5, color = blues[3])+
  theme_bw() + 
  ylim(0,100)+
  labs(
    title = "Mistrust Percentage Over Time",
    x = "Quarter",
    y = "Percentage of Posts [%]"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom"
  )
ggsave( paste0(saving_dir, "SupplementaryFig4.png"), width = 8, height = 4.5, dpi = 1000)
