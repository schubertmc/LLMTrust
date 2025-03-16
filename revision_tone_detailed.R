
library(ggplot2)
library(viridis)

### Functions


# Define name replacements
name_replacements <- c(
  "trust" = "Trust", 
  "mistrust" = "mistrust", 
  "NA." = "NA",
  
  
  "Healthcare.Professionals" = "Healthcare Professionals",
  "Healthcare.Institutions" = "Healthcare Institutions",
  "Insurance.Providers" = "Insurance Providers",
  "Medical.Science" = "Medical Science",
  
  "Communication" = "Communication",
  "Disregard.for.Patient.Concerns" = "Disregard Pt. Concerns",
  "Lack.of.Trust.in.Medical.Procedures" = "Lack of Trust in Med. Proc.",
  "Perceived.Incompetence.of.Medical.Management" = "Perceived Incompetence",
  "Profit.Driven.Medical.Management" = "Profit Driven"
)


rename_and_format_cols <- function(df) {
  #colnames(df) <- ifelse(colnames(df) %in% names(name_replacements),paste0(name_replacements[colnames(df)], "\n N (%)"), colnames(df)) 
  colnames(df)[colnames(df) == "trust"] <- "Trust"
  colnames(df)[colnames(df) == "mistrust"] <- "mistrust"
  colnames(df)[colnames(df) == "NA."] <- "NA"
  
  colnames(df)[colnames(df) == "Healthcare.Professionals"] <- "Healthcare Professionals"
  colnames(df)[colnames(df) == "Healthcare.Institutions"] <- "Healthcare Institutions"
  colnames(df)[colnames(df) == "Insurance.Providers"] <- "Insurance Providers"
  colnames(df)[colnames(df) == "Medical.Science"] <- "Medical Science"
  
  colnames(df)[colnames(df) == "Communication"] <- "Communication"
  colnames(df)[colnames(df) == "Disregard.for.Patient.Concerns"] <- "Disregard Pt. Concerns"
  colnames(df)[colnames(df) == "Lack.of.Trust.in.Medical.Procedures"] <- "Lack of Trust in Med. Proc."
  colnames(df)[colnames(df) == "Perceived.Incompetence.of.Medical.Management"] <- "Perceived Incompetence"
  colnames(df)[colnames(df) == "Profit.Driven.Medical.Management"] <- "Profit Driven"
  df <- relocate(df, "cramers_v", .after = "padj_formatted")
  colnames(df)[colnames(df)=="padj_formatted"] <- "Adjusted P value"
  colnames(df)[colnames(df)=="cramers_v"] <- "Cramér's V "
  return(df)
}

get_chisq_results <- function(mat, group) {
  
  total_per_groups = colSums(mat)  # Get total count per group
  
  # Convert raw counts into presence vs. absence format
  mat_presence_absence <- t(apply(mat, 1, function(row) {
    n_other = total_per_groups - row  # Calculate non-emotion occurrences
    c(row, n_other)  # Combine presence and absence counts
  }))
  
  # Add row and column names
  rownames(mat_presence_absence) <- rownames(mat)
  cols_mat <- colnames(mat)
  colnames(mat_presence_absence) <- c(paste0(cols_mat, "_true"), paste0(cols_mat,"_other"))
  
  # Perform Chi-Square test for each tone (row)
  presence_absence_results <- apply(mat_presence_absence, 1, function(row) {
    cur_mat <- matrix(row, nrow=2, byrow=TRUE)
    test <- chisq.test(cur_mat) 
    cramers_v <- rcompanion::cramerV(cur_mat)[[1]]
    return(c(Chi2 = test$statistic, p_value = test$p.value, cramers_v = cramers_v))
  })
  
  # Convert results to a data frame
  presence_absence_results <- as.data.frame(t(presence_absence_results))
  presence_absence_results$Tone <- rownames(mat_presence_absence)
  presence_absence_results$group <- group
  # Print results
  
  return(presence_absence_results)
  
}


p_vals <-  get_chisq_results(mat2, "2")
p_vals <- p_vals$p_value

format_pvals <- function(p_vals) {
  df <- data.frame(pvals= p_vals, 
                   pstring = p_vals)
  
  df$pstring[df$pvals >0.99] <- ">0.99"
  df$pstring[df$pvals <.0001] <- "<.0001"
  is.na(p_vals)
  df$pstring[df$pvals>=.0001 & df$pvals <=0.99 & !is.na(p_vals)] <- round(df$pvals[df$pstring>=.0001 & df$pvals <=0.99 & !is.na(p_vals)], 2)
  return(df$pstring)
}

getTable <- function(matdf, chi_res){
  mer <- merge(matdf, chi_res, by = "Tone")
  cols <- colnames(mer)
  cols <- cols[!cols %in% c("Tone",  "Chi2.X-squared", "p_value","group","p.adj", "cramers_v") ]
  rowsums <- rowSums(mer[,cols])
  
  for (col in cols) {
    print(col)
    n_total <- sum(mer[,col])
    percs <- round(100*mer[,col] / n_total, 1)
    mer[,col] <- paste0(mer[,col], " (",percs, ")" )
    mer$padj_formatted <- format_pvals(mer$p.adj)
  }
  mer <- mer[order(rowsums, decreasing = T),]
  mer <- mer %>% 
    select(all_of(c("Tone", cols, "Chi2.X-squared", "padj_formatted", "cramers_v")))
  return(mer)
}




#####
setwd("..")

detailed <- read.csv("../Tone_eval_all_vBatch_1.csv")

main <- read.csv("../B_eval_all_vBatch_1.csv")

main$postid <- str_before_first(str_after_first(main$custom_id, "jsonl_"), "_")
detailed$postid <- str_before_first(str_after_first(detailed$custom_id, "jsonl_"), "_")
length(intersect(detailed$postid, main$postid))

merged <- merge(main%>% select(-tone), detailed, by = "postid")
data <- merged
data$mistrust_trust_NA[data$mistrust_trust_NA == ""] <- "NA"
mistrust <- data %>% 
  filter(mistrust_trust_NA == "mistrust")

ggplot(merged, aes(y=tone, fill = mistrust_trust_NA)) + 
  geom_bar(position = "fill")


ggplot(merged%>% filter(mistrust_trust_NA=="mistrust"),
         aes(y="tone", fill = tone)) + 
  geom_bar()


### TONES
counts <- table(data$tone, data$mistrust_trust_NA) %>% data.frame()
mat <- pivot_wider(counts, names_from = Var2, values_from = Freq) %>% data.frame()
rownames(mat) <- mat$Var1
mat$Var1 <- NULL
chi_result <- chisq.test(mat)
print(chi_result)
mat1 <- mat

counts <- table(mistrust$tone, mistrust$object_of_mistrust_category) %>% data.frame()
mat <- pivot_wider(counts, names_from = Var2, values_from = Freq) %>% data.frame()
rownames(mat) <- mat$Var1
mat$Var1 <- NULL
mat$Other <- NULL
chi_result <- chisq.test(mat)
print(chi_result)
mat2 <- mat


counts <- table(mistrust$tone, mistrust$mistrust_reason_category) %>% data.frame()
mat <- pivot_wider(counts, names_from = Var2, values_from = Freq) %>% data.frame()
rownames(mat) <- mat$Var1
mat$Var1 <- NULL
mat$Other <- NULL
chi_result <- chisq.test(mat)
print(chi_result)

chisqs <- do.call("rbind", list(get_chisq_results(mat1, "1"),
                      get_chisq_results(mat2, "2"),
                      get_chisq_results(mat3, "3")))

chisqs$p.adj <- p.adjust(chisqs$p_value, method="bonferroni")
table(chisqs$p.adj< 0.05)
mat1df <- data.frame(mat1)
mat2df <- data.frame(mat2)
mat3df <- data.frame(mat3)
mat1df$Tone<- rownames(mat1df)
mat2df$Tone<- rownames(mat2df)
mat3df$Tone<- rownames(mat3df)

chisqs %>% 
  filter(group=="2")

hist(p_vals)
table1 <- getTable(mat1df, chisqs %>% 
                     filter(group=="1"))
table2 <- getTable(mat2df, chisqs %>% 
                     filter(group=="2"))
table3 <- getTable(mat3df, chisqs %>%
                     filter(group=="3"))

plot(table1$cramers_v)
plot(table2$cramers_v)
plot(table3$cramers_v)

colnames(table1)
colnames(table2)
colnames(table3)
# Apply the function to both tables
table1 <- rename_and_format_cols(table1)
table2 <- rename_and_format_cols(table2)
table3 <- rename_and_format_cols(table3)

writexl::write_xlsx(table1, "Table 1.xlsx")
writexl::write_xlsx(table2, "Table 2.xlsx")
writexl::write_xlsx(table3, "Table 3.xlsx")
#






#
mat_perc <- apply(mat1, 2, function(x) x / sum(x))
print(mat_perc)
mat_perc <- mat_perc * 100
colnames(mat_perc)
colnames(mat_perc) <- c("Mistrust", "NA", "Trust")
pdf(file = "Fig4_Tone_MistrustTrust_V2.pdf", width = 2.5, height= 2)
ht1 <- Heatmap(mat_perc, 
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
ht1 <- Heatmap(mat_perc, 
               name = "Distribution\n[%]",
               col = viridis(10, option="inferno", direction = -1), 
               row_names_gp = gpar(fontsize =6), 
               column_names_gp = gpar(fontsize=6),
               heatmap_legend_param = list(
                 title_gp = gpar(fontsize = 6),                # Font size for the legend title
                 labels_gp = gpar(fontsize = 6)                # Font size for legend labels
               ),
               column_names_rot =45, 
               show_column_names = F
)
draw(ht1)
dev.off()




mat_perc <- apply(mat2, 2, function(x) x / sum(x))
print(mat_perc)
mat_perc <- mat_perc * 100
colnames(mat_perc)
colnames(mat_perc) <- c(
                  "Healthcare Institutions"  ,
                  "Healthcare Professionals", 
                   "Insurance Providers"  , 
                   "Medical Science")

pdf(file = "Fig4b_Tone_MistrustTrust_V2.pdf", width = 2.5, height= 2)
ht2 <- Heatmap(mat_perc, 
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
ht2 <- Heatmap(mat_perc, 
               name = "Distribution\n[%]",
               col = viridis(10, option="inferno", direction = -1), 
               row_names_gp = gpar(fontsize =6), 
               column_names_gp = gpar(fontsize=6),
               heatmap_legend_param = list(
                 title_gp = gpar(fontsize = 6),                # Font size for the legend title
                 labels_gp = gpar(fontsize = 6)                # Font size for legend labels
               ),
               column_names_rot =45, 
               show_column_names = F
)
draw(ht2)
dev.off()






mat_perc <- apply(mat3, 2, function(x) x / sum(x))
print(mat_perc)
mat_perc <- mat_perc * 100
colnames(mat_perc)
# colnames(mat_perc) <- c(
#   "Communication",
#   "Disregard for Patient Concerns",
#   "Perceived Incompetence of Medical Management",
#   "Profit Driven Medical Management",
#   "Lack of Trust in Medical Procedures"
# )

colnames(mat_perc) <- c(
  "Communication",
  "Disregard Pt. Concerns",
  "Lack of Trust in Med. Proc.",
  "Perceived Incompetence",
  "Profit Driven"
)

mat <- mat * 100
pdf(file = "Fig4c_Tone_MistrustTrust_V2.pdf", width = 2.5, height= 2)
ht3 <- Heatmap(mat_perc, 
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
ht3 <- Heatmap(mat_perc, 
               name = "Distribution\n[%]",
               col = viridis(10, option="inferno", direction = -1), 
               row_names_gp = gpar(fontsize =6), 
               column_names_gp = gpar(fontsize=6),
               heatmap_legend_param = list(
                 title_gp = gpar(fontsize = 6),                # Font size for the legend title
                 labels_gp = gpar(fontsize = 6)                # Font size for legend labels
               ),
               column_names_rot =45, 
               show_column_names = F, 
               column_names_max_height = unit(10, "cm"),
               row_names_max_width = unit(10, "cm")
)
draw(ht3)
dev.off()



mat <- mat * 100
pdf(file = "Fig4c_Tone_MistrustTrust_V2_WIDE.pdf", width = 3.5, height= 5)
ht3 <- Heatmap(mat_perc, 
               name = "Distribution\n[%]",
               col = viridis(10, option="inferno", direction = -1), 
               row_names_gp = gpar(fontsize =6), 
               column_names_gp = gpar(fontsize=6),
               heatmap_legend_param = list(
                 title_gp = gpar(fontsize = 6),                # Font size for the legend title
                 labels_gp = gpar(fontsize = 6)                # Font size for legend labels
               ),
               column_names_rot =45,
               column_names_max_height = unit(10, units = "cm"),
               row_names_max_width = unit(10, units = "cm")
               
)
draw(ht3+ht3)
ht3 <- Heatmap(mat_perc, 
               name = "Distribution\n[%]",
               col = viridis(10, option="inferno", direction = -1), 
               row_names_gp = gpar(fontsize =6), 
               column_names_gp = gpar(fontsize=6),
               heatmap_legend_param = list(
                 title_gp = gpar(fontsize = 6),                # Font size for the legend title
                 labels_gp = gpar(fontsize = 6)                # Font size for legend labels
               ),
               column_names_rot =45, 
               show_column_names = F
)
draw(ht3)
dev.off()
